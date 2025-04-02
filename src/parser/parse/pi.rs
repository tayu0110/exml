#[cfg(feature = "catalog")]
use crate::libxml::catalog::{XML_CATALOG_PI, XmlCatalogAllow, xml_catalog_get_defaults};
use crate::{
    error::XmlParserErrors,
    libxml::parser_internals::{XML_MAX_HUGE_LENGTH, XML_MAX_TEXT_LENGTH},
    parser::{
        XmlParserCtxt, XmlParserInputState, XmlParserOption, xml_fatal_err, xml_fatal_err_msg,
        xml_fatal_err_msg_str, xml_is_char, xml_ns_err, xml_warning_msg,
    },
};

// List of XML prefixed PI allowed by W3C specs
const XML_W3_CPIS: &[&str] = &["xml-stylesheet", "xml-model"];

impl XmlParserCtxt {
    /// Parse the name of a PI
    ///
    /// ```text
    /// [17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
    /// ```
    ///
    /// Returns the PITarget name or NULL
    #[doc(alias = "xmlParsePITarget")]
    unsafe fn parse_pi_target(&mut self) -> Option<String> {
        unsafe {
            let name = self.parse_name()?;
            if name.as_bytes()[..name.len().min(3)].eq_ignore_ascii_case(b"xml") {
                if name == "xml" {
                    xml_fatal_err_msg(
                        self,
                        XmlParserErrors::XmlErrReservedXmlName,
                        "XML declaration allowed only at the start of the document\n",
                    );
                    return Some(name);
                } else if name.len() == 3 {
                    xml_fatal_err(self, XmlParserErrors::XmlErrReservedXmlName, None);
                    return Some(name);
                }
                if XML_W3_CPIS.iter().any(|&pi| pi == name) {
                    return Some(name);
                }
                xml_warning_msg!(
                    self,
                    XmlParserErrors::XmlErrReservedXmlName,
                    "xmlParsePITarget: invalid name prefix 'xml'\n"
                );
            }
            if name.contains(':') {
                xml_ns_err!(
                    self,
                    XmlParserErrors::XmlNsErrColon,
                    "colons are forbidden from PI names '{}'\n",
                    name
                );
            }
            Some(name)
        }
    }

    /// Parse an XML Catalog Processing Instruction.
    ///
    /// `<?oasis-xml-catalog catalog="http://example.com/catalog.xml"?>`
    ///
    /// Occurs only if allowed by the user and if happening in the Misc
    /// part of the document before any doctype information.  
    /// This will add the given catalog to the parsing context in order
    /// to be used if there is a resolution need further down in the document.
    #[doc(alias = "xmlParseCatalogPI")]
    #[cfg(feature = "catalog")]
    unsafe fn parse_catalog_pi(&mut self, catalog: &str) {
        unsafe {
            use crate::libxml::{catalog::XmlCatalogEntry, chvalid::xml_is_blank_char};

            macro_rules! syntax_error {
                () => {
                    xml_warning_msg!(
                        self,
                        XmlParserErrors::XmlWarCatalogPI,
                        "Catalog PI syntax error: {}\n",
                        catalog
                    );
                };
            }

            let mut tmp = catalog;
            tmp = tmp.trim_start_matches(|c| xml_is_blank_char(c as u32));
            let Some(rem) = tmp.strip_prefix("catalog") else {
                syntax_error!();
                return;
            };
            tmp = rem.trim_start_matches(|c| xml_is_blank_char(c as u32));
            let Some(rem) = tmp.strip_prefix('=') else {
                syntax_error!();
                return;
            };
            tmp = rem.trim_start_matches(|c| xml_is_blank_char(c as u32));
            let Some(quoto) = tmp.chars().next().filter(|&c| c == '\'' || c == '"') else {
                syntax_error!();
                return;
            };
            tmp = &tmp[1..];
            let Some((url, rem)) = tmp.split_once(quoto) else {
                syntax_error!();
                return;
            };
            tmp = rem.trim_start_matches(|c| xml_is_blank_char(c as u32));
            if !tmp.is_empty() {
                syntax_error!();
                return;
            }
            if let Some(catalogs) = self.catalogs.as_mut() {
                catalogs.add_local(url);
            } else {
                self.catalogs = Some(XmlCatalogEntry::new(url));
            }
        }
    }

    /// Parse an XML Processing Instruction.
    ///
    /// ```text
    /// [16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
    /// ```
    ///
    /// The processing is transferred to SAX once parsed.
    #[doc(alias = "xmlParsePI")]
    pub(crate) unsafe fn parse_pi(&mut self) {
        unsafe {
            let max_length = if self.options & XmlParserOption::XmlParseHuge as i32 != 0 {
                XML_MAX_HUGE_LENGTH
            } else {
                XML_MAX_TEXT_LENGTH
            };

            let mut buf = String::new();
            if self.content_bytes().starts_with(b"<?") {
                let inputid = self.input().unwrap().id;
                let state = self.instate;
                self.instate = XmlParserInputState::XmlParserPI;
                // this is a Processing Instruction.
                self.advance(2);

                // Parse the target name and check for special support like namespace.
                if let Some(target) = self.parse_pi_target() {
                    if self.content_bytes().starts_with(b"?>") {
                        if inputid != self.input().unwrap().id {
                            xml_fatal_err_msg(
                                self,
                                XmlParserErrors::XmlErrEntityBoundary,
                                "PI declaration doesn't start and stop in the same entity\n",
                            );
                        }
                        self.advance(2);

                        // SAX: PI detected.
                        if self.disable_sax == 0 {
                            if let Some(processing_instruction) = self
                                .sax
                                .as_deref_mut()
                                .and_then(|sax| sax.processing_instruction)
                            {
                                processing_instruction(self.user_data.clone(), &target, None);
                            }
                        }
                        if !matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                            self.instate = state;
                        }
                        return;
                    }
                    if self.skip_blanks() == 0 {
                        xml_fatal_err_msg_str!(
                            self,
                            XmlParserErrors::XmlErrSpaceRequired,
                            "ParsePI: PI {} space expected\n",
                            target
                        );
                    }
                    while let Some(cur) = self.consume_char_if(|ctxt, c| {
                        xml_is_char(c as u32) && (c != '?' || ctxt.nth_byte(1) != b'>')
                    }) {
                        buf.push(cur);
                        if buf.len() > max_length {
                            xml_fatal_err_msg_str!(
                                self,
                                XmlParserErrors::XmlErrPINotFinished,
                                "PI {} too big found",
                                target
                            );
                            self.instate = state;
                            return;
                        }
                    }
                    if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                        return;
                    }
                    let mut l = 0;
                    if self.current_char(&mut l).unwrap_or('\0') != '?' {
                        xml_fatal_err_msg_str!(
                            self,
                            XmlParserErrors::XmlErrPINotFinished,
                            "ParsePI: PI {} never end ...\n",
                            target
                        );
                    } else {
                        if inputid != self.input().unwrap().id {
                            xml_fatal_err_msg(
                                self,
                                XmlParserErrors::XmlErrEntityBoundary,
                                "PI declaration doesn't start and stop in the same entity\n",
                            );
                        }
                        self.advance(2);

                        #[cfg(feature = "catalog")]
                        if matches!(
                            state,
                            XmlParserInputState::XmlParserMisc
                                | XmlParserInputState::XmlParserStart
                        ) && target == XML_CATALOG_PI
                        {
                            let allow = xml_catalog_get_defaults();
                            if matches!(allow, XmlCatalogAllow::Document | XmlCatalogAllow::All) {
                                self.parse_catalog_pi(&buf);
                            }
                        }

                        // SAX: PI detected.
                        if self.disable_sax == 0 {
                            if let Some(processing_instruction) = self
                                .sax
                                .as_deref_mut()
                                .and_then(|sax| sax.processing_instruction)
                            {
                                processing_instruction(self.user_data.clone(), &target, Some(&buf));
                            }
                        }
                    }
                } else {
                    xml_fatal_err(self, XmlParserErrors::XmlErrPINotStarted, None);
                }
                if !matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                    self.instate = state;
                }
            }
        }
    }
}
