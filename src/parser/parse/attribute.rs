use std::borrow::Cow;

use crate::{
    error::XmlParserErrors,
    libxml::{
        chvalid::xml_is_char,
        parser_internals::{XML_MAX_HUGE_LENGTH, XML_MAX_TEXT_LENGTH},
    },
    parser::{
        XML_SUBSTITUTE_REF, XmlParserCtxt, XmlParserInputState, XmlParserOption, check_language_id,
        xml_err_memory, xml_fatal_err, xml_fatal_err_msg, xml_fatal_err_msg_str, xml_warning_msg,
    },
    tree::{NodeCommon, XML_ENT_CHECKED, XmlEntityType},
};

/// Normalize the space in non CDATA attribute values:
/// If the attribute type is not CDATA, then the XML processor MUST further
/// process the normalized attribute value by discarding any leading and
/// trailing space (#x20) characters, and by replacing sequences of space
/// (#x20) characters by a single space (#x20) character.
/// Note that the size of dst need to be at least src, and if one doesn't need
/// to preserve dst (and it doesn't come from a dictionary or read-only) then
/// passing src as dst is just fine.
///
/// If some conversion occurs, return the original string wrapped `Cow::Borrowed`.  
/// Otherwise, return the modified string wrapped `Cow::Owned`.
#[doc(alias = "xmlAttrNormalizeSpace")]
pub(crate) fn attr_normalize_space(mut src: &str) -> Cow<'_, str> {
    src = src.trim_matches(' ');
    if !src.contains("  ") {
        return Cow::Borrowed(src);
    }
    let mut dst = String::with_capacity(src.len());
    let mut src = src.chars().peekable();
    while let Some(b) = src.next() {
        if b == ' ' {
            // reduce single spaces
            while src.next_if(|&b| b == ' ').is_some() {}
            if src.peek().is_some() {
                dst.push(' ');
            }
        } else {
            dst.push(b);
        }
    }
    Cow::Owned(dst)
}

impl XmlParserCtxt {
    /// Parse a value for an attribute.
    ///
    /// # Note
    /// If no normalization is needed, the routine will return pointers directly from the data buffer.
    ///
    /// 3.3.3 Attribute-Value Normalization:
    /// Before the value of an attribute is passed to the application or
    /// checked for validity, the XML processor must normalize it as follows:
    /// - a character reference is processed by appending the referenced
    ///   character to the attribute value
    /// - an entity reference is processed by recursively processing the
    ///   replacement text of the entity
    /// - a whitespace character (#x20, #xD, #xA, #x9) is processed by
    ///   appending #x20 to the normalized value, except that only a single
    ///   #x20 is appended for a "#xD#xA" sequence that is part of an external
    ///   parsed entity or the literal entity value of an internal parsed entity
    /// - other characters are processed by appending them to the normalized value
    ///   If the declared value is not CDATA, then the XML processor must further
    ///   process the normalized attribute value by discarding any leading and
    ///   trailing space (#x20) characters, and by replacing sequences of space
    ///   (#x20) characters by a single space (#x20) character.
    ///   All attributes for which no declaration has been read should be treated
    ///   by a non-validating parser as if declared CDATA.
    ///
    /// Returns the AttValue parsed or NULL. The value has to be freed by the
    ///     caller if it was copied, this can be detected by val[*len] == 0.
    #[doc(alias = "xmlParseAttValueInternal")]
    pub(crate) unsafe fn parse_att_value_internal(&mut self, normalize: bool) -> Option<String> {
        unsafe {
            let max_length = if self.options & XmlParserOption::XmlParseHuge as i32 != 0 {
                XML_MAX_HUGE_LENGTH
            } else {
                XML_MAX_TEXT_LENGTH
            };

            macro_rules! GROW_PARSE_ATT_VALUE_INTERNAL {
                ($self:expr, $input:expr, $start:expr) => {
                    let diff = $start.len() - $input.len();
                    $self.grow();
                    if matches!($self.instate, XmlParserInputState::XmlParserEOF) {
                        return None;
                    }
                    $start = $self.content_bytes();
                    $input = &$start[diff..];
                };
            }

            self.grow();
            let input = self.content_bytes();
            let mut line: i32 = self.input().unwrap().line;
            let mut col: i32 = self.input().unwrap().col;
            if input.is_empty() || (input[0] != b'"' && input[0] != b'\'') {
                xml_fatal_err(self, XmlParserErrors::XmlErrAttributeNotStarted, None);
                return None;
            }
            self.instate = XmlParserInputState::XmlParserAttributeValue;
            let mut input = self.content_bytes();

            // try to handle in this routine the most common case where no
            // allocation of a new string is required and where content is pure ASCII.
            let limit = input[0];
            input = &input[1..];
            col += 1;
            let mut start = input;
            if input.is_empty() {
                GROW_PARSE_ATT_VALUE_INTERNAL!(self, input, start);
            }
            let (last, consumed) = if normalize {
                let mut trimmed = 0;
                // Skip any leading spaces
                while !input.is_empty()
                    && input[0] != limit
                    && matches!(input[0], b'\x20' | b'\t' | b'\n' | b'\r')
                {
                    if input[0] == 0xA {
                        line += 1;
                        col = 1;
                    } else {
                        col += 1;
                    }
                    input = &input[1..];
                    trimmed += 1;
                    start = input;
                    if input.is_empty() {
                        GROW_PARSE_ATT_VALUE_INTERNAL!(self, input, start);
                        if start.len() - input.len() > max_length {
                            xml_fatal_err_msg(
                                self,
                                XmlParserErrors::XmlErrAttributeNotFinished,
                                "AttValue length too long\n",
                            );
                            return None;
                        }
                    }
                }
                while !input.is_empty()
                    && input[0] != limit
                    && input[0] != b'&'
                    && input[0] != b'<'
                    && matches!(input[0], b'\x20'..=b'\x7F')
                {
                    col += 1;
                    let now = input[0];
                    input = &input[1..];
                    if now == b'\x20' && !input.is_empty() && input[0] == b'\x20' {
                        break;
                    }
                    if input.is_empty() {
                        GROW_PARSE_ATT_VALUE_INTERNAL!(self, input, start);
                        if start.len() - input.len() > max_length {
                            xml_fatal_err_msg(
                                self,
                                XmlParserErrors::XmlErrAttributeNotFinished,
                                "AttValue length too long\n",
                            );
                            return None;
                        }
                    }
                }
                // skip the trailing blanks
                let mut len = start.len() - input.len();
                while len > 0 && start[len - 1] == b'\x20' {
                    len -= 1;
                }
                let mut last = &start[len..];
                while !input.is_empty()
                    && input[0] != limit
                    && matches!(input[0], b'\x20' | b'\t' | b'\n' | b'r')
                {
                    if input[0] == 0xA {
                        line += 1;
                        col = 1;
                    } else {
                        col += 1;
                    }
                    input = &input[1..];
                    if input.is_empty() {
                        let diff = start.len() - input.len();
                        let diffl = start.len() - last.len();
                        self.grow();
                        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                            return None;
                        }
                        start = self.content_bytes();
                        input = &start[diff..];
                        last = &start[diffl..];

                        if start.len() - input.len() > max_length {
                            xml_fatal_err_msg(
                                self,
                                XmlParserErrors::XmlErrAttributeNotFinished,
                                "AttValue length too long\n",
                            );
                            return None;
                        }
                    }
                }
                if start.len() - input.len() > max_length {
                    xml_fatal_err_msg(
                        self,
                        XmlParserErrors::XmlErrAttributeNotFinished,
                        "AttValue length too long\n",
                    );
                    return None;
                }
                if input.is_empty() || input[0] != limit {
                    return self.parse_att_value_complex(normalize);
                }
                (last, trimmed + start.len() - input.len())
            } else {
                while !input.is_empty()
                    && input[0] != limit
                    && input[0] != b'&'
                    && input[0] != b'<'
                    && matches!(input[0], b'\x20'..=b'\x7F')
                {
                    input = &input[1..];
                    col += 1;
                    if input.is_empty() {
                        GROW_PARSE_ATT_VALUE_INTERNAL!(self, input, start);
                        if start.len() - input.len() > max_length {
                            xml_fatal_err_msg(
                                self,
                                XmlParserErrors::XmlErrAttributeNotFinished,
                                "AttValue length too long\n",
                            );
                            return None;
                        }
                    }
                }
                if start.len() - input.len() > max_length {
                    xml_fatal_err_msg(
                        self,
                        XmlParserErrors::XmlErrAttributeNotFinished,
                        "AttValue length too long\n",
                    );
                    return None;
                }
                if input.is_empty() || input[0] != limit {
                    return self.parse_att_value_complex(normalize);
                }
                (input, start.len() - input.len())
            };
            col += 1;
            let len = start.len() - last.len();
            #[allow(unused_unsafe)]
            let ret = unsafe {
                // # Safety
                // If we can reach here, UTF-8 validation will never fail
                // because `start` contains only ASCII characters.
                String::from_utf8_unchecked(start[..len].to_vec())
            };
            // consumed : the length of parsed value (and trimmed spaces if `normalize` is `true`)
            // 1        : the head of `limit`
            // 1        : the tail of `limit`
            self.advance(consumed + 2);
            let input = self.input_mut().unwrap();
            input.line = line;
            input.col = col;
            Some(ret)
        }
    }

    /// Parse a value for an attribute, this is the fallback function
    /// of xmlParseAttValue() when the attribute parsing requires handling
    /// of non-ASCII characters, or normalization compaction.
    ///
    /// Returns the AttValue parsed or NULL. The value has to be freed by the caller.
    #[doc(alias = "xmlParseAttValueComplex")]
    unsafe fn parse_att_value_complex(&mut self, normalize: bool) -> Option<String> {
        unsafe {
            let max_length = if self.options & XmlParserOption::XmlParseHuge as i32 != 0 {
                XML_MAX_HUGE_LENGTH
            } else {
                XML_MAX_TEXT_LENGTH
            };
            let mut l: i32 = 0;
            let mut in_space: i32 = 0;

            let limit = if self.current_byte() == b'"' {
                self.instate = XmlParserInputState::XmlParserAttributeValue;
                self.skip_char();
                b'"'
            } else if self.current_byte() == b'\'' {
                self.instate = XmlParserInputState::XmlParserAttributeValue;
                self.skip_char();
                b'\''
            } else {
                xml_fatal_err(self, XmlParserErrors::XmlErrAttributeNotStarted, None);
                return None;
            };

            // allocate a translation buffer.
            let mut buf = String::with_capacity(100);
            // OK loop until we reach one of the ending c_char or a size limit.
            let mut c = self.current_char(&mut l).unwrap_or('\0');
            while self.current_byte() != limit
                && xml_is_char(c as u32)
                && c != '<'
                && !matches!(self.instate, XmlParserInputState::XmlParserEOF)
            {
                if c == '&' {
                    in_space = 0;
                    if self.nth_byte(1) == b'#' {
                        let val = self.parse_char_ref();
                        if val == Some('&') {
                            if self.replace_entities != 0 {
                                buf.push('&');
                            } else {
                                // The reparsing will be done in xmlStringGetNodeList()
                                // called by the attribute() function in SAX.c
                                buf.push_str("&#38;");
                            }
                        } else if let Some(val) = val {
                            buf.push(val);
                        }
                    } else {
                        let ent = self.parse_entity_ref();
                        if let Some(ent) = ent.filter(|ent| {
                            matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity)
                        }) {
                            let content = ent.content.as_deref().unwrap();
                            if self.replace_entities == 0 && content.starts_with('&') {
                                buf.push_str("&#38;");
                            } else {
                                buf.push_str(content);
                            }
                        } else if let Some(ent) = ent.filter(|_| self.replace_entities != 0) {
                            if !matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity) {
                                if self.parser_entity_check(ent.length as _) != 0 {
                                    return None;
                                }

                                self.depth += 1;
                                let rep = ent.content.as_deref().and_then(|content| {
                                    self.string_decode_entities_int(
                                        content,
                                        XML_SUBSTITUTE_REF as _,
                                        '\0',
                                        '\0',
                                        '\0',
                                        1,
                                    )
                                });
                                self.depth -= 1;
                                if let Some(rep) = rep {
                                    for c in rep.chars() {
                                        if c == '\r' || c == '\n' || c == '\t' {
                                            buf.push('\x20');
                                        } else {
                                            buf.push(c);
                                        }
                                    }
                                }
                            } else if let Some(content) = ent.content.as_deref() {
                                buf.push_str(content);
                            }
                        } else if let Some(mut ent) = ent {
                            // We also check for recursion and amplification
                            // when entities are not substituted. They're
                            // often expanded later.
                            if !matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity)
                                && ent.content.is_some()
                            {
                                if ent.flags & XML_ENT_CHECKED as i32 == 0 {
                                    let old_copy: u64 = self.sizeentcopy;

                                    self.sizeentcopy = ent.length as _;

                                    self.depth += 1;
                                    let rep = self.string_decode_entities_int(
                                        ent.content.as_deref().unwrap(),
                                        XML_SUBSTITUTE_REF as _,
                                        '\0',
                                        '\0',
                                        '\0',
                                        1,
                                    );
                                    self.depth -= 1;

                                    // If we're parsing DTD content, the entity
                                    // might reference other entities which
                                    // weren't defined yet, so the check isn't
                                    // reliable.
                                    if self.in_subset == 0 {
                                        ent.flags |= XML_ENT_CHECKED as i32;
                                        ent.expanded_size = self.sizeentcopy;
                                    }

                                    if rep.is_none() {
                                        ent.content = Some(Cow::Borrowed(""));
                                    }

                                    if self.parser_entity_check(old_copy) != 0 {
                                        return None;
                                    }
                                } else if self.parser_entity_check(ent.expanded_size) != 0 {
                                    return None;
                                }
                            }

                            // Just output the reference
                            buf.push('&');
                            buf.push_str(&ent.name().unwrap());
                            buf.push(';');
                        }
                    }
                } else {
                    if c == '\u{20}' || c == '\u{D}' || c == '\u{A}' || c == '\u{9}' {
                        if !buf.is_empty() || !normalize {
                            if !normalize || in_space == 0 {
                                buf.push('\x20');
                            }
                            in_space = 1;
                        }
                    } else {
                        in_space = 0;
                        buf.push(c);
                    }
                    self.advance_with_line_handling(l as usize);
                }
                self.grow();
                c = self.current_char(&mut l).unwrap_or('\0');
                if buf.len() > max_length {
                    xml_fatal_err_msg(
                        self,
                        XmlParserErrors::XmlErrAttributeNotFinished,
                        "AttValue length too long\n",
                    );
                    xml_err_memory(Some(self), None);
                    return None;
                }
            }
            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                return None;
            }

            if in_space != 0 && normalize {
                while buf.ends_with('\x20') {
                    buf.pop();
                }
            }
            if self.current_byte() == b'<' {
                xml_fatal_err(self, XmlParserErrors::XmlErrLtInAttribute, None);
            } else if self.current_byte() != limit {
                if c != '\0' && !xml_is_char(c as u32) {
                    xml_fatal_err_msg(
                        self,
                        XmlParserErrors::XmlErrInvalidChar,
                        "invalid character in attribute value\n",
                    );
                } else {
                    xml_fatal_err_msg(
                        self,
                        XmlParserErrors::XmlErrAttributeNotFinished,
                        "AttValue: ' expected\n",
                    );
                }
            } else {
                self.skip_char();
            }

            Some(buf)
        }
    }

    /// parse a value for an attribute
    /// Note: the parser won't do substitution of entities here, this
    /// will be handled later in xmlStringGetNodeList
    ///
    /// ```text
    /// [10] AttValue ::= '"' ([^<&"] | Reference)* '"' | "'" ([^<&'] | Reference)* "'"
    /// ```
    ///
    /// # 3.3.3 Attribute-Value Normalization:
    /// Before the value of an attribute is passed to the application or
    /// checked for validity, the XML processor must normalize it as follows:
    /// - a character reference is processed by appending the referenced
    ///   character to the attribute value
    /// - an entity reference is processed by recursively processing the
    ///   replacement text of the entity
    /// - a whitespace character (#x20, #xD, #xA, #x9) is processed by
    ///   appending #x20 to the normalized value, except that only a single
    ///   #x20 is appended for a "#xD#xA" sequence that is part of an external
    ///   parsed entity or the literal entity value of an internal parsed entity
    /// - other characters are processed by appending them to the normalized value
    ///   If the declared value is not CDATA, then the XML processor must further
    ///   process the normalized attribute value by discarding any leading and
    ///   trailing space (#x20) characters, and by replacing sequences of space
    ///   (#x20) characters by a single space (#x20) character.
    ///   All attributes for which no declaration has been read should be treated
    ///   by a non-validating parser as if declared CDATA.
    ///
    /// Returns the AttValue parsed or NULL. The value has to be freed by the caller.
    #[doc(alias = "xmlParseAttValue")]
    pub(crate) unsafe fn parse_att_value(&mut self) -> Option<String> {
        unsafe {
            self.input()?;
            self.parse_att_value_internal(false)
        }
    }

    /// Parse an attribute
    ///
    /// ```text
    /// [41] Attribute ::= Name Eq AttValue
    ///
    /// [ WFC: No External Entity References ]
    /// Attribute values cannot contain direct or indirect entity references
    /// to external entities.
    ///
    /// [ WFC: No < in Attribute Values ]
    /// The replacement text of any entity referred to directly or indirectly in
    /// an attribute value (other than "&lt;") must not contain a <.
    ///
    /// [ VC: Attribute Value Type ]
    /// The attribute must have been declared; the value must be of the type declared for it.
    ///
    /// [25] Eq ::= S? '=' S?
    ///
    /// With namespace:
    /// [NS 11] Attribute ::= QName Eq AttValue
    /// ```
    ///
    /// Also the case QName == xmlns:??? is handled independently as a namespace definition.
    ///
    /// Returns (Name, AttValue).
    #[doc(alias = "xmlParseAttribute")]
    #[cfg(feature = "sax1")]
    pub(crate) unsafe fn parse_attribute(&mut self) -> (Option<String>, Option<String>) {
        unsafe {
            self.grow();
            let Some(name) = self.parse_name() else {
                xml_fatal_err_msg(
                    self,
                    XmlParserErrors::XmlErrNameRequired,
                    "error parsing attribute name\n",
                );
                return (None, None);
            };

            // read the value
            self.skip_blanks();
            if self.current_byte() != b'=' {
                xml_fatal_err_msg_str!(
                    self,
                    XmlParserErrors::XmlErrAttributeWithoutValue,
                    "Specification mandates value for attribute {}\n",
                    name
                );
                return (Some(name), None);
            }

            self.skip_char();
            self.skip_blanks();
            let val = self.parse_att_value().unwrap();
            self.instate = XmlParserInputState::XmlParserContent;

            // Check that xml:lang conforms to the specification
            // No more registered as an error, just generate a warning now
            // since this was deprecated in XML second edition
            if self.pedantic != 0 && name == "xml:lang" && !check_language_id(&val) {
                xml_warning_msg!(
                    self,
                    XmlParserErrors::XmlWarLangValue,
                    "Malformed value for xml:lang : {}\n",
                    val
                );
            }

            // Check that xml:space conforms to the specification
            if name == "xml:space" {
                if val == "default" {
                    *self.space_mut() = 0;
                } else if val == "preserve" {
                    *self.space_mut() = 1;
                } else {
                    xml_warning_msg!(
                        self,
                        XmlParserErrors::XmlWarSpaceValue,
                        "Invalid value \"{}\" for xml:space : \"default\" or \"preserve\" expected\n",
                        val
                    );
                }
            }

            (Some(name), Some(val))
        }
    }

    /// Returns (Prefix, LocalPart, AttValue).
    #[doc(alias = "xmlParseAttribute2")]
    pub(crate) unsafe fn parse_attribute2(
        &mut self,
        pref: Option<&str>,
        elem: &str,
    ) -> Option<(Option<String>, String, Option<String>)> {
        unsafe {
            let mut normalize = false;

            self.grow();

            let (prefix, Some(name)) = self.parse_qname() else {
                xml_fatal_err_msg(
                    self,
                    XmlParserErrors::XmlErrNameRequired,
                    "error parsing attribute name\n",
                );
                return None;
            };

            // get the type if needed
            if let Some(atts) = self.atts_special {
                if atts
                    .qlookup2(pref, elem, prefix.as_deref(), Some(&name))
                    .is_some()
                {
                    normalize = true;
                }
            }

            // read the value
            self.skip_blanks();

            if self.current_byte() != b'=' {
                xml_fatal_err_msg_str!(
                    self,
                    XmlParserErrors::XmlErrAttributeWithoutValue,
                    "Specification mandates value for attribute {}\n",
                    name
                );
                return Some((None, name, None));
            }
            self.skip_char();
            self.skip_blanks();
            let mut val = self.parse_att_value_internal(normalize)?;
            if normalize {
                // Sometimes a second normalisation pass for spaces is needed
                // but that only happens if charrefs or entities references
                // have been used in the attribute value, i.e. the attribute
                // value have been extracted in an allocated string already.
                let val2 = attr_normalize_space(&val);
                if val != val2 {
                    val = val2.into_owned();
                }
            }
            self.instate = XmlParserInputState::XmlParserContent;

            if prefix.as_deref() == self.str_xml.as_deref() {
                // Check that xml:lang conforms to the specification
                // No more registered as an error, just generate a warning now
                // since this was deprecated in XML second edition
                if self.pedantic != 0 && name == "lang" && !check_language_id(&val) {
                    xml_warning_msg!(
                        self,
                        XmlParserErrors::XmlWarLangValue,
                        "Malformed value for xml:lang : {}\n",
                        val
                    );
                }

                // Check that xml:space conforms to the specification
                if name == "space" {
                    if val == "default" {
                        *self.space_mut() = 0;
                    } else if val == "preserve" {
                        *self.space_mut() = 1;
                    } else {
                        xml_warning_msg!(
                            self,
                            XmlParserErrors::XmlWarSpaceValue,
                            "Invalid value \"{}\" for xml:space : \"default\" or \"preserve\" expected\n",
                            val
                        );
                    }
                }
            }

            Some((prefix, name, Some(val)))
        }
    }
}
