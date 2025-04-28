use std::{borrow::Cow, str::from_utf8_unchecked};

use crate::{
    encoding::XmlCharEncoding,
    error::XmlParserErrors,
    libxml::sax2::xml_sax2_get_entity,
    parser::{
        XmlParserCtxt, XmlParserInputState, XmlParserMode, XmlParserOption,
        parse_external_entity_private, xml_err_msg_str, xml_fatal_err, xml_fatal_err_msg,
        xml_fatal_err_msg_int, xml_fatal_err_msg_str, xml_is_char,
        xml_parse_balanced_chunk_memory_internal, xml_warning_msg,
    },
    tree::{
        NodeCommon, XML_ENT_CHECKED, XML_ENT_CHECKED_LT, XML_ENT_CONTAINS_LT, XML_ENT_EXPANDING,
        XML_ENT_PARSED, XmlElementType, XmlEntityPtr, XmlEntityType, XmlGenericNodePtr, XmlNodePtr,
        xml_doc_copy_node, xml_free_node_list, xml_get_predefined_entity,
    },
};

impl XmlParserCtxt {
    /// Parse a numeric character reference. Always consumes '&'.
    ///
    /// ```text
    /// [66] CharRef ::= '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'
    ///
    /// [ WFC: Legal Character ]
    /// Characters referred to using character references must match the production for Char.
    /// ```
    ///
    /// Returns the value parsed (as an int), 0 in case of error
    #[doc(alias = "xmlParseCharRef")]
    pub(crate) fn parse_char_ref(&mut self) -> Option<char> {
        let mut val = 0u32;
        let mut count = 0;

        // Using RAW/CUR/NEXT is okay since we are working on ASCII range here
        if self.content_bytes().starts_with(b"&#x") {
            self.advance(3);
            self.grow();
            while self.current_byte() != b';' {
                // loop blocked by count
                if count > 20 {
                    count = 0;
                    self.grow();
                    if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                        return None;
                    }
                } else {
                    count += 1;
                }
                let cur = self.current_byte();
                if cur.is_ascii_digit() {
                    val = val * 16 + (cur - b'0') as u32;
                } else if (b'a'..=b'f').contains(&cur) && count < 20 {
                    val = val * 16 + (cur - b'a') as u32 + 10;
                } else if (b'A'..=b'F').contains(&cur) && count < 20 {
                    val = val * 16 + (cur - b'A') as u32 + 10;
                } else {
                    xml_fatal_err(self, XmlParserErrors::XmlErrInvalidHexCharRef, None);
                    val = 0;
                    break;
                }
                val = val.min(0x110000);
                self.skip_char();
                count += 1;
            }
            if self.current_byte() == b';' {
                // on purpose to avoid reentrancy problems with NEXT and SKIP
                self.input_mut().unwrap().col += 1;
                self.input_mut().unwrap().cur += 1;
            }
        } else if self.current_byte() == b'&' && self.nth_byte(1) == b'#' {
            self.advance(2);
            self.grow();
            while self.current_byte() != b';' {
                // loop blocked by count
                if count > 20 {
                    count = 0;
                    self.grow();
                    if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                        return None;
                    }
                } else {
                    count += 1;
                }
                let cur = self.current_byte();
                if cur.is_ascii_digit() {
                    val = val * 10 + (cur - b'0') as u32;
                } else {
                    xml_fatal_err(self, XmlParserErrors::XmlErrInvalidDecCharRef, None);
                    val = 0;
                    break;
                }
                val = val.min(0x110000);
                self.skip_char();
                count += 1;
            }
            if self.current_byte() == b';' {
                // on purpose to avoid reentrancy problems with NEXT and SKIP
                self.input_mut().unwrap().col += 1;
                self.input_mut().unwrap().cur += 1;
            }
        } else {
            if self.current_byte() == b'&' {
                self.advance(1);
            }
            xml_fatal_err(self, XmlParserErrors::XmlErrInvalidCharRef, None);
        }

        // [ WFC: Legal Character ]
        // Characters referred to using character references must match the
        // production for Char.
        if val >= 0x110000 {
            xml_fatal_err_msg_int!(
                self,
                XmlParserErrors::XmlErrInvalidChar,
                "xmlParseCharRef: character reference out of bounds\n",
                val as i32
            );
        } else if xml_is_char(val) {
            return char::from_u32(val);
        } else {
            xml_fatal_err_msg_int!(
                self,
                XmlParserErrors::XmlErrInvalidChar,
                format!("xmlParseCharRef: invalid XmlChar value {val}\n").as_str(),
                val as i32
            );
        }
        None
    }

    /// Parse Reference declarations, variant parsing from a string rather
    /// than an an input flow.
    ///
    /// ```text
    /// [66] CharRef ::= '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'
    ///
    /// [ WFC: Legal Character ]
    /// Characters referred to using character references must match the production for Char.
    /// ```
    ///
    /// Returns the value parsed (as an int), 0 in case of error, str will be
    /// updated to the current value of the index
    #[doc(alias = "xmlParseStringCharRef")]
    pub(super) fn parse_string_char_ref<'a>(&mut self, s: &'a str) -> (Option<char>, &'a str) {
        let mut val = 0;

        let mut ptr = s;
        if let Some(rem) = ptr.strip_prefix("&#x") {
            ptr = rem;
            if let Some((dig, rem)) = ptr
                .split_once(';')
                .filter(|(dig, _)| dig.bytes().all(|b| b.is_ascii_hexdigit()))
            {
                val = u32::from_str_radix(dig, 16).unwrap_or(0x110000);
                ptr = rem;
            } else {
                xml_fatal_err(self, XmlParserErrors::XmlErrInvalidHexCharRef, None);
            }
        } else if let Some(rem) = ptr.strip_prefix("&#") {
            ptr = rem;
            if let Some((dig, rem)) = ptr
                .split_once(';')
                .filter(|(dig, _)| dig.bytes().all(|b| b.is_ascii_digit()))
            {
                val = dig.parse::<u32>().unwrap_or(0x110000);
                ptr = rem;
            } else {
                xml_fatal_err(self, XmlParserErrors::XmlErrInvalidDecCharRef, None);
            }
        } else {
            xml_fatal_err(self, XmlParserErrors::XmlErrInvalidCharRef, None);
            return (None, s);
        }

        // [ WFC: Legal Character ]
        // Characters referred to using character references must match the
        // production for Char.
        if val >= 0x110000 {
            xml_fatal_err_msg_int!(
                self,
                XmlParserErrors::XmlErrInvalidChar,
                "xmlParseStringCharRef: character reference out of bounds\n",
                val as i32
            );
        } else if xml_is_char(val) {
            return (Some(char::from_u32(val).expect("Internal Error")), ptr);
        } else {
            xml_fatal_err_msg_int!(
                self,
                XmlParserErrors::XmlErrInvalidChar,
                format!("xmlParseStringCharRef: invalid xmlChar value {val}\n").as_str(),
                val as i32
            );
        }
        (None, ptr)
    }

    /// Parse an entitiy reference. Always consumes '&'.
    ///
    /// ```text
    /// [68] EntityRef ::= '&' Name ';'
    ///
    /// [ WFC: Entity Declared ]
    /// In a document without any DTD, a document with only an internal DTD
    /// subset which contains no parameter entity references, or a document
    /// with "standalone='yes'", the Name given in the entity reference
    /// must match that in an entity declaration, except that well-formed
    /// documents need not declare any of the following entities: amp, lt,
    /// gt, apos, quot.  The declaration of a parameter entity must precede
    /// any reference to it.  Similarly, the declaration of a general entity
    /// must precede any reference to it which appears in a default value in an
    /// attribute-list declaration. Note that if entities are declared in the
    /// external subset or in external parameter entities, a non-validating
    /// processor is not obligated to read and process their declarations;
    /// for such documents, the rule that an entity must be declared is a
    /// well-formedness constraint only if standalone='yes'.
    ///
    /// [ WFC: Parsed Entity ]
    /// An entity reference must not contain the name of an unparsed entity
    /// ```
    ///
    /// Returns the xmlEntityPtr if found, or NULL otherwise.
    #[doc(alias = "xmlParseEntityRef")]
    pub(crate) fn parse_entity_ref(&mut self) -> Option<XmlEntityPtr> {
        self.grow();
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }

        if self.current_byte() != b'&' {
            return None;
        }
        self.skip_char();
        let Some(name) = self.parse_name() else {
            xml_fatal_err_msg(
                self,
                XmlParserErrors::XmlErrNameRequired,
                "xmlParseEntityRef: no name\n",
            );
            return None;
        };
        if self.current_byte() != b';' {
            xml_fatal_err(self, XmlParserErrors::XmlErrEntityRefSemicolMissing, None);
            return None;
        }
        self.skip_char();

        // Predefined entities override any extra definition
        if self.options & XmlParserOption::XmlParseOldSAX as i32 == 0 {
            if let Some(ent) = xml_get_predefined_entity(&name) {
                return Some(ent);
            }
        }

        let mut ent = None;
        // Ask first SAX for entity resolution, otherwise try the
        // entities which may have stored in the parser context.
        if let Some(sax) = self.sax.as_deref_mut() {
            if let Some(get_entity) = sax.get_entity {
                ent = get_entity(self, &name);
            }
            if self.well_formed
                && ent.is_none()
                && self.options & XmlParserOption::XmlParseOldSAX as i32 != 0
            {
                ent = xml_get_predefined_entity(&name);
            }
            if self.well_formed && ent.is_none() && self.user_data.is_none() {
                ent = xml_sax2_get_entity(self, &name);
            }
        }
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }
        // [ WFC: Entity Declared ]
        // In a document without any DTD, a document with only an
        // internal DTD subset which contains no parameter entity
        // references, or a document with "standalone='yes'", the
        // Name given in the entity reference must match that in an
        // entity declaration, except that well-formed documents
        // need not declare any of the following entities: amp, lt,
        // gt, apos, quot.
        // The declaration of a parameter entity must precede any
        // reference to it.
        // Similarly, the declaration of a general entity must
        // precede any reference to it which appears in a default
        // value in an attribute-list declaration. Note that if
        // entities are declared in the external subset or in
        // external parameter entities, a non-validating processor
        // is not obligated to read and process their declarations;
        // for such documents, the rule that an entity must be
        // declared is a well-formedness constraint only if
        // standalone='yes'.
        if let Some(mut ent) = ent {
            if matches!(ent.etype, XmlEntityType::XmlExternalGeneralUnparsedEntity) {
                // [ WFC: Parsed Entity ]
                // An entity reference must not contain the name of an unparsed entity
                xml_fatal_err_msg_str!(
                    self,
                    XmlParserErrors::XmlErrUnparsedEntity,
                    "Entity reference to unparsed entity {}\n",
                    name
                );
            } else if matches!(self.instate, XmlParserInputState::XmlParserAttributeValue)
                && matches!(ent.etype, XmlEntityType::XmlExternalGeneralParsedEntity)
            {
                // [ WFC: No External Entity References ]
                // Attribute values cannot contain direct or indirect
                // entity references to external entities.
                xml_fatal_err_msg_str!(
                    self,
                    XmlParserErrors::XmlErrEntityIsExternal,
                    "Attribute references external entity '{}'\n",
                    name
                );
            } else if matches!(self.instate, XmlParserInputState::XmlParserAttributeValue)
                && !matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity)
            {
                // [ WFC: No < in Attribute Values ]
                // The replacement text of any entity referred to directly or
                // indirectly in an attribute value (other than "&lt;") must not contain a <.
                if ent.flags & XML_ENT_CHECKED_LT as i32 == 0 {
                    if let Some(content) = ent.content.as_deref() {
                        if content.contains('<') {
                            ent.flags |= XML_ENT_CONTAINS_LT as i32;
                        }
                    }
                    ent.flags |= XML_ENT_CHECKED_LT as i32;
                }
                if ent.flags & XML_ENT_CONTAINS_LT as i32 != 0 {
                    xml_fatal_err_msg_str!(
                        self,
                        XmlParserErrors::XmlErrLtInAttribute,
                        "'<' in entity '{}' is not allowed in attributes values\n",
                        name
                    );
                }
            } else {
                // Internal check, no parameter entities here ...
                match ent.etype {
                    XmlEntityType::XmlInternalParameterEntity
                    | XmlEntityType::XmlExternalParameterEntity => {
                        xml_fatal_err_msg_str!(
                            self,
                            XmlParserErrors::XmlErrEntityIsParameter,
                            "Attempt to reference the parameter entity '{}'\n",
                            name
                        );
                    }
                    _ => {}
                }
            }
        } else {
            if self.standalone == 1 || (self.has_external_subset == 0 && self.has_perefs == 0) {
                xml_fatal_err_msg_str!(
                    self,
                    XmlParserErrors::XmlErrUndeclaredEntity,
                    "Entity '{}' not defined\n",
                    name
                );
            } else {
                xml_err_msg_str!(
                    self,
                    XmlParserErrors::XmlWarUndeclaredEntity,
                    "Entity '{}' not defined\n",
                    name
                );
                if self.in_subset == 0 {
                    if let Some(reference) = self.sax.as_deref_mut().and_then(|sax| sax.reference) {
                        reference(self, &name);
                    }
                }
            }
            self.valid = 0;
        }

        // [ WFC: No Recursion ]
        // A parsed entity must not contain a recursive reference
        // to itself, either directly or indirectly.
        // Done somewhere else
        ent
    }

    /// Parse ENTITY references declarations, but this version parses it from
    /// a string value.
    ///
    /// ```text
    /// [68] EntityRef ::= '&' Name ';'
    ///
    /// [ WFC: Entity Declared ]
    /// In a document without any DTD, a document with only an internal DTD
    /// subset which contains no parameter entity references, or a document
    /// with "standalone='yes'", the Name given in the entity reference
    /// must match that in an entity declaration, except that well-formed
    /// documents need not declare any of the following entities: amp, lt,
    /// gt, apos, quot.  The declaration of a parameter entity must precede
    /// any reference to it.  Similarly, the declaration of a general entity
    /// must precede any reference to it which appears in a default value in an
    /// attribute-list declaration. Note that if entities are declared in the
    /// external subset or in external parameter entities, a non-validating
    /// processor is not obligated to read and process their declarations;
    /// for such documents, the rule that an entity must be declared is a
    /// well-formedness constraint only if standalone='yes'.
    ///
    /// [ WFC: Parsed Entity ]
    /// An entity reference must not contain the name of an unparsed entity
    /// ```
    ///
    /// Returns the xmlEntityPtr if found, or NULL otherwise. The str pointer
    /// is updated to the current location in the string.
    #[doc(alias = "xmlParseStringEntityRef")]
    pub(super) fn parse_string_entity_ref<'a>(
        &mut self,
        s: &'a str,
    ) -> (Option<XmlEntityPtr>, &'a str) {
        let Some(ptr) = s.strip_prefix('&') else {
            return (None, s);
        };

        let (Some(name), rem) = self.parse_string_name(ptr) else {
            xml_fatal_err_msg(
                self,
                XmlParserErrors::XmlErrNameRequired,
                "xmlParseStringEntityRef: no name\n",
            );
            return (None, ptr);
        };
        let Some(ptr) = rem.strip_prefix(';') else {
            xml_fatal_err(self, XmlParserErrors::XmlErrEntityRefSemicolMissing, None);
            return (None, rem);
        };

        let mut ent = None;
        // Predefined entities override any extra definition
        if self.options & XmlParserOption::XmlParseOldSAX as i32 == 0 {
            ent = xml_get_predefined_entity(name);
            if ent.is_some() {
                return (ent, ptr);
            }
        }

        // Ask first SAX for entity resolution, otherwise try the
        // entities which may have stored in the parser context.
        if let Some(sax) = self.sax.as_deref_mut() {
            if let Some(get_entity) = sax.get_entity {
                ent = get_entity(self, name);
            }
            if ent.is_none() && self.options & XmlParserOption::XmlParseOldSAX as i32 != 0 {
                ent = xml_get_predefined_entity(name);
            }
            if ent.is_none() && self.user_data.is_none() {
                ent = xml_sax2_get_entity(self, name);
            }
        }
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            return (None, s);
        }

        // [ WFC: Entity Declared ]
        // In a document without any DTD, a document with only an
        // internal DTD subset which contains no parameter entity
        // references, or a document with "standalone='yes'", the
        // Name given in the entity reference must match that in an
        // entity declaration, except that well-formed documents
        // need not declare any of the following entities: amp, lt,
        // gt, apos, quot.
        // The declaration of a parameter entity must precede any
        // reference to it.
        // Similarly, the declaration of a general entity must
        // precede any reference to it which appears in a default
        // value in an attribute-list declaration. Note that if
        // entities are declared in the external subset or in
        // external parameter entities, a non-validating processor
        // is not obligated to read and process their declarations;
        // for such documents, the rule that an entity must be
        // declared is a well-formedness constraint only if
        // standalone='yes'.
        if let Some(mut ent) = ent {
            if matches!(ent.etype, XmlEntityType::XmlExternalGeneralUnparsedEntity) {
                // [ WFC: Parsed Entity ]
                // An entity reference must not contain the name of an
                // unparsed entity
                xml_fatal_err_msg_str!(
                    self,
                    XmlParserErrors::XmlErrUnparsedEntity,
                    "Entity reference to unparsed entity {}\n",
                    name
                );
            } else if matches!(self.instate, XmlParserInputState::XmlParserAttributeValue)
                && matches!(ent.etype, XmlEntityType::XmlExternalGeneralParsedEntity)
            {
                // [ WFC: No External Entity References ]
                // Attribute values cannot contain direct or indirect
                // entity references to external entities.
                xml_fatal_err_msg_str!(
                    self,
                    XmlParserErrors::XmlErrEntityIsExternal,
                    "Attribute references external entity '{}'\n",
                    name
                );
            } else if matches!(self.instate, XmlParserInputState::XmlParserAttributeValue)
                && !matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity)
            {
                // [ WFC: No < in Attribute Values ]
                // The replacement text of any entity referred to directly or
                // indirectly in an attribute value (other than "&lt;") must
                // not contain a <.
                if ent.flags & XML_ENT_CHECKED_LT as i32 == 0 {
                    if let Some(content) = ent.content.as_deref() {
                        if content.contains('<') {
                            ent.flags |= XML_ENT_CONTAINS_LT as i32;
                        }
                    }
                    ent.flags |= XML_ENT_CHECKED_LT as i32;
                }
                if ent.flags & XML_ENT_CONTAINS_LT as i32 != 0 {
                    xml_fatal_err_msg_str!(
                        self,
                        XmlParserErrors::XmlErrLtInAttribute,
                        "'<' in entity '{}' is not allowed in attributes values\n",
                        name
                    );
                }
            } else {
                // Internal check, no parameter entities here ...
                match ent.etype {
                    XmlEntityType::XmlInternalParameterEntity
                    | XmlEntityType::XmlExternalParameterEntity => {
                        xml_fatal_err_msg_str!(
                            self,
                            XmlParserErrors::XmlErrEntityIsParameter,
                            "Attempt to reference the parameter entity '{}'\n",
                            name
                        );
                    }
                    _ => {}
                }
            }
        } else if self.standalone == 1 || (self.has_external_subset == 0 && self.has_perefs == 0) {
            xml_fatal_err_msg_str!(
                self,
                XmlParserErrors::XmlErrUndeclaredEntity,
                "Entity '{}' not defined\n",
                name
            );
        } else {
            xml_err_msg_str!(
                self,
                XmlParserErrors::XmlWarUndeclaredEntity,
                "Entity '{}' not defined\n",
                name
            );
        }

        // [ WFC: No Recursion ]
        // A parsed entity must not contain a recursive reference
        // to itself, either directly or indirectly.
        // Done somewhere else
        (ent, ptr)
    }

    /// Parse PEReference declarations
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
    ///
    /// Returns the string of the entity content.
    /// str is updated to the current value of the index
    #[doc(alias = "xmlParseStringPEReference")]
    pub(super) fn parse_string_pereference<'a>(
        &mut self,
        s: &'a str,
    ) -> (Option<XmlEntityPtr>, &'a str) {
        let Some(ptr) = s.strip_prefix('%') else {
            return (None, s);
        };
        let (Some(name), rem) = self.parse_string_name(ptr) else {
            xml_fatal_err_msg(
                self,
                XmlParserErrors::XmlErrNameRequired,
                "xmlParseStringPEReference: no name\n",
            );
            return (None, ptr);
        };
        let Some(ptr) = rem.strip_prefix(';') else {
            xml_fatal_err(self, XmlParserErrors::XmlErrEntityRefSemicolMissing, None);
            return (None, rem);
        };

        // Request the entity from SAX
        let entity = self
            .sax
            .as_deref_mut()
            .and_then(|sax| sax.get_parameter_entity)
            .and_then(|get_parameter_entity| get_parameter_entity(self, name));
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            return (None, ptr);
        }
        if let Some(entity) = entity {
            // Internal checking in case the entity quest barfed
            if !matches!(
                entity.etype,
                XmlEntityType::XmlInternalParameterEntity
                    | XmlEntityType::XmlExternalParameterEntity
            ) {
                xml_warning_msg!(
                    self,
                    XmlParserErrors::XmlWarUndeclaredEntity,
                    "%{}; is not a parameter entity\n",
                    name
                );
            }
        } else {
            // [ WFC: Entity Declared ]
            // In a document without any DTD, a document with only an
            // internal DTD subset which contains no parameter entity
            // references, or a document with "standalone='yes'", ...
            // ... The declaration of a parameter entity must precede
            // any reference to it...
            if self.standalone == 1 || (self.has_external_subset == 0 && self.has_perefs == 0) {
                xml_fatal_err_msg_str!(
                    self,
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
                xml_warning_msg!(
                    self,
                    XmlParserErrors::XmlWarUndeclaredEntity,
                    "PEReference: %{}; not found\n",
                    name
                );
                self.valid = 0;
            }
        }
        self.has_perefs = 1;
        (entity, ptr)
    }

    /// Parse and handle entity references in content, depending on the SAX interface,
    /// this may end-up in a call to character() if this is a CharRef,
    /// a predefined entity, if there is no reference() callback.
    /// or if the parser was asked to match to that mode.
    ///
    /// Always consumes '&'.
    ///
    /// ```text
    /// [67] Reference ::= EntityRef | CharRef
    /// ```
    #[doc(alias = "xmlParseReference")]
    pub(crate) fn parse_reference(&mut self) {
        unsafe {
            let mut was_checked: i32;
            let mut list = None;
            let mut ret: XmlParserErrors;

            if self.current_byte() != b'&' {
                return;
            }

            // Simple case of a CharRef
            if self.nth_byte(1) == b'#' {
                let mut out = [0; 16];
                let hex = self.nth_byte(2);
                let Some(value) = self.parse_char_ref() else {
                    return;
                };

                if self.charset != XmlCharEncoding::UTF8 {
                    // So we are using non-UTF-8 buffers
                    // Check that the c_char fit on 8bits, if not
                    // generate a CharRef.
                    if value as u32 <= 0xFF {
                        let out = value.encode_utf8(&mut out[..]);
                        if self.disable_sax == 0 {
                            if let Some(characters) =
                                self.sax.as_deref_mut().and_then(|sax| sax.characters)
                            {
                                characters(self, out);
                            }
                        }
                    } else {
                        use std::io::Write;
                        let mut slice = &mut out[..];
                        if hex == b'x' || hex == b'X' {
                            write!(slice, "#x{:X}", value as u32).ok();
                        } else {
                            write!(slice, "#{}", value as u32).ok();
                        }
                        let rem = slice.len();
                        let len = out.len() - rem;
                        if self.disable_sax == 0 {
                            if let Some(reference) =
                                self.sax.as_deref_mut().and_then(|sax| sax.reference)
                            {
                                // # Safety
                                // `out` contains only '#', 'x' and ascii hex digit characters.
                                // Therefore, UTF-8 validation won't fail.
                                let out = from_utf8_unchecked(&out[..len]);
                                reference(self, out);
                            }
                        }
                    }
                } else {
                    // Just encode the value in UTF-8
                    let out = value.encode_utf8(&mut out[..]);
                    if self.disable_sax == 0 {
                        if let Some(characters) =
                            self.sax.as_deref_mut().and_then(|sax| sax.characters)
                        {
                            characters(self, out);
                        }
                    }
                }
                return;
            }

            // We are seeing an entity reference
            let Some(mut ent) = self.parse_entity_ref() else {
                return;
            };
            if !self.well_formed {
                return;
            }
            was_checked = ent.flags & XML_ENT_PARSED as i32;

            // special case of predefined entities
            if matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity) {
                let Some(val) = ent.content.as_deref() else {
                    return;
                };
                // inline the entity.
                if self.disable_sax == 0 {
                    if let Some(characters) = self.sax.as_deref_mut().and_then(|sax| sax.characters)
                    {
                        characters(self, val);
                    }
                }
                return;
            }

            // The first reference to the entity trigger a parsing phase
            // where the ent.children is filled with the result from the parsing.
            // Note: external parsed entities will not be loaded, it is not
            // required for a non-validating parser, unless the parsing option
            // of validating, or substituting entities were given. Doing so is
            // far more secure as the parser will only process data coming from
            // the document entity by default.
            if ent.flags & XML_ENT_PARSED as i32 == 0
                && (!matches!(ent.etype, XmlEntityType::XmlExternalGeneralParsedEntity)
                    || self.options
                        & (XmlParserOption::XmlParseNoEnt as i32
                            | XmlParserOption::XmlParseDTDValid as i32)
                        != 0)
            {
                let oldsizeentcopy: u64 = self.sizeentcopy;

                // This is a bit hackish but this seems the best
                // way to make sure both SAX and DOM entity support
                // behaves okay.
                let user_data = self.user_data.clone();

                // Avoid overflow as much as possible
                self.sizeentcopy = 0;

                if ent.flags & XML_ENT_EXPANDING as i32 != 0 {
                    xml_fatal_err(self, XmlParserErrors::XmlErrEntityLoop, None);
                    self.halt();
                    return;
                }

                ent.flags |= XML_ENT_EXPANDING as i32;

                // Check that this entity is well formed
                // 4.3.2: An internal general parsed entity is well-formed
                // if its replacement text matches the production labeled content.
                if matches!(ent.etype, XmlEntityType::XmlInternalGeneralEntity) {
                    self.depth += 1;
                    ret = xml_parse_balanced_chunk_memory_internal(
                        self,
                        ent.content.as_deref().unwrap(),
                        user_data,
                        Some(&mut list),
                    );
                    self.depth -= 1;
                } else if matches!(ent.etype, XmlEntityType::XmlExternalGeneralParsedEntity) {
                    self.depth += 1;

                    let has_sax = self.sax.is_some();
                    let sax = self.sax.take();
                    let (sax, error) = parse_external_entity_private(
                        self.my_doc.unwrap(),
                        self,
                        sax,
                        user_data,
                        self.depth,
                        ent.uri.as_deref(),
                        ent.external_id.as_deref(),
                        Some(&mut list),
                    );
                    assert_eq!(has_sax, sax.is_some());
                    self.sax = sax;
                    ret = error;
                    self.depth -= 1;
                } else {
                    ret = XmlParserErrors::XmlErrEntityPEInternal;
                    xml_err_msg_str!(
                        self,
                        XmlParserErrors::XmlErrInternalError,
                        "invalid entity type found\n"
                    );
                }

                ent.flags &= !XML_ENT_EXPANDING as i32;
                ent.flags |= (XML_ENT_PARSED | XML_ENT_CHECKED) as i32;
                ent.expanded_size = self.sizeentcopy;
                if matches!(ret, XmlParserErrors::XmlErrEntityLoop) {
                    self.halt();
                    xml_free_node_list(list);
                    return;
                }
                if self.parser_entity_check(oldsizeentcopy) != 0 {
                    xml_free_node_list(list);
                    return;
                }

                if let Some(l) = list.filter(|_| matches!(ret, XmlParserErrors::XmlErrOK)) {
                    ent.children = Some(XmlNodePtr::try_from(l).unwrap());
                    // Prune it directly in the generated document
                    // except for single text nodes.
                    if !self.replace_entities
                        || matches!(self.parse_mode, XmlParserMode::XmlParseReader)
                        || (matches!(l.element_type(), XmlElementType::XmlTextNode)
                            && l.next().is_none())
                    {
                        ent.owner = 1;
                        let mut cur = Some(l);
                        while let Some(mut now) = cur {
                            now.set_parent(Some(ent.into()));
                            if now.document() != ent.doc {
                                now.set_doc(ent.doc);
                            }
                            if now.next().is_none() {
                                ent.last = Some(XmlNodePtr::try_from(now).unwrap());
                            }
                            cur = now.next();
                        }
                        list = None;
                    } else {
                        ent.owner = 0;
                        while let Some(mut now) = list {
                            now.set_parent(self.node.map(|node| node.into()));
                            now.set_document(self.my_doc);
                            if now.next().is_none() {
                                ent.last = Some(XmlNodePtr::try_from(now).unwrap());
                            }
                            list = now.next();
                        }
                        list = ent.children.map(|children| children.into());
                    }
                } else if !matches!(
                    ret,
                    XmlParserErrors::XmlErrOK | XmlParserErrors::XmlWarUndeclaredEntity
                ) {
                    xml_fatal_err_msg_str!(
                        self,
                        XmlParserErrors::XmlErrUndeclaredEntity,
                        "Entity '{}' failed to parse\n",
                        ent.name
                    );
                    if ent.content.is_some() {
                        ent.content = Some(Cow::Borrowed(""));
                    }
                } else if let Some(list) = list.take() {
                    xml_free_node_list(Some(list));
                }

                // Prevent entity from being parsed and expanded twice (Bug 760367).
                was_checked = 0;
            }

            // Now that the entity content has been gathered
            // provide it to the application, this can take different forms based
            // on the parsing modes.
            if ent.children.is_none() {
                // Probably running in SAX mode and the callbacks don't
                // build the entity content. So unless we already went
                // though parsing for first checking go though the entity
                // content to generate callbacks associated to the entity
                if was_checked != 0 {
                    // This is a bit hackish but this seems the best
                    // way to make sure both SAX and DOM entity support behaves okay.
                    let user_data = self.user_data.clone();

                    if matches!(ent.etype, XmlEntityType::XmlInternalGeneralEntity) {
                        self.depth += 1;
                        ret = xml_parse_balanced_chunk_memory_internal(
                            self,
                            ent.content.as_deref().unwrap(),
                            user_data,
                            None,
                        );
                        self.depth -= 1;
                    } else if matches!(ent.etype, XmlEntityType::XmlExternalGeneralParsedEntity) {
                        let oldsizeentities: u64 = self.sizeentities;

                        self.depth += 1;

                        let has_sax = self.sax.is_some();
                        let sax = self.sax.take();
                        let (sax, error) = parse_external_entity_private(
                            self.my_doc.unwrap(),
                            self,
                            sax,
                            user_data,
                            self.depth,
                            ent.uri.as_deref(),
                            ent.external_id.as_deref(),
                            None,
                        );
                        assert_eq!(has_sax, sax.is_some());
                        self.sax = sax;
                        ret = error;
                        self.depth -= 1;

                        // Undo the change to sizeentities
                        self.sizeentities = oldsizeentities;
                    } else {
                        ret = XmlParserErrors::XmlErrEntityPEInternal;
                        xml_err_msg_str!(
                            self,
                            XmlParserErrors::XmlErrInternalError,
                            "invalid entity type found\n"
                        );
                    }
                    if matches!(ret, XmlParserErrors::XmlErrEntityLoop) {
                        xml_fatal_err(self, XmlParserErrors::XmlErrEntityLoop, None);
                        return;
                    }
                    if self.parser_entity_check(0) != 0 {
                        return;
                    }
                }
                if !self.replace_entities && self.disable_sax == 0 {
                    if let Some(reference) = self.sax.as_deref_mut().and_then(|sax| sax.reference) {
                        // Entity reference callback comes second, it's somewhat
                        // superfluous but a compatibility to historical behaviour
                        reference(self, &ent.name().unwrap());
                    }
                }
                return;
            }

            // We also check for amplification if entities aren't substituted.
            // They might be expanded later.
            if was_checked != 0 && self.parser_entity_check(ent.expanded_size) != 0 {
                return;
            }

            // If we didn't get any children for the entity being built
            if !self.replace_entities && self.disable_sax == 0 {
                if let Some(reference) = self.sax.as_deref_mut().and_then(|sax| sax.reference) {
                    // Create a node.
                    reference(self, &ent.name().unwrap());
                    return;
                }
            }

            if self.replace_entities {
                // There is a problem on the handling of _private for entities
                // (bug 155816): Should we copy the content of the field from
                // the entity (possibly overwriting some value set by the user
                // when a copy is created), should we leave it alone, or should
                // we try to take care of different situations?  The problem
                // is exacerbated by the usage of this field by the xmlReader.
                // To fix this bug, we look at _private on the created node
                // and, if it's NULL, we copy in whatever was in the entity.
                // If it's not NULL we leave it alone.  This is somewhat of a
                // hack - maybe we should have further tests to determine what to do.
                if let Some(mut context_node) = self.node {
                    // Seems we are generating the DOM content, do
                    // a simple tree copy for all references except the first
                    // In the first occurrence list contains the replacement.
                    if (list.is_none() && ent.owner == 0)
                        || matches!(self.parse_mode, XmlParserMode::XmlParseReader)
                    {
                        let mut first_child = None;

                        // when operating on a reader, the entities definitions
                        // are always owning the entities subtree.
                        // if (self.parseMode == XML_PARSE_READER)
                        //     ent.owner = 1;

                        let mut cur = ent.children;
                        while let Some(cur_node) = cur {
                            let mut nw = xml_doc_copy_node(
                                XmlGenericNodePtr::from(cur_node),
                                self.my_doc,
                                1,
                            )
                            .map(|nw| XmlNodePtr::try_from(nw).unwrap());
                            if let Some(mut now) = nw {
                                if now._private.is_null() {
                                    now._private = cur_node._private;
                                }
                                if first_child.is_none() {
                                    first_child = nw;
                                }
                                nw = XmlNodePtr::try_from(
                                    context_node.add_child(now.into()).unwrap(),
                                )
                                .ok();
                            }
                            if cur == ent.last {
                                // needed to detect some strange empty
                                // node cases in the reader tests
                                if matches!(self.parse_mode, XmlParserMode::XmlParseReader) {
                                    if let Some(mut nw) = nw
                                        .filter(|nw| {
                                            nw.element_type() == XmlElementType::XmlElementNode
                                        })
                                        .filter(|nw| nw.children.is_none())
                                    {
                                        nw.extra = 1;
                                    }
                                }

                                break;
                            }
                            cur = cur_node
                                .next
                                .map(|node| XmlNodePtr::try_from(node).unwrap());
                        }
                    } else if list.is_none() || !self.input_tab.is_empty() {
                        let mut first_child = None;

                        // Copy the entity child list and make it the new
                        // entity child list. The goal is to make sure any
                        // ID or REF referenced will be the one from the
                        // document content and not the entity copy.
                        let mut cur = ent.children.take();
                        let last = ent.last.take();
                        while let Some(mut cur_node) = cur {
                            let next = cur_node.next.take();
                            cur_node.set_parent(None);
                            let nw = xml_doc_copy_node(cur_node.into(), self.my_doc, 1)
                                .map(|nw| XmlNodePtr::try_from(nw).unwrap());
                            if let Some(mut nw) = nw {
                                if nw._private.is_null() {
                                    nw._private = cur_node._private;
                                }
                                if first_child.is_none() {
                                    first_child = Some(cur_node);
                                }
                                ent.add_child(nw.into());
                            }
                            context_node.add_child(cur_node.into());
                            if Some(cur_node) == last {
                                break;
                            }
                            cur = next.map(|node| XmlNodePtr::try_from(node).unwrap());
                        }
                        if ent.owner == 0 {
                            ent.owner = 1;
                        }
                    } else {
                        // the name change is to avoid coalescing of the
                        // node with a possible previous text one which
                        // would make ent.children a dangling pointer
                        if matches!(
                            ent.children.unwrap().element_type(),
                            XmlElementType::XmlTextNode
                        ) {
                            ent.children.unwrap().name = Cow::Borrowed("nbktext");
                        }
                        if ent.last != ent.children
                            && matches!(
                                ent.last.unwrap().element_type(),
                                XmlElementType::XmlTextNode
                            )
                        {
                            ent.last.unwrap().name = Cow::Borrowed("nbktext");
                        }
                        context_node.add_child_list(ent.children.unwrap().into());
                    }

                    // This is to avoid a nasty side effect, see characters() in SAX.c
                    self.nodemem = 0;
                    self.nodelen = 0;
                }
            }
        }
    }
}
