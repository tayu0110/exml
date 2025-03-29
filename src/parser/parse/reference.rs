use std::ffi::CStr;

use crate::{
    error::XmlParserErrors,
    globals::GenericErrorContext,
    libxml::{
        parser::{XmlParserInputState, XmlParserOption},
        sax2::xml_sax2_get_entity,
    },
    parser::{
        XmlParserCtxt, XmlParserCtxtPtr, xml_err_msg_str, xml_fatal_err, xml_fatal_err_msg,
        xml_fatal_err_msg_int, xml_fatal_err_msg_str, xml_is_char, xml_warning_msg,
    },
    tree::{
        XML_ENT_CHECKED_LT, XML_ENT_CONTAINS_LT, XmlEntityPtr, XmlEntityType,
        xml_get_predefined_entity,
    },
};

use super::{parse_name, parse_string_name};

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
pub(crate) unsafe fn parse_char_ref(ctxt: &mut XmlParserCtxt) -> Option<char> {
    unsafe {
        let mut val = 0u32;
        let mut count = 0;

        // Using RAW/CUR/NEXT is okay since we are working on ASCII range here
        if ctxt.current_byte() == b'&' && ctxt.nth_byte(1) == b'#' && ctxt.nth_byte(2) == b'x' {
            ctxt.advance(3);
            ctxt.grow();
            while ctxt.current_byte() != b';' {
                // loop blocked by count
                if count > 20 {
                    count = 0;
                    ctxt.grow();
                    if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                        return None;
                    }
                } else {
                    count += 1;
                }
                let cur = ctxt.current_byte();
                if cur.is_ascii_digit() {
                    val = val * 16 + (cur - b'0') as u32;
                } else if (b'a'..=b'f').contains(&cur) && count < 20 {
                    val = val * 16 + (cur - b'a') as u32 + 10;
                } else if (b'A'..=b'F').contains(&cur) && count < 20 {
                    val = val * 16 + (cur - b'A') as u32 + 10;
                } else {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidHexCharRef, None);
                    val = 0;
                    break;
                }
                val = val.min(0x110000);
                ctxt.skip_char();
                count += 1;
            }
            if ctxt.current_byte() == b';' {
                // on purpose to avoid reentrancy problems with NEXT and SKIP
                ctxt.input_mut().unwrap().col += 1;
                ctxt.input_mut().unwrap().cur = ctxt.input().unwrap().cur.add(1);
            }
        } else if ctxt.current_byte() == b'&' && ctxt.nth_byte(1) == b'#' {
            ctxt.advance(2);
            ctxt.grow();
            while ctxt.current_byte() != b';' {
                // loop blocked by count
                if count > 20 {
                    count = 0;
                    ctxt.grow();
                    if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                        return None;
                    }
                } else {
                    count += 1;
                }
                let cur = ctxt.current_byte();
                if cur.is_ascii_digit() {
                    val = val * 10 + (cur - b'0') as u32;
                } else {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidDecCharRef, None);
                    val = 0;
                    break;
                }
                val = val.min(0x110000);
                ctxt.skip_char();
                count += 1;
            }
            if ctxt.current_byte() == b';' {
                // on purpose to avoid reentrancy problems with NEXT and SKIP
                ctxt.input_mut().unwrap().col += 1;
                ctxt.input_mut().unwrap().cur = ctxt.input().unwrap().cur.add(1);
            }
        } else {
            if ctxt.current_byte() == b'&' {
                ctxt.advance(1);
            }
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidCharRef, None);
        }

        // [ WFC: Legal Character ]
        // Characters referred to using character references must match the
        // production for Char.
        if val >= 0x110000 {
            xml_fatal_err_msg_int!(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                "xmlParseCharRef: character reference out of bounds\n",
                val as i32
            );
        } else if xml_is_char(val) {
            return char::from_u32(val);
        } else {
            xml_fatal_err_msg_int!(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                format!("xmlParseCharRef: invalid XmlChar value {val}\n").as_str(),
                val as i32
            );
        }
        None
    }
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
pub(super) unsafe fn parse_string_char_ref<'a>(
    ctxt: &mut XmlParserCtxt,
    s: &'a str,
) -> (Option<char>, &'a str) {
    unsafe {
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
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidHexCharRef, None);
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
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidDecCharRef, None);
            }
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidCharRef, None);
            return (None, s);
        }

        // [ WFC: Legal Character ]
        // Characters referred to using character references must match the
        // production for Char.
        if val >= 0x110000 {
            xml_fatal_err_msg_int!(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                "xmlParseStringCharRef: character reference out of bounds\n",
                val as i32
            );
        } else if xml_is_char(val) {
            return (Some(char::from_u32(val).expect("Internal Error")), ptr);
        } else {
            xml_fatal_err_msg_int!(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                format!("xmlParseStringCharRef: invalid xmlChar value {val}\n").as_str(),
                val as i32
            );
        }
        (None, ptr)
    }
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
pub(crate) unsafe fn parse_entity_ref(ctxt: &mut XmlParserCtxt) -> Option<XmlEntityPtr> {
    unsafe {
        ctxt.grow();
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }

        if ctxt.current_byte() != b'&' {
            return None;
        }
        ctxt.skip_char();
        let Some(name) = parse_name(&mut *ctxt) else {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                "xmlParseEntityRef: no name\n",
            );
            return None;
        };
        if ctxt.current_byte() != b';' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityRefSemicolMissing, None);
            return None;
        }
        ctxt.skip_char();

        // Predefined entities override any extra definition
        if ctxt.options & XmlParserOption::XmlParseOldSAX as i32 == 0 {
            if let Some(ent) = xml_get_predefined_entity(&name) {
                return Some(ent);
            }
        }

        let mut ent = None;
        // Ask first SAX for entity resolution, otherwise try the
        // entities which may have stored in the parser context.
        if let Some(sax) = ctxt.sax.as_deref_mut() {
            if let Some(f) = sax.get_entity {
                ent = f(ctxt.user_data.clone(), &name);
            }
            if ctxt.well_formed == 1
                && ent.is_none()
                && ctxt.options & XmlParserOption::XmlParseOldSAX as i32 != 0
            {
                ent = xml_get_predefined_entity(&name);
            }
            if ctxt.well_formed == 1
                && ent.is_none()
                && ctxt
                    .user_data
                    .as_ref()
                    .and_then(|d| d.lock().downcast_ref::<XmlParserCtxtPtr>().copied())
                    == Some(ctxt)
            {
                ent = xml_sax2_get_entity(
                    Some(GenericErrorContext::new(ctxt as *mut XmlParserCtxt)),
                    &name,
                );
            }
        }
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
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
                    ctxt,
                    XmlParserErrors::XmlErrUnparsedEntity,
                    "Entity reference to unparsed entity {}\n",
                    name
                );
            } else if matches!(ctxt.instate, XmlParserInputState::XmlParserAttributeValue)
                && matches!(ent.etype, XmlEntityType::XmlExternalGeneralParsedEntity)
            {
                // [ WFC: No External Entity References ]
                // Attribute values cannot contain direct or indirect
                // entity references to external entities.
                xml_fatal_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlErrEntityIsExternal,
                    "Attribute references external entity '{}'\n",
                    name
                );
            } else if matches!(ctxt.instate, XmlParserInputState::XmlParserAttributeValue)
                && !matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity)
            {
                // [ WFC: No < in Attribute Values ]
                // The replacement text of any entity referred to directly or
                // indirectly in an attribute value (other than "&lt;") must not contain a <.
                if ent.flags & XML_ENT_CHECKED_LT as i32 == 0 {
                    if !ent.content.is_null() {
                        let content = CStr::from_ptr(ent.content as *const i8).to_string_lossy();
                        if content.contains('<') {
                            ent.flags |= XML_ENT_CONTAINS_LT as i32;
                        }
                    }
                    ent.flags |= XML_ENT_CHECKED_LT as i32;
                }
                if ent.flags & XML_ENT_CONTAINS_LT as i32 != 0 {
                    xml_fatal_err_msg_str!(
                        ctxt,
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
                            ctxt,
                            XmlParserErrors::XmlErrEntityIsParameter,
                            "Attempt to reference the parameter entity '{}'\n",
                            name
                        );
                    }
                    _ => {}
                }
            }
        } else {
            if ctxt.standalone == 1 || (ctxt.has_external_subset == 0 && ctxt.has_perefs == 0) {
                xml_fatal_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlErrUndeclaredEntity,
                    "Entity '{}' not defined\n",
                    name
                );
            } else {
                xml_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlWarUndeclaredEntity,
                    "Entity '{}' not defined\n",
                    name
                );
                if ctxt.in_subset == 0 {
                    if let Some(reference) = ctxt.sax.as_deref_mut().and_then(|sax| sax.reference) {
                        reference(ctxt.user_data.clone(), &name);
                    }
                }
            }
            ctxt.valid = 0;
        }

        // [ WFC: No Recursion ]
        // A parsed entity must not contain a recursive reference
        // to itself, either directly or indirectly.
        // Done somewhere else
        ent
    }
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
pub(super) unsafe fn parse_string_entity_ref<'a>(
    ctxt: &mut XmlParserCtxt,
    s: &'a str,
) -> (Option<XmlEntityPtr>, &'a str) {
    unsafe {
        let Some(ptr) = s.strip_prefix('&') else {
            return (None, s);
        };

        let (Some(name), rem) = parse_string_name(ctxt, ptr) else {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                "xmlParseStringEntityRef: no name\n",
            );
            return (None, ptr);
        };
        let Some(ptr) = rem.strip_prefix(';') else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityRefSemicolMissing, None);
            return (None, rem);
        };

        let mut ent = None;
        // Predefined entities override any extra definition
        if ctxt.options & XmlParserOption::XmlParseOldSAX as i32 == 0 {
            ent = xml_get_predefined_entity(name);
            if ent.is_some() {
                return (ent, ptr);
            }
        }

        // Ask first SAX for entity resolution, otherwise try the
        // entities which may have stored in the parser context.
        if let Some(sax) = ctxt.sax.as_deref_mut() {
            if let Some(get_entity) = sax.get_entity {
                ent = get_entity(ctxt.user_data.clone(), name);
            }
            if ent.is_none() && ctxt.options & XmlParserOption::XmlParseOldSAX as i32 != 0 {
                ent = xml_get_predefined_entity(name);
            }
            if ent.is_none()
                && ctxt
                    .user_data
                    .as_ref()
                    .and_then(|d| d.lock().downcast_ref::<XmlParserCtxtPtr>().copied())
                    == Some(ctxt)
            {
                ent = xml_sax2_get_entity(
                    Some(GenericErrorContext::new(ctxt as *mut XmlParserCtxt)) as _,
                    name,
                );
            }
        }
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
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
                    ctxt,
                    XmlParserErrors::XmlErrUnparsedEntity,
                    "Entity reference to unparsed entity {}\n",
                    name
                );
            } else if matches!(ctxt.instate, XmlParserInputState::XmlParserAttributeValue)
                && matches!(ent.etype, XmlEntityType::XmlExternalGeneralParsedEntity)
            {
                // [ WFC: No External Entity References ]
                // Attribute values cannot contain direct or indirect
                // entity references to external entities.
                xml_fatal_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlErrEntityIsExternal,
                    "Attribute references external entity '{}'\n",
                    name
                );
            } else if matches!(ctxt.instate, XmlParserInputState::XmlParserAttributeValue)
                && !matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity)
            {
                // [ WFC: No < in Attribute Values ]
                // The replacement text of any entity referred to directly or
                // indirectly in an attribute value (other than "&lt;") must
                // not contain a <.
                if ent.flags & XML_ENT_CHECKED_LT as i32 == 0 {
                    if !ent.content.is_null() {
                        let content = CStr::from_ptr(ent.content as *const i8).to_string_lossy();
                        if content.contains('<') {
                            ent.flags |= XML_ENT_CONTAINS_LT as i32;
                        }
                    }
                    ent.flags |= XML_ENT_CHECKED_LT as i32;
                }
                if ent.flags & XML_ENT_CONTAINS_LT as i32 != 0 {
                    xml_fatal_err_msg_str!(
                        ctxt,
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
                            ctxt,
                            XmlParserErrors::XmlErrEntityIsParameter,
                            "Attempt to reference the parameter entity '{}'\n",
                            name
                        );
                    }
                    _ => {}
                }
            }
        } else if ctxt.standalone == 1 || (ctxt.has_external_subset == 0 && ctxt.has_perefs == 0) {
            xml_fatal_err_msg_str!(
                ctxt,
                XmlParserErrors::XmlErrUndeclaredEntity,
                "Entity '{}' not defined\n",
                name
            );
        } else {
            xml_err_msg_str!(
                ctxt,
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
pub(super) unsafe fn parse_string_pereference<'a>(
    ctxt: &mut XmlParserCtxt,
    s: &'a str,
) -> (Option<XmlEntityPtr>, &'a str) {
    unsafe {
        let Some(ptr) = s.strip_prefix('%') else {
            return (None, s);
        };
        let (Some(name), rem) = parse_string_name(ctxt, ptr) else {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                "xmlParseStringPEReference: no name\n",
            );
            return (None, ptr);
        };
        let Some(ptr) = rem.strip_prefix(';') else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityRefSemicolMissing, None);
            return (None, rem);
        };

        // Request the entity from SAX
        let entity = ctxt
            .sax
            .as_deref_mut()
            .and_then(|sax| sax.get_parameter_entity)
            .and_then(|get_parameter_entity| get_parameter_entity(ctxt.user_data.clone(), name));
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
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
                    ctxt,
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
                xml_warning_msg!(
                    ctxt,
                    XmlParserErrors::XmlWarUndeclaredEntity,
                    "PEReference: %{}; not found\n",
                    name
                );
                ctxt.valid = 0;
            }
        }
        ctxt.has_perefs = 1;
        (entity, ptr)
    }
}
