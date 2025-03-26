use std::ffi::CStr;

use crate::{
    error::XmlParserErrors,
    generic_error,
    globals::get_parser_debug_entities,
    libxml::{
        chvalid::xml_is_char,
        parser::{
            XML_ENT_FIXED_COST, XML_PARSER_ALLOWED_EXPANSION, XML_PARSER_NON_LINEAR,
            XmlParserInputState, XmlParserOption,
        },
        parser_internals::{
            XML_MAX_HUGE_LENGTH, XML_MAX_TEXT_LENGTH, XML_SUBSTITUTE_PEREF, XML_SUBSTITUTE_REF,
        },
        xmlstring::xml_strndup,
    },
    parser::{
        XmlParserCtxt, XmlParserInput, parse::parse_string_char_ref, xml_fatal_err,
        xml_fatal_err_msg, xml_fatal_err_msg_int, xml_warning_msg,
    },
    tree::{NodeCommon, XML_ENT_EXPANDING, XML_ENT_PARSED, XmlEntityPtr, XmlEntityType},
};

use super::{parse_string_entity_ref, parse_string_name, parse_string_pereference};

#[doc(alias = "xmlStringDecodeEntitiesInt")]
pub(super) unsafe fn string_decode_entities_int(
    ctxt: &mut XmlParserCtxt,
    mut s: &str,
    what: i32,
    end: char,
    end2: char,
    end3: char,
    check: i32,
) -> Option<String> {
    unsafe {
        if (ctxt.depth > 40 && ctxt.options & XmlParserOption::XmlParseHuge as i32 == 0)
            || ctxt.depth > 100
        {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrEntityLoop,
                "Maximum entity nesting depth exceeded",
            );
            return None;
        }

        // allocate a translation buffer.
        let mut buffer = String::new();
        // OK loop until we reach one of the ending c_char or a size limit.
        // we are operating on already parsed values.
        while let Some(c) = s.chars().next().filter(|&c| {
            c != end
                && c != end2
                && c != end3
                && !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF)
        }) {
            if s.starts_with("&#") {
                let (Some(val), rem) = parse_string_char_ref(ctxt, s) else {
                    return None;
                };
                buffer.push(val);
                s = rem;
            } else if c == '&' && what & XML_SUBSTITUTE_REF as i32 != 0 {
                if get_parser_debug_entities() != 0 {
                    generic_error!(
                        "String decoding Entity Reference: {}\n",
                        s.chars().take(30).collect::<String>()
                    );
                }
                let (ent, rem) = parse_string_entity_ref(ctxt, s);
                s = rem;
                if let Some(ent) = ent
                    .filter(|ent| matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity))
                {
                    if !ent.content.is_null() {
                        buffer.push(*ent.content as char);
                    } else {
                        xml_fatal_err_msg(
                            ctxt,
                            XmlParserErrors::XmlErrInternalError,
                            "predefined entity has no content\n",
                        );
                        return None;
                    }
                } else if let Some(mut ent) = ent.filter(|ent| !ent.content.is_null()) {
                    if check != 0 && parser_entity_check(ctxt, ent.length as _) != 0 {
                        return None;
                    }

                    if ent.flags & XML_ENT_EXPANDING as i32 != 0 {
                        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityLoop, None);
                        ctxt.halt();
                        *ent.content.add(0) = 0;
                        return None;
                    }

                    ent.flags |= XML_ENT_EXPANDING as i32;
                    ctxt.depth += 1;
                    let rep = string_decode_entities_int(
                        ctxt,
                        &CStr::from_ptr(ent.content as *const i8).to_string_lossy(),
                        what,
                        '\0',
                        '\0',
                        '\0',
                        check,
                    );
                    ctxt.depth -= 1;
                    ent.flags &= !XML_ENT_EXPANDING as i32;

                    let Some(rep) = rep else {
                        *ent.content.add(0) = 0;
                        return None;
                    };

                    buffer.push_str(&rep);
                } else if let Some(ent) = ent {
                    buffer.push('&');
                    buffer.push_str(&ent.name().unwrap());
                    buffer.push(';');
                }
            } else if c == '%' && what & XML_SUBSTITUTE_PEREF as i32 != 0 {
                if get_parser_debug_entities() != 0 {
                    generic_error!(
                        "String decoding PE Reference: {}\n",
                        s.chars().take(30).collect::<String>()
                    );
                }
                let (ent, rem) = parse_string_pereference(ctxt, s);
                s = rem;
                if let Some(mut ent) = ent {
                    if ent.content.is_null() {
                        // Note: external parsed entities will not be loaded,
                        // it is not required for a non-validating parser to
                        // complete external PEReferences coming from the
                        // internal subset
                        if ctxt.options & XmlParserOption::XmlParseNoEnt as i32 != 0
                            || ctxt.options & XmlParserOption::XmlParseDTDValid as i32 != 0
                            || ctxt.validate != 0
                        {
                            load_entity_content(ctxt, ent);
                        } else {
                            let name = ent.name().unwrap().into_owned();
                            xml_warning_msg!(
                                ctxt,
                                XmlParserErrors::XmlErrEntityProcessing,
                                "not validating will not read content for PE entity {}\n",
                                name
                            );
                        }
                    }

                    if check != 0 && parser_entity_check(ctxt, ent.length as _) != 0 {
                        return None;
                    }

                    if ent.flags & XML_ENT_EXPANDING as i32 != 0 {
                        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityLoop, None);
                        ctxt.halt();
                        if !ent.content.is_null() {
                            *ent.content.add(0) = 0;
                        }
                        return None;
                    }

                    ent.flags |= XML_ENT_EXPANDING as i32;
                    ctxt.depth += 1;
                    let rep = string_decode_entities_int(
                        ctxt,
                        &CStr::from_ptr(ent.content as *const i8).to_string_lossy(),
                        what,
                        '\0',
                        '\0',
                        '\0',
                        check,
                    );
                    ctxt.depth -= 1;
                    ent.flags &= !XML_ENT_EXPANDING as i32;

                    let Some(rep) = rep else {
                        if !ent.content.is_null() {
                            *ent.content.add(0) = 0;
                        }
                        return None;
                    };
                    buffer.push_str(&rep);
                }
            } else {
                buffer.push(c);
                s = &s[c.len_utf8()..];
            }
        }
        Some(buffer)
    }
}

/// Takes a entity string content and process to do the adequate substitutions.
///
/// ```text
/// [67] Reference      ::= EntityRef | CharRef
/// [69] PEReference    ::= '%' Name ';'
/// ```
///
/// Returns A newly allocated string with the substitution done. The caller must deallocate it !
#[doc(alias = "xmlStringDecodeEntities")]
pub(crate) unsafe fn string_decode_entities(
    ctxt: &mut XmlParserCtxt,
    s: &str,
    what: i32,
    end: char,
    end2: char,
    end3: char,
) -> Option<String> {
    unsafe { string_decode_entities_int(ctxt, s, what, end, end2, end3, 0) }
}

/// Load the original content of the given system entity from the
/// ExternalID/SystemID given. This is to be used for Included in Literal
/// http://www.w3.org/TR/REC-xml/#inliteral processing of entities references
///
/// Returns 0 in case of success and -1 in case of failure
#[doc(alias = "xmlLoadEntityContent")]
unsafe fn load_entity_content(ctxt: &mut XmlParserCtxt, mut entity: XmlEntityPtr) -> i32 {
    unsafe {
        let mut l: i32 = 0;

        if !matches!(
            entity.etype,
            XmlEntityType::XmlExternalParameterEntity
                | XmlEntityType::XmlExternalGeneralParsedEntity
        ) || !entity.content.is_null()
        {
            xml_fatal_err(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                Some("xmlLoadEntityContent parameter error"),
            );
            return -1;
        }

        if get_parser_debug_entities() != 0 {
            generic_error!(
                "Reading {} entity content input\n",
                CStr::from_ptr(entity.name as *const i8).to_string_lossy()
            );
        }

        let Some(input) = XmlParserInput::from_entity(ctxt, entity) else {
            xml_fatal_err(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                Some("xmlLoadEntityContent input error"),
            );
            return -1;
        };
        let input_id = input.id;

        // Push the entity as the current input, read c_char by c_char
        // saving to the buffer until the end of the entity or an error
        if ctxt.push_input(input) < 0 {
            return -1;
        }

        ctxt.grow();
        let mut buf = String::new();
        let mut c = ctxt.current_char(&mut l).unwrap_or('\0');
        while ctxt.input().unwrap().id == input_id
            && !ctxt.content_bytes().is_empty()
            && xml_is_char(c as u32)
        {
            buf.push(c);
            ctxt.advance_with_line_handling(c.len_utf8());
            c = ctxt.current_char(&mut l).unwrap_or('\0');
        }
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return -1;
        }

        if ctxt
            .input()
            .is_some_and(|_| ctxt.content_bytes().is_empty())
        {
            ctxt.sizeentities = ctxt
                .sizeentities
                .saturating_add(ctxt.input().unwrap().consumed);
            ctxt.pop_input();
        } else if !xml_is_char(c as u32) {
            xml_fatal_err_msg_int!(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                format!("xmlLoadEntityContent: invalid char value {}\n", c as i32).as_str(),
                c as i32
            );
            return -1;
        }
        entity.length = buf.len() as i32;
        entity.content = xml_strndup(buf.as_ptr(), buf.len() as i32);

        0
    }
}

/// Check for non-linear entity expansion behaviour.
///
/// In some cases like xmlStringDecodeEntities, this function is called
/// for each, possibly nested entity and its unexpanded content length.
///
/// In other cases like xmlParseReference, it's only called for each
/// top-level entity with its unexpanded content length plus the sum of
/// the unexpanded content lengths (plus fixed cost) of all nested
/// entities.
///
/// Summing the unexpanded lengths also adds the length of the reference.
/// This is by design. Taking the length of the entity name into account
/// discourages attacks that try to waste CPU time with abusively long
/// entity names. See test/recurse/lol6.xml for example. Each call also
/// adds some fixed cost XML_ENT_FIXED_COST to discourage attacks with
/// short entities.
///
/// Returns 1 on error, 0 on success.
#[doc(alias = "xmlParserEntityCheck")]
pub(crate) unsafe fn parser_entity_check(ctxt: &mut XmlParserCtxt, extra: u64) -> i32 {
    unsafe {
        let input = ctxt.input().unwrap();
        let entity = input.entity;

        // Compute total consumed bytes so far, including input streams of external entities.
        let mut consumed = input.parent_consumed;
        if entity.is_none_or(|entity| {
            matches!(entity.etype, XmlEntityType::XmlExternalParameterEntity)
                && entity.flags & XML_ENT_PARSED as i32 == 0
        }) {
            consumed = consumed.saturating_add(input.consumed);
            consumed = consumed.saturating_add(input.offset_from_base() as u64);
        }
        consumed = consumed.saturating_add(ctxt.sizeentities);

        // Add extra cost and some fixed cost.
        ctxt.sizeentcopy = ctxt.sizeentcopy.saturating_add(extra);
        ctxt.sizeentcopy = ctxt.sizeentcopy.saturating_add(XML_ENT_FIXED_COST as _);

        // It's important to always use saturation arithmetic when tracking
        // entity sizes to make the size checks reliable. If "sizeentcopy"
        // overflows, we have to abort.
        if ctxt.sizeentcopy > XML_PARSER_ALLOWED_EXPANSION as u64
            && (ctxt.sizeentcopy == u64::MAX
                || ctxt.sizeentcopy / XML_PARSER_NON_LINEAR as u64 > consumed)
        {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrEntityLoop,
                "Maximum entity amplification factor exceeded",
            );
            ctxt.halt();
            return 1;
        }

        0
    }
}

/// Parse a value for ENTITY declarations
///
/// ```text
/// [9] EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"' | "'" ([^%&'] | PEReference | Reference)* "'"
/// ```
///
/// If successfully parsed, return (substituted EntityValue, EntityValue without substituted),
/// otherwise return `(None, None)`.
#[doc(alias = "xmlParseEntityValue")]
pub(crate) unsafe fn parse_entity_value(
    ctxt: &mut XmlParserCtxt,
) -> (Option<String>, Option<String>) {
    unsafe {
        let mut l: i32 = 0;
        let max_length = if ctxt.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_HUGE_LENGTH
        } else {
            XML_MAX_TEXT_LENGTH
        };

        if !matches!(ctxt.current_byte(), b'"' | b'\'') {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityNotStarted, None);
            return (None, None);
        }

        let stop = ctxt.current_byte();

        // The content of the entity definition is copied in a buffer.
        let mut buf = String::new();

        ctxt.instate = XmlParserInputState::XmlParserEntityValue;
        let inputid = ctxt.input().unwrap().id;
        ctxt.grow();
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return (None, None);
        }
        ctxt.skip_char();
        let mut c = ctxt.current_char(&mut l).unwrap_or('\0');
        // NOTE: 4.4.5 Included in Literal
        // When a parameter entity reference appears in a literal entity
        // value, ... a single or double quote character in the replacement
        // text is always treated as a normal data character and will not
        // terminate the literal.
        // In practice it means we stop the loop only when back at parsing
        // the initial entity and the quote is found
        while xml_is_char(c as u32)
            && (c as i32 != stop as i32 || ctxt.input().unwrap().id != inputid)
            && !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF)
        {
            buf.push(c);
            ctxt.advance_with_line_handling(c.len_utf8());
            ctxt.grow();
            c = ctxt.current_char(&mut l).unwrap_or('\0');
            if c == '\0' {
                ctxt.grow();
                c = ctxt.current_char(&mut l).unwrap_or('\0');
            }

            if buf.len() > max_length {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityNotFinished,
                    "entity value too long\n",
                );
                return (None, None);
            }
        }
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return (None, None);
        }
        if c as i32 != stop as i32 {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityNotFinished, None);
            return (None, None);
        }
        ctxt.skip_char();

        // Raise problem w.r.t. '&' and '%' being used in non-entities
        // reference constructs. Note Charref will be handled in
        // xmlStringDecodeEntities()
        let mut cur = buf.as_str();
        while let Some(pos) = cur.find(['&', '%']) {
            cur = &cur[pos..];
            if cur.starts_with("&#") {
                cur = &cur[1..];
                continue;
            }

            let tmp = cur.as_bytes()[0];
            // trim the head of '&' or '%'
            cur = &cur[1..];
            let (name, rem) = parse_string_name(ctxt, cur);
            cur = rem;

            if name.is_none() || !cur.starts_with(';') {
                xml_fatal_err_msg_int!(
                    ctxt,
                    XmlParserErrors::XmlErrEntityCharError,
                    format!(
                        "EntityValue: '{}' forbidden except for entities references\n",
                        tmp as char
                    )
                    .as_str(),
                    tmp as i32
                );
                return (None, None);
            }

            if tmp == b'%' && ctxt.in_subset == 1 && ctxt.input_tab.len() == 1 {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityPEInternal, None);
                return (None, None);
            }

            // trim the head of ';'
            cur = &cur[1..];
        }

        // Then PEReference entities are substituted.
        //
        // NOTE: 4.4.7 Bypassed
        // When a general entity reference appears in the EntityValue in
        // an entity declaration, it is bypassed and left as is.
        // so XML_SUBSTITUTE_REF is not set here.
        ctxt.depth += 1;
        let ret = string_decode_entities_int(
            ctxt,
            &buf,
            XML_SUBSTITUTE_PEREF as i32,
            '\0',
            '\0',
            '\0',
            1,
        );
        ctxt.depth -= 1;

        (ret, Some(buf))
    }
}
