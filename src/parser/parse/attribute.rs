use std::{borrow::Cow, ffi::CStr};

use crate::{
    error::XmlParserErrors,
    libxml::{
        chvalid::xml_is_char,
        parser::{XmlParserInputState, XmlParserOption},
        parser_internals::{XML_MAX_HUGE_LENGTH, XML_MAX_TEXT_LENGTH, XML_SUBSTITUTE_REF},
    },
    parser::{
        XmlParserCtxt, check_language_id, xml_err_memory, xml_fatal_err, xml_fatal_err_msg,
        xml_fatal_err_msg_str, xml_warning_msg,
    },
    tree::{NodeCommon, XML_ENT_CHECKED, XmlEntityType},
};

use super::{
    parse_char_ref, parse_entity_ref, parse_qname, parser_entity_check, string_decode_entities_int,
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
pub(crate) unsafe fn parse_att_value_internal(
    ctxt: &mut XmlParserCtxt,
    normalize: bool,
) -> Option<String> {
    unsafe {
        let max_length = if ctxt.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_HUGE_LENGTH
        } else {
            XML_MAX_TEXT_LENGTH
        };

        macro_rules! GROW_PARSE_ATT_VALUE_INTERNAL {
            ($ctxt:expr, $input:expr, $start:expr) => {
                let diff = $start.len() - $input.len();
                $ctxt.grow();
                if matches!($ctxt.instate, XmlParserInputState::XmlParserEOF) {
                    return None;
                }
                $start = $ctxt.content_bytes();
                $input = &$start[diff..];
            };
        }

        ctxt.grow();
        let input = ctxt.content_bytes();
        let mut line: i32 = ctxt.input().unwrap().line;
        let mut col: i32 = ctxt.input().unwrap().col;
        if input.is_empty() || (input[0] != b'"' && input[0] != b'\'') {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrAttributeNotStarted, None);
            return None;
        }
        ctxt.instate = XmlParserInputState::XmlParserAttributeValue;
        let mut input = ctxt.content_bytes();

        // try to handle in this routine the most common case where no
        // allocation of a new string is required and where content is pure ASCII.
        let limit = input[0];
        input = &input[1..];
        col += 1;
        let mut start = input;
        if input.is_empty() {
            GROW_PARSE_ATT_VALUE_INTERNAL!(ctxt, input, start);
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
                    GROW_PARSE_ATT_VALUE_INTERNAL!(ctxt, input, start);
                    if start.len() - input.len() > max_length {
                        xml_fatal_err_msg(
                            ctxt,
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
                    GROW_PARSE_ATT_VALUE_INTERNAL!(ctxt, input, start);
                    if start.len() - input.len() > max_length {
                        xml_fatal_err_msg(
                            ctxt,
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
                    ctxt.grow();
                    if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                        return None;
                    }
                    start = ctxt.content_bytes();
                    input = &start[diff..];
                    last = &start[diffl..];

                    if start.len() - input.len() > max_length {
                        xml_fatal_err_msg(
                            ctxt,
                            XmlParserErrors::XmlErrAttributeNotFinished,
                            "AttValue length too long\n",
                        );
                        return None;
                    }
                }
            }
            if start.len() - input.len() > max_length {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrAttributeNotFinished,
                    "AttValue length too long\n",
                );
                return None;
            }
            if input.is_empty() || input[0] != limit {
                return parse_att_value_complex(ctxt, normalize);
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
                    GROW_PARSE_ATT_VALUE_INTERNAL!(ctxt, input, start);
                    if start.len() - input.len() > max_length {
                        xml_fatal_err_msg(
                            ctxt,
                            XmlParserErrors::XmlErrAttributeNotFinished,
                            "AttValue length too long\n",
                        );
                        return None;
                    }
                }
            }
            if start.len() - input.len() > max_length {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrAttributeNotFinished,
                    "AttValue length too long\n",
                );
                return None;
            }
            if input.is_empty() || input[0] != limit {
                return parse_att_value_complex(ctxt, normalize);
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
        ctxt.advance(consumed + 2);
        let input = ctxt.input_mut().unwrap();
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
unsafe fn parse_att_value_complex(ctxt: &mut XmlParserCtxt, normalize: bool) -> Option<String> {
    unsafe {
        let max_length = if ctxt.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_HUGE_LENGTH
        } else {
            XML_MAX_TEXT_LENGTH
        };
        let mut l: i32 = 0;
        let mut in_space: i32 = 0;

        let limit = if ctxt.current_byte() == b'"' {
            ctxt.instate = XmlParserInputState::XmlParserAttributeValue;
            ctxt.skip_char();
            b'"'
        } else if ctxt.current_byte() == b'\'' {
            ctxt.instate = XmlParserInputState::XmlParserAttributeValue;
            ctxt.skip_char();
            b'\''
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrAttributeNotStarted, None);
            return None;
        };

        // allocate a translation buffer.
        let mut buf = String::with_capacity(100);
        // OK loop until we reach one of the ending c_char or a size limit.
        let mut c = ctxt.current_char(&mut l).unwrap_or('\0');
        while ctxt.current_byte() != limit
            && xml_is_char(c as u32)
            && c != '<'
            && !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF)
        {
            if c == '&' {
                in_space = 0;
                if ctxt.nth_byte(1) == b'#' {
                    let val = parse_char_ref(&mut *ctxt);
                    if val == Some('&') {
                        if ctxt.replace_entities != 0 {
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
                    let ent = parse_entity_ref(ctxt);
                    if let Some(ent) = ent.filter(|ent| {
                        matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity)
                    }) {
                        if ctxt.replace_entities == 0 && *ent.content.add(0) == b'&' {
                            buf.push_str("&#38;");
                        } else {
                            buf.push(*ent.content as char);
                        }
                    } else if let Some(ent) = ent.filter(|_| ctxt.replace_entities != 0) {
                        if !matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity) {
                            if parser_entity_check(ctxt, ent.length as _) != 0 {
                                return None;
                            }

                            ctxt.depth += 1;
                            let rep = (!ent.content.is_null())
                                .then(|| {
                                    string_decode_entities_int(
                                        ctxt,
                                        &CStr::from_ptr(ent.content as *const i8).to_string_lossy(),
                                        XML_SUBSTITUTE_REF as _,
                                        '\0',
                                        '\0',
                                        '\0',
                                        1,
                                    )
                                })
                                .flatten();
                            ctxt.depth -= 1;
                            if let Some(rep) = rep {
                                for c in rep.chars() {
                                    if c == '\r' || c == '\n' || c == '\t' {
                                        buf.push('\x20');
                                    } else {
                                        buf.push(c);
                                    }
                                }
                            }
                        } else if !ent.content.is_null() {
                            buf.push(*ent.content as char);
                        }
                    } else if let Some(mut ent) = ent {
                        // We also check for recursion and amplification
                        // when entities are not substituted. They're
                        // often expanded later.
                        if !matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity)
                            && !ent.content.is_null()
                        {
                            if ent.flags & XML_ENT_CHECKED as i32 == 0 {
                                let old_copy: u64 = ctxt.sizeentcopy;

                                ctxt.sizeentcopy = ent.length as _;

                                ctxt.depth += 1;
                                let rep = (!ent.content.is_null())
                                    .then(|| {
                                        string_decode_entities_int(
                                            ctxt,
                                            &CStr::from_ptr(ent.content as *const i8)
                                                .to_string_lossy(),
                                            XML_SUBSTITUTE_REF as _,
                                            '\0',
                                            '\0',
                                            '\0',
                                            1,
                                        )
                                    })
                                    .flatten();
                                ctxt.depth -= 1;

                                // If we're parsing DTD content, the entity
                                // might reference other entities which
                                // weren't defined yet, so the check isn't
                                // reliable.
                                if ctxt.in_subset == 0 {
                                    ent.flags |= XML_ENT_CHECKED as i32;
                                    ent.expanded_size = ctxt.sizeentcopy;
                                }

                                if rep.is_none() {
                                    *ent.content.add(0) = 0;
                                }

                                if parser_entity_check(ctxt, old_copy) != 0 {
                                    return None;
                                }
                            } else if parser_entity_check(ctxt, ent.expanded_size) != 0 {
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
                ctxt.advance_with_line_handling(l as usize);
            }
            ctxt.grow();
            c = ctxt.current_char(&mut l).unwrap_or('\0');
            if buf.len() > max_length {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrAttributeNotFinished,
                    "AttValue length too long\n",
                );
                xml_err_memory(ctxt, None);
                return None;
            }
        }
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }

        if in_space != 0 && normalize {
            while buf.ends_with('\x20') {
                buf.pop();
            }
        }
        if ctxt.current_byte() == b'<' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrLtInAttribute, None);
        } else if ctxt.current_byte() != limit {
            if c != '\0' && !xml_is_char(c as u32) {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrInvalidChar,
                    "invalid character in attribute value\n",
                );
            } else {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrAttributeNotFinished,
                    "AttValue: ' expected\n",
                );
            }
        } else {
            ctxt.skip_char();
        }

        Some(buf)
    }
}

/// parse a value for an attribute
/// Note: the parser won't do substitution of entities here, this
/// will be handled later in xmlStringGetNodeList
///
/// `[10] AttValue ::= '"' ([^<&"] | Reference)* '"' | "'" ([^<&'] | Reference)* "'"`
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
pub(crate) unsafe fn parse_att_value(ctxt: &mut XmlParserCtxt) -> Option<String> {
    unsafe {
        ctxt.input()?;
        parse_att_value_internal(ctxt, false)
    }
}

/// Returns (Prefix, LocalPart, AttValue).
#[doc(alias = "xmlParseAttribute2")]
pub(crate) unsafe fn parse_attribute2(
    ctxt: &mut XmlParserCtxt,
    pref: Option<&str>,
    elem: &str,
) -> Option<(Option<String>, String, Option<String>)> {
    unsafe {
        let mut normalize = false;

        ctxt.grow();

        let (prefix, Some(name)) = parse_qname(ctxt) else {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                "error parsing attribute name\n",
            );
            return None;
        };

        // get the type if needed
        if let Some(atts) = ctxt.atts_special {
            if atts
                .qlookup2(pref, elem, prefix.as_deref(), Some(&name))
                .is_some()
            {
                normalize = true;
            }
        }

        // read the value
        ctxt.skip_blanks();

        if ctxt.current_byte() != b'=' {
            xml_fatal_err_msg_str!(
                ctxt,
                XmlParserErrors::XmlErrAttributeWithoutValue,
                "Specification mandates value for attribute {}\n",
                name
            );
            return Some((None, name, None));
        }
        ctxt.skip_char();
        ctxt.skip_blanks();
        let mut val = parse_att_value_internal(ctxt, normalize)?;
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
        ctxt.instate = XmlParserInputState::XmlParserContent;

        if prefix.as_deref() == ctxt.str_xml.as_deref() {
            // Check that xml:lang conforms to the specification
            // No more registered as an error, just generate a warning now
            // since this was deprecated in XML second edition
            if ctxt.pedantic != 0 && name == "lang" && !check_language_id(&val) {
                xml_warning_msg!(
                    ctxt,
                    XmlParserErrors::XmlWarLangValue,
                    "Malformed value for xml:lang : {}\n",
                    val
                );
            }

            // Check that xml:space conforms to the specification
            if name == "space" {
                if val == "default" {
                    *ctxt.space_mut() = 0;
                } else if val == "preserve" {
                    *ctxt.space_mut() = 1;
                } else {
                    xml_warning_msg!(
                        ctxt,
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
