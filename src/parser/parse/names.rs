use crate::{
    error::XmlParserErrors,
    libxml::{
        parser::{XmlParserInputState, XmlParserOption},
        parser_internals::{XML_MAX_NAME_LENGTH, XML_MAX_TEXT_LENGTH, xml_is_letter},
    },
    parser::{
        XmlParserCharValid, XmlParserCtxt, build_qname, xml_fatal_err, xml_is_combining,
        xml_is_digit, xml_is_extender, xml_ns_err,
    },
};

/// Parse an XML Nmtoken.
///
/// `[7] Nmtoken ::= (NameChar)+`
///
/// `[8] Nmtokens ::= Nmtoken (#x20 Nmtoken)*`
///
/// Returns the Nmtoken parsed or NULL
#[doc(alias = "xmlParseNmtoken")]
pub(crate) unsafe fn parse_nmtoken(ctxt: &mut XmlParserCtxt) -> Option<String> {
    unsafe {
        let max_length = if ctxt.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_TEXT_LENGTH
        } else {
            XML_MAX_NAME_LENGTH
        };
        let mut res = String::new();

        while let Some(c) = ctxt.consume_char_if(|ctxt, c| c.is_name_char(ctxt)) {
            res.push(c);
            if res.len() > max_length {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("NmToken"));
                return None;
            }
        }
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }
        if res.is_empty() {
            return None;
        }
        Some(res)
    }
}

pub(crate) unsafe fn parse_ncname_complex(ctxt: &mut XmlParserCtxt) -> Option<String> {
    unsafe {
        let max_length = if ctxt.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_TEXT_LENGTH
        } else {
            XML_MAX_NAME_LENGTH
        };

        // Handler for more complex cases
        let c = ctxt.consume_char_if(|ctxt, c| {
            c != ' ' && c != '>' && c != '/' && c.is_name_start_char(ctxt) && c != ':'
        })?;
        let mut buf = String::with_capacity(c.len_utf8());
        buf.push(c);

        while let Some(c) = ctxt.consume_char_if(|ctxt, c| {
            c != ' ' && c != '>' && c != '/' && c.is_name_char(ctxt) && c != ':'
        }) {
            buf.push(c);
            if buf.len() > max_length {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("NCName"));
                return None;
            }
        }
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }
        (!buf.is_empty()).then_some(buf)
    }
}

/// Parse an XML name.
///
/// ```text
/// [4NS] NCNameChar ::= Letter | Digit | '.' | '-' | '_' | CombiningChar | Extender
///
/// [5NS] NCName ::= (Letter | '_') (NCNameChar)*
/// ```
///
/// Returns the Name parsed or NULL
#[doc(alias = "xmlParseNCName")]
pub(crate) unsafe fn parse_ncname(ctxt: &mut XmlParserCtxt) -> Option<String> {
    unsafe {
        let max_length = if ctxt.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_TEXT_LENGTH
        } else {
            XML_MAX_NAME_LENGTH
        };

        // Accelerator for simple ASCII names
        let content = ctxt.content_bytes();
        if !content.is_empty() && (content[0].is_ascii_alphabetic() || content[0] == b'_') {
            for (i, &b) in content.iter().enumerate().skip(1) {
                if !b.is_ascii_alphanumeric() && b != b'_' && b != b'-' && b != b'.' {
                    if !(1..0x80).contains(&b) {
                        break;
                    }
                    if i > max_length {
                        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("NCName"));
                        return None;
                    }
                    // # Safety
                    // `content[..i]` contains only ASCII characters.
                    // Therefore, UTF-8 validation won't fail.
                    let res = String::from_utf8_unchecked(content[..i].to_vec());
                    // `content[..i]` contains no line delimiters,
                    // so we need not use `ctxt.advance_with_line_handling(i)`.
                    ctxt.advance(i);
                    return Some(res);
                }
            }
        }
        parse_ncname_complex(ctxt)
    }
}

unsafe fn parse_name_complex(ctxt: &mut XmlParserCtxt) -> Option<String> {
    unsafe {
        let max_length = if ctxt.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_TEXT_LENGTH
        } else {
            XML_MAX_NAME_LENGTH
        };
        let mut buf = String::new();

        // Handler for more complex cases
        if ctxt.options & XmlParserOption::XmlParseOld10 as i32 == 0 {
            // Use the new checks of production [4] [4a] amd [5] of the
            // Update 5 of XML-1.0
            let c = ctxt.consume_char_if(|_, c| {
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
            while let Some(c) = ctxt.consume_char_if(|_, c| {
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
            let c = ctxt.consume_char_if(|_, c| {
                c != ' '
                    && c != '>'
                    && c != '/'
                    && (xml_is_letter(c as u32) || c == '_' || c == ':')
            })?;
            buf.push(c);

            while let Some(c) = ctxt.consume_char_if(|_, c| {
                c != ' '
                    && c != '>'
                    && c != '/'
                    && (xml_is_letter(c as u32)
                        || xml_is_digit(c as u32)
                        || c == '.'
                        || c == '-'
                        || c == '_'
                        || c == ':'
                        || xml_is_combining(c as u32)
                        || xml_is_extender(c as u32))
            }) {
                buf.push(c);
            }
        }
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }
        if buf.len() > max_length {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("Name"));
            return None;
        }
        if (*ctxt.input().unwrap()).offset_from_base() < buf.len() {
            // There were a couple of bugs where PERefs lead to to a change
            // of the buffer. Check the buffer size to avoid passing an invalid
            // pointer to xmlDictLookup.
            xml_fatal_err(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                Some("unexpected change of input buffer"),
            );
            return None;
        }
        Some(buf)
    }
}

/// Parse an XML name.
///
/// ```text
/// [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender
///
/// [5] Name ::= (Letter | '_' | ':') (NameChar)*
///
/// [6] Names ::= Name (#x20 Name)*
/// ```
///
/// Returns the Name parsed or NULL
#[doc(alias = "xmlParseName")]
pub(crate) unsafe fn parse_name(ctxt: &mut XmlParserCtxt) -> Option<String> {
    unsafe {
        let max_length = if ctxt.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_TEXT_LENGTH
        } else {
            XML_MAX_NAME_LENGTH
        };

        ctxt.grow();
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }

        // Accelerator for simple ASCII names
        let content = ctxt.content_bytes();
        if !content.is_empty()
            && (content[0].is_ascii_alphabetic() || content[0] == b'_' || content[0] == b':')
        {
            for (i, &b) in content.iter().enumerate().skip(1) {
                if !b.is_ascii_alphanumeric() && b != b'_' && b != b'-' && b != b':' && b != b'.' {
                    if !(1..0x80).contains(&b) {
                        break;
                    }
                    if i > max_length {
                        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("Name"));
                        return None;
                    }
                    // # Safety
                    // `content[..i]` contains only ASCII characters.
                    // Therefore, UTF-8 validation won't fail.
                    let res = String::from_utf8_unchecked(content[..i].to_vec());
                    // `content[..i]` contains no line delimiters,
                    // so we need not use `ctxt.advance_with_line_handling(i)`.
                    ctxt.advance(i);
                    return Some(res);
                }
            }
        }
        // accelerator for special cases
        parse_name_complex(ctxt)
    }
}

/// Parse an XML Namespace QName
///
/// ```text
/// [6]  QName  ::= (Prefix ':')? LocalPart
/// [7]  Prefix  ::= NCName
/// [8]  LocalPart  ::= NCName
/// ```
///
/// Returns the Name parsed or NULL
#[doc(alias = "xmlParseQName")]
pub(crate) unsafe fn parse_qname(ctxt: &mut XmlParserCtxt) -> (Option<String>, Option<String>) {
    unsafe {
        ctxt.grow();
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return (None, None);
        }

        let Some(l) = parse_ncname(ctxt) else {
            if ctxt.current_byte() == b':' {
                if let Some(l) = parse_name(ctxt) {
                    xml_ns_err!(
                        ctxt,
                        XmlParserErrors::XmlNsErrQname,
                        "Failed to parse QName '{}'\n",
                        l
                    );
                    return (None, Some(l));
                }
            }
            return (None, None);
        };
        if ctxt.current_byte() == b':' {
            ctxt.skip_char();
            let p = l;
            let Some(l) = parse_ncname(ctxt) else {
                if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                    return (None, None);
                }
                xml_ns_err!(
                    ctxt,
                    XmlParserErrors::XmlNsErrQname,
                    "Failed to parse QName '{}:'\n",
                    p
                );
                let l = parse_nmtoken(ctxt);
                let p = if let Some(l) = l.as_deref() {
                    build_qname(l, Some(&p))
                } else {
                    if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                        return (None, None);
                    }
                    build_qname("", Some(&p))
                };
                return (None, Some(p.into_owned()));
            };
            if ctxt.current_byte() == b':' {
                xml_ns_err!(
                    ctxt,
                    XmlParserErrors::XmlNsErrQname,
                    "Failed to parse QName '{}:{}:'\n",
                    p,
                    l
                );
                ctxt.skip_char();
                if let Some(tmp) = parse_name(ctxt) {
                    let l = build_qname(&tmp, Some(&l));
                    return (Some(p), Some(l.into_owned()));
                }
                if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
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
