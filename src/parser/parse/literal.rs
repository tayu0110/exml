use crate::{
    error::XmlParserErrors,
    libxml::{
        chvalid::{xml_is_char, xml_is_pubid_char},
        parser::{XmlParserInputState, XmlParserOption},
        parser_internals::{XML_MAX_NAME_LENGTH, XML_MAX_TEXT_LENGTH},
    },
    parser::{XmlParserCtxt, xml_fatal_err, xml_fatal_err_msg},
};

/// Parse an XML public literal
///
/// ```text
/// [12] PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
/// ```
///
/// Returns the PubidLiteral parsed or NULL.
#[doc(alias = "xmlParsePubidLiteral")]
unsafe fn parse_pubid_literal(ctxt: &mut XmlParserCtxt) -> Option<String> {
    unsafe {
        let max_length = if ctxt.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_TEXT_LENGTH
        } else {
            XML_MAX_NAME_LENGTH
        };

        let oldstate = ctxt.instate;

        if !matches!(ctxt.current_byte(), b'"' | b'\'') {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrLiteralNotStarted, None);
            return None;
        }
        let stop = ctxt.current_byte();
        ctxt.skip_char();

        let mut buf = String::new();
        ctxt.instate = XmlParserInputState::XmlParserPublicLiteral;
        let mut cur = ctxt.current_byte();
        while xml_is_pubid_char(cur as u32) && cur != stop {
            // Since PubidChar is a subset of ASCII,
            // there is no problem with casting to `char`.
            buf.push(cur as char);
            if buf.len() > max_length {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("Public ID"));
                return None;
            }
            ctxt.skip_char();
            cur = ctxt.current_byte();
        }
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }
        if cur != stop {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrLiteralNotFinished, None);
        } else {
            ctxt.advance(1);
        }
        ctxt.instate = oldstate;
        Some(buf)
    }
}

/// Parse an XML Literal
///
/// ```text
/// [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
/// ```
///
/// Returns the SystemLiteral parsed or NULL
#[doc(alias = "xmlParseSystemLiteral")]
unsafe fn parse_system_literal(ctxt: &mut XmlParserCtxt) -> Option<String> {
    unsafe {
        let mut l = 0;
        let max_length = if ctxt.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_TEXT_LENGTH
        } else {
            XML_MAX_NAME_LENGTH
        };
        let state = ctxt.instate;

        if !matches!(ctxt.current_byte(), b'"' | b'\'') {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrLiteralNotStarted, None);
            return None;
        }
        let stop = ctxt.current_byte();
        ctxt.skip_char();

        let mut buf = String::new();
        ctxt.instate = XmlParserInputState::XmlParserSystemLiteral;
        let mut cur = ctxt.current_char(&mut l).unwrap_or('\0');
        while xml_is_char(cur as u32) && cur as i32 != stop as i32 {
            buf.push(cur);
            if buf.len() > max_length {
                xml_fatal_err(
                    ctxt,
                    XmlParserErrors::XmlErrNameTooLong,
                    Some("SystemLiteral"),
                );
                ctxt.instate = state;
                return None;
            }
            ctxt.advance_with_line_handling(cur.len_utf8());
            cur = ctxt.current_char(&mut l).unwrap_or('\0');
        }
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }
        ctxt.instate = state;
        if !xml_is_char(cur as u32) {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrLiteralNotFinished, None);
        } else {
            ctxt.skip_char();
        }
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
    ctxt: &mut XmlParserCtxt,
    strict: bool,
) -> (Option<String>, Option<String>) {
    unsafe {
        let mut uri = None;
        let mut public_id = None;

        if ctxt.content_bytes().starts_with(b"SYSTEM") {
            ctxt.advance(6);
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after b'SYSTEM'\n",
                );
            }
            uri = parse_system_literal(ctxt);
            if uri.is_none() {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrURIRequired, None);
            }
        } else if ctxt.content_bytes().starts_with(b"PUBLIC") {
            ctxt.advance(6);
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after 'PUBLIC'\n",
                );
            }
            public_id = parse_pubid_literal(ctxt);
            if public_id.is_none() {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrPubidRequired, None);
            }
            if strict {
                // We don't handle [83] so "S SystemLiteral" is required.
                if ctxt.skip_blanks() == 0 {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrSpaceRequired,
                        "Space required after the Public Identifier\n",
                    );
                }
            } else {
                // We handle [83] so we return immediately, if
                // "S SystemLiteral" is not detected. We skip blanks if no
                // system literal was found, but this is harmless since we must
                // be at the end of a NotationDecl.
                if ctxt.skip_blanks() == 0 {
                    return (public_id, None);
                }
                if ctxt.current_byte() != b'\'' && ctxt.current_byte() != b'"' {
                    return (public_id, None);
                }
            }
            uri = parse_system_literal(ctxt);
            if uri.is_none() {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrURIRequired, None);
            }
        }
        (public_id, uri)
    }
}
