use crate::{
    error::XmlParserErrors,
    libxml::parser::XmlParserInputState,
    parser::{xml_fatal_err, xml_fatal_err_msg_int, xml_is_char, XmlParserCtxt},
};

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
            (*ctxt.input).col += 1;
            (*ctxt.input).cur = (*ctxt.input).cur.add(1);
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
            (*ctxt.input).col += 1;
            (*ctxt.input).cur = (*ctxt.input).cur.add(1);
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
