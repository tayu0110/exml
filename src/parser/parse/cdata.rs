use std::{ptr::fn_addr_eq, str::from_utf8_unchecked};

use crate::{
    error::XmlParserErrors,
    libxml::{
        chvalid::{xml_is_blank_char, xml_is_char},
        parser::{XML_PARSER_BIG_BUFFER_SIZE, XmlParserInputState, XmlParserOption},
        parser_internals::{XML_MAX_HUGE_LENGTH, XML_MAX_TEXT_LENGTH},
        valid::xml_is_mixed_element,
    },
    parser::{
        XmlParserCtxt, xml_fatal_err, xml_fatal_err_msg, xml_fatal_err_msg_int,
        xml_fatal_err_msg_str,
    },
    tree::{NodeCommon, XmlElementType},
};

/// Is this a sequence of blank chars that one can ignore ?
///
/// Returns 1 if ignorable 0 otherwise.
#[doc(alias = "areBlanks")]
unsafe fn are_blanks(ctxt: &mut XmlParserCtxt, s: &str, blank_chars: bool) -> bool {
    unsafe {
        // Don't spend time trying to differentiate them, the same callback is used !
        if ctxt.sax.as_deref().is_none_or(|sax| {
            (sax.ignorable_whitespace.is_none() && sax.characters.is_none())
                || sax
                    .ignorable_whitespace
                    .zip(sax.characters)
                    .is_some_and(|(l, r)| fn_addr_eq(l, r))
        }) {
            return false;
        }

        // Check for xml:space value.
        if ctxt.space() == 1 || ctxt.space() == -2 {
            return false;
        }

        // Check that the string is made of blanks
        if !blank_chars && s.chars().any(|c| !xml_is_blank_char(c as u32)) {
            return false;
        }

        // Look if the element is mixed content in the DTD if available
        let Some(context_node) = ctxt.node else {
            return false;
        };
        if let Some(my_doc) = ctxt.my_doc {
            let ret = xml_is_mixed_element(my_doc, &context_node.name().unwrap());
            if ret == 0 {
                return true;
            }
            if ret == 1 {
                return false;
            }
        }

        // Otherwise, heuristic :-\
        if ctxt.current_byte() != b'<' && ctxt.current_byte() != 0xD {
            return false;
        }
        if context_node.children().is_none()
            && ctxt.current_byte() == b'<'
            && ctxt.nth_byte(1) == b'/'
        {
            // index out of bound may occur at this `nth_byte` ??? It may be necessary to fix.
            return false;
        }

        if let Some(last_child) = context_node.get_last_child() {
            if last_child.is_text_node()
                || context_node
                    .children()
                    .filter(|c| c.is_text_node())
                    .is_some()
            {
                return false;
            }
        } else if context_node.element_type() != XmlElementType::XmlElementNode
            && context_node.content.is_some()
        {
            return false;
        }
        true
    }
}

/// Always makes progress if the first char isn't '<' or '&'.
///
/// parse a CharData section.this is the fallback function
/// of xmlParseCharData() when the parsing requires handling
/// of non-ASCII characters.
#[doc(alias = "xmlParseCharDataComplex")]
unsafe fn parse_char_data_complex(ctxt: &mut XmlParserCtxt, partial: i32) {
    unsafe {
        let mut l: i32 = 0;

        let mut buf = String::with_capacity(XML_PARSER_BIG_BUFFER_SIZE + 5);
        let mut cur = ctxt.current_char(&mut l).unwrap_or('\0');
        // test also done in xmlCurrentChar()
        while cur != '<' && cur != '&' && xml_is_char(cur as u32) {
            if cur == ']' && ctxt.nth_byte(1) == b']' && ctxt.nth_byte(2) == b'>' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrMisplacedCDATAEnd, None);
            }
            buf.push(cur);
            // move current position before possible calling of (*ctxt.sax).characters
            ctxt.advance_with_line_handling(l as usize);
            if buf.len() >= XML_PARSER_BIG_BUFFER_SIZE {
                // OK the segment is to be consumed as chars.
                if ctxt.disable_sax == 0 {
                    let blank = are_blanks(ctxt, &buf, false);
                    if let Some(sax) = ctxt.sax.as_deref_mut() {
                        if blank {
                            if let Some(ignorable_whitespace) = sax.ignorable_whitespace {
                                ignorable_whitespace(ctxt.user_data.clone(), &buf);
                            }
                        } else {
                            if let Some(characters) = sax.characters {
                                characters(ctxt.user_data.clone(), &buf);
                            }
                            if (sax.ignorable_whitespace.is_some() || sax.characters.is_some())
                                && sax
                                    .ignorable_whitespace
                                    .zip(sax.characters)
                                    .is_none_or(|(l, r)| !fn_addr_eq(l, r))
                                && ctxt.space() == -1
                            {
                                *ctxt.space_mut() = -2;
                            }
                        }
                    }
                }
                buf.clear();
                // something really bad happened in the SAX callback
                if !matches!(ctxt.instate, XmlParserInputState::XmlParserContent) {
                    return;
                }
                ctxt.shrink();
            }
            cur = ctxt.current_char(&mut l).unwrap_or('\0');
        }
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return;
        }
        if !buf.is_empty() {
            // OK the segment is to be consumed as chars.
            if ctxt.disable_sax == 0 {
                let blank = are_blanks(ctxt, &buf, false);
                if let Some(sax) = ctxt.sax.as_deref_mut() {
                    if blank {
                        if let Some(ignorable_whitespace) = sax.ignorable_whitespace {
                            ignorable_whitespace(ctxt.user_data.clone(), &buf);
                        }
                    } else {
                        if let Some(characters) = sax.characters {
                            characters(ctxt.user_data.clone(), &buf);
                        }
                        if (sax.ignorable_whitespace.is_some() || sax.characters.is_some())
                            && sax
                                .ignorable_whitespace
                                .zip(sax.characters)
                                .is_none_or(|(l, r)| !fn_addr_eq(l, r))
                            && ctxt.space() == -1
                        {
                            *ctxt.space_mut() = -2;
                        }
                    }
                }
            }
        }
        // cur == 0 can mean
        //
        // - xmlParserInputState::XmlParserEOF or memory error. This is checked above.
        // - An actual 0 character.
        // - End of buffer.
        // - An incomplete UTF-8 sequence. This is allowed if partial is set.
        if !ctxt.content_bytes().is_empty() {
            if cur == '\0' && ctxt.current_byte() != 0 {
                if partial == 0 {
                    xml_fatal_err_msg_int!(
                        ctxt,
                        XmlParserErrors::XmlErrInvalidChar,
                        format!(
                            "Incomplete UTF-8 sequence starting with {:02X}\n",
                            ctxt.current_byte()
                        )
                        .as_str(),
                        ctxt.current_byte() as i32
                    );
                    ctxt.advance_with_line_handling(1);
                }
            } else if cur != '<' && cur != '&' {
                // Generate the error and skip the offending character
                xml_fatal_err_msg_int!(
                    ctxt,
                    XmlParserErrors::XmlErrInvalidChar,
                    format!("PCDATA invalid Char value {}\n", cur as i32).as_str(),
                    cur as i32
                );
                ctxt.advance_with_line_handling(l as usize);
            }
        }
    }
}

// used for the test in the inner loop of the char data testing
// &    : start reference
// <    : start tag
// ]    : end CDATA section
const TEST_CHAR_DATA: [u8; 256] = [
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x09, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x00, 0x27, 0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B, 0x00, 0x3D, 0x3E, 0x3F,
    0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
    0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x00, 0x5E, 0x5F,
    0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
    0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
];

/// Parse character data. Always makes progress if the first char isn't '<' or '&'.
///
/// The right angle bracket (>) may be represented using the string "&gt;",
/// and must, for compatibility, be escaped using "&gt;" or a character
/// reference when it appears in the string "]]>" in content, when that
/// string is not marking the end of a CDATA section.
///
/// ````text
/// [14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
/// ```
#[doc(alias = "xmlParseCharDataInternal")]
pub(crate) unsafe fn parse_char_data_internal(ctxt: &mut XmlParserCtxt, partial: i32) {
    unsafe {
        ctxt.grow();
        // Accelerated common case where input don't need to be
        // modified before passing it to the handler.
        let mut input = ctxt.content_bytes();
        let mut line = ctxt.input().unwrap().line;
        let mut col = ctxt.input().unwrap().col;
        let mut buf = String::new();
        while matches!(input.first(), Some(&b'\n' | &b'\t' | &(0x20..=0x7F))) {
            while matches!(input.first(), Some(&b' ' | &b'\n')) {
                let spaces = input.iter().position(|&b| b != b' ').unwrap_or(input.len());
                col += spaces as i32;
                input = &input[spaces..];
                let lines = input
                    .iter()
                    .position(|&b| b != b'\n')
                    .unwrap_or(input.len());
                if lines != 0 {
                    line += lines as i32;
                    col = 1;
                    input = &input[lines..];
                }
            }

            if input.first() == Some(&b'<') {
                let len = ctxt.content_bytes().len() - input.len();
                if len > 0 {
                    buf.clear();
                    // # Safety
                    // `ctxt.content_bytes()[..len]` contains only ASCII characters.
                    // Therefore, UTF-8 validation won't be failed.
                    buf.push_str(from_utf8_unchecked(&ctxt.content_bytes()[..len]));

                    // commit consumed bytes for SAX interface
                    ctxt.input_mut().unwrap().cur = ctxt.input().unwrap().cur.add(len);
                    ctxt.input_mut().unwrap().line = line;
                    ctxt.input_mut().unwrap().col = col;

                    let blank = are_blanks(ctxt, &buf, true);

                    if let Some(sax) = ctxt.sax.as_deref_mut().filter(|sax| {
                        (sax.ignorable_whitespace.is_some() || sax.characters.is_some())
                            && sax
                                .ignorable_whitespace
                                .zip(sax.characters)
                                .is_none_or(|(l, r)| !fn_addr_eq(l, r))
                    }) {
                        if blank {
                            if let Some(ignorable_whitespace) = sax.ignorable_whitespace {
                                ignorable_whitespace(ctxt.user_data.clone(), &buf);
                            }
                        } else {
                            if let Some(characters) = sax.characters {
                                characters(ctxt.user_data.clone(), &buf);
                            }
                            if ctxt.space() == -1 {
                                *ctxt.space_mut() = -2;
                            }
                        }
                    } else if let Some(characters) =
                        ctxt.sax.as_deref_mut().and_then(|sax| sax.characters)
                    {
                        characters(ctxt.user_data.clone(), &buf);
                    }
                }
                return;
            }

            while input
                .first()
                .filter(|&&b| TEST_CHAR_DATA[b as usize] != 0 || matches!(b, 0xA | b']'))
                .is_some()
            {
                let chars = input
                    .iter()
                    .position(|&b| TEST_CHAR_DATA[b as usize] == 0)
                    .unwrap_or(input.len());
                col += chars as i32;
                input = &input[chars..];

                let lines = input.iter().position(|&b| b != 0xA).unwrap_or(input.len());
                if lines != 0 {
                    line += lines as i32;
                    col = 1;
                    input = &input[lines..];
                }

                if let Some(rem) = input.strip_prefix(b"]") {
                    // "]]>" must not be contained in CharData
                    if rem.starts_with(b"]>") {
                        // commit consumed bytes before raise an error
                        let diff = ctxt.content_bytes().len() - input.len();
                        ctxt.input_mut().unwrap().line = line;
                        ctxt.input_mut().unwrap().col = col;
                        ctxt.input_mut().unwrap().cur = ctxt.input().unwrap().cur.add(diff);
                        xml_fatal_err(ctxt, XmlParserErrors::XmlErrMisplacedCDATAEnd, None);
                        if !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                            // if the parser input state is not EOF,
                            // consume the head of ']'.
                            ctxt.input_mut().unwrap().cur = ctxt.input_mut().unwrap().cur.add(1);
                        }
                        return;
                    }
                    col += 1;
                    input = rem;
                }
            }

            let len = ctxt.content_bytes().len() - input.len();
            if len > 0 {
                buf.clear();
                // # Safety
                // `ctxt.content_bytes()[..len]` contains only ASCII characters.
                // Therefore, UTF-8 validation won't be failed.
                buf.push_str(from_utf8_unchecked(&ctxt.content_bytes()[..len]));

                // commit consumed bytes for SAX interface
                ctxt.input_mut().unwrap().cur = ctxt.input().unwrap().cur.add(len);
                ctxt.input_mut().unwrap().line = line;
                ctxt.input_mut().unwrap().col = col;

                let blank = are_blanks(ctxt, &buf, false);

                if let Some(sax) = ctxt.sax.as_deref_mut().filter(|sax| {
                    (sax.ignorable_whitespace.is_some() || sax.characters.is_some())
                        && sax
                            .ignorable_whitespace
                            .zip(sax.characters)
                            .is_none_or(|(l, r)| !fn_addr_eq(l, r))
                }) {
                    if blank {
                        if let Some(ignorable_whitespace) = sax.ignorable_whitespace {
                            ignorable_whitespace(ctxt.user_data.clone(), &buf);
                        }
                    } else {
                        if let Some(characters) = sax.characters {
                            characters(ctxt.user_data.clone(), &buf);
                        }
                        if ctxt.space() == -1 {
                            *ctxt.space_mut() = -2;
                        }
                    }
                } else if let Some(characters) =
                    ctxt.sax.as_deref_mut().and_then(|sax| sax.characters)
                {
                    characters(ctxt.user_data.clone(), &buf);
                }

                // refresh input buffer
                input = ctxt.content_bytes();
            }

            // Replace "\r\n" to "\n" and skip
            if input.starts_with(b"\r\n") {
                line += 1;
                col = 1;
                // To skip '\r', `+1` is needed.
                let diff = ctxt.content_bytes().len() - input.len() + 1;
                // At this point, `content_bytes` will start with '\n' ('\r' is skipped)
                ctxt.input_mut().unwrap().cur = ctxt.input().unwrap().cur.add(diff);
                // and skip '\n' at the head of `content_bytes`.
                input = &ctxt.content_bytes()[1..];
                continue;
            }

            // At this point, the buffer is already committed,
            // so commit only line and col
            ctxt.input_mut().unwrap().line = line;
            ctxt.input_mut().unwrap().col = col;

            if matches!(ctxt.content_bytes().first(), Some(&(b'<' | b'&'))) {
                return;
            }

            ctxt.shrink();
            ctxt.grow();
            if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                return;
            }

            // refresh input buffer
            input = ctxt.content_bytes();
        }

        let len = ctxt.content_bytes().len() - input.len();
        ctxt.input_mut().unwrap().cur = ctxt.input_mut().unwrap().cur.add(len);
        ctxt.input_mut().unwrap().line = line;
        ctxt.input_mut().unwrap().col = col;
        parse_char_data_complex(ctxt, partial);
    }
}

/// Check that the block of characters is okay as SCdata content [20]
///
/// Returns the number of bytes to pass if okay, a negative index where an
/// UTF-8 error occurred otherwise
#[doc(alias = "xmlCheckCdataPush")]
#[cfg(feature = "libxml_push")]
pub(crate) fn check_cdata_push(utf: &[u8], complete: bool) -> Result<usize, usize> {
    use std::str::{from_utf8, from_utf8_unchecked};

    if utf.is_empty() {
        return Ok(0);
    }

    let s = match from_utf8(utf) {
        Ok(s) => s,
        Err(e) => {
            let s = unsafe {
                // # Safety
                // Refer to the document of `from_utf8` and `Utf8Error`.
                from_utf8_unchecked(&utf[..e.valid_up_to()])
            };
            // If `complete` is `true`, it is invalid not to reach the end.
            // If `e.error_len().is_some()` is `true`,
            // it is still invalid because it contains an invalid byte sequence.
            if complete || e.error_len().is_some() {
                return Err(s.find(|c: char| !xml_is_char(c as u32)).unwrap_or(s.len()));
            }
            s
        }
    };

    // Even a valid UTF-8 sequence may contain characters
    // that do not conform to the XML specification.
    s.find(|c: char| !xml_is_char(c as u32))
        .map_or(Ok(s.len()), Err)
}

/// Check whether the input buffer contains a character.
#[doc(alias = "xmlParseLookupChar")]
pub(crate) unsafe fn parse_lookup_char(ctxt: &mut XmlParserCtxt, c: u8) -> i32 {
    unsafe {
        let cur = &ctxt.content_bytes()[ctxt.check_index.max(1) as usize..];

        if !cur.contains(&c) {
            if cur.len() > i64::MAX as usize {
                ctxt.check_index = 0;
                1
            } else {
                ctxt.check_index = cur.len() as i64;
                0
            }
        } else {
            ctxt.check_index = 0;
            1
        }
    }
}

/// Check whether the input buffer contains terminated c_char data.
#[doc(alias = "xmlParseLookupCharData")]
pub(crate) unsafe fn parse_lookup_char_data(ctxt: &mut XmlParserCtxt) -> i32 {
    unsafe {
        let cur = &ctxt.content_bytes()[ctxt.check_index as usize..];

        if cur.contains(&b'<') || cur.contains(&b'&') || cur.len() > i64::MAX as usize {
            ctxt.check_index = 0;
            1
        } else {
            ctxt.check_index = cur.len() as i64;
            0
        }
    }
}

/// Parse escaped pure raw content. Always consumes '<!['.
///
/// ```text
/// [18] CDSect ::= CDStart CData CDEnd
/// [19] CDStart ::= '<![CDATA['
/// [20] Data ::= (Char* - (Char* ']]>' Char*))
/// [21] CDEnd ::= ']]>'
/// ```
#[doc(alias = "xmlParseCDSect")]
pub(crate) unsafe fn parse_cdsect(ctxt: &mut XmlParserCtxt) {
    unsafe {
        let mut rl: i32 = 0;
        let mut sl: i32 = 0;
        let mut l: i32 = 0;
        let max_length = if ctxt.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_HUGE_LENGTH
        } else {
            XML_MAX_TEXT_LENGTH
        };

        if !ctxt.content_bytes().starts_with(b"<![CDATA[") {
            return;
        }
        ctxt.advance(9);

        ctxt.instate = XmlParserInputState::XmlParserCDATASection;
        let mut r = ctxt.current_char(&mut rl).unwrap_or('\0');
        if !xml_is_char(r as u32) {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrCDATANotFinished, None);
            if !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                ctxt.instate = XmlParserInputState::XmlParserContent;
            }
            return;
        }
        ctxt.advance_with_line_handling(rl as usize);
        let mut s = ctxt.current_char(&mut sl).unwrap_or('\0');
        if !xml_is_char(s as u32) {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrCDATANotFinished, None);
            if !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                ctxt.instate = XmlParserInputState::XmlParserContent;
            }
            return;
        }
        ctxt.advance_with_line_handling(sl as usize);
        let mut cur = ctxt.current_char(&mut l).unwrap_or('\0');
        let mut buf = String::new();
        while xml_is_char(cur as u32) && (r != ']' || s != ']' || cur != '>') {
            buf.push(r);
            if buf.len() > max_length {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrCDATANotFinished,
                    "CData section too big found\n",
                );
                if !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                    ctxt.instate = XmlParserInputState::XmlParserContent;
                }
                return;
            }
            r = s;
            s = cur;
            ctxt.advance_with_line_handling(l as usize);
            cur = ctxt.current_char(&mut l).unwrap_or('\0');
        }
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return;
        }
        if cur != '>' {
            xml_fatal_err_msg_str!(
                ctxt,
                XmlParserErrors::XmlErrCDATANotFinished,
                "CData section not finished\n{}\n",
                buf
            );
            // goto out;
            if !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                ctxt.instate = XmlParserInputState::XmlParserContent;
            }
            return;
        }
        ctxt.advance_with_line_handling(l as usize);

        // OK the buffer is to be consumed as cdata.
        if ctxt.disable_sax == 0 {
            if let Some(sax) = ctxt.sax.as_deref_mut() {
                if let Some(cdata) = sax.cdata_block {
                    cdata(ctxt.user_data.clone(), &buf);
                } else if let Some(characters) = sax.characters {
                    characters(ctxt.user_data.clone(), &buf);
                }
            }
        }

        // out:
        if !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            ctxt.instate = XmlParserInputState::XmlParserContent;
        }
    }
}
