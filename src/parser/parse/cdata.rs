use std::{ptr::fn_addr_eq, str::from_utf8_unchecked};

use crate::{
    error::XmlParserErrors,
    libxml::chvalid::{xml_is_blank_char, xml_is_char},
    parser::{
        XML_MAX_HUGE_LENGTH, XML_MAX_TEXT_LENGTH, XML_PARSER_BIG_BUFFER_SIZE, XmlParserCtxt,
        XmlParserInputState, XmlParserOption, xml_fatal_err, xml_fatal_err_msg,
        xml_fatal_err_msg_int, xml_fatal_err_msg_str,
    },
    tree::{NodeCommon, XmlElementType},
    valid::xml_is_mixed_element,
};

impl XmlParserCtxt<'_> {
    /// Is this a sequence of blank chars that one can ignore ?
    ///
    /// Returns 1 if ignorable 0 otherwise.
    #[doc(alias = "areBlanks")]
    fn are_blanks(&mut self, s: &str, blank_chars: bool) -> bool {
        // Don't spend time trying to differentiate them, the same callback is used !
        if self.sax.as_deref().is_none_or(|sax| {
            (sax.ignorable_whitespace.is_none() && sax.characters.is_none())
                || sax
                    .ignorable_whitespace
                    .zip(sax.characters)
                    .is_some_and(|(l, r)| fn_addr_eq(l, r))
        }) {
            return false;
        }

        // Check for xml:space value.
        if self.space() == 1 || self.space() == -2 {
            return false;
        }

        // Check that the string is made of blanks
        if !blank_chars && s.chars().any(|c| !xml_is_blank_char(c as u32)) {
            return false;
        }

        // Look if the element is mixed content in the DTD if available
        let Some(context_node) = self.node else {
            return false;
        };
        if let Some(my_doc) = self.my_doc {
            let ret = xml_is_mixed_element(my_doc, &context_node.name().unwrap());
            if ret == 0 {
                return true;
            }
            if ret == 1 {
                return false;
            }
        }

        // Otherwise, heuristic :-\
        if !matches!(self.current_byte(), b'<' | 0xD) {
            return false;
        }
        if context_node.children().is_none() && self.content_bytes().starts_with(b"</") {
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

    /// Always makes progress if the first char isn't '<' or '&'.
    ///
    /// parse a CharData section.this is the fallback function
    /// of xmlParseCharData() when the parsing requires handling
    /// of non-ASCII characters.
    #[doc(alias = "xmlParseCharDataComplex")]
    fn parse_char_data_complex(&mut self, partial: i32) {
        let mut buf = String::with_capacity(XML_PARSER_BIG_BUFFER_SIZE + 5);
        let mut cur = self.current_char();
        // test also done in xmlCurrentChar()
        while let Some(nc) = cur.filter(|&cur| cur != '<' && cur != '&' && xml_is_char(cur as u32))
        {
            if nc == ']' && self.nth_byte(1) == b']' && self.nth_byte(2) == b'>' {
                xml_fatal_err(self, XmlParserErrors::XmlErrMisplacedCDATAEnd, None);
            }
            buf.push(nc);
            // move current position before possible calling of (*self.sax).characters
            self.advance_with_line_handling(nc.len_utf8());
            if buf.len() >= XML_PARSER_BIG_BUFFER_SIZE {
                // OK the segment is to be consumed as chars.
                if !self.disable_sax {
                    let blank = self.are_blanks(&buf, false);
                    if let Some(sax) = self.sax.as_deref_mut() {
                        if blank {
                            if let Some(ignorable_whitespace) = sax.ignorable_whitespace {
                                ignorable_whitespace(self, &buf);
                            }
                        } else {
                            if let Some(characters) = sax.characters {
                                characters(self, &buf);
                            }
                            let sax = self.sax.as_deref_mut().unwrap();
                            if (sax.ignorable_whitespace.is_some() || sax.characters.is_some())
                                && sax
                                    .ignorable_whitespace
                                    .zip(sax.characters)
                                    .is_none_or(|(l, r)| !fn_addr_eq(l, r))
                                && self.space() == -1
                            {
                                *self.space_mut() = -2;
                            }
                        }
                    }
                }
                buf.clear();
                // something really bad happened in the SAX callback
                if !matches!(self.instate, XmlParserInputState::XmlParserContent) {
                    return;
                }
                self.shrink();
            }
            cur = self.current_char();
        }
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            return;
        }
        if !buf.is_empty() {
            // OK the segment is to be consumed as chars.
            if !self.disable_sax {
                let blank = self.are_blanks(&buf, false);
                if let Some(sax) = self.sax.as_deref_mut() {
                    if blank {
                        if let Some(ignorable_whitespace) = sax.ignorable_whitespace {
                            ignorable_whitespace(self, &buf);
                        }
                    } else {
                        if let Some(characters) = sax.characters {
                            characters(self, &buf);
                        }
                        let sax = self.sax.as_deref_mut().unwrap();
                        if (sax.ignorable_whitespace.is_some() || sax.characters.is_some())
                            && sax
                                .ignorable_whitespace
                                .zip(sax.characters)
                                .is_none_or(|(l, r)| !fn_addr_eq(l, r))
                            && self.space() == -1
                        {
                            *self.space_mut() = -2;
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
        if !self.content_bytes().is_empty() {
            if cur.is_none_or(|cur| cur == '\0') && self.current_byte() != 0 {
                if partial == 0 {
                    xml_fatal_err_msg_int!(
                        self,
                        XmlParserErrors::XmlErrInvalidChar,
                        format!(
                            "Incomplete UTF-8 sequence starting with {:02X}\n",
                            self.current_byte()
                        )
                        .as_str(),
                        self.current_byte() as i32
                    );
                    self.advance_with_line_handling(1);
                }
            } else if !matches!(cur, Some('<' | '&')) {
                // Generate the error and skip the offending character
                let c = cur.unwrap_or('\0');
                xml_fatal_err_msg_int!(
                    self,
                    XmlParserErrors::XmlErrInvalidChar,
                    format!("PCDATA invalid Char value {}\n", c as i32).as_str(),
                    c as i32
                );
                self.advance_with_line_handling(cur.map_or(0, |c| c.len_utf8()));
            }
        }
    }

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
    pub(crate) fn parse_char_data_internal(&mut self, partial: i32) {
        // used for the test in the inner loop of the char data testing
        // &    : start reference
        // <    : start tag
        // ]    : end CDATA section
        const TEST_CHAR_DATA: [u8; 256] = [
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x09, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x00, 0x27, 0x28, 0x29,
            0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
            0x38, 0x39, 0x3A, 0x3B, 0x00, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45,
            0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F, 0x50, 0x51, 0x52, 0x53,
            0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x00, 0x5E, 0x5F, 0x60, 0x61,
            0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
            0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D,
            0x7E, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00,
        ];

        self.grow();
        // Accelerated common case where input don't need to be
        // modified before passing it to the handler.
        let mut input = self.content_bytes();
        let mut line = self.input().unwrap().line;
        let mut col = self.input().unwrap().col;
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
                let len = self.content_bytes().len() - input.len();
                if len > 0 {
                    buf.clear();
                    unsafe {
                        // # Safety
                        // `self.content_bytes()[..len]` contains only ASCII characters.
                        // Therefore, UTF-8 validation won't be failed.
                        buf.push_str(from_utf8_unchecked(&self.content_bytes()[..len]));
                    }

                    // commit consumed bytes for SAX interface
                    let input = self.input_mut().unwrap();
                    input.cur += len;
                    input.line = line;
                    input.col = col;

                    let blank = self.are_blanks(&buf, true);

                    if let Some(sax) = self.sax.as_deref_mut().filter(|sax| {
                        (sax.ignorable_whitespace.is_some() || sax.characters.is_some())
                            && sax
                                .ignorable_whitespace
                                .zip(sax.characters)
                                .is_none_or(|(l, r)| !fn_addr_eq(l, r))
                    }) {
                        if blank {
                            if let Some(ignorable_whitespace) = sax.ignorable_whitespace {
                                ignorable_whitespace(self, &buf);
                            }
                        } else {
                            if let Some(characters) = sax.characters {
                                characters(self, &buf);
                            }
                            if self.space() == -1 {
                                *self.space_mut() = -2;
                            }
                        }
                    } else if let Some(characters) =
                        self.sax.as_deref_mut().and_then(|sax| sax.characters)
                    {
                        characters(self, &buf);
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
                        let diff = self.content_bytes().len() - input.len();
                        let input = self.input_mut().unwrap();
                        input.line = line;
                        input.col = col;
                        input.cur += diff;
                        xml_fatal_err(self, XmlParserErrors::XmlErrMisplacedCDATAEnd, None);
                        if !matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                            // if the parser input state is not EOF,
                            // consume the head of ']'.
                            self.input_mut().unwrap().cur += 1;
                        }
                        return;
                    }
                    col += 1;
                    input = rem;
                }
            }

            let len = self.content_bytes().len() - input.len();
            if len > 0 {
                buf.clear();
                unsafe {
                    // # Safety
                    // `self.content_bytes()[..len]` contains only ASCII characters.
                    // Therefore, UTF-8 validation won't be failed.
                    buf.push_str(from_utf8_unchecked(&self.content_bytes()[..len]));
                }

                // commit consumed bytes for SAX interface
                let input_mut = self.input_mut().unwrap();
                input_mut.cur += len;
                input_mut.line = line;
                input_mut.col = col;

                let blank = self.are_blanks(&buf, false);

                if let Some(sax) = self.sax.as_deref_mut().filter(|sax| {
                    (sax.ignorable_whitespace.is_some() || sax.characters.is_some())
                        && sax
                            .ignorable_whitespace
                            .zip(sax.characters)
                            .is_none_or(|(l, r)| !fn_addr_eq(l, r))
                }) {
                    if blank {
                        if let Some(ignorable_whitespace) = sax.ignorable_whitespace {
                            ignorable_whitespace(self, &buf);
                        }
                    } else {
                        if let Some(characters) = sax.characters {
                            characters(self, &buf);
                        }
                        if self.space() == -1 {
                            *self.space_mut() = -2;
                        }
                    }
                } else if let Some(characters) =
                    self.sax.as_deref_mut().and_then(|sax| sax.characters)
                {
                    characters(self, &buf);
                }

                // refresh input buffer
                input = self.content_bytes();
            }

            // Replace "\r\n" to "\n" and skip
            if input.starts_with(b"\r\n") {
                line += 1;
                col = 1;
                // To skip '\r', `+1` is needed.
                let diff = self.content_bytes().len() - input.len() + 1;
                // At this point, `content_bytes` will start with '\n' ('\r' is skipped)
                self.input_mut().unwrap().cur += diff;
                // and skip '\n' at the head of `content_bytes`.
                input = &self.content_bytes()[1..];
                continue;
            }

            // At this point, the buffer is already committed,
            // so commit only line and col
            self.input_mut().unwrap().line = line;
            self.input_mut().unwrap().col = col;

            if matches!(self.content_bytes().first(), Some(&(b'<' | b'&'))) {
                return;
            }

            self.shrink();
            self.grow();
            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                return;
            }

            // refresh input buffer
            input = self.content_bytes();
        }

        let len = self.content_bytes().len() - input.len();
        self.input_mut().unwrap().cur += len;
        self.input_mut().unwrap().line = line;
        self.input_mut().unwrap().col = col;
        self.parse_char_data_complex(partial);
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
    pub(crate) fn parse_cdsect(&mut self) {
        let max_length = if self.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_HUGE_LENGTH
        } else {
            XML_MAX_TEXT_LENGTH
        };

        if !self.content_bytes().starts_with(b"<![CDATA[") {
            return;
        }
        self.advance(9);

        self.instate = XmlParserInputState::XmlParserCDATASection;
        let Some(mut r) = self.current_char().filter(|&r| xml_is_char(r as u32)) else {
            xml_fatal_err(self, XmlParserErrors::XmlErrCDATANotFinished, None);
            if !matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                self.instate = XmlParserInputState::XmlParserContent;
            }
            return;
        };
        self.advance_with_line_handling(r.len_utf8());
        let Some(mut s) = self.current_char().filter(|&s| xml_is_char(s as u32)) else {
            xml_fatal_err(self, XmlParserErrors::XmlErrCDATANotFinished, None);
            if !matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                self.instate = XmlParserInputState::XmlParserContent;
            }
            return;
        };
        self.advance_with_line_handling(s.len_utf8());
        let mut cur = self.current_char();
        let mut buf = String::new();
        while let Some(nc) =
            cur.filter(|&cur| xml_is_char(cur as u32) && (r != ']' || s != ']' || cur != '>'))
        {
            buf.push(r);
            if buf.len() > max_length {
                xml_fatal_err_msg(
                    self,
                    XmlParserErrors::XmlErrCDATANotFinished,
                    "CData section too big found\n",
                );
                if !matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                    self.instate = XmlParserInputState::XmlParserContent;
                }
                return;
            }
            r = s;
            s = nc;
            self.advance_with_line_handling(nc.len_utf8());
            cur = self.current_char();
        }
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            return;
        }
        if cur != Some('>') {
            xml_fatal_err_msg_str!(
                self,
                XmlParserErrors::XmlErrCDATANotFinished,
                "CData section not finished\n{}\n",
                buf
            );
            // goto out;
            if !matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                self.instate = XmlParserInputState::XmlParserContent;
            }
            return;
        }
        self.advance_with_line_handling(cur.map_or(0, |c| c.len_utf8()));

        // OK the buffer is to be consumed as cdata.
        if !self.disable_sax {
            if let Some(sax) = self.sax.as_deref_mut() {
                if let Some(cdata) = sax.cdata_block {
                    cdata(self, &buf);
                } else if let Some(characters) = sax.characters {
                    characters(self, &buf);
                }
            }
        }

        // out:
        if !matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            self.instate = XmlParserInputState::XmlParserContent;
        }
    }
}
