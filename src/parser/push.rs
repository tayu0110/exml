use std::{cell::RefCell, ptr::null_mut, rc::Rc, slice::from_raw_parts, str::from_utf8_unchecked};

use libc::strncmp;

use crate::{
    encoding::{XmlCharEncoding, detect_encoding},
    error::XmlParserErrors,
    generic_error,
    globals::GenericErrorContext,
    io::XmlParserInputBuffer,
    libxml::{
        chvalid::{xml_is_blank_char, xml_is_char},
        globals::xml_default_sax_locator,
        parser::{XML_PARSER_BIG_BUFFER_SIZE, XmlSAXHandler},
        parser_internals::XML_MAX_LOOKUP_LIMIT,
        valid::xml_validate_root,
    },
    parser::{__xml_err_encoding, xml_err_memory, xml_fatal_err_msg_str},
    uri::canonic_path,
};

use super::{
    XML_DEFAULT_VERSION, XmlParserCtxt, XmlParserCtxtPtr, XmlParserInputState, XmlParserOption,
    xml_fatal_err,
};

/// Check that the block of characters is okay as SCdata content [20]
///
/// Returns the number of bytes to pass if okay, a negative index where an
/// UTF-8 error occurred otherwise
#[doc(alias = "xmlCheckCdataPush")]
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

impl XmlParserCtxt {
    /// Check whether the input buffer contains a character.
    #[doc(alias = "xmlParseLookupChar")]
    unsafe fn lookup_char(&mut self, c: u8) -> i32 {
        unsafe {
            let cur = &self.content_bytes()[self.check_index.max(1) as usize..];

            if !cur.contains(&c) {
                if cur.len() > i64::MAX as usize {
                    self.check_index = 0;
                    1
                } else {
                    self.check_index = cur.len() as i64;
                    0
                }
            } else {
                self.check_index = 0;
                1
            }
        }
    }

    /// Check whether the input buffer contains terminated char data.
    #[doc(alias = "xmlParseLookupCharData")]
    unsafe fn lookup_char_data(&mut self) -> i32 {
        unsafe {
            let cur = &self.content_bytes()[self.check_index as usize..];

            if cur.contains(&b'<') || cur.contains(&b'&') || cur.len() > i64::MAX as usize {
                self.check_index = 0;
                1
            } else {
                self.check_index = cur.len() as i64;
                0
            }
        }
    }

    /// Check whether the input buffer contains a string.
    ///
    /// If found, return buffer which start with `s` wrapped `Some`,
    /// otherwise return `None`.
    #[doc(alias = "xmlParseLookupString")]
    unsafe fn lookup_string<'a>(&'a mut self, start_delta: usize, s: &str) -> Option<&'a [u8]> {
        unsafe {
            let cur = if self.check_index == 0 {
                &self.content_bytes()[start_delta..]
            } else {
                &self.content_bytes()[self.check_index as usize..]
            };
            let len = cur.len();

            if let Some(term) = cur.windows(s.len()).position(|chunk| chunk == s.as_bytes()) {
                self.check_index = 0;
                let cur = self.content_bytes().windows(len).next_back().unwrap();
                Some(&cur[term..])
            } else {
                // Rescan (strLen - 1) characters.
                let end = if cur.len() < s.len() {
                    cur
                } else {
                    cur.windows(s.len() - 1).next_back().unwrap()
                };
                let index = self.content_bytes().len() - end.len();
                if index > i64::MAX as usize {
                    self.check_index = 0;
                    return self.content_bytes().windows(s.len()).next_back();
                }
                self.check_index = index as _;
                None
            }
        }
    }

    /// Check whether there's enough data in the input buffer to finish parsing
    /// a start tag. This has to take quotes into account.
    #[doc(alias = "xmlParseLookupGt")]
    unsafe fn lookup_gt(&mut self) -> i32 {
        unsafe {
            let mut cur = if self.check_index == 0 {
                &self.content_bytes()[1..]
            } else {
                &self.content_bytes()[self.check_index as usize..]
            };

            let mut state = self.end_check_state as u8;
            while !cur.is_empty() {
                if state != 0 {
                    if cur[0] == state {
                        state = 0;
                    }
                } else if matches!(cur[0], b'\'' | b'"') {
                    state = cur[0];
                } else if cur[0] == b'>' {
                    self.check_index = 0;
                    self.end_check_state = 0;
                    return 1;
                }
                cur = &cur[1..];
            }

            let index = self.content_bytes().len() - cur.len();
            if index > i64::MAX as usize {
                self.check_index = 0;
                self.end_check_state = 0;
                return 1;
            }
            self.check_index = index as i64;
            self.end_check_state = state as i32;
            0
        }
    }

    /// Check whether there's enough data in the input buffer to finish parsing the internal subset.
    #[doc(alias = "xmlParseLookupInternalSubset")]
    unsafe fn lookup_internal_subset(&mut self) -> i32 {
        unsafe {
            // Sorry, but progressive parsing of the internal subset is not
            // supported. We first check that the full content of the internal
            // subset is available and parsing is launched only at that point.
            // Internal subset ends with "']' S? '>'" in an unescaped section and
            // not in a ']]>' sequence which are conditional sections.

            let mut cur = if self.check_index == 0 {
                &self.content_bytes()[1..]
            } else {
                &self.content_bytes()[self.check_index as usize..]
            };

            let mut start = cur;
            let mut state = self.end_check_state as u8;
            while !cur.is_empty() {
                if state == b'-' {
                    if let Some(rem) = cur.strip_prefix(b"-->") {
                        state = 0;
                        cur = rem;
                        start = rem;
                        continue;
                    }
                } else if state == b']' {
                    if cur[0] == b'>' {
                        self.check_index = 0;
                        self.end_check_state = 0;
                        return 1;
                    }
                    if xml_is_blank_char(cur[0] as u32) {
                        state = b' ';
                    } else if cur[0] != b']' {
                        state = 0;
                        start = cur;
                        continue;
                    }
                } else if state == b' ' {
                    if cur[0] == b'>' {
                        self.check_index = 0;
                        self.end_check_state = 0;
                        return 1;
                    }
                    if !xml_is_blank_char(cur[0] as u32) {
                        state = 0;
                        start = cur;
                        continue;
                    }
                } else if state != 0 {
                    if cur[0] == state {
                        state = 0;
                        start = &cur[1..];
                    }
                } else if let Some(rem) = cur.strip_prefix(b"<!--") {
                    state = b'-';
                    cur = rem;
                    // Don't treat <!--> as comment
                    start = rem;
                    continue;
                } else if matches!(cur[0], b'"' | b'\'' | b']') {
                    state = cur[0];
                }

                cur = &cur[1..];
            }

            // Rescan the three last characters to detect "<!--" and "-->"
            // split across chunks.
            if state == 0 || state == b'-' {
                let diff = start.len() - cur.len();
                if diff < 3 {
                    cur = start;
                } else {
                    cur = &start[diff - 3..];
                }
            }
            let index = self.content_bytes().len() - cur.len();
            if index > i64::MAX as usize {
                self.check_index = 0;
                self.end_check_state = 0;
                return 1;
            }
            self.check_index = index as _;
            self.end_check_state = state as i32;
            0
        }
    }

    /// Try to progress on parsing
    ///
    /// Returns zero if no parsing was possible
    #[doc(alias = "xmlParseTryOrFinish")]
    unsafe fn parse_try_or_finish(&mut self, terminate: i32) -> i32 {
        unsafe {
            let mut ret: i32 = 0;

            if self.input().is_none() {
                return 0;
            }

            if self.input().is_some() && self.input().unwrap().offset_from_base() > 4096 {
                self.force_shrink();
            }

            'encoding_error: {
                while !matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                    if self.err_no != XmlParserErrors::XmlErrOK as i32 && self.disable_sax == 1 {
                        return 0;
                    }

                    if self.input().is_none() {
                        break;
                    }
                    if let Some(input_buffer) = self.input().unwrap().buf.as_deref() {
                        // If we are operating on converted input, try to flush
                        // remaining chars to avoid them stalling in the non-converted buffer.
                        if input_buffer
                            .borrow()
                            .raw
                            .as_deref()
                            .is_some_and(|raw| !raw.is_empty())
                        {
                            let base = self.input().unwrap().get_base();
                            let current = self.input().unwrap().offset_from_base();

                            input_buffer.borrow_mut().push_bytes(b"");
                            self.input_mut().unwrap().set_base_and_cursor(base, current);
                        }
                    }
                    let mut avail = self.input().unwrap().remainder_len();
                    if avail < 1 {
                        // goto done;
                        return ret;
                    }
                    match self.instate {
                        XmlParserInputState::XmlParserEOF => {
                            // Document parsing is done !
                            // goto done;
                            return ret;
                        }
                        XmlParserInputState::XmlParserStart => 'to_break: {
                            if self.charset == XmlCharEncoding::None {
                                // Very first chars read from the document flow.
                                if avail < 4 {
                                    // goto done;
                                    return ret;
                                }

                                // Get the 4 first bytes and decode the charset
                                // if enc != xmlCharEncoding::XML_CHAR_ENCODING_NONE
                                // plug some encoding conversion routines,
                                // else xmlSwitchEncoding will set to (default) UTF8.
                                let enc = detect_encoding(&self.content_bytes()[..4]);
                                // We need more bytes to detect EBCDIC code pages.
                                // See xmlDetectEBCDIC.
                                if matches!(enc, XmlCharEncoding::EBCDIC)
                                    && terminate == 0
                                    && avail < 200
                                {
                                    // goto done;
                                    return ret;
                                }
                                self.switch_encoding(enc);
                                break 'to_break;
                            }
                            if avail < 2 {
                                // goto done;
                                return ret;
                            }
                            if self.content_bytes().is_empty() {
                                if let Some(set_document_locator) = self
                                    .sax
                                    .as_deref_mut()
                                    .and_then(|sax| sax.set_document_locator)
                                {
                                    set_document_locator(
                                        self.user_data.clone(),
                                        xml_default_sax_locator(),
                                    );
                                }
                                xml_fatal_err(
                                    &mut *self,
                                    XmlParserErrors::XmlErrDocumentEmpty,
                                    None,
                                );
                                self.halt();
                                if let Some(end_document) =
                                    self.sax.as_deref_mut().and_then(|sax| sax.end_document)
                                {
                                    end_document(self.user_data.clone());
                                }
                                // goto done;
                                return ret;
                            }
                            if self.content_bytes().starts_with(b"<?") {
                                // PI or XML decl
                                if avail < 5 {
                                    // goto done;
                                    return ret;
                                }
                                if terminate == 0 && self.lookup_string(2, "?>").is_none() {
                                    // goto done;
                                    return ret;
                                }
                                if let Some(set_document_locator) = self
                                    .sax
                                    .as_deref_mut()
                                    .and_then(|sax| sax.set_document_locator)
                                {
                                    set_document_locator(
                                        self.user_data.clone(),
                                        xml_default_sax_locator(),
                                    );
                                }
                                if self.content_bytes()[2..].starts_with(b"xml")
                                    && xml_is_blank_char(self.nth_byte(5) as u32)
                                {
                                    ret += 5;
                                    self.parse_xmldecl();
                                    if self.err_no
                                        == XmlParserErrors::XmlErrUnsupportedEncoding as i32
                                    {
                                        // The XML REC instructs us to stop parsing right here
                                        self.halt();
                                        return 0;
                                    }
                                    self.standalone = self.input().unwrap().standalone;
                                    if self.encoding().is_none()
                                        && self.input().unwrap().encoding.is_some()
                                    {
                                        self.encoding = self.input().unwrap().encoding.clone();
                                    }
                                    if self.disable_sax == 0 {
                                        if let Some(start_document) = self
                                            .sax
                                            .as_deref_mut()
                                            .and_then(|sax| sax.start_document)
                                        {
                                            start_document(self.user_data.clone());
                                        }
                                    }
                                    self.instate = XmlParserInputState::XmlParserMisc;
                                } else {
                                    self.version = Some(XML_DEFAULT_VERSION.to_owned());
                                    if self.disable_sax == 0 {
                                        if let Some(start_document) = self
                                            .sax
                                            .as_deref_mut()
                                            .and_then(|sax| sax.start_document)
                                        {
                                            start_document(self.user_data.clone());
                                        }
                                    }
                                    self.instate = XmlParserInputState::XmlParserMisc;
                                }
                            } else {
                                if let Some(set_document_locator) = self
                                    .sax
                                    .as_deref_mut()
                                    .and_then(|sax| sax.set_document_locator)
                                {
                                    set_document_locator(
                                        self.user_data.clone(),
                                        xml_default_sax_locator(),
                                    );
                                }
                                self.version = Some(XML_DEFAULT_VERSION.to_owned());
                                if self.disable_sax == 0 {
                                    if let Some(start_document) =
                                        self.sax.as_deref_mut().and_then(|sax| sax.start_document)
                                    {
                                        start_document(self.user_data.clone());
                                    }
                                }
                                self.instate = XmlParserInputState::XmlParserMisc;
                            }
                        }
                        XmlParserInputState::XmlParserStartTag => {
                            let line: i32 = self.input().unwrap().line;
                            let ns_nr = self.ns_tab.len();

                            if avail < 2 && self.input_tab.len() == 1 {
                                // goto done;
                                return ret;
                            }

                            if !self.content_bytes().starts_with(b"<") {
                                xml_fatal_err(self, XmlParserErrors::XmlErrDocumentEmpty, None);
                                self.halt();
                                if let Some(end_document) =
                                    self.sax.as_deref().and_then(|sax| sax.end_document)
                                {
                                    end_document(self.user_data.clone());
                                }
                                // goto done;
                                return ret;
                            }
                            if terminate == 0 && self.lookup_gt() == 0 {
                                // goto done;
                                return ret;
                            }
                            if self.space_tab.is_empty() || self.space() == -2 {
                                self.space_push(-1);
                            } else {
                                self.space_push(self.space());
                            }
                            #[cfg(feature = "sax1")]
                            let tag = if self.sax2 != 0 {
                                self.parse_start_tag2()
                            } else {
                                self.parse_start_tag().map(|name| (name, None, None))
                            };
                            #[cfg(not(feature = "sax1"))]
                            let tag = self.parse_start_tag2();
                            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                                // goto done;
                                return ret;
                            }
                            let Some((name, prefix, uri)) = tag else {
                                self.space_pop();
                                self.halt();
                                if let Some(end_document) =
                                    self.sax.as_deref_mut().and_then(|sax| sax.end_document)
                                {
                                    end_document(self.user_data.clone());
                                }
                                // goto done;
                                return ret;
                            };
                            // [ VC: Root Element Type ]
                            // The Name in the document type declaration must match
                            // the element type of the root element.
                            #[cfg(feature = "libxml_valid")]
                            if self.validate != 0 && self.well_formed != 0 {
                                if let Some(context_node) = self.node {
                                    if let Some(my_doc) = self
                                        .my_doc
                                        .filter(|doc| doc.children == Some(context_node.into()))
                                    {
                                        self.valid &=
                                            xml_validate_root(&raw mut self.vctxt as _, my_doc);
                                    }
                                }
                            }

                            // Check for an Empty Element.
                            if self.content_bytes().starts_with(b"/>") {
                                self.advance(2);

                                if self.sax2 != 0 {
                                    if self.disable_sax == 0 {
                                        if let Some(end_element_ns) = self
                                            .sax
                                            .as_deref_mut()
                                            .and_then(|sax| sax.end_element_ns)
                                        {
                                            end_element_ns(
                                                self.user_data.clone(),
                                                &name,
                                                prefix.as_deref(),
                                                uri.as_deref(),
                                            );
                                        }
                                    }
                                    if self.ns_tab.len() - ns_nr > 0 {
                                        self.ns_pop(self.ns_tab.len() - ns_nr);
                                    }
                                } else {
                                    #[cfg(feature = "sax1")]
                                    if self.disable_sax == 0 {
                                        if let Some(end_element) =
                                            self.sax.as_deref_mut().and_then(|sax| sax.end_element)
                                        {
                                            end_element(self.user_data.clone(), &name);
                                        }
                                    }
                                }
                                if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                                    // goto done;
                                    return ret;
                                }
                                self.space_pop();
                                if self.name_tab.is_empty() {
                                    self.instate = XmlParserInputState::XmlParserEpilog;
                                } else {
                                    self.instate = XmlParserInputState::XmlParserContent;
                                }
                                // break;
                            } else {
                                if self.current_byte() == b'>' {
                                    self.skip_char();
                                } else {
                                    xml_fatal_err_msg_str!(
                                        self,
                                        XmlParserErrors::XmlErrGtRequired,
                                        "Couldn't find end of Start Tag {}\n",
                                        name
                                    );
                                    self.node_pop();
                                    self.space_pop();
                                }
                                self.name_ns_push(
                                    &name,
                                    prefix.as_deref(),
                                    uri.as_deref(),
                                    line,
                                    self.ns_tab.len() as i32 - ns_nr as i32,
                                );

                                self.instate = XmlParserInputState::XmlParserContent;
                                // break;
                            }
                        }
                        XmlParserInputState::XmlParserContent => {
                            if avail < 2 && self.input_tab.len() == 1 {
                                // goto done;
                                return ret;
                            }

                            match self.content_bytes() {
                                [b'<', b'/', ..] => {
                                    self.instate = XmlParserInputState::XmlParserEndTag
                                }
                                [b'<', b'?', ..] => {
                                    if terminate == 0 && self.lookup_string(2, "?>").is_none() {
                                        // goto done;
                                        return ret;
                                    }
                                    self.parse_pi();
                                    self.instate = XmlParserInputState::XmlParserContent;
                                }
                                [b'<', b, ..] if *b != b'!' => {
                                    self.instate = XmlParserInputState::XmlParserStartTag
                                }
                                [b'<', b'!', b'-', b'-', ..] => {
                                    if terminate == 0 && self.lookup_string(4, "-->").is_none() {
                                        // goto done;
                                        return ret;
                                    }
                                    self.parse_comment();
                                    self.instate = XmlParserInputState::XmlParserContent;
                                }
                                [b'<', b'!', b'[', b'C', b'D', b'A', b'T', b'A', b'[', ..] => {
                                    self.advance(9);
                                    self.instate = XmlParserInputState::XmlParserCDATASection;
                                }
                                [b'<', b'!', ..] if avail < 9 => {
                                    // goto done;
                                    return ret;
                                }
                                [b'<', ..] => {
                                    xml_fatal_err(
                                        &mut *self,
                                        XmlParserErrors::XmlErrInternalError,
                                        Some("detected an error in element content\n"),
                                    );
                                    self.advance(1);
                                }
                                [b'&', ..] => {
                                    if terminate == 0 && self.lookup_char(b';') == 0 {
                                        // goto done;
                                        return ret;
                                    }
                                    self.parse_reference();
                                }
                                _ => {
                                    // TODO Avoid the extra copy, handle directly !!!

                                    // Goal of the following test is:
                                    //  - minimize calls to the SAX 'character' callback
                                    //    when they are mergeable
                                    //  - handle an problem for isBlank when we only parse
                                    //    a sequence of blank chars and the next one is
                                    //    not available to check against '<' presence.
                                    //  - tries to homogenize the differences in SAX
                                    //    callbacks between the push and pull versions
                                    //    of the parser.
                                    if (self.input_tab.len() == 1
                                        && avail < XML_PARSER_BIG_BUFFER_SIZE)
                                        && (terminate == 0 && self.lookup_char_data() == 0)
                                    {
                                        // goto done;
                                        return ret;
                                    }
                                    self.check_index = 0;
                                    self.parse_char_data_internal((terminate == 0) as i32);
                                }
                            }
                        }
                        XmlParserInputState::XmlParserEndTag => {
                            if avail < 2 {
                                // goto done;
                                return ret;
                            }
                            if terminate == 0 && self.lookup_char(b'>' as _) == 0 {
                                // goto done;
                                return ret;
                            }
                            if self.sax2 != 0 {
                                self.parse_end_tag2();
                                self.name_ns_pop();
                            } else {
                                #[cfg(feature = "sax1")]
                                {
                                    self.parse_end_tag1(0);
                                }
                            }
                            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                                // Nothing
                            } else if self.name_tab.is_empty() {
                                self.instate = XmlParserInputState::XmlParserEpilog;
                            } else {
                                self.instate = XmlParserInputState::XmlParserContent;
                            }
                        }
                        XmlParserInputState::XmlParserCDATASection => {
                            // The Push mode need to have the SAX callback for
                            // cdataBlock merge back contiguous callbacks.

                            let term = if terminate != 0 {
                                // Don't call xmlParseLookupString. If 'terminate'
                                // is set, checkIndex is invalid.
                                let target = b"]]>".as_slice();
                                self.content_bytes()
                                    .windows(target.len())
                                    .position(|s| s == target)
                                    .map(|pos| &self.content_bytes()[pos..])
                            } else {
                                self.lookup_string(0, "]]>")
                            };

                            if let Some(term) = term.map(|term| term.len()) {
                                let base = self.content_bytes().len() - term;

                                match check_cdata_push(&self.content_bytes()[..base], true) {
                                    Ok(tmp) if tmp == base => {}
                                    Ok(tmp) | Err(tmp) => {
                                        self.input_mut().unwrap().cur =
                                            self.input().unwrap().cur.add(tmp);
                                        break 'encoding_error;
                                    }
                                }
                                if self.disable_sax == 0 {
                                    if let Some(cdata_block) = self
                                        .sax
                                        .as_deref_mut()
                                        .filter(|_| base == 0)
                                        .and_then(|sax| sax.cdata_block)
                                    {
                                        // Special case to provide identical behaviour
                                        // between pull and push parsers on enpty CDATA sections
                                        if self.input().unwrap().offset_from_base() >= 9
                                            && strncmp(
                                                self.input().unwrap().cur.sub(9) as _,
                                                c"<![CDATA[".as_ptr() as _,
                                                9,
                                            ) == 0
                                        {
                                            cdata_block(self.user_data.clone(), "");
                                        }
                                    } else if let Some(sax) =
                                        self.sax.as_deref_mut().filter(|_| base > 0)
                                    {
                                        if let Some(cdata_block) = sax.cdata_block {
                                            // # Safety
                                            // The contents of `self.content_bytes()[..base]` is already checked
                                            // in `check_cdata_push`, so UTF-8 validation won't fail.
                                            let s =
                                                from_utf8_unchecked(&self.content_bytes()[..base]);
                                            cdata_block(self.user_data.clone(), s);
                                        } else if let Some(characters) = sax.characters {
                                            // # Safety
                                            // The contents of `self.content_bytes()[..base]` is already checked
                                            // in `check_cdata_push`, so UTF-8 validation won't fail.
                                            let s =
                                                from_utf8_unchecked(&self.content_bytes()[..base]);
                                            characters(self.user_data.clone(), s);
                                        }
                                    }
                                }
                                if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                                    // goto done;
                                    return ret;
                                }
                                self.advance_with_line_handling(base + 3);
                                self.instate = XmlParserInputState::XmlParserContent;
                            } else {
                                let size = if terminate != 0 {
                                    // Unfinished CDATA section
                                    self.input().unwrap().remainder_len()
                                } else {
                                    if avail < XML_PARSER_BIG_BUFFER_SIZE + 2 {
                                        // goto done;
                                        return ret;
                                    }
                                    self.check_index = 0;
                                    // XXX: Why don't we pass the full buffer?
                                    XML_PARSER_BIG_BUFFER_SIZE
                                };
                                let tmp =
                                    match check_cdata_push(&self.content_bytes()[..size], false) {
                                        Ok(tmp) => tmp,
                                        Err(tmp) => {
                                            self.input_mut().unwrap().cur =
                                                self.input().unwrap().cur.add(tmp);
                                            break 'encoding_error;
                                        }
                                    };
                                if self.disable_sax == 0 {
                                    if let Some(sax) = self.sax.as_deref_mut() {
                                        if let Some(cdata_block) = sax.cdata_block {
                                            // # Safety
                                            // The contents of `self.content_bytes()[..tmp]` is already checked
                                            // in `check_cdata_push`, so UTF-8 validation won't fail.
                                            let s =
                                                from_utf8_unchecked(&self.content_bytes()[..tmp]);
                                            cdata_block(self.user_data.clone(), s);
                                        } else if let Some(characters) = sax.characters {
                                            // # Safety
                                            // The contents of `self.content_bytes()[..tmp]` is already checked
                                            // in `check_cdata_push`, so UTF-8 validation won't fail.
                                            let s =
                                                from_utf8_unchecked(&self.content_bytes()[..tmp]);
                                            characters(self.user_data.clone(), s);
                                        }
                                    }
                                }
                                if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                                    // goto done;
                                    return ret;
                                }
                                self.advance_with_line_handling(tmp as usize);
                            }
                        }
                        XmlParserInputState::XmlParserMisc
                        | XmlParserInputState::XmlParserProlog
                        | XmlParserInputState::XmlParserEpilog => {
                            self.skip_blanks();
                            avail = self.input().unwrap().remainder_len();
                            if avail < 2 {
                                // goto done;
                                return ret;
                            }

                            match self.content_bytes() {
                                [b'<', b'?', ..] => {
                                    if terminate == 0 && self.lookup_string(2, "?>").is_none() {
                                        // goto done;
                                        return ret;
                                    }
                                    self.parse_pi();
                                    if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                                        // goto done;
                                        return ret;
                                    }
                                }
                                [b'<', b'!', b'-', b'-', ..] => {
                                    if terminate == 0 && self.lookup_string(4, "-->").is_none() {
                                        // goto done;
                                        return ret;
                                    }
                                    self.parse_comment();
                                    if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                                        // goto done;
                                        return ret;
                                    }
                                }
                                [b'<', b'!', b'D', b'O', b'C', b'T', b'Y', b'P', b'E', ..]
                                    if self.instate == XmlParserInputState::XmlParserMisc =>
                                {
                                    if terminate == 0 && self.lookup_gt() == 0 {
                                        // goto done;
                                        return ret;
                                    }
                                    self.in_subset = 1;
                                    self.parse_doctypedecl();
                                    if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                                        // goto done;
                                        return ret;
                                    }
                                    if self.current_byte() == b'[' {
                                        self.instate = XmlParserInputState::XmlParserDTD;
                                    } else {
                                        // Create and update the external subset.
                                        self.in_subset = 2;
                                        if self.disable_sax == 0 {
                                            if let Some(external_subset) = self
                                                .sax
                                                .as_deref_mut()
                                                .and_then(|sax| sax.external_subset)
                                            {
                                                external_subset(
                                                    self.user_data.clone(),
                                                    self.int_sub_name.as_deref(),
                                                    self.ext_sub_system.as_deref(),
                                                    self.ext_sub_uri.as_deref(),
                                                );
                                            }
                                        }
                                        self.in_subset = 0;
                                        self.clean_special_attr();
                                        self.instate = XmlParserInputState::XmlParserProlog;
                                    }
                                }
                                [b'<', b'!', ..]
                                    if avail
                                        < if matches!(
                                            self.instate,
                                            XmlParserInputState::XmlParserMisc
                                        ) {
                                            9
                                        } else {
                                            4
                                        } =>
                                {
                                    // goto done;
                                    return ret;
                                }
                                _ => {
                                    if self.instate == XmlParserInputState::XmlParserEpilog {
                                        xml_fatal_err(
                                            &mut *self,
                                            XmlParserErrors::XmlErrDocumentEnd,
                                            None,
                                        );
                                        self.halt();
                                        if let Some(end_document) =
                                            self.sax.as_deref_mut().and_then(|sax| sax.end_document)
                                        {
                                            end_document(self.user_data.clone());
                                        }
                                        // goto done;
                                        return ret;
                                    } else {
                                        self.instate = XmlParserInputState::XmlParserStartTag;
                                    }
                                }
                            }
                        }
                        XmlParserInputState::XmlParserDTD => {
                            if terminate == 0 && self.lookup_internal_subset() == 0 {
                                // goto done;
                                return ret;
                            }
                            self.parse_internal_subset();
                            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                                // goto done;
                                return ret;
                            }
                            self.in_subset = 2;
                            if self.disable_sax == 0 {
                                if let Some(external_subset) =
                                    self.sax.as_deref_mut().and_then(|sax| sax.external_subset)
                                {
                                    external_subset(
                                        self.user_data.clone(),
                                        self.int_sub_name.as_deref(),
                                        self.ext_sub_system.as_deref(),
                                        self.ext_sub_uri.as_deref(),
                                    );
                                }
                            }
                            self.in_subset = 0;
                            self.clean_special_attr();
                            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                                // goto done;
                                return ret;
                            }
                            self.instate = XmlParserInputState::XmlParserProlog;
                            // break;
                        }
                        XmlParserInputState::XmlParserComment => {
                            generic_error!("PP: internal error, state == COMMENT\n",);
                            self.instate = XmlParserInputState::XmlParserContent;
                        }
                        XmlParserInputState::XmlParserIgnore => {
                            generic_error!("PP: internal error, state == IGNORE",);
                            self.instate = XmlParserInputState::XmlParserDTD;
                        }
                        XmlParserInputState::XmlParserPI => {
                            generic_error!("PP: internal error, state == PI\n",);
                            self.instate = XmlParserInputState::XmlParserContent;
                        }
                        XmlParserInputState::XmlParserEntityDecl => {
                            generic_error!("PP: internal error, state == ENTITY_DECL\n",);
                            self.instate = XmlParserInputState::XmlParserDTD;
                        }
                        XmlParserInputState::XmlParserEntityValue => {
                            generic_error!("PP: internal error, state == ENTITY_VALUE\n",);
                            self.instate = XmlParserInputState::XmlParserContent;
                        }
                        XmlParserInputState::XmlParserAttributeValue => {
                            generic_error!("PP: internal error, state == ATTRIBUTE_VALUE\n",);
                            self.instate = XmlParserInputState::XmlParserStartTag;
                        }
                        XmlParserInputState::XmlParserSystemLiteral => {
                            generic_error!("PP: internal error, state == SYSTEM_LITERAL\n",);
                            self.instate = XmlParserInputState::XmlParserStartTag;
                        }
                        XmlParserInputState::XmlParserPublicLiteral => {
                            generic_error!("PP: internal error, state == PUBLIC_LITERAL\n",);
                            self.instate = XmlParserInputState::XmlParserStartTag;
                        }
                    }
                }
                // done:
                return ret;
            }
            // encoding_error:
            if self.input().unwrap().remainder_len() < 4 {
                __xml_err_encoding!(
                    self,
                    XmlParserErrors::XmlErrInvalidChar,
                    "Input is not proper UTF-8, indicate encoding !\n"
                );
            } else {
                let content = self.content_bytes();
                let buffer = format!(
                    "Bytes: 0x{:02X} 0x{:02X} 0x{:02X} 0x{:02X}\n",
                    *content.first().unwrap_or(&0),
                    *content.get(1).unwrap_or(&0),
                    *content.get(2).unwrap_or(&0),
                    *content.get(3).unwrap_or(&0),
                );
                __xml_err_encoding!(
                    self,
                    XmlParserErrors::XmlErrInvalidChar,
                    "Input is not proper UTF-8, indicate encoding !\n{}",
                    buffer
                );
            }
            0
        }
    }
}

/// Parse a Chunk of memory
///
/// Returns zero if no error, the xmlParserErrors otherwise.
#[doc(alias = "xmlParseChunk")]
pub unsafe fn xml_parse_chunk(
    ctxt: XmlParserCtxtPtr,
    chunk: *const i8,
    mut size: i32,
    terminate: i32,
) -> i32 {
    unsafe {
        let mut end_in_lf: i32 = 0;

        if ctxt.is_null() {
            return XmlParserErrors::XmlErrInternalError as i32;
        }
        if (*ctxt).err_no != XmlParserErrors::XmlErrOK as i32 && (*ctxt).disable_sax == 1 {
            return (*ctxt).err_no;
        }
        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            return -1;
        }
        if (*ctxt).input().is_none() {
            return -1;
        }

        (*ctxt).progressive = 1;
        if matches!((*ctxt).instate, XmlParserInputState::XmlParserStart) {
            (*ctxt).detect_sax2();
        }
        if size > 0
            && !chunk.is_null()
            && terminate == 0
            && *chunk.add(size as usize - 1) == b'\r' as i8
        {
            end_in_lf = 1;
            size -= 1;
        }

        if size > 0
            && !chunk.is_null()
            && (*ctxt).input().is_some()
            && (*ctxt).input().unwrap().buf.is_some()
            && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            let base: usize = (*ctxt).input().unwrap().get_base();
            let cur = (*ctxt).input().unwrap().offset_from_base();

            let res: i32 = (*ctxt)
                .input_mut()
                .unwrap()
                .buf
                .as_mut()
                .unwrap()
                .borrow_mut()
                .push_bytes(from_raw_parts(chunk as *const u8, size as usize));
            (*ctxt).input_mut().unwrap().set_base_and_cursor(base, cur);
            if res < 0 {
                (*ctxt).err_no = XmlParserInputState::XmlParserEOF as i32;
                (*ctxt).halt();
                return XmlParserInputState::XmlParserEOF as i32;
            }
        } else if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
            && ((*ctxt).input().is_some() && (*ctxt).input().unwrap().buf.is_some())
        {
            let input = (*ctxt).input_mut().unwrap().buf.as_mut().unwrap();
            if input.borrow().encoder.is_some()
                && input.borrow().buffer.is_some()
                && input.borrow().raw.is_some()
            {
                let base: usize = (*ctxt).input().unwrap().get_base();
                let current = (*ctxt).input().unwrap().offset_from_base();

                let res = input.borrow_mut().decode(terminate != 0);
                (*ctxt)
                    .input_mut()
                    .unwrap()
                    .set_base_and_cursor(base, current);
                if res.is_err() {
                    // TODO 2.6.0
                    generic_error!("xmlParseChunk: encoder error\n");
                    (*ctxt).halt();
                    return XmlParserErrors::XmlErrInvalidEncoding as i32;
                }
            }
        }

        (*ctxt).parse_try_or_finish(terminate);
        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            return (*ctxt).err_no;
        }

        if (*ctxt).input().is_some_and(|input| {
            input.remainder_len() > XML_MAX_LOOKUP_LIMIT
                || input.offset_from_base() > XML_MAX_LOOKUP_LIMIT
        }) && (*ctxt).options & XmlParserOption::XmlParseHuge as i32 == 0
        {
            xml_fatal_err(
                &mut *ctxt,
                XmlParserErrors::XmlErrInternalError,
                Some(format!("Huge input lookup: {}:{}", file!(), line!()).as_str()),
            );
            (*ctxt).halt();
        }
        if (*ctxt).err_no != XmlParserErrors::XmlErrOK as i32 && (*ctxt).disable_sax == 1 {
            return (*ctxt).err_no;
        }

        if end_in_lf == 1 && (*ctxt).input().is_some() && (*ctxt).input().unwrap().buf.is_some() {
            let base: usize = (*ctxt).input().unwrap().get_base();
            let current = (*ctxt).input().unwrap().offset_from_base();

            (*ctxt)
                .input_mut()
                .unwrap()
                .buf
                .as_mut()
                .unwrap()
                .borrow_mut()
                .push_bytes(b"\r");
            (*ctxt)
                .input_mut()
                .unwrap()
                .set_base_and_cursor(base, current);
        }
        if terminate != 0 {
            // Check for termination
            if !matches!(
                (*ctxt).instate,
                XmlParserInputState::XmlParserEOF | XmlParserInputState::XmlParserEpilog
            ) {
                xml_fatal_err(&mut *ctxt, XmlParserErrors::XmlErrDocumentEnd, None);
            }
            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEpilog)
                && !(*ctxt).content_bytes().is_empty()
            {
                xml_fatal_err(&mut *ctxt, XmlParserErrors::XmlErrDocumentEnd, None);
            }
            if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                if let Some(end_document) =
                    (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_document)
                {
                    end_document((*ctxt).user_data.clone());
                }
            }
            (*ctxt).instate = XmlParserInputState::XmlParserEOF;
        }
        if (*ctxt).well_formed == 0 {
            (*ctxt).err_no
        } else {
            0
        }
    }
}

/// Create a parser context for using the XML parser in push mode.
/// If @buffer and @size are non-NULL, the data is used to detect
/// the encoding.  The remaining characters will be parsed so they
/// don't need to be fed in again through xmlParseChunk.
/// To allow content encoding detection, @size should be >= 4
/// The value of @filename is used for fetching external entities
/// and error/warning reports.
///
/// Returns the new parser context or NULL
#[doc(alias = "xmlCreatePushParserCtxt")]
pub unsafe fn xml_create_push_parser_ctxt(
    sax: Option<Box<XmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
    chunk: *const i8,
    size: i32,
    filename: Option<&str>,
) -> XmlParserCtxtPtr {
    use crate::parser::XmlParserInput;

    unsafe {
        use crate::{io::xml_parser_get_directory, parser::xml_new_sax_parser_ctxt};

        let buf = Rc::new(RefCell::new(XmlParserInputBuffer::new(
            XmlCharEncoding::None,
        )));

        let Ok(ctxt) = xml_new_sax_parser_ctxt(sax, user_data) else {
            xml_err_memory(null_mut(), Some("creating parser: out of memory\n"));
            return null_mut();
        };
        (*ctxt).dict_names = 1;
        if filename.is_none() {
            (*ctxt).directory = None;
        } else if let Some(dir) = filename.and_then(xml_parser_get_directory) {
            (*ctxt).directory = Some(dir.to_string_lossy().into_owned());
        }

        let Some(mut input_stream) = XmlParserInput::new((!ctxt.is_null()).then(|| &mut *ctxt))
        else {
            return null_mut();
        };

        if let Some(filename) = filename {
            let canonic = canonic_path(filename);
            input_stream.filename = Some(canonic.into_owned());
        } else {
            input_stream.filename = None;
        }
        input_stream.buf = Some(buf);
        input_stream.reset_base();
        (*ctxt).input_push(input_stream);

        // If the caller didn't provide an initial 'chunk' for determining
        // the encoding, we set the context to xmlCharEncoding::XML_CHAR_ENCODING_NONE so
        // that it can be automatically determined later
        (*ctxt).charset = XmlCharEncoding::None;

        if size != 0
            && !chunk.is_null()
            && (*ctxt).input().is_some()
            && (*ctxt).input().unwrap().buf.is_some()
        {
            let base: usize = (*ctxt).input().unwrap().get_base();
            let cur = (*ctxt).input().unwrap().offset_from_base();

            (*ctxt)
                .input_mut()
                .unwrap()
                .buf
                .as_mut()
                .unwrap()
                .borrow_mut()
                .push_bytes(from_raw_parts(chunk as *const u8, size as usize));
            (*ctxt).input_mut().unwrap().set_base_and_cursor(base, cur);
        }

        ctxt
    }
}
