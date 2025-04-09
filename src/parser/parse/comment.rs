use std::str::from_utf8_unchecked;

use crate::{
    error::XmlParserErrors,
    parser::{
        XML_MAX_HUGE_LENGTH, XML_MAX_TEXT_LENGTH, XmlParserCtxt, XmlParserInputState,
        XmlParserOption, xml_fatal_err, xml_fatal_err_msg, xml_fatal_err_msg_int,
        xml_fatal_err_msg_str, xml_is_char,
    },
};

impl XmlParserCtxt {
    /// Skip an XML (SGML) comment `<!-- .... -->`.  
    /// The spec says that "For compatibility, the string `"--"` (double-hyphen)
    /// must not occur within comments."  
    ///
    /// This is the slow routine in case the accelerator for ascii didn't work
    ///
    /// ```text
    /// [15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
    /// ```
    #[doc(alias = "xmlParseCommentComplex")]
    unsafe fn parse_comment_complex(&mut self, buf: &mut String) {
        unsafe {
            let mut ql = 0;
            let mut rl = 0;
            let mut l = 0;
            let max_length = if self.options & XmlParserOption::XmlParseHuge as i32 != 0 {
                XML_MAX_HUGE_LENGTH
            } else {
                XML_MAX_TEXT_LENGTH
            };

            let inputid = self.input().unwrap().id;

            macro_rules! not_terminated {
                () => {
                    xml_fatal_err_msg_str!(
                        self,
                        XmlParserErrors::XmlErrCommentNotFinished,
                        "Comment not terminated\n"
                    );
                    return;
                };
            }

            let Some(mut q) = self.current_char(&mut ql) else {
                not_terminated!();
            };
            if !xml_is_char(q as u32) {
                xml_fatal_err_msg_int!(
                    self,
                    XmlParserErrors::XmlErrInvalidChar,
                    format!("xmlParseComment: invalid xmlChar value {}\n", q as i32).as_str(),
                    q as i32
                );
                return;
            }
            self.consume_char_if(|_, _| true);
            let Some(mut r) = self.current_char(&mut rl) else {
                not_terminated!();
            };
            if !xml_is_char(r as u32) {
                xml_fatal_err_msg_int!(
                    self,
                    XmlParserErrors::XmlErrInvalidChar,
                    format!("xmlParseComment: invalid xmlChar value {}\n", r as i32).as_str(),
                    r as i32
                );
                return;
            }
            self.consume_char_if(|_, _| true);
            let Some(mut cur) = self.current_char(&mut l) else {
                not_terminated!();
            };
            while xml_is_char(cur as u32) && (cur != '>' || r != '-' || q != '-') {
                if r == '-' && q == '-' {
                    xml_fatal_err(self, XmlParserErrors::XmlErrHyphenInComment, None);
                }
                buf.push(q);
                if buf.len() > max_length {
                    xml_fatal_err_msg_str!(
                        self,
                        XmlParserErrors::XmlErrCommentNotFinished,
                        "Comment too big found"
                    );
                    return;
                }

                q = r;
                r = cur;

                self.consume_char_if(|_, _| true);
                cur = self.current_char(&mut l).unwrap_or('\0');
            }
            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                return;
            }
            if cur == '\0' {
                xml_fatal_err_msg_str!(
                    self,
                    XmlParserErrors::XmlErrCommentNotFinished,
                    "Comment not terminated \n<!--{}\n",
                    buf
                );
            } else if !xml_is_char(cur as u32) {
                xml_fatal_err_msg_int!(
                    self,
                    XmlParserErrors::XmlErrInvalidChar,
                    format!("xmlParseComment: invalid xmlChar value {}\n", cur as i32).as_str(),
                    cur as i32
                );
            } else {
                if inputid != self.input().unwrap().id {
                    xml_fatal_err_msg(
                        self,
                        XmlParserErrors::XmlErrEntityBoundary,
                        "Comment doesn't start and stop in the same entity\n",
                    );
                }
                self.skip_char();
                if self.disable_sax == 0 {
                    if let Some(comment) = self.sax.as_deref_mut().and_then(|sax| sax.comment) {
                        comment(self, buf);
                    }
                }
            }
        }
    }

    /// Parse an XML (SGML) comment. Always consumes '<!'.
    ///
    /// The spec says that "For compatibility, the string "--" (double-hyphen)
    /// must not occur within comments. "
    ///
    /// ```text
    /// [15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
    /// ```
    #[doc(alias = "xmlParseComment")]
    pub(crate) unsafe fn parse_comment(&mut self) {
        unsafe {
            let max_length = if self.options & XmlParserOption::XmlParseHuge as i32 != 0 {
                XML_MAX_HUGE_LENGTH
            } else {
                XML_MAX_TEXT_LENGTH
            };

            // Check that there is a comment right here.
            if !self.content_bytes().starts_with(b"<!") {
                return;
            }
            self.advance(2);
            if !self.content_bytes().starts_with(b"--") {
                return;
            }
            let state = self.instate;
            self.instate = XmlParserInputState::XmlParserComment;
            let inputid = self.input().unwrap().id;
            self.advance(2);
            self.grow();

            // Accelerated common case where input don't need to be
            // modified before passing it to the handler.
            let mut buf = String::new();
            let has_sax_comment = self.sax.as_deref().is_some_and(|sax| sax.comment.is_some());
            let mut input = self.content_bytes();
            while !input.is_empty() {
                let mut len = input
                    .iter()
                    .take_while(|&&b| {
                        b != b'-'
                            && b != b'\r'
                            && (b == b'\t' || b == b'\n' || (0x20..0x80).contains(&b))
                    })
                    .count();
                if has_sax_comment {
                    // # Safety
                    // `input[..len]` contains only ASCII characters.
                    // Therefore, UTF-8 validation won't fail.
                    buf.push_str(from_utf8_unchecked(&input[..len]));
                    if buf.len() > max_length {
                        xml_fatal_err_msg_str!(
                            self,
                            XmlParserErrors::XmlErrCommentNotFinished,
                            "Comment too big found"
                        );
                        return;
                    }
                }

                if len < input.len() && input[len] == b'\r' {
                    if len + 1 < input.len() && input[len + 1] == b'\n' {
                        len += 1;
                    } else {
                        self.advance_with_line_handling(len);
                        break;
                    }
                }
                self.advance_with_line_handling(len);
                self.shrink();
                self.grow();
                if self.instate == XmlParserInputState::XmlParserEOF {
                    return;
                }
                input = self.content_bytes();
                if !input.is_empty() && input[0] == b'-' {
                    if input.len() >= 2 && input[1] == b'-' {
                        if input.len() >= 3 && input[2] == b'>' {
                            if self.input().unwrap().id != inputid {
                                xml_fatal_err_msg(
                                    self,
                                    XmlParserErrors::XmlErrEntityBoundary,
                                    "comment doesn't start and stop input the same entity\n",
                                );
                            }
                            self.advance(3);
                            if self.disable_sax == 0 {
                                if let Some(comment) =
                                    self.sax.as_deref_mut().and_then(|sax| sax.comment)
                                {
                                    comment(self, &buf);
                                }
                            }
                            if !matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                                self.instate = state;
                            }
                            return;
                        }
                        if !buf.is_empty() {
                            xml_fatal_err_msg_str!(
                                self,
                                XmlParserErrors::XmlErrHyphenInComment,
                                "Double hyphen within comment: <!--{}\n",
                                buf
                            );
                        } else {
                            xml_fatal_err_msg_str!(
                                self,
                                XmlParserErrors::XmlErrHyphenInComment,
                                "Double hyphen within comment\n"
                            );
                        }
                        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                            return;
                        }
                        buf.push_str("--");
                        self.advance(2);
                    } else {
                        buf.push('-');
                        self.advance(1);
                    }
                } else {
                    break;
                }
                input = self.content_bytes();
            }
            self.parse_comment_complex(&mut buf);
            self.instate = state;
        }
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use crate::parser::{XmlParserCtxt, XmlSAXHandler, xml_ctxt_read_memory};

    thread_local! {
        static RESULT: RefCell<String> = const {
            RefCell::new(String::new())
        };
    }

    fn make_sax_handler_only_comment() -> XmlSAXHandler {
        fn test_comment(_context: &mut XmlParserCtxt, comment: &str) {
            RESULT.with_borrow_mut(|result| {
                result.push_str(comment);
            });
        }

        XmlSAXHandler {
            comment: Some(test_comment),
            ..Default::default()
        }
    }

    fn make_parser_context() -> XmlParserCtxt {
        XmlParserCtxt::new_sax_parser(Some(Box::new(make_sax_handler_only_comment())), None)
            .unwrap()
    }

    fn do_test(docs: &[(&str, &str)]) {
        for &(doc, res) in docs {
            unsafe {
                let mut ctxt = make_parser_context();
                xml_ctxt_read_memory(&mut ctxt, doc.as_bytes().to_vec(), None, None, 0);
            }
            let result = RESULT.with_borrow(|result| result.clone());
            assert_eq!(result, res);
            RESULT.with_borrow_mut(|result| result.clear());
        }
    }

    #[test]
    fn only_ascii_comment_test() {
        const DOCUMENT_AND_RESULT: &[(&str, &str)] = &[
            ("<!--Hello, World!!!--><root/>", "Hello, World!!!"),
            ("<!--\n--><root/>", "\n"),
            ("<!--\r\r\n--><root/>", "\n\n"),
            ("<!--\r\r\r\r--><root/>", "\n\n\n\n"),
            (
                "<!--Hello\t,\rWorld\r\n!!!   --><root/>",
                "Hello\t,\nWorld\n!!!   ",
            ),
        ];
        do_test(DOCUMENT_AND_RESULT);
    }

    #[test]
    fn complex_comment_test() {
        const DOCUMENT_AND_RESULT: &[(&str, &str)] = &[
            ("<!--あいうえお--><root/>", "あいうえお"),
            ("<!--あいうえお\n--><root/>", "あいうえお\n"),
            ("<!--あい\r\r\nうえお--><root/>", "あい\n\nうえお"),
            (
                "<!--ai\r\nあい\r\nうえ\nueo\r\nお--><root/>",
                "ai\nあい\nうえ\nueo\nお",
            ),
        ];
        do_test(DOCUMENT_AND_RESULT);
    }

    #[test]
    fn invalid_comment_test() {
        const DOCUMENT_AND_RESULT: &[(&str, &str)] = &[
            // "--" (double-hyphen) MUST NOT occur within comments.
            ("<!--Hello,--World!!!--><root/>", ""),
            ("<!--あいう--えお--><root/>", ""),
            // Note that the grammer does not allow a comment ending in --->.
            ("<!--Hello,World!!!---><root/>", ""),
            ("<!--あいうえお---><root/>", ""),
        ];
        do_test(DOCUMENT_AND_RESULT);
    }
}
