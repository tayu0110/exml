use std::str::from_utf8_unchecked;

use crate::{
    error::XmlParserErrors,
    libxml::{
        parser::{XmlParserInputState, XmlParserOption},
        parser_internals::{XML_MAX_HUGE_LENGTH, XML_MAX_TEXT_LENGTH},
    },
    parser::{
        XmlParserCtxt, xml_fatal_err, xml_fatal_err_msg, xml_fatal_err_msg_int,
        xml_fatal_err_msg_str, xml_is_char,
    },
};

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
unsafe fn parse_comment_complex(ctxt: &mut XmlParserCtxt, buf: &mut String) {
    unsafe {
        let mut ql = 0;
        let mut rl = 0;
        let mut l = 0;
        let max_length = if ctxt.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_HUGE_LENGTH
        } else {
            XML_MAX_TEXT_LENGTH
        };

        let inputid = ctxt.input().unwrap().id;

        macro_rules! not_terminated {
            () => {
                xml_fatal_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlErrCommentNotFinished,
                    "Comment not terminated\n"
                );
                return;
            };
        }

        let Some(mut q) = ctxt.current_char(&mut ql) else {
            not_terminated!();
        };
        if !xml_is_char(q as u32) {
            xml_fatal_err_msg_int!(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                format!("xmlParseComment: invalid xmlChar value {}\n", q as i32).as_str(),
                q as i32
            );
            return;
        }
        ctxt.consume_char_if(|_, _| true);
        let Some(mut r) = ctxt.current_char(&mut rl) else {
            not_terminated!();
        };
        if !xml_is_char(r as u32) {
            xml_fatal_err_msg_int!(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                format!("xmlParseComment: invalid xmlChar value {}\n", r as i32).as_str(),
                r as i32
            );
            return;
        }
        ctxt.consume_char_if(|_, _| true);
        let Some(mut cur) = ctxt.current_char(&mut l) else {
            not_terminated!();
        };
        while xml_is_char(cur as u32) && (cur != '>' || r != '-' || q != '-') {
            if r == '-' && q == '-' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrHyphenInComment, None);
            }
            buf.push(q);
            if buf.len() > max_length {
                xml_fatal_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlErrCommentNotFinished,
                    "Comment too big found"
                );
                return;
            }

            q = r;
            r = cur;

            ctxt.consume_char_if(|_, _| true);
            cur = ctxt.current_char(&mut l).unwrap_or('\0');
        }
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return;
        }
        if cur == '\0' {
            xml_fatal_err_msg_str!(
                ctxt,
                XmlParserErrors::XmlErrCommentNotFinished,
                "Comment not terminated \n<!--{}\n",
                buf
            );
        } else if !xml_is_char(cur as u32) {
            xml_fatal_err_msg_int!(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                format!("xmlParseComment: invalid xmlChar value {}\n", cur as i32).as_str(),
                cur as i32
            );
        } else {
            if inputid != ctxt.input().unwrap().id {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    "Comment doesn't start and stop in the same entity\n",
                );
            }
            ctxt.skip_char();
            if ctxt.disable_sax == 0 {
                if let Some(comment) = ctxt.sax.as_deref_mut().and_then(|sax| sax.comment) {
                    comment(ctxt.user_data.clone(), buf);
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
pub(crate) unsafe fn parse_comment(ctxt: &mut XmlParserCtxt) {
    unsafe {
        let max_length = if ctxt.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_HUGE_LENGTH
        } else {
            XML_MAX_TEXT_LENGTH
        };

        // Check that there is a comment right here.
        if !ctxt.content_bytes().starts_with(b"<!") {
            return;
        }
        ctxt.advance(2);
        if !ctxt.content_bytes().starts_with(b"--") {
            return;
        }
        let state = ctxt.instate;
        ctxt.instate = XmlParserInputState::XmlParserComment;
        let inputid = ctxt.input().unwrap().id;
        ctxt.advance(2);
        ctxt.grow();

        // Accelerated common case where input don't need to be
        // modified before passing it to the handler.
        let mut buf = String::new();
        let has_sax_comment = ctxt.sax.as_deref().is_some_and(|sax| sax.comment.is_some());
        let mut input = ctxt.content_bytes();
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
                        ctxt,
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
                    ctxt.advance_with_line_handling(len);
                    break;
                }
            }
            ctxt.advance_with_line_handling(len);
            ctxt.shrink();
            ctxt.grow();
            if ctxt.instate == XmlParserInputState::XmlParserEOF {
                return;
            }
            input = ctxt.content_bytes();
            if !input.is_empty() && input[0] == b'-' {
                if input.len() >= 2 && input[1] == b'-' {
                    if input.len() >= 3 && input[2] == b'>' {
                        if ctxt.input().unwrap().id != inputid {
                            xml_fatal_err_msg(
                                ctxt,
                                XmlParserErrors::XmlErrEntityBoundary,
                                "comment doesn't start and stop input the same entity\n",
                            );
                        }
                        ctxt.advance(3);
                        if ctxt.disable_sax == 0 {
                            if let Some(comment) =
                                ctxt.sax.as_deref_mut().and_then(|sax| sax.comment)
                            {
                                comment(ctxt.user_data.clone(), &buf);
                            }
                        }
                        if !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                            ctxt.instate = state;
                        }
                        return;
                    }
                    if !buf.is_empty() {
                        xml_fatal_err_msg_str!(
                            ctxt,
                            XmlParserErrors::XmlErrHyphenInComment,
                            "Double hyphen within comment: <!--{}\n",
                            buf
                        );
                    } else {
                        xml_fatal_err_msg_str!(
                            ctxt,
                            XmlParserErrors::XmlErrHyphenInComment,
                            "Double hyphen within comment\n"
                        );
                    }
                    if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                        return;
                    }
                    buf.push_str("--");
                    ctxt.advance(2);
                } else {
                    buf.push('-');
                    ctxt.advance(1);
                }
            } else {
                break;
            }
            input = ctxt.content_bytes();
        }
        parse_comment_complex(ctxt, &mut buf);
        ctxt.instate = state;
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use crate::{
        globals::GenericErrorContext,
        libxml::parser::XmlSAXHandler,
        parser::{XmlParserCtxt, xml_ctxt_read_memory, xml_new_sax_parser_ctxt},
    };

    thread_local! {
        static RESULT: RefCell<String> = const {
            RefCell::new(String::new())
        };
    }

    fn make_sax_handler_only_comment() -> XmlSAXHandler {
        fn test_comment(_context: Option<GenericErrorContext>, comment: &str) {
            RESULT.with_borrow_mut(|result| {
                result.push_str(comment);
            });
        }

        XmlSAXHandler {
            comment: Some(test_comment),
            ..Default::default()
        }
    }

    fn make_parser_context() -> *mut XmlParserCtxt {
        unsafe {
            xml_new_sax_parser_ctxt(Some(Box::new(make_sax_handler_only_comment())), None).unwrap()
        }
    }

    fn do_test(docs: &[(&str, &str)]) {
        for &(doc, res) in docs {
            unsafe {
                let ctxt = make_parser_context();
                xml_ctxt_read_memory(ctxt, doc.as_bytes().to_vec(), None, None, 0);
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
