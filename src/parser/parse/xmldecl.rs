use crate::{
    encoding::find_encoding_handler,
    error::XmlParserErrors,
    libxml::{
        chvalid::xml_is_blank_char,
        parser::{XmlParserInputState, XmlParserOption, XML_DEFAULT_VERSION},
        parser_internals::{XML_MAX_NAME_LENGTH, XML_MAX_TEXT_LENGTH},
    },
    parser::{
        xml_fatal_err, xml_fatal_err_msg, xml_fatal_err_msg_str, xml_warning_msg, XmlParserCtxt,
    },
};

/// Parse the XML version value.
///
/// ```text
/// [26] VersionNum ::= '1.' [0-9]+
/// ```
///
/// In practice allow [0-9].[0-9]+ at that level
///
/// Returns the string giving the XML version number, or NULL
#[doc(alias = "xmlParseVersionNum")]
unsafe fn parse_version_num(ctxt: &mut XmlParserCtxt) -> Option<String> {
    let mut buf = String::with_capacity(10);
    buf.push(ctxt.consume_char_if(|_, c| c.is_ascii_digit())?);
    ctxt.consume_char_if(|_, c| c == '.')?;
    buf.push('.');
    while let Some(c) = ctxt.consume_char_if(|_, c| c.is_ascii_digit()) {
        buf.push(c);
    }
    Some(buf)
}

/// Parse the XML version.
///
/// ```text
/// [24] VersionInfo ::= S 'version' Eq (' VersionNum ' | " VersionNum ")
///
/// [25] Eq ::= S? '=' S?
/// ```
///
/// Returns the version string, e.g. "1.0"
#[doc(alias = "xmlParseVersionInfo")]
unsafe fn parse_version_info(ctxt: &mut XmlParserCtxt) -> Option<String> {
    if !ctxt.content_bytes().starts_with(b"version") {
        return None;
    }
    ctxt.advance(7);
    ctxt.skip_blanks();
    if ctxt.consume_char_if(|_, c| c == '=').is_none() {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEqualRequired, None);
        return None;
    }
    ctxt.skip_blanks();
    let Some(quoto) = ctxt.consume_char_if(|_, c| c == '"' || c == '\'') else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotStarted, None);
        return None;
    };
    let version = parse_version_num(ctxt);
    if ctxt.consume_char_if(|_, c| c == quoto).is_none() {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotClosed, None);
        return None;
    }
    version
}

/// parse the XML encoding name
///
/// ```text
/// [81] EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
/// ```
///
/// Returns the encoding name value or NULL
#[doc(alias = "xmlParseEncName")]
unsafe fn parse_enc_name(ctxt: &mut XmlParserCtxt) -> Option<String> {
    let max_length = if ctxt.options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH
    } else {
        XML_MAX_NAME_LENGTH
    };
    let Some(first) = ctxt.consume_char_if(|_, c| c.is_ascii_alphabetic()) else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEncodingName, None);
        return None;
    };
    let mut buf = String::with_capacity(10);
    buf.push(first);
    while let Some(c) =
        ctxt.consume_char_if(|_, c| c.is_alphanumeric() || c == '.' || c == '_' || c == '-')
    {
        buf.push(c);
        if buf.len() > max_length {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("EncName"));
            return None;
        }
    }
    Some(buf)
}

/// Parse the XML encoding declaration
///
/// ```text
/// [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' |  "'" EncName "'")
/// ```
///
/// this setups the conversion filters.
///
/// Returns the encoding value or NULL
#[doc(alias = "xmlParseEncodingDecl")]
unsafe fn parse_encoding_decl(ctxt: &mut XmlParserCtxt) -> Option<String> {
    ctxt.skip_blanks();
    if !ctxt.content_bytes().starts_with(b"encoding") {
        return None;
    }
    ctxt.advance(8);
    ctxt.skip_blanks();
    if ctxt.consume_char_if(|_, c| c == '=').is_none() {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEqualRequired, None);
        return None;
    }
    ctxt.skip_blanks();
    let encoding = if let Some(quote) = ctxt.consume_char_if(|_, c| c == '"' || c == '\'') {
        let encoding = parse_enc_name(ctxt);
        if ctxt.consume_char_if(|_, c| c == quote).is_none() {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotClosed, None);
            return None;
        }
        encoding
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotStarted, None);
        None
    };

    // Non standard parsing, allowing the user to ignore encoding
    if ctxt.options & XmlParserOption::XmlParseIgnoreEnc as i32 != 0 {
        return None;
    }

    // UTF-16 encoding match has already taken place at this stage,
    // more over the little-endian/big-endian selection is already done
    if let Some(encoding) = encoding
        .as_deref()
        .filter(|e| e.eq_ignore_ascii_case("UTF-16") || e.eq_ignore_ascii_case("UTF16"))
    {
        // If no encoding was passed to the parser, that we are
        // using UTF-16 and no decoder is present i.e. the
        // document is apparently UTF-8 compatible, then raise an
        // encoding mismatch fatal error
        if ctxt.encoding.is_none()
            && (*ctxt.input).buf.is_some()
            && (*ctxt.input)
                .buf
                .as_ref()
                .unwrap()
                .borrow()
                .encoder
                .is_none()
        {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrInvalidEncoding,
                "Document labelled UTF-16 but has UTF-8 content\n",
            );
        }
        ctxt.encoding = Some(encoding.to_owned());
    } else if let Some(encoding) = encoding.as_deref().filter(|e| {
        // UTF-8 encoding is handled natively
        e.eq_ignore_ascii_case("UTF-8") || e.eq_ignore_ascii_case("UTF8")
    }) {
        // TODO: Check for encoding mismatch.
        ctxt.encoding = Some(encoding.to_owned());
    } else if let Some(encoding) = encoding.as_deref() {
        (*ctxt.input).encoding = Some(encoding.to_owned());

        if let Some(handler) = find_encoding_handler(encoding) {
            if ctxt.switch_to_encoding(handler) < 0 {
                // failed to convert
                ctxt.err_no = XmlParserErrors::XmlErrUnsupportedEncoding as i32;
                return None;
            }
        } else {
            xml_fatal_err_msg_str!(
                ctxt,
                XmlParserErrors::XmlErrUnsupportedEncoding,
                "Unsupported encoding {}\n",
                encoding
            );
            return None;
        }
    }
    encoding
}

/// Parse the XML standalone declaration
///
/// ```text
/// [32] SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no')'"'))
///
/// [ VC: Standalone Document Declaration ]
/// TODO The standalone document declaration must have the value "no"
/// if any external markup declarations contain declarations of:
///  - attributes with default values, if elements to which these
///    attributes apply appear in the document without specifications
///    of values for these attributes, or
///  - entities (other than amp, lt, gt, apos, quot), if references
///    to those entities appear in the document, or
///  - attributes with values subject to normalization, where the
///    attribute appears in the document with a value which will change
///    as a result of normalization, or
///  - element types with element content, if white space occurs directly
///    within any instance of those types.
/// ```
///
/// Returns:
/// - 1 if standalone="yes"
/// - 0 if standalone="no"
/// - -2 if standalone attribute is missing or invalid
///      (A standalone value of -2 means that the XML declaration was found,
///       but no value was specified for the standalone attribute).
#[doc(alias = "xmlParseSDDecl")]
unsafe fn parse_sddecl(ctxt: &mut XmlParserCtxt) -> i32 {
    ctxt.skip_blanks();
    if !ctxt.content_bytes().starts_with(b"standalone") {
        return -2;
    }
    ctxt.advance(10);
    ctxt.skip_blanks();
    if ctxt.consume_char_if(|_, c| c == '=').is_none() {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEqualRequired, None);
        return -2;
    }
    ctxt.skip_blanks();
    let Some(quoto) = ctxt.consume_char_if(|_, c| c == '"' || c == '\'') else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotStarted, None);
        return -2;
    };
    let standalone = if ctxt.content_bytes().starts_with(b"no") {
        ctxt.advance(2);
        0
    } else if ctxt.content_bytes().starts_with(b"yes") {
        ctxt.advance(3);
        1
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrStandaloneValue, None);
        -2
    };
    if ctxt.consume_char_if(|_, c| c == quoto).is_none() {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotClosed, None);
    }
    standalone
}

/// parse an XML declaration header
///
/// ```text
/// [23] XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
/// ```
#[doc(alias = "xmlParseXMLDecl")]
pub(crate) unsafe fn parse_xmldecl(ctxt: &mut XmlParserCtxt) {
    // This value for standalone indicates that the document has an
    // XML declaration but it does not have a standalone attribute.
    // It will be overwritten later if a standalone attribute is found.
    (*ctxt.input).standalone = -2;

    // We know that '<?xml' is here.
    assert!(ctxt.content_bytes().starts_with(b"<?xml"));
    ctxt.advance(5);

    if !xml_is_blank_char(ctxt.current_byte() as u32) {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrSpaceRequired,
            "Blank needed after '<?xml'\n",
        );
    }
    ctxt.skip_blanks();

    // We must have the VersionInfo here.
    if let Some(version) = parse_version_info(ctxt) {
        if version != XML_DEFAULT_VERSION {
            // Changed here for XML-1.0 5th edition
            if ctxt.options & XmlParserOption::XmlParseOld10 as i32 != 0 {
                xml_fatal_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlErrUnknownVersion,
                    "Unsupported version '{}'\n",
                    version
                );
            } else if version.starts_with("1.") {
                xml_warning_msg!(
                    ctxt,
                    XmlParserErrors::XmlWarUnknownVersion,
                    "Unsupported version '{}'\n",
                    version
                );
            } else {
                xml_fatal_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlErrUnknownVersion,
                    "Unsupported version '{}'\n",
                    version
                );
            }
        }
        ctxt.version = Some(version);
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrVersionMissing, None);
    }

    // We may have the encoding declaration
    if !xml_is_blank_char(ctxt.current_byte() as u32) {
        if ctxt.current_byte() == b'?' && ctxt.nth_byte(1) == b'>' {
            ctxt.advance(2);
            return;
        }
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrSpaceRequired,
            "Blank needed here\n",
        );
    }
    parse_encoding_decl(ctxt);
    if ctxt.err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32
        || matches!(ctxt.instate, XmlParserInputState::XmlParserEOF)
    {
        // The XML REC instructs us to stop parsing right here
        return;
    }

    // We may have the standalone status.
    if (*ctxt.input).encoding.is_some() && !xml_is_blank_char(ctxt.current_byte() as u32) {
        if ctxt.content_bytes().starts_with(b"?>") {
            ctxt.advance(2);
            return;
        }
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrSpaceRequired,
            "Blank needed here\n",
        );
    }

    // We can grow the input buffer freely at that point
    ctxt.grow();

    ctxt.skip_blanks();
    (*ctxt.input).standalone = parse_sddecl(ctxt);
    ctxt.skip_blanks();
    if ctxt.content_bytes().starts_with(b"?>") {
        ctxt.advance(2);
    } else if ctxt.current_byte() == b'>' {
        // Deprecated old WD ...
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrXMLDeclNotFinished, None);
        ctxt.skip_char();
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrXMLDeclNotFinished, None);
        ctxt.grow();
        while !ctxt.content_bytes().is_empty() {
            match ctxt.content_bytes().iter().position(|&c| c == b'>') {
                Some(pos) => {
                    ctxt.advance_with_line_handling(pos + 1);
                    break;
                }
                None => {
                    ctxt.advance_with_line_handling(ctxt.content_bytes().len());
                    ctxt.grow();
                }
            }
        }
    }
}

/// Parse an XML declaration header for external entities
///
/// ```text
/// [77] TextDecl ::= '<?xml' VersionInfo? EncodingDecl S? '?>'
/// ```
#[doc(alias = "xmlParseTextDecl")]
pub(crate) unsafe fn parse_text_decl(ctxt: &mut XmlParserCtxt) {
    // We know that '<?xml' is here.
    if !ctxt.content_bytes().starts_with(b"<?xml") || !xml_is_blank_char(ctxt.nth_byte(5) as u32) {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrXMLDeclNotStarted, None);
        return;
    }
    ctxt.advance(5);

    // Avoid expansion of parameter entities when skipping blanks.
    let oldstate = ctxt.instate;
    ctxt.instate = XmlParserInputState::XmlParserStart;

    if ctxt.skip_blanks() == 0 {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrSpaceRequired,
            "Space needed after '<?xml'\n",
        );
    }

    // We may have the VersionInfo here.
    let version = parse_version_info(ctxt);
    if version.is_some() && ctxt.skip_blanks() == 0 {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrSpaceRequired,
            "Space needed here\n",
        );
    }
    (*ctxt.input).version = version.or(Some(XML_DEFAULT_VERSION.to_owned()));

    // We must have the encoding declaration
    let encoding = parse_encoding_decl(ctxt);
    if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
        return;
    }
    if ctxt.err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32 {
        // The XML REC instructs us to stop parsing right here
        ctxt.instate = oldstate;
        return;
    }
    if encoding.is_none() && ctxt.err_no == XmlParserErrors::XmlErrOK as i32 {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrMissingEncoding,
            "Missing encoding in text declaration\n",
        );
    }

    ctxt.skip_blanks();
    if ctxt.content_bytes().starts_with(b"?>") {
        ctxt.advance(2);
    } else if ctxt.consume_char_if(|_, c| c == '>').is_some() {
        // Deprecated old WD ...
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrXMLDeclNotFinished, None);
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrXMLDeclNotFinished, None);
        ctxt.grow();
        while !ctxt.content_bytes().is_empty() {
            match ctxt.content_bytes().iter().position(|&c| c == b'>') {
                Some(pos) => {
                    ctxt.advance_with_line_handling(pos + 1);
                    break;
                }
                None => {
                    ctxt.advance_with_line_handling(ctxt.content_bytes().len());
                    ctxt.grow();
                }
            }
        }
    }

    ctxt.instate = oldstate;
}
