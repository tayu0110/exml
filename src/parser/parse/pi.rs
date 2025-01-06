use crate::{
    error::XmlParserErrors,
    libxml::{
        catalog::{xml_catalog_get_defaults, XmlCatalogAllow, XML_CATALOG_PI},
        parser::{XmlParserInputState, XmlParserOption},
        parser_internals::{XML_MAX_HUGE_LENGTH, XML_MAX_TEXT_LENGTH},
    },
    parser::{
        xml_fatal_err, xml_fatal_err_msg, xml_fatal_err_msg_str, xml_is_char, xml_ns_err,
        xml_warning_msg, XmlParserCtxt,
    },
};

use super::parse_name;

// List of XML prefixed PI allowed by W3C specs
const XML_W3_CPIS: &[&str] = &["xml-stylesheet", "xml-model"];

/// Parse the name of a PI
///
/// ```text
/// [17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
/// ```
///
/// Returns the PITarget name or NULL
#[doc(alias = "xmlParsePITarget")]
pub(crate) unsafe fn parse_pi_target(ctxt: &mut XmlParserCtxt) -> Option<String> {
    let name = parse_name(ctxt)?;
    if name.as_bytes()[..name.len().min(3)].eq_ignore_ascii_case(b"xml") {
        if name == "xml" {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrReservedXmlName,
                "XML declaration allowed only at the start of the document\n",
            );
            return Some(name);
        } else if name.len() == 3 {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrReservedXmlName, None);
            return Some(name);
        }
        if XML_W3_CPIS.iter().any(|&pi| pi == name) {
            return Some(name);
        }
        xml_warning_msg!(
            ctxt,
            XmlParserErrors::XmlErrReservedXmlName,
            "xmlParsePITarget: invalid name prefix 'xml'\n"
        );
    }
    if name.contains(':') {
        xml_ns_err!(
            ctxt,
            XmlParserErrors::XmlNsErrColon,
            "colons are forbidden from PI names '{}'\n",
            name
        );
    }
    Some(name)
}

/// Parse an XML Catalog Processing Instruction.
///
/// `<?oasis-xml-catalog catalog="http://example.com/catalog.xml"?>`
///
/// Occurs only if allowed by the user and if happening in the Misc
/// part of the document before any doctype information.  
/// This will add the given catalog to the parsing context in order
/// to be used if there is a resolution need further down in the document.
#[doc(alias = "xmlParseCatalogPI")]
#[cfg(feature = "catalog")]
unsafe fn parse_catalog_pi(ctxt: &mut XmlParserCtxt, catalog: &str) {
    use crate::libxml::{catalog::XmlCatalogEntry, chvalid::xml_is_blank_char};

    macro_rules! syntax_error {
        () => {
            xml_warning_msg!(
                ctxt,
                XmlParserErrors::XmlWarCatalogPI,
                "Catalog PI syntax error: {}\n",
                catalog
            );
        };
    }

    let mut tmp = catalog;
    tmp = tmp.trim_start_matches(|c| xml_is_blank_char(c as u32));
    let Some(rem) = tmp.strip_prefix("catalog") else {
        syntax_error!();
        return;
    };
    tmp = rem.trim_start_matches(|c| xml_is_blank_char(c as u32));
    let Some(rem) = tmp.strip_prefix('=') else {
        syntax_error!();
        return;
    };
    tmp = rem.trim_start_matches(|c| xml_is_blank_char(c as u32));
    let Some(quoto) = tmp.chars().next().filter(|&c| c == '\'' || c == '"') else {
        syntax_error!();
        return;
    };
    tmp = &tmp[1..];
    let Some((url, rem)) = tmp.split_once(quoto) else {
        syntax_error!();
        return;
    };
    tmp = rem.trim_start_matches(|c| xml_is_blank_char(c as u32));
    if !tmp.is_empty() {
        syntax_error!();
        return;
    }
    if let Some(catalogs) = ctxt.catalogs.as_mut() {
        catalogs.add_local(url);
    } else {
        ctxt.catalogs = Some(XmlCatalogEntry::new(url));
    }
}

/// Parse an XML Processing Instruction.
///
/// `[16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'`
///
/// The processing is transferred to SAX once parsed.
#[doc(alias = "xmlParsePI")]
pub(crate) unsafe fn parse_pi(ctxt: &mut XmlParserCtxt) {
    let max_length = if ctxt.options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH
    } else {
        XML_MAX_TEXT_LENGTH
    };

    let mut buf = String::new();
    if ctxt.content_bytes().starts_with(b"<?") {
        let inputid = (*ctxt.input).id;
        let state = ctxt.instate;
        ctxt.instate = XmlParserInputState::XmlParserPI;
        // this is a Processing Instruction.
        ctxt.advance(2);

        // Parse the target name and check for special support like namespace.
        if let Some(target) = parse_pi_target(ctxt) {
            if ctxt.content_bytes().starts_with(b"?>") {
                if inputid != (*ctxt.input).id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        "PI declaration doesn't start and stop in the same entity\n",
                    );
                }
                ctxt.advance(2);

                // SAX: PI detected.
                if ctxt.disable_sax == 0 {
                    if let Some(processing_instruction) = ctxt
                        .sax
                        .as_deref_mut()
                        .and_then(|sax| sax.processing_instruction)
                    {
                        processing_instruction(ctxt.user_data.clone(), &target, None);
                    }
                }
                if !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                    ctxt.instate = state;
                }
                return;
            }
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "ParsePI: PI {} space expected\n",
                    target
                );
            }
            while let Some(cur) = ctxt.consume_char_if(|ctxt, c| {
                xml_is_char(c as u32) && (c != '?' || ctxt.nth_byte(1) != b'>')
            }) {
                buf.push(cur);
                if buf.len() > max_length {
                    xml_fatal_err_msg_str!(
                        ctxt,
                        XmlParserErrors::XmlErrPINotFinished,
                        "PI {} too big found",
                        target
                    );
                    ctxt.instate = state;
                    return;
                }
            }
            if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                return;
            }
            let mut l = 0;
            if ctxt.current_char(&mut l).unwrap_or('\0') != '?' {
                xml_fatal_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlErrPINotFinished,
                    "ParsePI: PI {} never end ...\n",
                    target
                );
            } else {
                if inputid != (*ctxt.input).id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        "PI declaration doesn't start and stop in the same entity\n",
                    );
                }
                ctxt.advance(2);

                #[cfg(feature = "catalog")]
                if matches!(
                    state,
                    XmlParserInputState::XmlParserMisc | XmlParserInputState::XmlParserStart
                ) && target == XML_CATALOG_PI
                {
                    let allow = xml_catalog_get_defaults();
                    if matches!(allow, XmlCatalogAllow::Document | XmlCatalogAllow::All) {
                        parse_catalog_pi(ctxt, &buf);
                    }
                }

                // SAX: PI detected.
                if ctxt.disable_sax == 0 {
                    if let Some(processing_instruction) = ctxt
                        .sax
                        .as_deref_mut()
                        .and_then(|sax| sax.processing_instruction)
                    {
                        processing_instruction(ctxt.user_data.clone(), &target, Some(&buf));
                    }
                }
            }
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrPINotStarted, None);
        }
        if !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            ctxt.instate = state;
        }
    }
}
