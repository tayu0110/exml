use crate::{
    error::XmlParserErrors,
    libxml::{
        parser::{XmlParserInputState, XmlParserOption},
        parser_internals::{XML_MAX_NAME_LENGTH, XML_MAX_TEXT_LENGTH},
    },
};

use super::{xml_fatal_err, XmlParserCharValid, XmlParserCtxt};

/// Parse an XML Nmtoken.
///
/// `[7] Nmtoken ::= (NameChar)+`
///
/// `[8] Nmtokens ::= Nmtoken (#x20 Nmtoken)*`
///
/// Returns the Nmtoken parsed or NULL
#[doc(alias = "xmlParseNmtoken")]
pub(crate) unsafe fn parse_nmtoken(ctxt: &mut XmlParserCtxt) -> Option<String> {
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
