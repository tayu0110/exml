use std::borrow::Cow;

use crate::{error::XmlParserErrors, parser::xml_fatal_err_msg_str};

use super::{XmlParserCtxt, xml_is_letter};

/// parse an UTF8 encoded XML qualified name string
///
/// ```text
/// [NS 5] QName ::= (Prefix ':')? LocalPart
///
/// [NS 6] Prefix ::= NCName
///
/// [NS 7] LocalPart ::= NCName
/// ```
///
/// If a prefix is found, return `(Some(Prefix), LocalPart)`.  
/// Otherwise, return `(None, name)`.
///
/// # Note
/// - Both `Prefix` and `LocalPart` does not include the delimiter ':'.
/// - This function does not perform validation.
#[doc(alias = "xmlSplitQName")]
pub fn split_qname<'a>(ctxt: &mut XmlParserCtxt, name: &'a str) -> (Option<&'a str>, &'a str) {
    // nasty but well=formed
    if name.starts_with(':') {
        return (None, name);
    }
    let Some((prefix, local)) = name.split_once(':').filter(|qname| !qname.1.is_empty()) else {
        return (None, name);
    };
    if local.starts_with(|c| {
        // Check that the first character is proper to start a new name
        !xml_is_letter(c as u32) && c != '_'
    }) {
        xml_fatal_err_msg_str!(
            ctxt,
            XmlParserErrors::XmlNsErrQname,
            "Name {} is not XML Namespace compliant\n",
            name
        );
    }
    (Some(prefix), local)
}

/// Parse an XML qualified name string
///
/// ```text
/// [NS 5] QName ::= (Prefix ':')? LocalPart
///
/// [NS 6] Prefix ::= NCName
///
/// [NS 7] LocalPart ::= NCName
/// ```
///
/// Returns `None` if the name doesn't have a prefix.
/// Otherwise, returns `Some((Prefix, LocalPart))`.
///
/// # Note
/// This function does not perform validation.
#[doc(alias = "xmlSplitQName2")]
pub fn split_qname2(name: &str) -> Option<(&str, &str)> {
    // nasty but valid
    if name.starts_with(':') {
        return None;
    }

    // we are not trying to validate but just to cut, and yes it will
    // work even if this is as set of UTF-8 encoded chars
    name.split_once(':')
}

/// Parse an XML qualified name string
///
/// If a prefix is found, return `Some(LocalPart)` and update `len` to the length of `Prefix`.  
/// Otherwise, return `None`.
///
/// # Note
/// This function does not perform validation.
#[doc(alias = "xmlSplitQName3")]
pub fn split_qname3<'a>(name: &'a str, len: &mut usize) -> Option<&'a str> {
    // nasty but valid
    if name.starts_with(':') {
        return None;
    }

    // we are not trying to validate but just to cut, and yes it will
    // work even if this is as set of UTF-8 encoded chars
    let (prefix, local) = name.split_once(':').filter(|qname| !qname.1.is_empty())?;
    *len = prefix.len();
    Some(local)
}

/// Builds the QName `"prefix:ncname"`.
///
/// If `prefix` is `Some` and not empty, return `Cow::Owned(QName)`.  
/// Otherwise, return `Cow::Borrowed(ncname)`.
#[doc(alias = "xmlBuildQName")]
pub fn build_qname<'a>(ncname: &'a str, prefix: Option<&str>) -> Cow<'a, str> {
    let Some(prefix) = prefix.filter(|p| !p.is_empty()) else {
        return Cow::Borrowed(ncname);
    };
    Cow::Owned(format!("{prefix}:{ncname}"))
}
