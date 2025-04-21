//! Provide methods and data structures for XML Schemas types.
//!
//! This module is based on `libxml/xmlschemastypes.h`, `xmlschemas.c`, `xmlschemastypes.c` and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: implementation of XML Schema Datatypes
// Description: module providing the XML Schema Datatypes implementation
//              both definition and validity checking
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// schemastypes.c : implementation of the XML Schema Datatypes definition and validity checking
//
// See Copyright for the status of this software.
//
// Daniel Veillard <veillard@redhat.com>

pub mod parse;
pub mod primitives;

use std::{borrow::Cow, iter::once};

use primitives::XmlSchemaValPrimitives;

use crate::libxml::{chvalid::xml_is_blank_char, schemas_internals::XmlSchemaValType};

pub struct XmlSchemaVal {
    pub(crate) typ: XmlSchemaValType,
    next: Option<Box<XmlSchemaVal>>,
    pub(crate) value: XmlSchemaValPrimitives,
}

#[doc(alias = "IS_WSP_REPLACE_CH")]
fn is_wsp_replace_ch(c: char) -> bool {
    matches!(c, '\x09' | '\x0A' | '\x0D')
}

#[doc(alias = "IS_WSP_SPACE_CH")]
fn is_wsp_space_ch(c: char) -> bool {
    c == '\x20'
}

#[doc(alias = "IS_WSP_BLANK_CH")]
fn is_wsp_blank_ch(c: char) -> bool {
    xml_is_blank_char(c as u32)
}

/// Removes and normalize white spaces in the string
///
/// Returns the new string or `None` if no change was required.
#[doc(alias = "xmlSchemaCollapseString")]
pub fn xml_schema_collapse_string(value: &str) -> Option<Cow<'_, str>> {
    let start = value.trim_start_matches(|c| xml_is_blank_char(c as u32));
    let Some(col) = start
        .chars()
        .zip(start.chars().skip(1).chain(once('\0')))
        .position(|(f, s)| {
            (f == ' ' && xml_is_blank_char(s as u32)) || f == '\x0A' || f == '\x09' || f == '\x0D'
        })
    else {
        let res = start.trim_end_matches(|c| xml_is_blank_char(c as u32));
        return (res.len() != value.len()).then_some(Cow::Borrowed(res));
    };
    let mut buf = String::with_capacity(start.len());
    buf.push_str(&start[..col]);
    let res = start[col..]
        .split(|c| xml_is_blank_char(c as u32))
        .filter(|s| !s.is_empty())
        .fold(buf, |mut buf, s| {
            buf.push(' ');
            buf.push_str(s);
            buf
        });
    Some(Cow::Owned(res))
}

/// Replaces 0xd, 0x9 and 0xa with a space.
///
/// Returns the new string or NULL if no change was required.
#[doc(alias = "xmlSchemaWhiteSpaceReplace")]
pub fn xml_schema_white_space_replace(value: &str) -> Option<String> {
    if !value.contains(['\x0D', '\x09', '\x0A']) {
        return None;
    }
    Some(value.replace(['\x0D', '\x09', '\x0A'], " "))
}
