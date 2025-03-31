// Copyright of the original code is the following.
// --------
// Summary: interfaces for tree manipulation
// Description: this module describes the structures found in an tree resulting
//              from an XML or HTML parsing, as well as the API provided for
//              various processing on that tree
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// tree.c : implementation of access function for an XML tree.
//
// References:
//   XHTML 1.0 W3C REC: http://www.w3.org/TR/2002/REC-xhtml1-20020801/
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

#[cfg(feature = "libxml_output")]
use std::io::Write;

/// A DTD Notation definition.
#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct XmlNotation {
    // Notation name
    name: String,
    // Public identifier, if any
    pub(crate) public_id: Option<String>,
    // System identifier, if any
    pub(crate) system_id: Option<String>,
}

impl XmlNotation {
    pub(crate) fn new(name: &str, public_id: Option<&str>, system_id: Option<&str>) -> Self {
        Self {
            name: name.to_owned(),
            public_id: public_id.map(|p| p.to_owned()),
            system_id: system_id.map(|s| s.to_owned()),
        }
    }
}

/// This will dump the content the notation declaration as an XML DTD definition
#[doc(alias = "xmlDumpNotationDecl")]
#[cfg(feature = "libxml_output")]
pub fn xml_dump_notation_decl<'a>(out: &mut (impl Write + 'a), nota: &XmlNotation) {
    use crate::io::write_quoted;

    write!(out, "<!NOTATION {}", nota.name).ok();
    if let Some(public_id) = nota.public_id.as_deref() {
        write!(out, " PUBLIC ").ok();
        write_quoted(out, public_id).ok();
        if let Some(system_id) = nota.system_id.as_deref() {
            write!(out, " ").ok();
            write_quoted(out, system_id).ok();
        }
    } else {
        write!(out, " SYSTEM ").ok();
        write_quoted(out, nota.system_id.as_deref().unwrap()).ok();
    }
    writeln!(out, " >").ok();
}
