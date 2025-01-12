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
#[derive(Clone, Default)]
pub struct XmlNotation {
    // Notation name
    pub(crate) name: String,
    // Public identifier, if any
    pub(crate) public_id: Option<String>,
    // System identifier, if any
    pub(crate) system_id: Option<String>,
}

/// This will dump the content the notation declaration as an XML DTD definition
#[doc(alias = "xmlDumpNotationDecl")]
#[cfg(feature = "libxml_output")]
pub fn xml_dump_notation_decl<'a>(out: &mut (impl Write + 'a), nota: &XmlNotation) {
    fn write_quoted<'a>(out: &mut (impl Write + 'a), s: &str) -> std::io::Result<()> {
        if s.contains('"') {
            if s.contains('\'') {
                // If `s` contains both single and double-quote, quote with double-quote
                // and escape inner double-quotes
                write!(out, "\"")?;
                let mut split = s.split('"');
                write!(out, "{}", split.next().unwrap())?;
                for chunk in split {
                    write!(out, "&quot;{chunk}")?;
                }
                write!(out, "\"")?;
            } else {
                // If `s` contains only double-quote, quote with single-quote
                write!(out, "'{s}'")?;
            }
        } else {
            // If `s` does not contain double-quotes, quote with double-quote
            write!(out, "\"{s}\"")?;
        }
        Ok(())
    }

    write!(out, "<!NOTATION {}", nota.name).ok();
    if let Some(public_id) = nota.public_id.as_deref() {
        write!(out, " PUBLIC ").ok();
        write_quoted(out, public_id).ok();
        if let Some(system_id) = nota.system_id.as_deref() {
            write!(out, " ");
            write_quoted(out, system_id).ok();
        }
    } else {
        write!(out, " SYSTEM ");
        write_quoted(out, nota.system_id.as_deref().unwrap());
    }
    writeln!(out, " >").ok();
}
