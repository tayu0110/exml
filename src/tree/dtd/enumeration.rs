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

/// List structure used when there is an enumeration in DTDs.
// pub type XmlEnumerationPtr = *mut XmlEnumeration;
#[repr(C)]
#[derive(Debug, Default)]
pub struct XmlEnumeration {
    pub(crate) next: Option<Box<XmlEnumeration>>, /* next one */
    pub(crate) name: Option<String>,              /* Enumeration name */
}

impl Clone for XmlEnumeration {
    #[doc(alias = "xmlCopyEnumeration")]
    fn clone(&self) -> Self {
        Self {
            next: self.next.as_deref().map(|next| Box::new(next.clone())),
            name: self.name.clone(),
        }
    }
}

/// create and initialize an enumeration attribute node.
///
/// Returns the xmlEnumerationPtr just created or null_mut() in case of error.
#[doc(alias = "xmlCreateEnumeration")]
pub fn xml_create_enumeration(name: Option<&str>) -> Box<XmlEnumeration> {
    Box::new(XmlEnumeration {
        name: name.map(|n| n.to_owned()),
        next: None,
    })
}

/// This will dump the content of the enumeration
///
/// # Note
/// '(' is not printed.
#[doc(alias = "xmlDumpEnumeration")]
#[cfg(feature = "libxml_output")]
pub(crate) fn xml_dump_enumeration<'a>(out: &mut (impl Write + 'a), cur: &XmlEnumeration) {
    write!(out, "{}", cur.name.as_deref().unwrap()).ok();
    if let Some(next) = cur.next.as_deref() {
        write!(out, " | ").ok();
        xml_dump_enumeration(out, next);
    } else {
        write!(out, ")").ok();
    }
}
