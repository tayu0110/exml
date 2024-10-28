#[cfg(all(feature = "libxml_c14n", feature = "output"))]
pub mod c14n;
#[cfg(feature = "catalog")]
pub mod catalog;
pub mod chvalid;
#[cfg(feature = "libxml_debug")]
pub mod debug_xml;
pub mod dict;
pub mod entities;
pub mod globals;
pub mod hash;
#[cfg(feature = "html")]
pub mod htmlparser;
#[cfg(feature = "html")]
pub mod htmltree;
pub mod list;
#[cfg(feature = "ftp")]
pub mod nanoftp;
#[cfg(feature = "http")]
pub mod nanohttp;
pub mod parser;
pub mod parser_internals;
#[cfg(feature = "libxml_pattern")]
pub mod pattern;
#[cfg(feature = "schema")]
pub mod relaxng;
#[cfg(feature = "legacy")]
pub mod sax;
pub mod sax2;
#[cfg(feature = "schema")]
pub mod schemas_internals;
#[cfg(feature = "libxml_schematron")]
pub mod schematron;
pub mod threads;
pub mod tree;
pub mod uri;
pub mod valid;
#[cfg(feature = "xinclude")]
pub mod xinclude;
#[cfg(feature = "libxml_xptr")]
pub mod xlink;
#[cfg(any(feature = "regexp", feature = "libxml_automata"))]
pub mod xmlautomata;
pub mod xmlerror;
pub mod xmlmemory;
#[cfg(feature = "libxml_modules")]
pub mod xmlmodule;
pub mod xmlreader;
#[cfg(feature = "regexp")]
pub mod xmlregexp;
#[cfg(feature = "output")]
pub mod xmlsave;
#[cfg(feature = "schema")]
pub mod xmlschemas;
#[cfg(feature = "schema")]
pub mod xmlschemastypes;
pub mod xmlstring;
#[cfg(feature = "libxml_unicode")]
pub mod xmlunicode;
pub mod xmlversion;
#[cfg(feature = "writer")]
pub mod xmlwriter;
pub mod xpath;
#[cfg(feature = "xpath")]
pub mod xpath_internals;
#[cfg(feature = "libxml_xptr")]
pub mod xpointer;
