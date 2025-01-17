#[cfg(all(feature = "c14n", feature = "libxml_output"))]
pub mod c14n;
#[cfg(feature = "catalog")]
pub mod catalog;
pub mod chvalid;
#[cfg(feature = "libxml_debug")]
pub mod debug_xml;
pub mod dict;
pub mod globals;
pub mod hash;
#[cfg(feature = "html")]
pub mod htmlparser;
#[cfg(feature = "html")]
pub mod htmltree;
#[cfg(feature = "ftp")]
pub mod nanoftp;
pub mod parser;
pub mod parser_internals;
#[cfg(feature = "libxml_pattern")]
pub mod pattern;
#[cfg(feature = "schema")]
pub mod relaxng;
pub mod sax2;
#[cfg(feature = "schema")]
pub mod schemas_internals;
#[cfg(feature = "schematron")]
pub mod schematron;
pub mod threads;
pub mod uri;
pub mod valid;
#[cfg(feature = "xinclude")]
pub mod xinclude;
#[cfg(feature = "xpointer")]
pub mod xlink;
#[cfg(any(feature = "libxml_regexp", feature = "libxml_automata"))]
pub mod xmlautomata;
pub mod xmlmemory;
#[cfg(feature = "libxml_modules")]
pub mod xmlmodule;
pub mod xmlreader;
#[cfg(feature = "libxml_regexp")]
pub mod xmlregexp;
#[cfg(feature = "libxml_output")]
pub mod xmlsave;
#[cfg(feature = "schema")]
pub mod xmlschemas;
#[cfg(feature = "schema")]
pub mod xmlschemastypes;
pub mod xmlstring;
#[cfg(feature = "libxml_unicode")]
pub mod xmlunicode;
#[cfg(feature = "libxml_writer")]
pub mod xmlwriter;
#[cfg(feature = "xpointer")]
pub mod xpointer;
