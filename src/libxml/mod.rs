#[cfg(feature = "catalog")]
pub mod catalog;
pub mod chvalid;
pub mod dict;
pub mod globals;
pub mod hash;
#[cfg(feature = "schema")]
pub mod relaxng;
pub mod sax2;
#[cfg(feature = "schema")]
pub mod schemas_internals;
pub mod threads;
pub mod valid;
#[cfg(feature = "xpointer")]
pub mod xlink;
#[cfg(any(feature = "libxml_regexp", feature = "libxml_automata"))]
pub mod xmlautomata;
pub mod xmlmemory;
pub mod xmlreader;
#[cfg(feature = "libxml_regexp")]
pub mod xmlregexp;
#[cfg(feature = "schema")]
pub mod xmlschemas;
#[cfg(feature = "schema")]
pub mod xmlschemastypes;
pub mod xmlstring;
#[cfg(feature = "libxml_unicode")]
pub mod xmlunicode;
#[cfg(feature = "xpointer")]
pub mod xpointer;
