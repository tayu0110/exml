#![allow(clippy::missing_safety_doc)]
#![allow(clippy::blocks_in_conditions)]
#![allow(clippy::needless_range_loop)]
#![allow(unused_assignments)]
#![allow(deprecated)]
#![allow(unused)]
#![warn(unused_assignments)]
#![warn(unused_mut)]
#![warn(unused_imports)]
#![warn(unused_labels)]
#![warn(unused_parens)]
#![warn(unused_variables)]
#![warn(unused_unsafe)]

#[cfg(all(feature = "c14n", feature = "libxml_output"))]
pub mod c14n;
mod chvalid;
#[cfg(feature = "libxml_debug")]
pub mod debug_xml;
pub mod dict;
pub mod encoding;
pub mod error;
pub mod globals;
pub mod hash;
#[cfg(feature = "html")]
pub mod html;
pub mod io;
pub mod libxml;
pub mod list;
#[cfg(feature = "http")]
pub mod nanohttp;
pub mod parser;
#[cfg(feature = "libxml_pattern")]
pub mod pattern;
#[cfg(feature = "schema")]
pub mod relaxng;
#[cfg(feature = "libxml_output")]
pub mod save;
#[cfg(feature = "schematron")]
pub mod schematron;
#[cfg(test)]
pub(crate) mod test_util;
pub mod tree;
pub mod uri;
pub mod valid;
#[cfg(feature = "libxml_writer")]
pub mod writer;
#[cfg(feature = "xinclude")]
pub mod xinclude;
#[cfg(feature = "schema")]
pub mod xmlschemas;
#[cfg(feature = "schema")]
pub mod xmlschemastypes;
pub mod xpath;
#[cfg(feature = "xpointer")]
pub mod xpointer;

pub const SYSCONFDIR: &str = if let Some(sysconfdir) = option_env!("SYSCONFDIR") {
    sysconfdir
} else {
    "/etc"
};
