//! Version informations.
//! This module is based on `libxml/xmlversion.h` in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::ffi::CStr;

/* *
 * LIBXML_DOTTED_VERSION:
 *
 * the version string like "1.2.3"
 */
const LIBXML_DOTTED_VERSION: &str = env!("CARGO_PKG_VERSION");

/* *
 * LIBXML_VERSION:
 *
 * the version number: 1.2.3 value is 10203
 */
const LIBXML_VERSION: i32 = 21108;

/* *
 * LIBXML_VERSION_STRING:
 *
 * the version number string, 1.2.3 value is "10203"
 */
pub(crate) const LIBXML_VERSION_STRING: &CStr = c"21108";

/* *
 * LIBXML_VERSION_EXTRA:
 *
 * extra version information, used to show a git commit description
 */
const LIBXML_VERSION_EXTRA: &str = "";
