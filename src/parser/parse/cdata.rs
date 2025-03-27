/// Check that the block of characters is okay as SCdata content [20]
///
/// Returns the number of bytes to pass if okay, a negative index where an
/// UTF-8 error occurred otherwise
#[doc(alias = "xmlCheckCdataPush")]
#[cfg(feature = "libxml_push")]
pub(crate) fn check_cdata_push(utf: &[u8], complete: bool) -> Result<usize, usize> {
    use std::str::{from_utf8, from_utf8_unchecked};

    use crate::libxml::chvalid::xml_is_char;

    if utf.is_empty() {
        return Ok(0);
    }

    let s = match from_utf8(utf) {
        Ok(s) => s,
        Err(e) => {
            let s = unsafe {
                // # Safety
                // Refer to the document of `from_utf8` and `Utf8Error`.
                from_utf8_unchecked(&utf[..e.valid_up_to()])
            };
            // If `complete` is `true`, it is invalid not to reach the end.
            // If `e.error_len().is_some()` is `true`,
            // it is still invalid because it contains an invalid byte sequence.
            if complete || e.error_len().is_some() {
                return Err(s.find(|c: char| !xml_is_char(c as u32)).unwrap_or(s.len()));
            }
            s
        }
    };

    // Even a valid UTF-8 sequence may contain characters
    // that do not conform to the XML specification.
    s.find(|c: char| !xml_is_char(c as u32))
        .map_or(Ok(s.len()), Err)
}
