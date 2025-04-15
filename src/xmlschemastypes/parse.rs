/// Parse an u64 into 3 fields.  
/// Each fields has at most 8 digits integer.
///
/// If successfully parsed, return `Some((lo, mid, hi))`.
/// Otherwise, return `None`.
#[doc(alias = "xmlSchemaParseUInt")]
fn parse_uint(s: &mut &str) -> Option<(u64, u64, u64)> {
    if !s.starts_with(|c: char| c.is_ascii_digit()) {
        return None;
    }
    // ignore leading zeroes
    let cur = s.trim_start_matches('0');
    let len = cur
        .bytes()
        .position(|b| !b.is_ascii_digit())
        .unwrap_or(cur.len());
    let (dig, rem) = cur.split_at(len);
    *s = rem;
    if len > 24 {
        return None;
    }

    // [lo, mid, hi]
    let mut res = [0; 3];
    for (res, chunk) in res.iter_mut().zip(dig.as_bytes().rchunks(8)) {
        *res = chunk
            .iter()
            .copied()
            .fold(0u64, |s, v| s * 10 + (v - b'0') as u64);
    }

    Some(res.into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_uint_test() {
        let mut s = "12345678";
        assert_eq!(parse_uint(&mut s), Some((12345678, 0, 0)));
        assert_eq!(s, "");

        let mut s = "1234567890123456";
        assert_eq!(parse_uint(&mut s), Some((90123456, 12345678, 0)));
        assert_eq!(s, "");

        let mut s = "123456789012345678901234";
        assert_eq!(parse_uint(&mut s), Some((78901234, 90123456, 12345678)));
        assert_eq!(s, "");

        let mut s = "00000123456";
        assert_eq!(parse_uint(&mut s), Some((123456, 0, 0)));
        assert_eq!(s, "");

        let mut s = "12345abc12345";
        assert_eq!(parse_uint(&mut s), Some((12345, 0, 0)));
        assert_eq!(s, "abc12345");

        let mut s = "1234567890123456789012345";
        assert_eq!(parse_uint(&mut s), None);
        assert_eq!(s, "");

        let mut s = "abcd";
        assert_eq!(parse_uint(&mut s), None);
        assert_eq!(s, "abcd");
    }
}
