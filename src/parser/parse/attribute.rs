use std::borrow::Cow;

/// Normalize the space in non CDATA attribute values:
/// If the attribute type is not CDATA, then the XML processor MUST further
/// process the normalized attribute value by discarding any leading and
/// trailing space (#x20) characters, and by replacing sequences of space
/// (#x20) characters by a single space (#x20) character.
/// Note that the size of dst need to be at least src, and if one doesn't need
/// to preserve dst (and it doesn't come from a dictionary or read-only) then
/// passing src as dst is just fine.
///
/// If some conversion occurs, return the original string wrapped `Cow::Borrowed`.  
/// Otherwise, return the modified string wrapped `Cow::Owned`.
#[doc(alias = "xmlAttrNormalizeSpace")]
pub(crate) fn attr_normalize_space(mut src: &str) -> Cow<'_, str> {
    src = src.trim_start_matches(' ');
    if !src.contains("  ") {
        return Cow::Borrowed(src);
    }
    let mut dst = String::with_capacity(src.len());
    let mut src = src.chars().peekable();
    while let Some(b) = src.next() {
        if b == ' ' {
            // reduce single spaces
            while src.next_if(|&b| b == ' ').is_some() {}
            dst.push(' ');
        } else {
            dst.push(b);
        }
    }
    Cow::Owned(dst)
}

// /// Normalize the space in non CDATA attribute values, a slightly more complex
// /// front end to avoid allocation problems when running on attribute values
// /// coming from the input.
// ///
// /// If some conversion occurs, return the original string wrapped `Cow::Borrowed`.
// /// Otherwise, return the modified string wrapped `Cow::Owned`.
// #[doc(alias = "xmlAttrNormalizeSpace2")]
// unsafe fn attr_normalize_space2<'a>(ctxt: &mut XmlParserCtxt, src: &'a str) -> Cow<'a, str> {
//     let mut remove_head: i32 = 0;
//     let mut need_realloc: i32 = 0;
//     let mut cur: *const XmlChar;

//     cur = src;
//     while *cur == 0x20 {
//         cur = cur.add(1);
//         remove_head += 1;
//     }
//     while *cur != 0 {
//         if *cur == 0x20 {
//             cur = cur.add(1);
//             if *cur == 0x20 || *cur == 0 {
//                 need_realloc = 1;
//                 break;
//             }
//         } else {
//             cur = cur.add(1);
//         }
//     }
//     if need_realloc != 0 {
//         let ret: *mut XmlChar = xml_strndup(src.add(remove_head as usize), i - remove_head + 1);
//         if ret.is_null() {
//             xml_err_memory(ctxt, None);
//             return null_mut();
//         }
//         xml_attr_normalize_space(ret, ret);
//         *len = strlen(ret as _) as _;
//         return ret;
//     } else if remove_head != 0 {
//         *len -= remove_head;
//         memmove(
//             src as _,
//             src.add(remove_head as usize) as _,
//             1 + *len as usize,
//         );
//         return src;
//     }
//     null_mut()
// }
