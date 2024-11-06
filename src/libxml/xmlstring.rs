//! Provide methods and data structures for processing strings.  
//! This module is based on `libxml/xmlstring.h`, `xmlstring.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::ffi::c_char;
use std::ptr::{null, null_mut};

use libc::{memcpy, strlen, INT_MAX};

use super::globals::{xml_free, xml_malloc, xml_malloc_atomic, xml_realloc};

/**
 * XmlChar:
 *
 * This is a basic byte in an UTF-8 encoded string.
 * It's unsigned allowing to pinpoint case where char * are assigned
 * to XmlChar * (possibly making serialization back impossible).
 */
pub type XmlChar = u8;

/************************************************************************
 *                                                                      *
 *                Commodity functions to handle xmlChars                *
 *                                                                      *
 ************************************************************************/

/**
 * xmlStrndup:
 * @cur:  the input XmlChar *
 * @len:  the len of @cur
 *
 * a strndup for array of XmlChar's
 *
 * Returns a new XmlChar * or NULL
 */
pub unsafe extern "C" fn xml_strndup(cur: *const XmlChar, len: i32) -> *mut XmlChar {
    if cur.is_null() || len < 0 {
        return null_mut();
    };
    let ret: *mut XmlChar = xml_malloc_atomic(len as usize + 1) as *mut XmlChar;
    if ret.is_null() {
        return null_mut();
    }
    memcpy(ret as _, cur as _, len as usize);
    *ret.add(len as usize) = 0;
    ret
}

/**
 * xmlStrdup:
 * @cur:  the input XmlChar *
 *
 * a strdup for array of XmlChar's. Since they are supposed to be
 * encoded in UTF-8 or an encoding with 8bit based chars, we assume
 * a termination mark of '0'.
 *
 * Returns a new XmlChar * or NULL
 */
pub unsafe extern "C" fn xml_strdup(cur: *const XmlChar) -> *mut XmlChar {
    let mut p = cur;

    if cur.is_null() {
        return null_mut();
    };
    while *p != 0 {
        p = p.add(1)
    } /* non input consuming */
    xml_strndup(cur, p.offset_from(cur).abs() as i32)
}

/**
 * xmlCharStrndup:
 * @cur:  the input char *
 * @len:  the len of @cur
 *
 * a strndup for char's to XmlChar's
 *
 * Returns a new XmlChar * or NULL
 */
pub unsafe extern "C" fn xml_char_strndup(cur: *const c_char, len: i32) -> *mut XmlChar {
    if cur.is_null() || len < 0 {
        return null_mut();
    };
    let ret: *mut XmlChar = xml_malloc_atomic(len as usize + 1) as *mut XmlChar;
    if ret.is_null() {
        return null_mut();
    }
    for i in 0..len as usize {
        /* Explicit sign change */
        *ret.add(i) = *cur.add(i) as XmlChar;
        if *ret.add(i) == 0 {
            return ret;
        };
    }
    *ret.add(len as usize) = 0;
    ret
}

/**
 * xmlCharStrdup:
 * @cur:  the input char *
 *
 * a strdup for char's to XmlChar's
 *
 * Returns a new XmlChar * or NULL
 */
pub unsafe extern "C" fn xml_char_strdup(cur: *const c_char) -> *mut XmlChar {
    let mut p: *const c_char = cur;

    if cur.is_null() {
        return null_mut();
    };
    while *p != b'\0' as c_char {
        p = p.add(1);
    } /* non input consuming */
    xml_char_strndup(cur, p.offset_from(cur).abs() as i32)
}

/**
 * xmlStrcmp:
 * @str1:  the first XmlChar *
 * @str2:  the second XmlChar *
 *
 * a strcmp for XmlChar's
 *
 * Returns the integer result of the comparison
 */
pub unsafe extern "C" fn xml_strcmp(mut str1: *const XmlChar, mut str2: *const XmlChar) -> i32 {
    if str1 == str2 {
        return 0;
    }
    if str1.is_null() {
        return -1;
    }
    if str2.is_null() {
        return 1;
    }
    // if cfg!(fuzzing) {
    //     strcmp(str1 as *const c_char, str2 as *const c_char)
    // } else {
    while {
        let tmp = *str1 as i32 - *str2 as i32;
        str1 = str1.add(1);
        if tmp != 0 {
            return tmp;
        };
        let f = *str2 != 0;
        str2 = str2.add(1);
        f
    } {}
    0
    // }
}

/// Check string `str1` is equal to `str2`.  
/// Return `true` if they are equal, otherwise return `false`.
///
/// Please refer to the document of `xmlStrEqual` for original libxml2 also.
pub unsafe fn xml_str_equal(mut str1: *const XmlChar, mut str2: *const XmlChar) -> bool {
    if str1 == str2 {
        return true;
    };
    if str1.is_null() || str2.is_null() {
        return false;
    };
    while {
        let s = *str1;
        str1 = str1.add(1);
        if s != *str2 {
            return false;
        };
        let f = *str2 != 0;
        str2 = str2.add(1);
        f
    } {}
    true
}

/// Check given QName that its prefix is `pref` and localname is `name` is equal to a string `str`.  
/// Return `true` if they are equal, otherwise return `false`.
///
/// Please refer to the document of `xmlStrQEqual` for original libxml2 also.
pub unsafe fn xml_str_qequal(
    mut pref: *const XmlChar,
    mut name: *const XmlChar,
    mut str: *const XmlChar,
) -> bool {
    if pref.is_null() {
        return xml_str_equal(name, str);
    }
    if name.is_null() {
        return false;
    }
    if str.is_null() {
        return false;
    }

    while {
        let p = *pref;
        pref = pref.add(1);
        if p != *str {
            return false;
        };
        let s = *str;
        str = str.add(1);
        s != 0 && *pref != 0
    } {}
    let s = *str;
    str = str.add(1);
    if s != b':' {
        return false;
    };
    while {
        let n = *name;
        name = name.add(1);
        if n != *str {
            return false;
        };
        let s = *str;
        str = str.add(1);
        s != 0
    } {}
    true
}

/**
 * xmlStrncmp:
 * @str1:  the first XmlChar *
 * @str2:  the second XmlChar *
 * @len:  the max comparison length
 *
 * a strncmp for XmlChar's
 *
 * Returns the integer result of the comparison
 */
pub unsafe extern "C" fn xml_strncmp(
    mut str1: *const XmlChar,
    mut str2: *const XmlChar,
    mut len: i32,
) -> i32 {
    if len <= 0 {
        return 0;
    }
    if str1 == str2 {
        return 0;
    }
    if str1.is_null() {
        return -1;
    }
    if str2.is_null() {
        return 1;
    }

    while {
        let tmp: i32 = *str1 as i32 - *str2 as i32;
        str1 = str1.add(1);
        if tmp != 0 {
            return tmp as _;
        }
        len -= 1;
        if len == 0 {
            return tmp as _;
        };
        let s = *str2;
        str2 = str2.add(1);
        s != 0
    } {}
    0
}

const CASEMAP: [XmlChar; 256] = [
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
    0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F,
    0x40, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
    0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x7B, 0x5C, 0x5D, 0x5E, 0x5F,
    0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
    0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F,
    0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF,
    0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF,
    0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF,
    0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF,
    0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF,
    0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF,
];

/**
 * xmlStrcasecmp:
 * @str1:  the first XmlChar *
 * @str2:  the second XmlChar *
 *
 * a strcasecmp for XmlChar's
 *
 * Returns the integer result of the comparison
 */
pub unsafe extern "C" fn xml_strcasecmp(mut str1: *const XmlChar, mut str2: *const XmlChar) -> i32 {
    let mut tmp: i32;

    if str1 == str2 {
        return 0;
    }
    if str1.is_null() {
        return -1;
    }
    if str2.is_null() {
        return 1;
    }
    while {
        tmp = CASEMAP[*str1 as usize] as i32 - CASEMAP[*str2 as usize] as i32;
        str1 = str1.add(1);
        if tmp != 0 {
            return tmp;
        };
        let s = *str2;
        str2 = str2.add(1);
        s != 0
    } {}
    0
}

/**
 * xmlStrncasecmp:
 * @str1:  the first XmlChar *
 * @str2:  the second XmlChar *
 * @len:  the max comparison length
 *
 * a strncasecmp for XmlChar's
 *
 * Returns the integer result of the comparison
 */
pub unsafe extern "C" fn xml_strncasecmp(
    mut str1: *const XmlChar,
    mut str2: *const XmlChar,
    mut len: i32,
) -> i32 {
    let mut tmp: i32;

    if len <= 0 {
        return 0;
    }
    if str1 == str2 {
        return 0;
    }
    if str1.is_null() {
        return -1;
    }
    if str2.is_null() {
        return 1;
    }
    while {
        tmp = CASEMAP[*str1 as usize] as i32 - CASEMAP[*str2 as usize] as i32;
        str1 = str1.add(1);
        if tmp != 0 {
            return tmp;
        };
        len -= 1;
        if len == 0 {
            return tmp;
        };
        let s = *str2;
        str2 = str2.add(1);
        s != 0
    } {}
    0
}

/**
 * xmlStrchr:
 * @str:  the XmlChar * array
 * @val:  the XmlChar to search
 *
 * a strchr for XmlChar's
 *
 * Returns the XmlChar * for the first occurrence or NULL.
 */
pub unsafe extern "C" fn xml_strchr(mut str: *const XmlChar, val: XmlChar) -> *const XmlChar {
    if str.is_null() {
        return null();
    };
    while *str != 0 {
        /* non input consuming */
        if *str == val {
            return str as *const XmlChar;
        }
        str = str.add(1);
    }
    null()
}

/**
 * xmlStrstr:
 * @str:  the XmlChar * array (haystack)
 * @val:  the XmlChar to search (needle)
 *
 * a strstr for XmlChar's
 *
 * Returns the XmlChar * for the first occurrence or NULL.
 */
pub unsafe extern "C" fn xml_strstr(
    mut str: *const XmlChar,
    val: *const XmlChar,
) -> *const XmlChar {
    if str.is_null() {
        return null();
    }
    if val.is_null() {
        return null();
    }
    let n: i32 = xml_strlen(val);

    if n == 0 {
        return str;
    }
    while *str != 0 {
        /* non input consuming */
        if *str == *val && xml_strncmp(str, val, n) == 0 {
            return str as *const XmlChar;
        }
        str = str.add(1);
    }
    null()
}

/**
 * xmlStrcasestr:
 * @str:  the XmlChar * array (haystack)
 * @val:  the XmlChar to search (needle)
 *
 * a case-ignoring strstr for XmlChar's
 *
 * Returns the XmlChar * for the first occurrence or NULL.
 */
pub unsafe extern "C" fn xml_strcasestr(
    mut str: *const XmlChar,
    val: *const XmlChar,
) -> *const XmlChar {
    if str.is_null() {
        return null();
    }
    if val.is_null() {
        return null();
    }
    let n: i32 = xml_strlen(val);

    if n == 0 {
        return str;
    }
    while *str != 0 {
        /* non input consuming */
        if CASEMAP[*str as usize] == CASEMAP[*val as usize] && xml_strncasecmp(str, val, n) == 0 {
            return str;
        }
        str = str.add(1);
    }
    null()
}

/**
 * xmlStrsub:
 * @str:  the XmlChar * array (haystack)
 * @start:  the index of the first char (zero based)
 * @len:  the length of the substring
 *
 * Extract a substring of a given string
 *
 * Returns the XmlChar * for the first occurrence or NULL.
 */
pub unsafe extern "C" fn xml_strsub(mut str: *const XmlChar, start: i32, len: i32) -> *mut XmlChar {
    if str.is_null() {
        return null_mut();
    }
    if start < 0 {
        return null_mut();
    }
    if len < 0 {
        return null_mut();
    }

    for _ in 0..start {
        if *str == 0 {
            return null_mut();
        }
        str = str.add(1);
    }
    if *str == 0 {
        return null_mut();
    }
    xml_strndup(str, len)
}

/**
 * xmlStrlen:
 * @str:  the XmlChar * array
 *
 * length of a XmlChar's string
 *
 * Returns the number of XmlChar contained in the ARRAY.
 */
pub unsafe extern "C" fn xml_strlen(str: *const XmlChar) -> i32 {
    let len = if !str.is_null() {
        strlen(str as *const c_char)
    } else {
        0
    };

    if len > INT_MAX as usize {
        0
    } else {
        len as i32
    }
}

/**
 * xmlStrncat:
 * @cur:  the original XmlChar * array
 * @add:  the XmlChar * array added
 * @len:  the length of @add
 *
 * a strncat for array of XmlChar's, it will extend @cur with the len
 * first bytes of @add. Note that if @len < 0 then this is an API error
 * and NULL will be returned.
 *
 * Returns a new XmlChar *, the original @cur is reallocated and should
 * not be freed.
 */
pub unsafe extern "C" fn xml_strncat(
    cur: *mut XmlChar,
    add: *const XmlChar,
    len: i32,
) -> *mut XmlChar {
    if add.is_null() || len == 0 {
        return cur;
    }
    if len < 0 {
        return null_mut();
    }
    if cur.is_null() {
        return xml_strndup(add, len);
    }

    let size: i32 = xml_strlen(cur);
    if size < 0 || size > INT_MAX - len {
        return null_mut();
    }
    let ret: *mut XmlChar = xml_realloc(cur as _, size as usize + len as usize + 1) as *mut XmlChar;
    if ret.is_null() {
        return cur;
    }
    memcpy(ret.add(size as usize) as _, add as _, len as usize);
    *ret.add(size as usize + len as usize) = 0;
    ret
}

/**
 * xmlStrncatNew:
 * @str1:  first XmlChar string
 * @str2:  second XmlChar string
 * @len:  the len of @str2 or < 0
 *
 * same as xmlStrncat, but creates a new string.  The original
 * two strings are not freed. If @len is < 0 then the length
 * will be calculated automatically.
 *
 * Returns a new XmlChar * or NULL
 */
pub unsafe extern "C" fn xml_strncat_new(
    str1: *const XmlChar,
    str2: *const XmlChar,
    mut len: i32,
) -> *mut XmlChar {
    if len < 0 {
        len = xml_strlen(str2);
        if len < 0 {
            return null_mut();
        }
    }
    if str2.is_null() || len == 0 {
        return xml_strdup(str1);
    }
    if str1.is_null() {
        return xml_strndup(str2, len);
    }

    let size: i32 = xml_strlen(str1);
    if size < 0 || size > INT_MAX - len {
        return null_mut();
    }
    let ret: *mut XmlChar = xml_malloc(size as usize + len as usize + 1) as *mut XmlChar;
    if ret.is_null() {
        return xml_strndup(str1, size);
    }
    memcpy(ret as _, str1 as _, size as _);
    memcpy(ret.add(size as _) as _, str2 as _, len as _);
    *ret.add(size as usize + len as usize) = 0;
    ret
}

/**
 * xmlStrcat:
 * @cur:  the original XmlChar * array
 * @add:  the XmlChar * array added
 *
 * a strcat for array of XmlChar's. Since they are supposed to be
 * encoded in UTF-8 or an encoding with 8bit based chars, we assume
 * a termination mark of '0'.
 *
 * Returns a new XmlChar * containing the concatenated string. The original
 * @cur is reallocated and should not be freed.
 */
pub unsafe extern "C" fn xml_strcat(cur: *mut XmlChar, add: *const XmlChar) -> *mut XmlChar {
    let mut p: *const XmlChar = add;

    if add.is_null() {
        return cur;
    }
    if cur.is_null() {
        return xml_strdup(add);
    }

    while *p != 0 {
        p = p.add(1); /* non input consuming */
    }
    xml_strncat(cur, add, p.offset_from(add).abs() as _)
}

/**
 * xmlStrPrintf:
 * @buf:   the result buffer.
 * @len:   the result buffer length.
 * @msg:   the message with printf formatting.
 * @...:   extra parameters for the message.
 *
 * Formats @msg and places result into @buf.
 *
 * Returns the number of characters written to @buf or -1 if an error occurs.
 */
// pub fn xmlStrPrintf             (XmlChar *buf, int len, const char *msg, ...) LIBXML_ATTR_FORMAT(3,4) -> i32;
// pub unsafe extern "C" fn xmlStrPrintf(buf: *mut XmlChar, len: i32, msg: *const c_char, ...) -> i32 {
//     let args: va_list;
//     let ret: i32;

//      if((buf == NULL) || (msg == NULL)) {
//          return(-1);
//      }

//      va_start(args, msg);
//      ret = vsnprintf((char *) buf, len, (const char *) msg, args);
//      va_end(args);
//      buf[len - 1] = 0; /* be safe ! */
//      return(ret);
//  }
#[macro_export]
macro_rules! xml_str_printf {
    ( $buf:expr, $len:expr, $msg:expr, $( $args:expr ),* ) => {
        (|buf: *mut XmlChar, len: i32, msg: *const c_char| -> i32 {
            if buf.is_null() || msg.is_null() {
                return -1;
            }

            let res = libc::snprintf(buf as *mut c_char, len as usize, msg as *const c_char, $( $args ),*);
            *buf.add(len as usize - 1) = 0;
            res
        })($buf, $len, $msg)
    };
}

/**
 * xmlStrVPrintf:
 * @buf:   the result buffer.
 * @len:   the result buffer length.
 * @msg:   the message with printf formatting.
 * @ap:    extra parameters for the message.
 *
 * Formats @msg and places result into @buf.
 *
 * Returns the number of characters written to @buf or -1 if an error occurs.
 */
// pub fn xmlStrVPrintf                (XmlChar *buf, int len, const char *msg, va_list ap) LIBXML_ATTR_FORMAT(3,0) -> i32;
// pub unsafe extern "C" fn xmlStrVPrintf(buf: *mut XmlChar, len: i32, msg: *const char, ap: va_list) -> i32 {
//     let ret: i32;

//      if((buf == null()) || (msg == null())) {
//          return(-1);
//      }

//      ret = vsnprintf((char *) buf, len, (const char *) msg, ap);
//      buf[len - 1] = 0; /* be safe ! */

//      return(ret);
//  }

/************************************************************************
 *                                                                      *
 *              Generic UTF8 handling routines                          *
 *                                                                      *
 * From rfc2044: encoding of the Unicode values on UTF-8:               *
 *                                                                      *
 * UCS-4 range (hex.)           UTF-8 octet sequence (binary)           *
 * 0000 0000-0000 007F   0xxxxxxx                                       *
 * 0000 0080-0000 07FF   110xxxxx 10xxxxxx                              *
 * 0000 0800-0000 FFFF   1110xxxx 10xxxxxx 10xxxxxx                     *
 *                                                                      *
 * I hope we won't use values > 0xFFFF anytime soon !                   *
 *                                                                      *
 ************************************************************************/

/**
 * xmlUTF8Size:
 * @utf: pointer to the UTF8 character
 *
 * calculates the internal size of a UTF8 character
 *
 * returns the numbers of bytes in the character, -1 on format error
 */
pub unsafe extern "C" fn xml_utf8_size(utf: *const XmlChar) -> i32 {
    let mut len: i32;

    if utf.is_null() {
        return -1;
    }
    if *utf < 0x80 {
        return 1;
    }
    /* check valid UTF8 character */
    if *utf & 0x40 == 0 {
        return -1;
    }
    /* determine number of bytes in char */
    len = 2;
    let mut mask = 0x20;
    while mask != 0 {
        if *utf & mask == 0 {
            return len;
        }
        len += 1;
        mask >>= 1;
    }
    -1
}

/**
 * xmlUTF8Charcmp:
 * @utf1: pointer to first UTF8 char
 * @utf2: pointer to second UTF8 char
 *
 * compares the two UCS4 values
 *
 * returns result of the compare as with xmlStrncmp
 */
pub unsafe extern "C" fn xml_utf8_charcmp(utf1: *const XmlChar, utf2: *const XmlChar) -> i32 {
    if utf1.is_null() {
        if utf2.is_null() {
            return 0;
        }
        return -1;
    }
    xml_strncmp(utf1, utf2, xml_utf8_size(utf1))
}

/**
 * xmlUTF8Strlen:
 * @utf:  a sequence of UTF-8 encoded bytes
 *
 * compute the length of an UTF8 string, it doesn't do a full UTF8
 * checking of the content of the string.
 *
 * Returns the number of characters in the string or -1 in case of error
 */
pub unsafe extern "C" fn xml_utf8_strlen(mut utf: *const XmlChar) -> i32 {
    let mut ret: usize = 0;

    if utf.is_null() {
        return -1;
    }

    while *utf != 0 {
        if *utf.add(0) & 0x80 != 0 {
            if *utf.add(1) & 0xc0 != 0x80 {
                return -1;
            }
            if *utf.add(0) & 0xe0 == 0xe0 {
                if *utf.add(2) & 0xc0 != 0x80 {
                    return -1;
                }
                if *utf.add(0) & 0xf0 == 0xf0 {
                    if *utf.add(0) & 0xf8 != 0xf0 || *utf.add(3) & 0xc0 != 0x80 {
                        return -1;
                    }
                    utf = utf.add(4);
                } else {
                    utf = utf.add(3);
                }
            } else {
                utf = utf.add(2);
            }
        } else {
            utf = utf.add(1);
        }
        ret += 1;
    }

    if ret > INT_MAX as usize {
        0
    } else {
        ret as i32
    }
}

/**
 * xmlGetUTF8Char:
 * @utf:  a sequence of UTF-8 encoded bytes
 * @len:  a pointer to the minimum number of bytes present in
 *        the sequence.  This is used to assure the next character
 *        is completely contained within the sequence.
 *
 * Read the first UTF8 character from @utf
 *
 * Returns the char value or -1 in case of error, and sets *len to
 *        the actual number of bytes consumed (0 in case of error)
 */
pub unsafe extern "C" fn xml_get_utf8_char(utf: *const u8, len: *mut i32) -> i32 {
    let mut c: u32;

    'error: {
        if utf.is_null() {
            break 'error;
        }
        if len.is_null() {
            break 'error;
        }
        if *len < 1 {
            break 'error;
        }

        c = *utf.add(0) as _;
        if c & 0x80 != 0 {
            if *len < 2 {
                break 'error;
            }
            if *utf.add(1) & 0xc0 != 0x80 {
                break 'error;
            }
            if c & 0xe0 == 0xe0 {
                if *len < 3 {
                    break 'error;
                }
                if *utf.add(2) & 0xc0 != 0x80 {
                    break 'error;
                }
                if c & 0xf0 == 0xf0 {
                    if *len < 4 {
                        break 'error;
                    }
                    if c & 0xf8 != 0xf0 || *utf.add(3) & 0xc0 != 0x80 {
                        break 'error;
                    }
                    *len = 4;
                    /* 4-byte code */
                    c = (*utf.add(0) as u32 & 0x7) << 18;
                    c |= (*utf.add(1) as u32 & 0x3f) << 12;
                    c |= (*utf.add(2) as u32 & 0x3f) << 6;
                    c |= *utf.add(3) as u32 & 0x3f;
                } else {
                    /* 3-byte code */
                    *len = 3;
                    c = (*utf.add(0) as u32 & 0xf) << 12;
                    c |= (*utf.add(1) as u32 & 0x3f) << 6;
                    c |= *utf.add(2) as u32 & 0x3f;
                }
            } else {
                /* 2-byte code */
                *len = 2;
                c = (*utf.add(0) as u32 & 0x1f) << 6;
                c |= *utf.add(1) as u32 & 0x3f;
            }
        } else {
            /* 1-byte code */
            *len = 1;
        }
        return c as _;
    }

    //  error:
    if !len.is_null() {
        *len = 0;
    }
    -1
}

/**
 * xmlCheckUTF8:
 * @utf: Pointer to putative UTF-8 encoded string.
 *
 * Checks @utf for being valid UTF-8. @utf is assumed to be
 * null-terminated. This function is not super-strict, as it will
 * allow longer UTF-8 sequences than necessary. Note that Java is
 * capable of producing these sequences if provoked. Also note, this
 * routine checks for the 4-byte maximum size, but does not check for
 * 0x10ffff maximum value.
 *
 * Return value: true if @utf is valid.
 **/
pub unsafe extern "C" fn xml_check_utf8(mut utf: *const u8) -> i32 {
    let mut ix: i32;
    let mut c: u8;

    if utf.is_null() {
        return 0;
    }
    /*
     * utf is a string of 1, 2, 3 or 4 bytes.  The valid strings
     * are as follows (in "bit format"):
     *    0xxxxxxx                                      valid 1-byte
     *    110xxxxx 10xxxxxx                             valid 2-byte
     *    1110xxxx 10xxxxxx 10xxxxxx                    valid 3-byte
     *    11110xxx 10xxxxxx 10xxxxxx 10xxxxxx           valid 4-byte
     */
    while {
        c = *utf.add(0);
        c != 0
    } {
        /* string is 0-terminated */
        // ix = 0;
        if c & 0x80 == 0x00 {
            /* 1-byte code, starts with 10 */
            ix = 1;
        } else if c & 0xe0 == 0xc0 {
            /* 2-byte code, starts with 110 */
            if *utf.add(1) & 0xc0 != 0x80 {
                return 0;
            }
            ix = 2;
        } else if c & 0xf0 == 0xe0 {
            /* 3-byte code, starts with 1110 */
            if *utf.add(1) & 0xc0 != 0x80 || *utf.add(2) & 0xc0 != 0x80 {
                return 0;
            }
            ix = 3;
        } else if c & 0xf8 == 0xf0 {
            /* 4-byte code, starts with 11110 */
            if *utf.add(1) & 0xc0 != 0x80
                || *utf.add(2) & 0xc0 != 0x80
                || *utf.add(3) & 0xc0 != 0x80
            {
                return 0;
            }
            ix = 4;
        } else {
            /* unknown encoding */
            return 0;
        }
        utf = utf.add(ix as _);
    }
    1
}

/**
 * xmlUTF8Strsize:
 * @utf:  a sequence of UTF-8 encoded bytes
 * @len:  the number of characters in the array
 *
 * storage size of an UTF8 string
 * the behaviour is not guaranteed if the input string is not UTF-8
 *
 * Returns the storage size of
 * the first 'len' characters of ARRAY
 */
pub unsafe extern "C" fn xml_utf8_strsize(utf: *const XmlChar, mut len: i32) -> i32 {
    let mut ptr: *const XmlChar = utf;
    let mut ch: i32;

    if utf.is_null() {
        return 0;
    }

    if len <= 0 {
        return 0;
    }

    while {
        let f = len > 0;
        len -= 1;
        f
    } {
        if *ptr == 0 {
            break;
        }
        ch = *ptr as _;
        ptr = ptr.add(1);
        if ch & 0x80 != 0 {
            while {
                ch <<= 1;
                ch & 0x80 != 0
            } {
                if *ptr == 0 {
                    break;
                }
                ptr = ptr.add(1);
            }
        }
    }
    let ret: usize = ptr.offset_from(utf).unsigned_abs();
    if ret > INT_MAX as usize {
        0
    } else {
        ret as _
    }
}

/**
 * xmlUTF8Strndup:
 * @utf:  the input UTF8 *
 * @len:  the len of @utf (in chars)
 *
 * a strndup for array of UTF8's
 *
 * Returns a new UTF8 * or NULL
 */
pub unsafe extern "C" fn xml_utf8_strndup(utf: *const XmlChar, len: i32) -> *mut XmlChar {
    if utf.is_null() || len < 0 {
        return null_mut();
    }
    let i: i32 = xml_utf8_strsize(utf, len);
    let ret: *mut XmlChar = xml_malloc_atomic(i as usize + 1) as *mut XmlChar;
    if ret.is_null() {
        return null_mut();
    }
    memcpy(ret as _, utf as _, i as _);
    *ret.add(i as _) = 0;
    ret
}

/**
 * xmlUTF8Strpos:
 * @utf:  the input UTF8 *
 * @pos:  the position of the desired UTF8 char (in chars)
 *
 * a function to provide the equivalent of fetching a
 * character from a string array
 *
 * Returns a pointer to the UTF8 character or NULL
 */
pub unsafe extern "C" fn xml_utf8_strpos(mut utf: *const XmlChar, mut pos: i32) -> *const XmlChar {
    let mut ch: i32;

    if utf.is_null() {
        return null();
    }
    if pos < 0 {
        return null();
    }
    while {
        let p = pos;
        pos -= 1;
        p != 0
    } {
        ch = *utf as _;
        utf = utf.add(1);
        if ch == 0 {
            return null();
        }
        if ch & 0x80 != 0 {
            /* if not simple ascii, verify proper format */
            if ch & 0xc0 != 0xc0 {
                return null();
            }
            /* then skip over remaining bytes for this char */
            while {
                ch <<= 1;
                ch & 0x80 != 0
            } {
                let f = *utf & 0xc0 != 0x80;
                utf = utf.add(1);
                if f {
                    return null();
                }
            }
        }
    }
    utf
}

/**
 * xmlUTF8Strloc:
 * @utf:  the input UTF8 *
 * @utfchar:  the UTF8 character to be found
 *
 * a function to provide the relative location of a UTF8 char
 *
 * Returns the relative character position of the desired char
 * or -1 if not found
 */
pub unsafe extern "C" fn xml_utf8_strloc(mut utf: *const XmlChar, utfchar: *const XmlChar) -> i32 {
    let mut i: usize;

    let mut ch: i32;

    if utf.is_null() || utfchar.is_null() {
        return -1;
    }
    let size: i32 = xml_utf8_strsize(utfchar, 1);
    i = 0;
    while {
        ch = *utf as _;
        ch != 0
    } {
        if xml_strncmp(utf, utfchar, size) == 0 {
            return if i > INT_MAX as _ { 0 } else { i as _ };
        }
        utf = utf.add(1);
        if ch & 0x80 != 0 {
            /* if not simple ascii, verify proper format */
            if ch & 0xc0 != 0xc0 {
                return -1;
            }
            /* then skip over remaining bytes for this char */
            while {
                ch <<= 1;
                ch & 0x80 != 0
            } {
                let f = *utf & 0xc0 != 0x80;
                utf = utf.add(1);
                if f {
                    return -1;
                }
            }
        }
        i += 1;
    }

    -1
}
/**
 * xmlUTF8Strsub:
 * @utf:  a sequence of UTF-8 encoded bytes
 * @start: relative pos of first char
 * @len:   total number to copy
 *
 * Create a substring from a given UTF-8 string
 * Note:  positions are given in units of UTF-8 chars
 *
 * Returns a pointer to a newly created string
 * or NULL if any problem
 */
pub unsafe extern "C" fn xml_utf8_strsub(
    mut utf: *const XmlChar,
    start: i32,
    len: i32,
) -> *mut XmlChar {
    let mut ch: i32;

    if utf.is_null() {
        return null_mut();
    }
    if start < 0 {
        return null_mut();
    }
    if len < 0 {
        return null_mut();
    }

    /*
     * Skip over any leading chars
     */
    for _ in 0..start {
        ch = *utf as _;
        utf = utf.add(1);
        if ch == 0 {
            return null_mut();
        }
        if ch & 0x80 != 0 {
            /* if not simple ascii, verify proper format */
            if ch & 0xc0 != 0xc0 {
                return null_mut();
            }
            /* then skip over remaining bytes for this char */
            while {
                ch <<= 1;
                ch & 0x80 != 0
            } {
                let f = *utf & 0xc0 != 0x80;
                utf = utf.add(1);
                if f {
                    return null_mut();
                }
            }
        }
    }

    xml_utf8_strndup(utf, len)
}

/**
 * xmlEscapeFormatString:
 * @msg:  a pointer to the string in which to escape '%' characters.
 * Must be a heap-allocated buffer created by libxml2 that may be
 * returned, or that may be freed and replaced.
 *
 * Replaces the string pointed to by 'msg' with an escaped string.
 * Returns the same string with all '%' characters escaped.
 */
pub unsafe extern "C" fn xml_escape_format_string(msg: *mut *mut XmlChar) -> *mut XmlChar {
    let mut msg_ptr: *mut XmlChar;

    let mut result_ptr: *mut XmlChar;
    let mut count: usize = 0;
    let mut msg_len: usize = 0;

    if msg.is_null() || (*msg).is_null() {
        return null_mut();
    }

    msg_ptr = *msg;
    while *msg_ptr != b'\0' {
        msg_len += 1;
        if *msg_ptr == b'%' as _ {
            count += 1;
        }
        msg_ptr = msg_ptr.add(1);
    }

    if count == 0 {
        return *msg;
    }

    if count > INT_MAX as _ || msg_len > INT_MAX as usize - count {
        return null_mut();
    }
    let result_len: usize = msg_len + count + 1;
    let result: *mut XmlChar = xml_malloc_atomic(result_len) as *mut XmlChar;
    if result.is_null() {
        /* Clear *msg to prevent format string vulnerabilities in
        out-of-memory situations. */
        xml_free(*msg as _);
        *msg = null_mut();
        return null_mut();
    }

    msg_ptr = *msg;
    result_ptr = result;
    while *msg_ptr != b'\0' {
        *result_ptr = *msg_ptr;
        if *msg_ptr == b'%' {
            result_ptr = result_ptr.add(1);
            *result_ptr = b'%';
        }
        msg_ptr = msg_ptr.add(1);
        result_ptr = result_ptr.add(1);
    }
    *result.add(result_len - 1) = b'\0';

    xml_free(*msg as _);
    *msg = result;

    *msg
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_char_strdup() {
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_CONST_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let cur = gen_const_char_ptr(n_cur, 0);

                let ret_val = xml_char_strdup(cur);
                desret_xml_char_ptr(ret_val);
                des_const_char_ptr(n_cur, cur, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCharStrdup",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlCharStrdup()");
                    eprintln!(" {}", n_cur);
                }
            }
        }
    }

    #[test]
    fn test_xml_char_strndup() {
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_CONST_CHAR_PTR {
                for n_len in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_const_char_ptr(n_cur, 0);
                    let mut len = gen_int(n_len, 1);
                    if !cur.is_null() && len > xml_strlen(cur as _) {
                        len = 0;
                    }

                    let ret_val = xml_char_strndup(cur, len);
                    desret_xml_char_ptr(ret_val);
                    des_const_char_ptr(n_cur, cur, 0);
                    des_int(n_len, len, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlCharStrndup",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlCharStrndup()");
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_len);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_check_utf8() {
        unsafe {
            let mut leaks = 0;

            for n_utf in 0..GEN_NB_CONST_UNSIGNED_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let utf = gen_const_unsigned_char_ptr(n_utf, 0);

                let ret_val = xml_check_utf8(utf as *const u8);
                desret_int(ret_val);
                des_const_unsigned_char_ptr(n_utf, utf as *const u8, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCheckUTF8",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlCheckUTF8()");
                    eprintln!(" {}", n_utf);
                }
            }
        }
    }

    #[test]
    fn test_xml_get_utf8_char() {
        unsafe {
            let mut leaks = 0;

            for n_utf in 0..GEN_NB_CONST_UNSIGNED_CHAR_PTR {
                for n_len in 0..GEN_NB_INT_PTR {
                    let mem_base = xml_mem_blocks();
                    let utf = gen_const_unsigned_char_ptr(n_utf, 0);
                    let len = gen_int_ptr(n_len, 1);

                    let ret_val = xml_get_utf8_char(utf as *const u8, len);
                    desret_int(ret_val);
                    des_const_unsigned_char_ptr(n_utf, utf as *const u8, 0);
                    des_int_ptr(n_len, len, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlGetUTF8Char",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlGetUTF8Char()");
                        eprint!(" {}", n_utf);
                        eprintln!(" {}", n_len);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_str_equal() {
        unsafe {
            let mut leaks = 0;

            for n_str1 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_str2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let str1 = gen_const_xml_char_ptr(n_str1, 0);
                    let str2 = gen_const_xml_char_ptr(n_str2, 1);

                    let ret_val = xml_str_equal(str1 as *const XmlChar, str2) as i32;
                    desret_int(ret_val);
                    des_const_xml_char_ptr(n_str1, str1, 0);
                    des_const_xml_char_ptr(n_str2, str2, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlStrEqual",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlStrEqual()");
                        eprint!(" {}", n_str1);
                        eprintln!(" {}", n_str2);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_str_printf() {

        /* missing type support */
    }

    #[test]
    fn test_xml_str_qequal() {
        unsafe {
            let mut leaks = 0;

            for n_pref in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let pref = gen_const_xml_char_ptr(n_pref, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let str = gen_const_xml_char_ptr(n_str, 2);

                        let ret_val = xml_str_qequal(pref as *const XmlChar, name, str) as i32;
                        desret_int(ret_val);
                        des_const_xml_char_ptr(n_pref, pref, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_const_xml_char_ptr(n_str, str, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlStrQEqual",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlStrQEqual()");
                            eprint!(" {}", n_pref);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_str);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_str_vprintf() {

        /* missing type support */
    }

    #[test]
    fn test_xml_strcasecmp() {
        unsafe {
            let mut leaks = 0;

            for n_str1 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_str2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let str1 = gen_const_xml_char_ptr(n_str1, 0);
                    let str2 = gen_const_xml_char_ptr(n_str2, 1);

                    let ret_val = xml_strcasecmp(str1 as *const XmlChar, str2);
                    desret_int(ret_val);
                    des_const_xml_char_ptr(n_str1, str1, 0);
                    des_const_xml_char_ptr(n_str2, str2, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlStrcasecmp",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlStrcasecmp()");
                        eprint!(" {}", n_str1);
                        eprintln!(" {}", n_str2);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_strcasestr() {
        unsafe {
            let mut leaks = 0;

            for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_val in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let str = gen_const_xml_char_ptr(n_str, 0);
                    let val = gen_const_xml_char_ptr(n_val, 1);

                    let ret_val = xml_strcasestr(str as *const XmlChar, val);
                    desret_const_xml_char_ptr(ret_val);
                    des_const_xml_char_ptr(n_str, str, 0);
                    des_const_xml_char_ptr(n_val, val, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlStrcasestr",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlStrcasestr()");
                        eprint!(" {}", n_str);
                        eprintln!(" {}", n_val);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_strchr() {
        unsafe {
            let mut leaks = 0;

            for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_val in 0..GEN_NB_XML_CHAR {
                    let mem_base = xml_mem_blocks();
                    let str = gen_const_xml_char_ptr(n_str, 0);
                    let val = gen_xml_char(n_val, 1);

                    let ret_val = xml_strchr(str as *const XmlChar, val);
                    desret_const_xml_char_ptr(ret_val);
                    des_const_xml_char_ptr(n_str, str, 0);
                    des_xml_char(n_val, val, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlStrchr",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlStrchr()");
                        eprint!(" {}", n_str);
                        eprintln!(" {}", n_val);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_strcmp() {
        unsafe {
            let mut leaks = 0;

            for n_str1 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_str2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let str1 = gen_const_xml_char_ptr(n_str1, 0);
                    let str2 = gen_const_xml_char_ptr(n_str2, 1);

                    let ret_val = xml_strcmp(str1 as *const XmlChar, str2);
                    desret_int(ret_val);
                    des_const_xml_char_ptr(n_str1, str1, 0);
                    des_const_xml_char_ptr(n_str2, str2, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlStrcmp",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlStrcmp()");
                        eprint!(" {}", n_str1);
                        eprintln!(" {}", n_str2);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_strdup() {
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let cur = gen_const_xml_char_ptr(n_cur, 0);

                let ret_val = xml_strdup(cur as *const XmlChar);
                desret_xml_char_ptr(ret_val);
                des_const_xml_char_ptr(n_cur, cur, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlStrdup",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlStrdup()");
                    eprintln!(" {}", n_cur);
                }
            }
        }
    }

    #[test]
    fn test_xml_strlen() {
        unsafe {
            let mut leaks = 0;

            for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let str = gen_const_xml_char_ptr(n_str, 0);

                let ret_val = xml_strlen(str as *const XmlChar);
                desret_int(ret_val);
                des_const_xml_char_ptr(n_str, str, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlStrlen",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlStrlen()");
                    eprintln!(" {}", n_str);
                }
            }
        }
    }

    #[test]
    fn test_xml_strncasecmp() {
        unsafe {
            let mut leaks = 0;

            for n_str1 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_str2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let str1 = gen_const_xml_char_ptr(n_str1, 0);
                        let str2 = gen_const_xml_char_ptr(n_str2, 1);
                        let mut len = gen_int(n_len, 2);
                        if !str2.is_null() && len > xml_strlen(str2) {
                            len = 0;
                        }

                        let ret_val = xml_strncasecmp(str1 as *const XmlChar, str2, len);
                        desret_int(ret_val);
                        des_const_xml_char_ptr(n_str1, str1, 0);
                        des_const_xml_char_ptr(n_str2, str2, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlStrncasecmp",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlStrncasecmp()");
                            eprint!(" {}", n_str1);
                            eprint!(" {}", n_str2);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_strncat_new() {
        unsafe {
            let mut leaks = 0;

            for n_str1 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_str2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let str1 = gen_const_xml_char_ptr(n_str1, 0);
                        let str2 = gen_const_xml_char_ptr(n_str2, 1);
                        let mut len = gen_int(n_len, 2);
                        if !str2.is_null() && len > xml_strlen(str2) {
                            len = 0;
                        }

                        let ret_val = xml_strncat_new(str1 as *const XmlChar, str2, len);
                        desret_xml_char_ptr(ret_val);
                        des_const_xml_char_ptr(n_str1, str1, 0);
                        des_const_xml_char_ptr(n_str2, str2, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlStrncatNew",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlStrncatNew()");
                            eprint!(" {}", n_str1);
                            eprint!(" {}", n_str2);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_strncmp() {
        unsafe {
            let mut leaks = 0;

            for n_str1 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_str2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let str1 = gen_const_xml_char_ptr(n_str1, 0);
                        let str2 = gen_const_xml_char_ptr(n_str2, 1);
                        let mut len = gen_int(n_len, 2);
                        if !str2.is_null() && len > xml_strlen(str2) {
                            len = 0;
                        }

                        let ret_val = xml_strncmp(str1 as *const XmlChar, str2, len);
                        desret_int(ret_val);
                        des_const_xml_char_ptr(n_str1, str1, 0);
                        des_const_xml_char_ptr(n_str2, str2, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlStrncmp",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlStrncmp()");
                            eprint!(" {}", n_str1);
                            eprint!(" {}", n_str2);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_strndup() {
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_len in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_const_xml_char_ptr(n_cur, 0);
                    let mut len = gen_int(n_len, 1);
                    if !cur.is_null() && len > xml_strlen(cur) {
                        len = 0;
                    }

                    let ret_val = xml_strndup(cur as *const XmlChar, len);
                    desret_xml_char_ptr(ret_val);
                    des_const_xml_char_ptr(n_cur, cur, 0);
                    des_int(n_len, len, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlStrndup",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlStrndup()");
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_len);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_strstr() {
        unsafe {
            let mut leaks = 0;

            for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_val in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let str = gen_const_xml_char_ptr(n_str, 0);
                    let val = gen_const_xml_char_ptr(n_val, 1);

                    let ret_val = xml_strstr(str as *const XmlChar, val);
                    desret_const_xml_char_ptr(ret_val);
                    des_const_xml_char_ptr(n_str, str, 0);
                    des_const_xml_char_ptr(n_val, val, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlStrstr",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlStrstr()");
                        eprint!(" {}", n_str);
                        eprintln!(" {}", n_val);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_strsub() {
        unsafe {
            let mut leaks = 0;

            for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_start in 0..GEN_NB_INT {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let str = gen_const_xml_char_ptr(n_str, 0);
                        let mut start = gen_int(n_start, 1);
                        let mut len = gen_int(n_len, 2);
                        if !str.is_null() && start > xml_strlen(str) {
                            start = 0;
                        }
                        if !str.is_null() && len > xml_strlen(str) {
                            len = 0;
                        }

                        let ret_val = xml_strsub(str as *const XmlChar, start, len);
                        desret_xml_char_ptr(ret_val);
                        des_const_xml_char_ptr(n_str, str, 0);
                        des_int(n_start, start, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlStrsub",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlStrsub()");
                            eprint!(" {}", n_str);
                            eprint!(" {}", n_start);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_utf8_charcmp() {
        unsafe {
            let mut leaks = 0;

            for n_utf1 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_utf2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let utf1 = gen_const_xml_char_ptr(n_utf1, 0);
                    let utf2 = gen_const_xml_char_ptr(n_utf2, 1);

                    let ret_val = xml_utf8_charcmp(utf1 as *const XmlChar, utf2);
                    desret_int(ret_val);
                    des_const_xml_char_ptr(n_utf1, utf1, 0);
                    des_const_xml_char_ptr(n_utf2, utf2, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlUTF8Charcmp",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlUTF8Charcmp()");
                        eprint!(" {}", n_utf1);
                        eprintln!(" {}", n_utf2);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_utf8_size() {
        unsafe {
            let mut leaks = 0;

            for n_utf in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let utf = gen_const_xml_char_ptr(n_utf, 0);

                let ret_val = xml_utf8_size(utf as *const XmlChar);
                desret_int(ret_val);
                des_const_xml_char_ptr(n_utf, utf, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUTF8Size",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUTF8Size()");
                    eprintln!(" {}", n_utf);
                }
            }
        }
    }

    #[test]
    fn test_xml_utf8_strlen() {
        unsafe {
            let mut leaks = 0;

            for n_utf in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let utf = gen_const_xml_char_ptr(n_utf, 0);

                let ret_val = xml_utf8_strlen(utf as *const XmlChar);
                desret_int(ret_val);
                des_const_xml_char_ptr(n_utf, utf, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUTF8Strlen",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUTF8Strlen()");
                    eprintln!(" {}", n_utf);
                }
            }
        }
    }

    #[test]
    fn test_xml_utf8_strloc() {
        unsafe {
            let mut leaks = 0;

            for n_utf in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_utfchar in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let utf = gen_const_xml_char_ptr(n_utf, 0);
                    let utfchar = gen_const_xml_char_ptr(n_utfchar, 1);

                    let ret_val = xml_utf8_strloc(utf as *const XmlChar, utfchar);
                    desret_int(ret_val);
                    des_const_xml_char_ptr(n_utf, utf, 0);
                    des_const_xml_char_ptr(n_utfchar, utfchar, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlUTF8Strloc",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlUTF8Strloc()");
                        eprint!(" {}", n_utf);
                        eprintln!(" {}", n_utfchar);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_utf8_strndup() {
        unsafe {
            let mut leaks = 0;

            for n_utf in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_len in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let utf = gen_const_xml_char_ptr(n_utf, 0);
                    let mut len = gen_int(n_len, 1);
                    if !utf.is_null() && len > xml_strlen(utf) {
                        len = 0;
                    }

                    let ret_val = xml_utf8_strndup(utf as *const XmlChar, len);
                    desret_xml_char_ptr(ret_val);
                    des_const_xml_char_ptr(n_utf, utf, 0);
                    des_int(n_len, len, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlUTF8Strndup",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlUTF8Strndup()");
                        eprint!(" {}", n_utf);
                        eprintln!(" {}", n_len);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_utf8_strpos() {
        unsafe {
            let mut leaks = 0;

            for n_utf in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_pos in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let utf = gen_const_xml_char_ptr(n_utf, 0);
                    let pos = gen_int(n_pos, 1);

                    let ret_val = xml_utf8_strpos(utf as *const XmlChar, pos);
                    desret_const_xml_char_ptr(ret_val);
                    des_const_xml_char_ptr(n_utf, utf, 0);
                    des_int(n_pos, pos, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlUTF8Strpos",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlUTF8Strpos()");
                        eprint!(" {}", n_utf);
                        eprintln!(" {}", n_pos);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_utf8_strsize() {
        unsafe {
            let mut leaks = 0;

            for n_utf in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_len in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let utf = gen_const_xml_char_ptr(n_utf, 0);
                    let mut len = gen_int(n_len, 1);
                    if !utf.is_null() && len > xml_strlen(utf) {
                        len = 0;
                    }

                    let ret_val = xml_utf8_strsize(utf as *const XmlChar, len);
                    desret_int(ret_val);
                    des_const_xml_char_ptr(n_utf, utf, 0);
                    des_int(n_len, len, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlUTF8Strsize",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlUTF8Strsize()");
                        eprint!(" {}", n_utf);
                        eprintln!(" {}", n_len);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_utf8_strsub() {
        unsafe {
            let mut leaks = 0;

            for n_utf in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_start in 0..GEN_NB_INT {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let utf = gen_const_xml_char_ptr(n_utf, 0);
                        let mut start = gen_int(n_start, 1);
                        let mut len = gen_int(n_len, 2);
                        if !utf.is_null() && start > xml_strlen(utf) {
                            start = 0;
                        }
                        if !utf.is_null() && len > xml_strlen(utf) {
                            len = 0;
                        }

                        let ret_val = xml_utf8_strsub(utf as *const XmlChar, start, len);
                        desret_xml_char_ptr(ret_val);
                        des_const_xml_char_ptr(n_utf, utf, 0);
                        des_int(n_start, start, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlUTF8Strsub",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlUTF8Strsub()");
                            eprint!(" {}", n_utf);
                            eprint!(" {}", n_start);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }
}
