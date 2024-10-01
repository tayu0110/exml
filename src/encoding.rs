use std::{
    borrow::Cow,
    collections::BTreeMap,
    fmt::Display,
    str::FromStr,
    sync::{Mutex, RwLock},
};

use encoding_rs::{
    mem::{convert_latin1_to_str, convert_utf8_to_latin1_lossy, is_str_latin1},
    Decoder, DecoderResult, Encoder, EncoderResult, Encoding, EUC_JP, ISO_2022_JP, ISO_8859_10,
    ISO_8859_13, ISO_8859_14, ISO_8859_15, ISO_8859_16, ISO_8859_2, ISO_8859_3, ISO_8859_4,
    ISO_8859_5, ISO_8859_6, ISO_8859_7, ISO_8859_8, SHIFT_JIS, UTF_16BE, UTF_16LE, WINDOWS_1254,
};

use crate::{__xml_raise_error, libxml::xmlerror::XmlParserErrors};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlCharEncoding {
    Error = -1,
    None = 0,
    UTF8,
    UTF16LE,
    UTF16BE,
    UCS4LE,
    UCS4BE,
    EBCDIC,
    UCS4_2143,
    UCS4_3412,
    UCS2,
    ISO8859_1,
    ISO8859_2,
    ISO8859_3,
    ISO8859_4,
    ISO8859_5,
    ISO8859_6,
    ISO8859_7,
    ISO8859_8,
    ISO8859_9,
    ISO8859_10,
    ISO8859_13,
    ISO8859_14,
    ISO8859_15,
    ISO8859_16,
    ISO2022JP,
    ShiftJIS,
    EUCJP,
    ASCII,
}

impl XmlCharEncoding {
    pub fn get_name(&self) -> Option<&'static str> {
        match *self {
            Self::UTF8 => Some("UTF-8"),
            Self::UTF16LE => Some("UTF-16"),
            Self::UTF16BE => Some("UTF-16"),
            Self::EBCDIC => Some("EBCDIC"),
            Self::UCS4LE => Some("ISO-10646-UCS-4"),
            Self::UCS4BE => Some("ISO-10646-UCS-4"),
            Self::UCS4_2143 => Some("ISO-10646-UCS-4"),
            Self::UCS4_3412 => Some("ISO-10646-UCS-4"),
            Self::UCS2 => Some("ISO-10646-UCS-2"),
            Self::ISO8859_1 => Some("ISO-8859-1"),
            Self::ISO8859_2 => Some("ISO-8859-2"),
            Self::ISO8859_3 => Some("ISO-8859-3"),
            Self::ISO8859_4 => Some("ISO-8859-4"),
            Self::ISO8859_5 => Some("ISO-8859-5"),
            Self::ISO8859_6 => Some("ISO-8859-6"),
            Self::ISO8859_7 => Some("ISO-8859-7"),
            Self::ISO8859_8 => Some("ISO-8859-8"),
            Self::ISO8859_9 => Some("ISO-8859-9"),
            Self::ISO8859_10 => Some("ISO-8859-10"),
            Self::ISO8859_13 => Some("ISO-8859-13"),
            Self::ISO8859_14 => Some("ISO-8859-14"),
            Self::ISO8859_15 => Some("ISO-8859-15"),
            Self::ISO8859_16 => Some("ISO-8859-16"),
            Self::ISO2022JP => Some("ISO-2022-JP"),
            Self::ShiftJIS => Some("Shift-JIS"),
            Self::EUCJP => Some("EUC-JP"),
            _ => None,
        }
    }
}

impl FromStr for XmlCharEncoding {
    type Err = EncodingError;

    fn from_str(name: &str) -> Result<Self, Self::Err> {
        let name = if let Some(alias) = get_encoding_alias(name) {
            alias
        } else {
            name.to_uppercase()
        };
        if name.is_empty() {
            Ok(XmlCharEncoding::None)
        } else if name == "UTF-8" || name == "UTF8" {
            Ok(XmlCharEncoding::UTF8)
        } else if name == "UTF-16" || name == "UTF16" {
            Ok(XmlCharEncoding::UTF16LE)
        } else if name == "ISO-10646-UCS-2" || name == "UCS-2" || name == "UCS2" {
            Ok(XmlCharEncoding::UCS2)
        } else if name == "ISO-10646-UCS-4" || name == "UCS-4" || name == "UCS4" {
            Ok(XmlCharEncoding::UCS4LE)
        } else if name == "ISO-8859-1" || name == "ISO-LATIN-1" || name == "ISO LATIN 1" {
            Ok(XmlCharEncoding::ISO8859_1)
        } else if name == "ISO-8859-2" || name == "ISO-LATIN-2" || name == "ISO LATIN 2" {
            Ok(XmlCharEncoding::ISO8859_2)
        } else if name == "ISO-8859-3" {
            Ok(XmlCharEncoding::ISO8859_3)
        } else if name == "ISO-8859-4" {
            Ok(XmlCharEncoding::ISO8859_4)
        } else if name == "ISO-8859-5" {
            Ok(XmlCharEncoding::ISO8859_5)
        } else if name == "ISO-8859-6" {
            Ok(XmlCharEncoding::ISO8859_6)
        } else if name == "ISO-8859-7" {
            Ok(XmlCharEncoding::ISO8859_7)
        } else if name == "ISO-8859-8" {
            Ok(XmlCharEncoding::ISO8859_8)
        } else if name == "ISO-8859-9" {
            Ok(XmlCharEncoding::ISO8859_9)
        } else if name == "ISO-8859-10" {
            Ok(XmlCharEncoding::ISO8859_10)
        } else if name == "ISO-8859-13" {
            Ok(XmlCharEncoding::ISO8859_13)
        } else if name == "ISO-8859-14" {
            Ok(XmlCharEncoding::ISO8859_14)
        } else if name == "ISO-8859-15" {
            Ok(XmlCharEncoding::ISO8859_15)
        } else if name == "ISO-8859-16" {
            Ok(XmlCharEncoding::ISO8859_16)
        } else if name == "ISO-2022-JP" {
            Ok(XmlCharEncoding::ISO2022JP)
        } else if name == "SHIFT_JIS" {
            Ok(XmlCharEncoding::ShiftJIS)
        } else if name == "EUC-JP" {
            Ok(XmlCharEncoding::EUCJP)
        } else {
            Err(EncodingError("No encoding matches.".into()))
        }
    }
}

pub enum XmlCharEncodingHandler {
    Predefined(PredefinedEncodingHandler),
    Custom(CustomEncodingHandler),
}

impl XmlCharEncodingHandler {
    pub fn encode(&mut self, src: &str, dst: &mut [u8]) -> Result<(usize, usize), EncodingError> {
        match self {
            Self::Predefined(handler) => handler.encode(src, dst),
            Self::Custom(handler) => (handler.encode)(src, dst),
        }
    }

    pub fn decode(&mut self, src: &[u8], dst: &mut str) -> Result<(usize, usize), EncodingError> {
        match self {
            Self::Predefined(handler) => handler.decode(src, dst),
            Self::Custom(handler) => (handler.decode)(src, dst),
        }
    }
}

pub struct PredefinedEncodingHandler {
    name: &'static str,
    encoder: Encoder,
    decoder: Decoder,
    elast: bool,
    dlast: bool,
}

impl PredefinedEncodingHandler {
    pub fn encode(&mut self, src: &str, dst: &mut [u8]) -> Result<(usize, usize), EncodingError> {
        let (res, read, write) = self
            .encoder
            .encode_from_utf8_without_replacement(src, dst, self.elast);
        match res {
            EncoderResult::OutputFull => Ok((read, write)),
            EncoderResult::InputEmpty => {
                self.elast = true;
                Ok((read, write))
            }
            EncoderResult::Unmappable(c) => Err(EncodingError(format!(
                "Failed to map `{c}` on encoding {}",
                self.name
            ))),
        }
    }

    pub fn decode(&mut self, src: &[u8], dst: &mut str) -> Result<(usize, usize), EncodingError> {
        let (res, read, write) = self
            .decoder
            .decode_to_str_without_replacement(src, dst, self.dlast);
        match res {
            DecoderResult::OutputFull => Ok((read, write)),
            DecoderResult::InputEmpty => {
                self.dlast = true;
                Ok((read, write))
            }
            DecoderResult::Malformed(c, d) => Err(EncodingError(format!(
                "Failed to decode {} byte sequence [{c}, {d}]",
                self.name
            ))),
        }
    }
}

impl From<&'static Encoding> for PredefinedEncodingHandler {
    fn from(value: &'static Encoding) -> Self {
        let name = value.name();
        let encoder = value.new_encoder();
        let decoder = value.new_decoder();
        Self {
            name,
            encoder,
            decoder,
            elast: false,
            dlast: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct EncodingError(pub String);

impl EncodingError {
    pub fn new(msg: &str) -> Self {
        Self(msg.to_owned())
    }
}

impl Display for EncodingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Encoding Error: {}", self.0)
    }
}

impl std::error::Error for EncodingError {}

pub type EncoderFunc = fn(src: &str, dst: &mut [u8]) -> Result<(usize, usize), EncodingError>;
pub type DecoderFunc = fn(src: &[u8], dst: &mut str) -> Result<(usize, usize), EncodingError>;

#[derive(Debug, Clone)]
pub struct CustomEncodingHandler {
    /// the name of this encoding
    name: Cow<'static, str>,
    /// encode source UTF-8 string `src` to the byte sequence `dst` of this encoding.  
    /// If successfully encoded, return `Ok((read_bytes, write_bytes))`.
    encode: EncoderFunc,
    /// decode the byte sequece `src` of this encoding to the UTF-8 string `dst`.  
    /// If successfully decoded, return `Ok((read_bytes, write_bytes))`.
    decode: DecoderFunc,
}

impl CustomEncodingHandler {
    /// Create and register new CustomEncodingHandler.
    ///
    /// If too many handlers are registered (more than 50 handlers specifically), return `Err`.  
    /// Otherwise, return `Ok`.
    pub fn new(
        name: &str,
        encode: EncoderFunc,
        decode: DecoderFunc,
    ) -> Result<Self, EncodingError> {
        let name = if let Some(alias) = get_encoding_alias(name) {
            alias.to_owned()
        } else {
            name.to_uppercase()
        };
        let new = CustomEncodingHandler {
            name: name.into(),
            encode,
            decode,
        };
        register_encoding_handler(new.clone())?;
        Ok(new)
    }
}

const MAX_ENCODING_HANDLERS: usize = 50;
static HANDLERS: Mutex<Vec<CustomEncodingHandler>> = Mutex::new(vec![]);

pub(crate) fn cleanup_encoding_handlers() {
    cleanup_encoding_aliases();

    let mut handlers = HANDLERS.lock().unwrap();
    handlers.clear();
}

pub(crate) unsafe extern "C" fn xml_encoding_err(
    error: XmlParserErrors,
    msg: *const i8,
    val: *const i8,
) {
    __xml_raise_error!(
        None,
        None,
        null_mut(),
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromI18N as i32,
        error as i32,
        XmlErrorLevel::XmlErrFatal,
        null_mut(),
        0,
        val,
        null_mut(),
        null_mut(),
        0,
        0,
        msg,
        val
    );
}

pub fn register_encoding_handler(handler: CustomEncodingHandler) -> Result<(), EncodingError> {
    let mut handlers = HANDLERS.lock().unwrap();
    if handlers.len() >= MAX_ENCODING_HANDLERS {
        unsafe {
            xml_encoding_err(
                XmlParserErrors::XmlI18nExcessHandler,
                c"xmlRegisterCharEncodingHandler: Too many handler registered, see %s\n".as_ptr(),
                c"MAX_ENCODING_HANDLERS".as_ptr() as _,
            );
            return Err(EncodingError(
                "Too many CustomEncodingHandlers are registerd.".into(),
            ));
        }
    }
    handlers.push(handler);
    Ok(())
}

fn encode_utf16le(src: &str, dst: &mut [u8]) -> Result<(usize, usize), EncodingError> {
    let mut buf = [0u16; 2];
    let mut read = 0;
    let mut write = 0;
    for c in src.chars() {
        let res = c.encode_utf16(&mut buf);
        if write + res.len() * 2 >= dst.len() {
            break;
        }
        read += c.len_utf8();
        for &mut code in res {
            dst[write] = code as u8;
            dst[write + 1] = (code >> 8) as u8;
            write += 2;
        }
    }
    Ok((read, write))
}

fn decode_utf16le(src: &[u8], dst: &mut str) -> Result<(usize, usize), EncodingError> {
    let mut decoder = UTF_16LE.new_decoder();
    let (res, read, write) = decoder.decode_to_str_without_replacement(src, dst, false);
    match res {
        DecoderResult::Malformed(c, d) => Err(EncodingError(format!(
            "The byte sequence [{c}, {d}] is not a valid UTF-16LE."
        ))),
        _ => Ok((read, write)),
    }
}

fn encode_utf16be(src: &str, dst: &mut [u8]) -> Result<(usize, usize), EncodingError> {
    let mut buf = [0u16; 2];
    let mut read = 0;
    let mut write = 0;
    for c in src.chars() {
        let res = c.encode_utf16(&mut buf);
        if write + res.len() * 2 >= dst.len() {
            break;
        }
        read += c.len_utf8();
        for &mut code in res {
            dst[write] = (code >> 8) as u8;
            dst[write + 1] = code as u8;
            write += 2;
        }
    }
    Ok((read, write))
}

fn decode_utf16be(src: &[u8], dst: &mut str) -> Result<(usize, usize), EncodingError> {
    let mut decoder = UTF_16BE.new_decoder();
    let (res, read, write) = decoder.decode_to_str_without_replacement(src, dst, false);
    match res {
        DecoderResult::Malformed(c, d) => Err(EncodingError(format!(
            "The byte sequence [{c}, {d}] is not a valid UTF-16BE."
        ))),
        _ => Ok((read, write)),
    }
}

fn encode_latin1(src: &str, dst: &mut [u8]) -> Result<(usize, usize), EncodingError> {
    // useful method `str::floor_char_boundary` exists, but this is nightly...
    //
    // ref: https://doc.rust-lang.org/1.81.0/src/core/str/mod.rs.html#256
    let len = if dst.len() >= src.len() {
        src.len()
    } else {
        let lower_bound = dst.len().saturating_sub(3);
        let new = src.as_bytes()[lower_bound..=dst.len()]
            .iter()
            .rposition(|&b| (b as i8) >= -0x40);
        lower_bound + new.unwrap()
    };
    // The convertion methods from UTF-8 to Latin1 requests
    // the length of destination buffer must be equal of larger than the length of source buffer.
    if len == 0 {
        return Err(EncodingError(
            "The length of destination buffer is too short".into(),
        ));
    }

    //
    if !is_str_latin1(&src[..len]) {
        return Err(EncodingError(
            "Source buffer is not compatible with Latin1.".into(),
        ));
    }
    let write = convert_utf8_to_latin1_lossy(&src.as_bytes()[..len], dst);
    Ok((len, write))
}

fn decode_latin1(src: &[u8], dst: &mut str) -> Result<(usize, usize), EncodingError> {
    if dst.len() <= 1 {
        return Err(EncodingError(
            "The length of destination buffer is too short".into(),
        ));
    }
    // This method requests the length of destination buffer must be at least
    // the length of the source buffer times two.
    //
    // ref: https://docs.rs/encoding_rs/latest/encoding_rs/mem/fn.convert_latin1_to_str.html
    let write = convert_latin1_to_str(&src[..dst.len() / 2], dst);
    Ok((dst.len() / 2, write))
}

fn encode_ucs4be(src: &str, dst: &mut [u8]) -> Result<(usize, usize), EncodingError> {
    if !src.is_empty() && dst.len() < 4 {
        return Err(EncodingError(
            "The length of destination buffer is too short".into(),
        ));
    }

    let (mut read, mut write) = (0, 0);
    for (c, chunk) in src.chars().zip(dst.chunks_exact_mut(4)) {
        let code = c as u32;
        chunk[0] = (code >> 24) as u8;
        chunk[1] = (code >> 16) as u8;
        chunk[2] = (code >> 8) as u8;
        chunk[3] = code as u8;
        read += c.len_utf8();
        write += 4;
    }
    Ok((read, write))
}

fn decode_ucs4be(src: &[u8], dst: &mut str) -> Result<(usize, usize), EncodingError> {
    if src.len() % 4 != 0 {
        return Err(EncodingError(
            "The length of UCS-4 sequence must be a multiple of 4.".into(),
        ));
    }

    let dst = unsafe { dst.as_bytes_mut() };
    let (mut read, mut write) = (0, 0);
    for chunk in src.chunks_exact(4) {
        let code = chunk.iter().fold(0u32, |s, &v| (s << 8) | v as u32);
        let c = char::from_u32(code).ok_or_else(|| {
            EncodingError(format!("{code:0X} is not a valid Unicode Scalar Value"))
        })?;
        let len = c.len_utf8();
        if write + len > dst.len() {
            break;
        }
        read += 4;
        write += len;
        c.encode_utf8(&mut dst[write..]);
    }
    Ok((read, write))
}

fn encode_ucs4le(src: &str, dst: &mut [u8]) -> Result<(usize, usize), EncodingError> {
    if !src.is_empty() && dst.len() < 4 {
        return Err(EncodingError(
            "The length of destination buffer is too short".into(),
        ));
    }

    let (mut read, mut write) = (0, 0);
    for (c, chunk) in src.chars().zip(dst.chunks_exact_mut(4)) {
        let code = c as u32;
        chunk[0] = code as u8;
        chunk[1] = (code >> 8) as u8;
        chunk[2] = (code >> 16) as u8;
        chunk[3] = (code >> 24) as u8;
        read += c.len_utf8();
        write += 4;
    }
    Ok((read, write))
}

fn decode_ucs4le(src: &[u8], dst: &mut str) -> Result<(usize, usize), EncodingError> {
    if src.len() % 4 != 0 {
        return Err(EncodingError(
            "The length of UCS-4 sequence must be a multiple of 4.".into(),
        ));
    }

    let dst = unsafe { dst.as_bytes_mut() };
    let (mut read, mut write) = (0, 0);
    for chunk in src.chunks_exact(4) {
        let code = chunk.iter().rev().fold(0u32, |s, &v| (s << 8) | v as u32);
        let c = char::from_u32(code).ok_or_else(|| {
            EncodingError(format!("{code:0X} is not a valid Unicode Scalar Value"))
        })?;
        let len = c.len_utf8();
        if write + len > dst.len() {
            break;
        }
        read += 4;
        write += len;
        c.encode_utf8(&mut dst[write..]);
    }
    Ok((read, write))
}

pub fn get_encoding_handler(enc: XmlCharEncoding) -> Option<XmlCharEncodingHandler> {
    match enc {
        XmlCharEncoding::Error | XmlCharEncoding::None => None,
        XmlCharEncoding::UTF8 => None,
        // For UTF-16, encoding-rs does not provide encoders.
        // Therefore, we should provide them as CustomEncodingHandler.
        //
        // ref: https://docs.rs/encoding_rs/latest/encoding_rs/index.html#utf-16le-utf-16be-and-unicode-encoding-schemes
        XmlCharEncoding::UTF16LE => Some(XmlCharEncodingHandler::Custom(CustomEncodingHandler {
            name: UTF_16LE.name().into(),
            encode: encode_utf16le,
            decode: decode_utf16le,
        })),
        XmlCharEncoding::UTF16BE => Some(XmlCharEncodingHandler::Custom(CustomEncodingHandler {
            name: UTF_16BE.name().into(),
            encode: encode_utf16be,
            decode: decode_utf16be,
        })),
        XmlCharEncoding::EBCDIC => ["EBCDIC", "ebcdic", "EBCDIC-US", "IBM-037"]
            .into_iter()
            .find_map(find_encoding_handler),
        XmlCharEncoding::UCS4BE => Some(XmlCharEncodingHandler::Custom(CustomEncodingHandler {
            name: "UCS-4".into(),
            encode: encode_ucs4be,
            decode: decode_ucs4be,
        })),
        XmlCharEncoding::UCS4LE => Some(XmlCharEncodingHandler::Custom(CustomEncodingHandler {
            name: "UCS-4".into(),
            encode: encode_ucs4le,
            decode: decode_ucs4le,
        })),
        XmlCharEncoding::UCS4_2143 | XmlCharEncoding::UCS4_3412 => None,
        XmlCharEncoding::UCS2 => ["ISO-10646-UCS-2", "UCS-2", "UCS2"]
            .into_iter()
            .find_map(find_encoding_handler),
        // Both Encoder and Decoder are not provided by encoding_rs.
        // However, it seems that `convert**latin1` in `encoding_rs::mem` can support this encoding ??
        // I have not understood yet...
        //
        // ref: https://docs.rs/encoding_rs/latest/encoding_rs/index.html#iso-8859-1
        XmlCharEncoding::ISO8859_1 => Some(XmlCharEncodingHandler::Custom(CustomEncodingHandler {
            name: "ISO-8859-1".into(),
            encode: encode_latin1,
            decode: decode_latin1,
        })),
        XmlCharEncoding::ISO8859_2 => Some(XmlCharEncodingHandler::Predefined(ISO_8859_2.into())),
        XmlCharEncoding::ISO8859_3 => Some(XmlCharEncodingHandler::Predefined(ISO_8859_3.into())),
        XmlCharEncoding::ISO8859_4 => Some(XmlCharEncodingHandler::Predefined(ISO_8859_4.into())),
        XmlCharEncoding::ISO8859_5 => Some(XmlCharEncodingHandler::Predefined(ISO_8859_5.into())),
        XmlCharEncoding::ISO8859_6 => Some(XmlCharEncodingHandler::Predefined(ISO_8859_6.into())),
        XmlCharEncoding::ISO8859_7 => Some(XmlCharEncodingHandler::Predefined(ISO_8859_7.into())),
        XmlCharEncoding::ISO8859_8 => Some(XmlCharEncodingHandler::Predefined(ISO_8859_8.into())),
        // Windows 1254 is the extention of ISO-8859-9, but they do not match exactly.
        // This should be fixed ???
        XmlCharEncoding::ISO8859_9 => Some(XmlCharEncodingHandler::Predefined(WINDOWS_1254.into())),
        XmlCharEncoding::ISO8859_10 => Some(XmlCharEncodingHandler::Predefined(ISO_8859_10.into())),
        XmlCharEncoding::ISO8859_13 => Some(XmlCharEncodingHandler::Predefined(ISO_8859_13.into())),
        XmlCharEncoding::ISO8859_14 => Some(XmlCharEncodingHandler::Predefined(ISO_8859_14.into())),
        XmlCharEncoding::ISO8859_15 => Some(XmlCharEncodingHandler::Predefined(ISO_8859_15.into())),
        XmlCharEncoding::ISO8859_16 => Some(XmlCharEncodingHandler::Predefined(ISO_8859_16.into())),
        XmlCharEncoding::ISO2022JP => Some(XmlCharEncodingHandler::Predefined(ISO_2022_JP.into())),
        XmlCharEncoding::ShiftJIS => Some(XmlCharEncodingHandler::Predefined(SHIFT_JIS.into())),
        XmlCharEncoding::EUCJP => Some(XmlCharEncodingHandler::Predefined(EUC_JP.into())),
        _ => None,
    }
}

pub fn find_encoding_handler(name: &str) -> Option<XmlCharEncodingHandler> {
    // use user-defined aliases in preference
    if let Some(alias) = get_encoding_alias(name) {
        if let Some(encoding) = Encoding::for_label(alias.as_bytes()) {
            return Some(XmlCharEncodingHandler::Predefined(encoding.into()));
        }

        let alias = alias.to_uppercase();
        let handlers = HANDLERS.lock().unwrap();
        for handler in handlers.iter() {
            if handler.name == alias {
                return Some(XmlCharEncodingHandler::Custom(handler.clone()));
            }
        }
    }

    // If the alias matches nothing, use canonical name
    match name.parse::<XmlCharEncoding>() {
        Err(_) => {
            return Encoding::for_label(name.as_bytes())
                .map(|enc| XmlCharEncodingHandler::Predefined(enc.into()));
        }
        Ok(enc) => {
            if let Some(enc) = enc.get_name().filter(|&enc| enc != name) {
                return find_encoding_handler(enc);
            }
        }
    }
    None
}

static ENCODING_ALIASES: RwLock<BTreeMap<String, String>> = RwLock::new(BTreeMap::new());

pub fn get_encoding_alias(alias: &str) -> Option<String> {
    let aliases: std::sync::RwLockReadGuard<'static, BTreeMap<String, String>> =
        ENCODING_ALIASES.read().ok()?;
    aliases.get(alias).cloned()
}

/// If `alias` already exists as an alias, it is overwritten.
///
/// # Note
/// `alias` and `name` are stored after converted to uppercase.  
/// This behaviour affects alias overrides.
pub fn add_encoding_alias(name: &str, alias: &str) {
    let mut aliases = ENCODING_ALIASES.write().unwrap();
    aliases.insert(alias.to_uppercase(), name.to_uppercase());
}

pub fn remove_encoding_alias(alias: &str) -> Option<String> {
    let mut aliases = ENCODING_ALIASES.write().ok()?;
    aliases.remove(alias)
}

pub fn cleanup_encoding_aliases() {
    let mut aliases = ENCODING_ALIASES.write().unwrap();
    aliases.clear();
}

pub fn detect_encoding(input: &[u8]) -> XmlCharEncoding {
    match input {
        [0x00, 0x00, 0x00, 0x3C, ..] => XmlCharEncoding::UCS4BE,
        [0x3C, 0x00, 0x00, 0x00, ..] => XmlCharEncoding::UCS4LE,
        [0x00, 0x00, 0x3C, 0x00, ..] => XmlCharEncoding::UCS4_2143,
        [0x00, 0x3C, 0x00, 0x00, ..] => XmlCharEncoding::UCS4_3412,
        [0x4C, 0x6F, 0xA7, 0x94, ..] => XmlCharEncoding::EBCDIC,
        [0x3C, 0x3F, 0x78, 0x6D, ..] => XmlCharEncoding::UTF8,
        [0x3C, 0x00, 0x3F, 0x00, ..] => XmlCharEncoding::UTF16LE,
        [0x00, 0x3C, 0x00, 0x3F, ..] => XmlCharEncoding::UTF16BE,
        // UTF-8 BOM
        [0xEF, 0xBB, 0xBF, ..] => XmlCharEncoding::UTF8,
        // UTF-16 BOM (BE)
        [0xFE, 0xFF, ..] => XmlCharEncoding::UTF16BE,
        // UTF-16 BOM (LE)
        [0xFF, 0xFE, ..] => XmlCharEncoding::UTF16LE,
        _ => XmlCharEncoding::None,
    }
}
