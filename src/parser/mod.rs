mod node_info;

use crate::libxml::{
    chvalid::{xml_is_combining, xml_is_digit, xml_is_extender},
    parser::{XmlParserCtxt, XmlParserOption},
    parser_internals::xml_is_letter,
};
pub use node_info::*;

pub(crate) trait XmlParserCharValid {
    fn is_name_char(&self, ctxt: &XmlParserCtxt) -> bool;
    // The two following functions are related to the change of accepted
    // characters for Name and NmToken in the Revision 5 of XML-1.0
    // They correspond to the modified production [4] and the new production [4a]
    // changes in that revision. Also note that the macros used for the
    // productions Letter, Digit, CombiningChar and Extender are not needed
    // anymore.
    // We still keep compatibility to pre-revision5 parsing semantic if the
    // new XML_PARSE_OLD10 option is given to the parser.
    fn is_name_start_char(&self, ctxt: &XmlParserCtxt) -> bool;
}

impl XmlParserCharValid for u8 {
    fn is_name_char(&self, ctxt: &XmlParserCtxt) -> bool {
        let c = *self;
        if ctxt.options & XmlParserOption::XmlParseOld10 as i32 == 0 {
            // Use the new checks of production [4] [4a] amd [5] of the
            // Update 5 of XML-1.0
            c != b' '
                && c != b'>'
                && c != b'/'
                && (c.is_ascii_lowercase()
                    || c.is_ascii_uppercase()
                    || c.is_ascii_digit()
                    || c == b'_'
                    || c == b':'
                    || c == b'-'
                    || c == b'.'
                    || c == 0xB7
                    || (0xC0..=0xD6).contains(&c)
                    || (0xD8..=0xF6).contains(&c)
                    || (0xF8..=0xFF).contains(&c))
        } else {
            xml_is_letter(c as u32)
                || xml_is_digit(c as u32)
                || c == b'.'
                || c == b'-'
                || c == b'_'
                || c == b':'
                || xml_is_combining(c as u32)
                || xml_is_extender(c as u32)
        }
    }

    fn is_name_start_char(&self, ctxt: &XmlParserCtxt) -> bool {
        let c = *self;
        if ctxt.options & XmlParserOption::XmlParseOld10 as i32 == 0 {
            // Use the new checks of production [4] [4a] amd [5] of the
            // Update 5 of XML-1.0
            c != b' '
                && c != b'>'
                && c != b'/'
                && (c.is_ascii_lowercase()
                    || c.is_ascii_uppercase()
                    || c == b'_'
                    || c == b':'
                    || (0xC0..=0xD6).contains(&c)
                    || (0xD8..=0xF6).contains(&c)
                    || (0xF8..=0xFF).contains(&c))
        } else {
            xml_is_letter(c as u32) || c == b'_' || c == b':'
        }
    }
}

impl XmlParserCharValid for u32 {
    fn is_name_char(&self, ctxt: &XmlParserCtxt) -> bool {
        let c = *self;
        if ctxt.options & XmlParserOption::XmlParseOld10 as i32 == 0 {
            // Use the new checks of production [4] [4a] amd [5] of the
            // Update 5 of XML-1.0
            c != b' ' as u32
                && c != b'>' as u32
                && c != b'/' as u32
                && ((c >= b'a' as u32 && c <= b'z' as u32)
                    || (c >= b'A' as u32 && c <= b'Z' as u32)
                    || (c >= b'0' as u32 && c <= b'9' as u32)
                    || c == b'_' as u32
                    || c == b':' as u32
                    || c == b'-' as u32
                    || c == b'.' as u32
                    || c == 0xB7
                    || (0xC0..=0xD6).contains(&c)
                    || (0xD8..=0xF6).contains(&c)
                    || (0xF8..=0x2FF).contains(&c)
                    || (0x300..=0x36F).contains(&c)
                    || (0x370..=0x37D).contains(&c)
                    || (0x37F..=0x1FFF).contains(&c)
                    || (0x200C..=0x200D).contains(&c)
                    || (0x203F..=0x2040).contains(&c)
                    || (0x2070..=0x218F).contains(&c)
                    || (0x2C00..=0x2FEF).contains(&c)
                    || (0x3001..=0xD7FF).contains(&c)
                    || (0xF900..=0xFDCF).contains(&c)
                    || (0xFDF0..=0xFFFD).contains(&c)
                    || (0x10000..=0xEFFFF).contains(&c))
        } else {
            xml_is_letter(c)
                || xml_is_digit(c)
                || c == b'.' as u32
                || c == b'-' as u32
                || c == b'_' as u32
                || c == b':' as u32
                || xml_is_combining(c)
                || xml_is_extender(c)
        }
    }

    fn is_name_start_char(&self, ctxt: &XmlParserCtxt) -> bool {
        let c = *self;
        if ctxt.options & XmlParserOption::XmlParseOld10 as i32 == 0 {
            // Use the new checks of production [4] [4a] amd [5] of the
            // Update 5 of XML-1.0
            c != b' ' as u32
                && c != b'>' as u32
                && c != b'/' as u32
                && ((c >= b'a' as u32 && c <= b'z' as u32)
                    || (c >= b'A' as u32 && c <= b'Z' as u32)
                    || c == b'_' as u32
                    || c == b':' as u32
                    || (0xC0..=0xD6).contains(&c)
                    || (0xD8..=0xF6).contains(&c)
                    || (0xF8..=0x2FF).contains(&c)
                    || (0x370..=0x37D).contains(&c)
                    || (0x37F..=0x1FFF).contains(&c)
                    || (0x200C..=0x200D).contains(&c)
                    || (0x2070..=0x218F).contains(&c)
                    || (0x2C00..=0x2FEF).contains(&c)
                    || (0x3001..=0xD7FF).contains(&c)
                    || (0xF900..=0xFDCF).contains(&c)
                    || (0xFDF0..=0xFFFD).contains(&c)
                    || (0x10000..=0xEFFFF).contains(&c))
        } else {
            xml_is_letter(c) || c == b'_' as u32 || c == b':' as u32
        }
    }
}

impl XmlParserCharValid for char {
    fn is_name_char(&self, ctxt: &XmlParserCtxt) -> bool {
        (*self as u32).is_name_char(ctxt)
    }

    fn is_name_start_char(&self, ctxt: &XmlParserCtxt) -> bool {
        (*self as u32).is_name_start_char(ctxt)
    }
}
