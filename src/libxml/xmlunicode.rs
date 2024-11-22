//! Provide methods and data structures for handling Unicode characters.  
//! This module is based on `libxml/xmlunicode.h`, `xmlunicode.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::ffi::c_char;

use libc::strcmp;

use super::chvalid::{xml_char_in_range, XmlChLRange, XmlChRangeGroup, XmlChSRange};

pub type XmlIntFunc = unsafe extern "C" fn(i32) -> i32; /* just to keep one's mind untwisted */

#[repr(C)]
pub struct XmlUnicodeRange {
    rangename: *const c_char,
    func: XmlIntFunc,
}

#[repr(C)]
pub struct XmlUnicodeNameTable {
    table: *const XmlUnicodeRange,
    numentries: i32,
}

const XML_UNICODE_BLOCKS: *const XmlUnicodeRange = [
    XmlUnicodeRange {
        rangename: c"AegeanNumbers".as_ptr(),
        func: xml_ucs_is_aegean_numbers,
    },
    XmlUnicodeRange {
        rangename: c"AlphabeticPresentationForms".as_ptr(),
        func: xml_ucs_is_alphabetic_presentation_forms,
    },
    XmlUnicodeRange {
        rangename: c"Arabic".as_ptr(),
        func: xml_ucs_is_arabic,
    },
    XmlUnicodeRange {
        rangename: c"ArabicPresentationForms-A".as_ptr(),
        func: xml_ucs_is_arabic_presentation_forms_a,
    },
    XmlUnicodeRange {
        rangename: c"ArabicPresentationForms-B".as_ptr(),
        func: xml_ucs_is_arabic_presentation_forms_b,
    },
    XmlUnicodeRange {
        rangename: c"Armenian".as_ptr(),
        func: xml_ucs_is_armenian,
    },
    XmlUnicodeRange {
        rangename: c"Arrows".as_ptr(),
        func: xml_ucs_is_arrows,
    },
    XmlUnicodeRange {
        rangename: c"BasicLatin".as_ptr(),
        func: xml_ucs_is_basic_latin,
    },
    XmlUnicodeRange {
        rangename: c"Bengali".as_ptr(),
        func: xml_ucs_is_bengali,
    },
    XmlUnicodeRange {
        rangename: c"BlockElements".as_ptr(),
        func: xml_ucs_is_block_elements,
    },
    XmlUnicodeRange {
        rangename: c"Bopomofo".as_ptr(),
        func: xml_ucs_is_bopomofo,
    },
    XmlUnicodeRange {
        rangename: c"BopomofoExtended".as_ptr(),
        func: xml_ucs_is_bopomofo_extended,
    },
    XmlUnicodeRange {
        rangename: c"BoxDrawing".as_ptr(),
        func: xml_ucs_is_box_drawing,
    },
    XmlUnicodeRange {
        rangename: c"BraillePatterns".as_ptr(),
        func: xml_ucs_is_braille_patterns,
    },
    XmlUnicodeRange {
        rangename: c"Buhid".as_ptr(),
        func: xml_ucs_is_buhid,
    },
    XmlUnicodeRange {
        rangename: c"ByzantineMusicalSymbols".as_ptr(),
        func: xml_ucs_is_byzantine_musical_symbols,
    },
    XmlUnicodeRange {
        rangename: c"CJKCompatibility".as_ptr(),
        func: xml_ucs_is_cjk_compatibility,
    },
    XmlUnicodeRange {
        rangename: c"CJKCompatibilityForms".as_ptr(),
        func: xml_ucs_is_cjk_compatibility_forms,
    },
    XmlUnicodeRange {
        rangename: c"CJKCompatibilityIdeographs".as_ptr(),
        func: xml_ucs_is_cjk_compatibility_ideographs,
    },
    XmlUnicodeRange {
        rangename: c"CJKCompatibilityIdeographsSupplement".as_ptr(),
        func: xml_ucs_is_cjk_compatibility_ideographs_supplement,
    },
    XmlUnicodeRange {
        rangename: c"CJKRadicalsSupplement".as_ptr(),
        func: xml_ucs_is_cjk_radicals_supplement,
    },
    XmlUnicodeRange {
        rangename: c"CJKSymbolsandPunctuation".as_ptr(),
        func: xml_ucs_is_cjk_symbolsand_punctuation,
    },
    XmlUnicodeRange {
        rangename: c"CJKUnifiedIdeographs".as_ptr(),
        func: xml_ucs_is_cjk_unified_ideographs,
    },
    XmlUnicodeRange {
        rangename: c"CJKUnifiedIdeographsExtensionA".as_ptr(),
        func: xml_ucs_is_cjk_unified_ideographs_extension_a,
    },
    XmlUnicodeRange {
        rangename: c"CJKUnifiedIdeographsExtensionB".as_ptr(),
        func: xml_ucs_is_cjk_unified_ideographs_extension_b,
    },
    XmlUnicodeRange {
        rangename: c"Cherokee".as_ptr(),
        func: xml_ucs_is_cherokee,
    },
    XmlUnicodeRange {
        rangename: c"CombiningDiacriticalMarks".as_ptr(),
        func: xml_ucs_is_combining_diacritical_marks,
    },
    XmlUnicodeRange {
        rangename: c"CombiningDiacriticalMarksforSymbols".as_ptr(),
        func: xml_ucs_is_combining_diacritical_marksfor_symbols,
    },
    XmlUnicodeRange {
        rangename: c"CombiningHalfMarks".as_ptr(),
        func: xml_ucs_is_combining_half_marks,
    },
    XmlUnicodeRange {
        rangename: c"CombiningMarksforSymbols".as_ptr(),
        func: xml_ucs_is_combining_marksfor_symbols,
    },
    XmlUnicodeRange {
        rangename: c"ControlPictures".as_ptr(),
        func: xml_ucs_is_control_pictures,
    },
    XmlUnicodeRange {
        rangename: c"CurrencySymbols".as_ptr(),
        func: xml_ucs_is_currency_symbols,
    },
    XmlUnicodeRange {
        rangename: c"CypriotSyllabary".as_ptr(),
        func: xml_ucs_is_cypriot_syllabary,
    },
    XmlUnicodeRange {
        rangename: c"Cyrillic".as_ptr(),
        func: xml_ucs_is_cyrillic,
    },
    XmlUnicodeRange {
        rangename: c"CyrillicSupplement".as_ptr(),
        func: xml_ucs_is_cyrillic_supplement,
    },
    XmlUnicodeRange {
        rangename: c"Deseret".as_ptr(),
        func: xml_ucs_is_deseret,
    },
    XmlUnicodeRange {
        rangename: c"Devanagari".as_ptr(),
        func: xml_ucs_is_devanagari,
    },
    XmlUnicodeRange {
        rangename: c"Dingbats".as_ptr(),
        func: xml_ucs_is_dingbats,
    },
    XmlUnicodeRange {
        rangename: c"EnclosedAlphanumerics".as_ptr(),
        func: xml_ucs_is_enclosed_alphanumerics,
    },
    XmlUnicodeRange {
        rangename: c"EnclosedCJKLettersandMonths".as_ptr(),
        func: xml_ucs_is_enclosed_cjk_lettersand_months,
    },
    XmlUnicodeRange {
        rangename: c"Ethiopic".as_ptr(),
        func: xml_ucs_is_ethiopic,
    },
    XmlUnicodeRange {
        rangename: c"GeneralPunctuation".as_ptr(),
        func: xml_ucs_is_general_punctuation,
    },
    XmlUnicodeRange {
        rangename: c"GeometricShapes".as_ptr(),
        func: xml_ucs_is_geometric_shapes,
    },
    XmlUnicodeRange {
        rangename: c"Georgian".as_ptr(),
        func: xml_ucs_is_georgian,
    },
    XmlUnicodeRange {
        rangename: c"Gothic".as_ptr(),
        func: xml_ucs_is_gothic,
    },
    XmlUnicodeRange {
        rangename: c"Greek".as_ptr(),
        func: xml_ucs_is_greek,
    },
    XmlUnicodeRange {
        rangename: c"GreekExtended".as_ptr(),
        func: xml_ucs_is_greek_extended,
    },
    XmlUnicodeRange {
        rangename: c"GreekandCoptic".as_ptr(),
        func: xml_ucs_is_greekand_coptic,
    },
    XmlUnicodeRange {
        rangename: c"Gujarati".as_ptr(),
        func: xml_ucs_is_gujarati,
    },
    XmlUnicodeRange {
        rangename: c"Gurmukhi".as_ptr(),
        func: xml_ucs_is_gurmukhi,
    },
    XmlUnicodeRange {
        rangename: c"HalfwidthandFullwidthForms".as_ptr(),
        func: xml_ucs_is_halfwidthand_fullwidth_forms,
    },
    XmlUnicodeRange {
        rangename: c"HangulCompatibilityJamo".as_ptr(),
        func: xml_ucs_is_hangul_compatibility_jamo,
    },
    XmlUnicodeRange {
        rangename: c"HangulJamo".as_ptr(),
        func: xml_ucs_is_hangul_jamo,
    },
    XmlUnicodeRange {
        rangename: c"HangulSyllables".as_ptr(),
        func: xml_ucs_is_hangul_syllables,
    },
    XmlUnicodeRange {
        rangename: c"Hanunoo".as_ptr(),
        func: xml_ucs_is_hanunoo,
    },
    XmlUnicodeRange {
        rangename: c"Hebrew".as_ptr(),
        func: xml_ucs_is_hebrew,
    },
    XmlUnicodeRange {
        rangename: c"HighPrivateUseSurrogates".as_ptr(),
        func: xml_ucs_is_high_private_use_surrogates,
    },
    XmlUnicodeRange {
        rangename: c"HighSurrogates".as_ptr(),
        func: xml_ucs_is_high_surrogates,
    },
    XmlUnicodeRange {
        rangename: c"Hiragana".as_ptr(),
        func: xml_ucs_is_hiragana,
    },
    XmlUnicodeRange {
        rangename: c"IPAExtensions".as_ptr(),
        func: xml_ucs_is_ipa_extensions,
    },
    XmlUnicodeRange {
        rangename: c"IdeographicDescriptionCharacters".as_ptr(),
        func: xml_ucs_is_ideographic_description_characters,
    },
    XmlUnicodeRange {
        rangename: c"Kanbun".as_ptr(),
        func: xml_ucs_is_kanbun,
    },
    XmlUnicodeRange {
        rangename: c"KangxiRadicals".as_ptr(),
        func: xml_ucs_is_kangxi_radicals,
    },
    XmlUnicodeRange {
        rangename: c"Kannada".as_ptr(),
        func: xml_ucs_is_kannada,
    },
    XmlUnicodeRange {
        rangename: c"Katakana".as_ptr(),
        func: xml_ucs_is_katakana,
    },
    XmlUnicodeRange {
        rangename: c"KatakanaPhoneticExtensions".as_ptr(),
        func: xml_ucs_is_katakana_phonetic_extensions,
    },
    XmlUnicodeRange {
        rangename: c"Khmer".as_ptr(),
        func: xml_ucs_is_khmer,
    },
    XmlUnicodeRange {
        rangename: c"KhmerSymbols".as_ptr(),
        func: xml_ucs_is_khmer_symbols,
    },
    XmlUnicodeRange {
        rangename: c"Lao".as_ptr(),
        func: xml_ucs_is_lao,
    },
    XmlUnicodeRange {
        rangename: c"Latin-1Supplement".as_ptr(),
        func: xml_ucs_is_latin1_supplement,
    },
    XmlUnicodeRange {
        rangename: c"LatinExtended-A".as_ptr(),
        func: xml_ucs_is_latin_extended_a,
    },
    XmlUnicodeRange {
        rangename: c"LatinExtended-B".as_ptr(),
        func: xml_ucs_is_latin_extended_b,
    },
    XmlUnicodeRange {
        rangename: c"LatinExtendedAdditional".as_ptr(),
        func: xml_ucs_is_latin_extended_additional,
    },
    XmlUnicodeRange {
        rangename: c"LetterlikeSymbols".as_ptr(),
        func: xml_ucs_is_letterlike_symbols,
    },
    XmlUnicodeRange {
        rangename: c"Limbu".as_ptr(),
        func: xml_ucs_is_limbu,
    },
    XmlUnicodeRange {
        rangename: c"LinearBIdeograms".as_ptr(),
        func: xml_ucs_is_linear_bideograms,
    },
    XmlUnicodeRange {
        rangename: c"LinearBSyllabary".as_ptr(),
        func: xml_ucs_is_linear_bsyllabary,
    },
    XmlUnicodeRange {
        rangename: c"LowSurrogates".as_ptr(),
        func: xml_ucs_is_low_surrogates,
    },
    XmlUnicodeRange {
        rangename: c"Malayalam".as_ptr(),
        func: xml_ucs_is_malayalam,
    },
    XmlUnicodeRange {
        rangename: c"MathematicalAlphanumericSymbols".as_ptr(),
        func: xml_ucs_is_mathematical_alphanumeric_symbols,
    },
    XmlUnicodeRange {
        rangename: c"MathematicalOperators".as_ptr(),
        func: xml_ucs_is_mathematical_operators,
    },
    XmlUnicodeRange {
        rangename: c"MiscellaneousMathematicalSymbols-A".as_ptr(),
        func: xml_ucs_is_miscellaneous_mathematical_symbols_a,
    },
    XmlUnicodeRange {
        rangename: c"MiscellaneousMathematicalSymbols-B".as_ptr(),
        func: xml_ucs_is_miscellaneous_mathematical_symbols_b,
    },
    XmlUnicodeRange {
        rangename: c"MiscellaneousSymbols".as_ptr(),
        func: xml_ucs_is_miscellaneous_symbols,
    },
    XmlUnicodeRange {
        rangename: c"MiscellaneousSymbolsandArrows".as_ptr(),
        func: xml_ucs_is_miscellaneous_symbolsand_arrows,
    },
    XmlUnicodeRange {
        rangename: c"MiscellaneousTechnical".as_ptr(),
        func: xml_ucs_is_miscellaneous_technical,
    },
    XmlUnicodeRange {
        rangename: c"Mongolian".as_ptr(),
        func: xml_ucs_is_mongolian,
    },
    XmlUnicodeRange {
        rangename: c"MusicalSymbols".as_ptr(),
        func: xml_ucs_is_musical_symbols,
    },
    XmlUnicodeRange {
        rangename: c"Myanmar".as_ptr(),
        func: xml_ucs_is_myanmar,
    },
    XmlUnicodeRange {
        rangename: c"NumberForms".as_ptr(),
        func: xml_ucs_is_number_forms,
    },
    XmlUnicodeRange {
        rangename: c"Ogham".as_ptr(),
        func: xml_ucs_is_ogham,
    },
    XmlUnicodeRange {
        rangename: c"OldItalic".as_ptr(),
        func: xml_ucs_is_old_italic,
    },
    XmlUnicodeRange {
        rangename: c"OpticalCharacterRecognition".as_ptr(),
        func: xml_ucs_is_optical_character_recognition,
    },
    XmlUnicodeRange {
        rangename: c"Oriya".as_ptr(),
        func: xml_ucs_is_oriya,
    },
    XmlUnicodeRange {
        rangename: c"Osmanya".as_ptr(),
        func: xml_ucs_is_osmanya,
    },
    XmlUnicodeRange {
        rangename: c"PhoneticExtensions".as_ptr(),
        func: xml_ucs_is_phonetic_extensions,
    },
    XmlUnicodeRange {
        rangename: c"PrivateUse".as_ptr(),
        func: xml_ucs_is_private_use,
    },
    XmlUnicodeRange {
        rangename: c"PrivateUseArea".as_ptr(),
        func: xml_ucs_is_private_use_area,
    },
    XmlUnicodeRange {
        rangename: c"Runic".as_ptr(),
        func: xml_ucs_is_runic,
    },
    XmlUnicodeRange {
        rangename: c"Shavian".as_ptr(),
        func: xml_ucs_is_shavian,
    },
    XmlUnicodeRange {
        rangename: c"Sinhala".as_ptr(),
        func: xml_ucs_is_sinhala,
    },
    XmlUnicodeRange {
        rangename: c"SmallFormVariants".as_ptr(),
        func: xml_ucs_is_small_form_variants,
    },
    XmlUnicodeRange {
        rangename: c"SpacingModifierLetters".as_ptr(),
        func: xml_ucs_is_spacing_modifier_letters,
    },
    XmlUnicodeRange {
        rangename: c"Specials".as_ptr(),
        func: xml_ucs_is_specials,
    },
    XmlUnicodeRange {
        rangename: c"SuperscriptsandSubscripts".as_ptr(),
        func: xml_ucs_is_superscriptsand_subscripts,
    },
    XmlUnicodeRange {
        rangename: c"SupplementalArrows-A".as_ptr(),
        func: xml_ucs_is_supplemental_arrows_a,
    },
    XmlUnicodeRange {
        rangename: c"SupplementalArrows-B".as_ptr(),
        func: xml_ucs_is_supplemental_arrows_b,
    },
    XmlUnicodeRange {
        rangename: c"SupplementalMathematicalOperators".as_ptr(),
        func: xml_ucs_is_supplemental_mathematical_operators,
    },
    XmlUnicodeRange {
        rangename: c"SupplementaryPrivateUseArea-A".as_ptr(),
        func: xml_ucs_is_supplementary_private_use_area_a,
    },
    XmlUnicodeRange {
        rangename: c"SupplementaryPrivateUseArea-B".as_ptr(),
        func: xml_ucs_is_supplementary_private_use_area_b,
    },
    XmlUnicodeRange {
        rangename: c"Syriac".as_ptr(),
        func: xml_ucs_is_syriac,
    },
    XmlUnicodeRange {
        rangename: c"Tagalog".as_ptr(),
        func: xml_ucs_is_tagalog,
    },
    XmlUnicodeRange {
        rangename: c"Tagbanwa".as_ptr(),
        func: xml_ucs_is_tagbanwa,
    },
    XmlUnicodeRange {
        rangename: c"Tags".as_ptr(),
        func: xml_ucs_is_tags,
    },
    XmlUnicodeRange {
        rangename: c"TaiLe".as_ptr(),
        func: xml_ucs_is_tai_le,
    },
    XmlUnicodeRange {
        rangename: c"TaiXuanJingSymbols".as_ptr(),
        func: xml_ucs_is_tai_xuan_jing_symbols,
    },
    XmlUnicodeRange {
        rangename: c"Tamil".as_ptr(),
        func: xml_ucs_is_tamil,
    },
    XmlUnicodeRange {
        rangename: c"Telugu".as_ptr(),
        func: xml_ucs_is_telugu,
    },
    XmlUnicodeRange {
        rangename: c"Thaana".as_ptr(),
        func: xml_ucs_is_thaana,
    },
    XmlUnicodeRange {
        rangename: c"Thai".as_ptr(),
        func: xml_ucs_is_thai,
    },
    XmlUnicodeRange {
        rangename: c"Tibetan".as_ptr(),
        func: xml_ucs_is_tibetan,
    },
    XmlUnicodeRange {
        rangename: c"Ugaritic".as_ptr(),
        func: xml_ucs_is_ugaritic,
    },
    XmlUnicodeRange {
        rangename: c"UnifiedCanadianAboriginalSyllabics".as_ptr(),
        func: xml_ucs_is_unified_canadian_aboriginal_syllabics,
    },
    XmlUnicodeRange {
        rangename: c"VariationSelectors".as_ptr(),
        func: xml_ucs_is_variation_selectors,
    },
    XmlUnicodeRange {
        rangename: c"VariationSelectorsSupplement".as_ptr(),
        func: xml_ucs_is_variation_selectors_supplement,
    },
    XmlUnicodeRange {
        rangename: c"YiRadicals".as_ptr(),
        func: xml_ucs_is_yi_radicals,
    },
    XmlUnicodeRange {
        rangename: c"YiSyllables".as_ptr(),
        func: xml_ucs_is_yi_syllables,
    },
    XmlUnicodeRange {
        rangename: c"YijingHexagramSymbols".as_ptr(),
        func: xml_ucs_is_yijing_hexagram_symbols,
    },
]
.as_ptr();

const XML_UNICODE_CATS: *const XmlUnicodeRange = [
    XmlUnicodeRange {
        rangename: c"C".as_ptr(),
        func: xml_ucs_is_cat_c,
    },
    XmlUnicodeRange {
        rangename: c"Cc".as_ptr(),
        func: xml_ucs_is_cat_cc,
    },
    XmlUnicodeRange {
        rangename: c"Cf".as_ptr(),
        func: xml_ucs_is_cat_cf,
    },
    XmlUnicodeRange {
        rangename: c"Co".as_ptr(),
        func: xml_ucs_is_cat_co,
    },
    XmlUnicodeRange {
        rangename: c"Cs".as_ptr(),
        func: xml_ucs_is_cat_cs,
    },
    XmlUnicodeRange {
        rangename: c"L".as_ptr(),
        func: xml_ucs_is_cat_l,
    },
    XmlUnicodeRange {
        rangename: c"Ll".as_ptr(),
        func: xml_ucs_is_cat_ll,
    },
    XmlUnicodeRange {
        rangename: c"Lm".as_ptr(),
        func: xml_ucs_is_cat_lm,
    },
    XmlUnicodeRange {
        rangename: c"Lo".as_ptr(),
        func: xml_ucs_is_cat_lo,
    },
    XmlUnicodeRange {
        rangename: c"Lt".as_ptr(),
        func: xml_ucs_is_cat_lt,
    },
    XmlUnicodeRange {
        rangename: c"Lu".as_ptr(),
        func: xml_ucs_is_cat_lu,
    },
    XmlUnicodeRange {
        rangename: c"M".as_ptr(),
        func: xml_ucs_is_cat_m,
    },
    XmlUnicodeRange {
        rangename: c"Mc".as_ptr(),
        func: xml_ucs_is_cat_mc,
    },
    XmlUnicodeRange {
        rangename: c"Me".as_ptr(),
        func: xml_ucs_is_cat_me,
    },
    XmlUnicodeRange {
        rangename: c"Mn".as_ptr(),
        func: xml_ucs_is_cat_mn,
    },
    XmlUnicodeRange {
        rangename: c"N".as_ptr(),
        func: xml_ucs_is_cat_n,
    },
    XmlUnicodeRange {
        rangename: c"Nd".as_ptr(),
        func: xml_ucs_is_cat_nd,
    },
    XmlUnicodeRange {
        rangename: c"Nl".as_ptr(),
        func: xml_ucs_is_cat_nl,
    },
    XmlUnicodeRange {
        rangename: c"No".as_ptr(),
        func: xml_ucs_is_cat_no,
    },
    XmlUnicodeRange {
        rangename: c"P".as_ptr(),
        func: xml_ucs_is_cat_p,
    },
    XmlUnicodeRange {
        rangename: c"Pc".as_ptr(),
        func: xml_ucs_is_cat_pc,
    },
    XmlUnicodeRange {
        rangename: c"Pd".as_ptr(),
        func: xml_ucs_is_cat_pd,
    },
    XmlUnicodeRange {
        rangename: c"Pe".as_ptr(),
        func: xml_ucs_is_cat_pe,
    },
    XmlUnicodeRange {
        rangename: c"Pf".as_ptr(),
        func: xml_ucs_is_cat_pf,
    },
    XmlUnicodeRange {
        rangename: c"Pi".as_ptr(),
        func: xml_ucs_is_cat_pi,
    },
    XmlUnicodeRange {
        rangename: c"Po".as_ptr(),
        func: xml_ucs_is_cat_po,
    },
    XmlUnicodeRange {
        rangename: c"Ps".as_ptr(),
        func: xml_ucs_is_cat_ps,
    },
    XmlUnicodeRange {
        rangename: c"S".as_ptr(),
        func: xml_ucs_is_cat_s,
    },
    XmlUnicodeRange {
        rangename: c"Sc".as_ptr(),
        func: xml_ucs_is_cat_sc,
    },
    XmlUnicodeRange {
        rangename: c"Sk".as_ptr(),
        func: xml_ucs_is_cat_sk,
    },
    XmlUnicodeRange {
        rangename: c"Sm".as_ptr(),
        func: xml_ucs_is_cat_sm,
    },
    XmlUnicodeRange {
        rangename: c"So".as_ptr(),
        func: xml_ucs_is_cat_so,
    },
    XmlUnicodeRange {
        rangename: c"Z".as_ptr(),
        func: xml_ucs_is_cat_z,
    },
    XmlUnicodeRange {
        rangename: c"Zl".as_ptr(),
        func: xml_ucs_is_cat_zl,
    },
    XmlUnicodeRange {
        rangename: c"Zp".as_ptr(),
        func: xml_ucs_is_cat_zp,
    },
    XmlUnicodeRange {
        rangename: c"Zs".as_ptr(),
        func: xml_ucs_is_cat_zs,
    },
]
.as_ptr();

const XML_CS: &[XmlChSRange] = &[
    XmlChSRange { range: 0x0..=0x1f },
    XmlChSRange { range: 0x7f..=0x9f },
    XmlChSRange { range: 0xad..=0xad },
    XmlChSRange {
        range: 0x600..=0x603,
    },
    XmlChSRange {
        range: 0x6dd..=0x6dd,
    },
    XmlChSRange {
        range: 0x70f..=0x70f,
    },
    XmlChSRange {
        range: 0x17b4..=0x17b5,
    },
    XmlChSRange {
        range: 0x200b..=0x200f,
    },
    XmlChSRange {
        range: 0x202a..=0x202e,
    },
    XmlChSRange {
        range: 0x2060..=0x2063,
    },
    XmlChSRange {
        range: 0x206a..=0x206f,
    },
    XmlChSRange {
        range: 0xd800..=0xd800,
    },
    XmlChSRange {
        range: 0xdb7f..=0xdb80,
    },
    XmlChSRange {
        range: 0xdbff..=0xdc00,
    },
    XmlChSRange {
        range: 0xdfff..=0xe000,
    },
    XmlChSRange {
        range: 0xf8ff..=0xf8ff,
    },
    XmlChSRange {
        range: 0xfeff..=0xfeff,
    },
    XmlChSRange {
        range: 0xfff9..=0xfffb,
    },
];

const XML_CL: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x1d173..=0x1d17a,
    },
    XmlChLRange {
        range: 0xe0001..=0xe0001,
    },
    XmlChLRange {
        range: 0xe0020..=0xe007f,
    },
    XmlChLRange {
        range: 0xf0000..=0xf0000,
    },
    XmlChLRange {
        range: 0xffffd..=0xffffd,
    },
    XmlChLRange {
        range: 0x100000..=0x100000,
    },
    XmlChLRange {
        range: 0x10fffd..=0x10fffd,
    },
];

const XML_CG: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_CS,
    long_range: XML_CL,
};

const XML_CF_S: &[XmlChSRange] = &[
    XmlChSRange { range: 0xad..=0xad },
    XmlChSRange {
        range: 0x600..=0x603,
    },
    XmlChSRange {
        range: 0x6dd..=0x6dd,
    },
    XmlChSRange {
        range: 0x70f..=0x70f,
    },
    XmlChSRange {
        range: 0x17b4..=0x17b5,
    },
    XmlChSRange {
        range: 0x200b..=0x200f,
    },
    XmlChSRange {
        range: 0x202a..=0x202e,
    },
    XmlChSRange {
        range: 0x2060..=0x2063,
    },
    XmlChSRange {
        range: 0x206a..=0x206f,
    },
    XmlChSRange {
        range: 0xfeff..=0xfeff,
    },
    XmlChSRange {
        range: 0xfff9..=0xfffb,
    },
];

const XML_CF_L: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x1d173..=0x1d17a,
    },
    XmlChLRange {
        range: 0xe0001..=0xe0001,
    },
    XmlChLRange {
        range: 0xe0020..=0xe007f,
    },
];

const XML_CF_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_CF_S,
    long_range: XML_CF_L,
};

const XML_LS: &[XmlChSRange] = &[
    XmlChSRange { range: 0x41..=0x5a },
    XmlChSRange { range: 0x61..=0x7a },
    XmlChSRange { range: 0xaa..=0xaa },
    XmlChSRange { range: 0xb5..=0xb5 },
    XmlChSRange { range: 0xba..=0xba },
    XmlChSRange { range: 0xc0..=0xd6 },
    XmlChSRange { range: 0xd8..=0xf6 },
    XmlChSRange {
        range: 0xf8..=0x236,
    },
    XmlChSRange {
        range: 0x250..=0x2c1,
    },
    XmlChSRange {
        range: 0x2c6..=0x2d1,
    },
    XmlChSRange {
        range: 0x2e0..=0x2e4,
    },
    XmlChSRange {
        range: 0x2ee..=0x2ee,
    },
    XmlChSRange {
        range: 0x37a..=0x37a,
    },
    XmlChSRange {
        range: 0x386..=0x386,
    },
    XmlChSRange {
        range: 0x388..=0x38a,
    },
    XmlChSRange {
        range: 0x38c..=0x38c,
    },
    XmlChSRange {
        range: 0x38e..=0x3a1,
    },
    XmlChSRange {
        range: 0x3a3..=0x3ce,
    },
    XmlChSRange {
        range: 0x3d0..=0x3f5,
    },
    XmlChSRange {
        range: 0x3f7..=0x3fb,
    },
    XmlChSRange {
        range: 0x400..=0x481,
    },
    XmlChSRange {
        range: 0x48a..=0x4ce,
    },
    XmlChSRange {
        range: 0x4d0..=0x4f5,
    },
    XmlChSRange {
        range: 0x4f8..=0x4f9,
    },
    XmlChSRange {
        range: 0x500..=0x50f,
    },
    XmlChSRange {
        range: 0x531..=0x556,
    },
    XmlChSRange {
        range: 0x559..=0x559,
    },
    XmlChSRange {
        range: 0x561..=0x587,
    },
    XmlChSRange {
        range: 0x5d0..=0x5ea,
    },
    XmlChSRange {
        range: 0x5f0..=0x5f2,
    },
    XmlChSRange {
        range: 0x621..=0x63a,
    },
    XmlChSRange {
        range: 0x640..=0x64a,
    },
    XmlChSRange {
        range: 0x66e..=0x66f,
    },
    XmlChSRange {
        range: 0x671..=0x6d3,
    },
    XmlChSRange {
        range: 0x6d5..=0x6d5,
    },
    XmlChSRange {
        range: 0x6e5..=0x6e6,
    },
    XmlChSRange {
        range: 0x6ee..=0x6ef,
    },
    XmlChSRange {
        range: 0x6fa..=0x6fc,
    },
    XmlChSRange {
        range: 0x6ff..=0x6ff,
    },
    XmlChSRange {
        range: 0x710..=0x710,
    },
    XmlChSRange {
        range: 0x712..=0x72f,
    },
    XmlChSRange {
        range: 0x74d..=0x74f,
    },
    XmlChSRange {
        range: 0x780..=0x7a5,
    },
    XmlChSRange {
        range: 0x7b1..=0x7b1,
    },
    XmlChSRange {
        range: 0x904..=0x939,
    },
    XmlChSRange {
        range: 0x93d..=0x93d,
    },
    XmlChSRange {
        range: 0x950..=0x950,
    },
    XmlChSRange {
        range: 0x958..=0x961,
    },
    XmlChSRange {
        range: 0x985..=0x98c,
    },
    XmlChSRange {
        range: 0x98f..=0x990,
    },
    XmlChSRange {
        range: 0x993..=0x9a8,
    },
    XmlChSRange {
        range: 0x9aa..=0x9b0,
    },
    XmlChSRange {
        range: 0x9b2..=0x9b2,
    },
    XmlChSRange {
        range: 0x9b6..=0x9b9,
    },
    XmlChSRange {
        range: 0x9bd..=0x9bd,
    },
    XmlChSRange {
        range: 0x9dc..=0x9dd,
    },
    XmlChSRange {
        range: 0x9df..=0x9e1,
    },
    XmlChSRange {
        range: 0x9f0..=0x9f1,
    },
    XmlChSRange {
        range: 0xa05..=0xa0a,
    },
    XmlChSRange {
        range: 0xa0f..=0xa10,
    },
    XmlChSRange {
        range: 0xa13..=0xa28,
    },
    XmlChSRange {
        range: 0xa2a..=0xa30,
    },
    XmlChSRange {
        range: 0xa32..=0xa33,
    },
    XmlChSRange {
        range: 0xa35..=0xa36,
    },
    XmlChSRange {
        range: 0xa38..=0xa39,
    },
    XmlChSRange {
        range: 0xa59..=0xa5c,
    },
    XmlChSRange {
        range: 0xa5e..=0xa5e,
    },
    XmlChSRange {
        range: 0xa72..=0xa74,
    },
    XmlChSRange {
        range: 0xa85..=0xa8d,
    },
    XmlChSRange {
        range: 0xa8f..=0xa91,
    },
    XmlChSRange {
        range: 0xa93..=0xaa8,
    },
    XmlChSRange {
        range: 0xaaa..=0xab0,
    },
    XmlChSRange {
        range: 0xab2..=0xab3,
    },
    XmlChSRange {
        range: 0xab5..=0xab9,
    },
    XmlChSRange {
        range: 0xabd..=0xabd,
    },
    XmlChSRange {
        range: 0xad0..=0xad0,
    },
    XmlChSRange {
        range: 0xae0..=0xae1,
    },
    XmlChSRange {
        range: 0xb05..=0xb0c,
    },
    XmlChSRange {
        range: 0xb0f..=0xb10,
    },
    XmlChSRange {
        range: 0xb13..=0xb28,
    },
    XmlChSRange {
        range: 0xb2a..=0xb30,
    },
    XmlChSRange {
        range: 0xb32..=0xb33,
    },
    XmlChSRange {
        range: 0xb35..=0xb39,
    },
    XmlChSRange {
        range: 0xb3d..=0xb3d,
    },
    XmlChSRange {
        range: 0xb5c..=0xb5d,
    },
    XmlChSRange {
        range: 0xb5f..=0xb61,
    },
    XmlChSRange {
        range: 0xb71..=0xb71,
    },
    XmlChSRange {
        range: 0xb83..=0xb83,
    },
    XmlChSRange {
        range: 0xb85..=0xb8a,
    },
    XmlChSRange {
        range: 0xb8e..=0xb90,
    },
    XmlChSRange {
        range: 0xb92..=0xb95,
    },
    XmlChSRange {
        range: 0xb99..=0xb9a,
    },
    XmlChSRange {
        range: 0xb9c..=0xb9c,
    },
    XmlChSRange {
        range: 0xb9e..=0xb9f,
    },
    XmlChSRange {
        range: 0xba3..=0xba4,
    },
    XmlChSRange {
        range: 0xba8..=0xbaa,
    },
    XmlChSRange {
        range: 0xbae..=0xbb5,
    },
    XmlChSRange {
        range: 0xbb7..=0xbb9,
    },
    XmlChSRange {
        range: 0xc05..=0xc0c,
    },
    XmlChSRange {
        range: 0xc0e..=0xc10,
    },
    XmlChSRange {
        range: 0xc12..=0xc28,
    },
    XmlChSRange {
        range: 0xc2a..=0xc33,
    },
    XmlChSRange {
        range: 0xc35..=0xc39,
    },
    XmlChSRange {
        range: 0xc60..=0xc61,
    },
    XmlChSRange {
        range: 0xc85..=0xc8c,
    },
    XmlChSRange {
        range: 0xc8e..=0xc90,
    },
    XmlChSRange {
        range: 0xc92..=0xca8,
    },
    XmlChSRange {
        range: 0xcaa..=0xcb3,
    },
    XmlChSRange {
        range: 0xcb5..=0xcb9,
    },
    XmlChSRange {
        range: 0xcbd..=0xcbd,
    },
    XmlChSRange {
        range: 0xcde..=0xcde,
    },
    XmlChSRange {
        range: 0xce0..=0xce1,
    },
    XmlChSRange {
        range: 0xd05..=0xd0c,
    },
    XmlChSRange {
        range: 0xd0e..=0xd10,
    },
    XmlChSRange {
        range: 0xd12..=0xd28,
    },
    XmlChSRange {
        range: 0xd2a..=0xd39,
    },
    XmlChSRange {
        range: 0xd60..=0xd61,
    },
    XmlChSRange {
        range: 0xd85..=0xd96,
    },
    XmlChSRange {
        range: 0xd9a..=0xdb1,
    },
    XmlChSRange {
        range: 0xdb3..=0xdbb,
    },
    XmlChSRange {
        range: 0xdbd..=0xdbd,
    },
    XmlChSRange {
        range: 0xdc0..=0xdc6,
    },
    XmlChSRange {
        range: 0xe01..=0xe30,
    },
    XmlChSRange {
        range: 0xe32..=0xe33,
    },
    XmlChSRange {
        range: 0xe40..=0xe46,
    },
    XmlChSRange {
        range: 0xe81..=0xe82,
    },
    XmlChSRange {
        range: 0xe84..=0xe84,
    },
    XmlChSRange {
        range: 0xe87..=0xe88,
    },
    XmlChSRange {
        range: 0xe8a..=0xe8a,
    },
    XmlChSRange {
        range: 0xe8d..=0xe8d,
    },
    XmlChSRange {
        range: 0xe94..=0xe97,
    },
    XmlChSRange {
        range: 0xe99..=0xe9f,
    },
    XmlChSRange {
        range: 0xea1..=0xea3,
    },
    XmlChSRange {
        range: 0xea5..=0xea5,
    },
    XmlChSRange {
        range: 0xea7..=0xea7,
    },
    XmlChSRange {
        range: 0xeaa..=0xeab,
    },
    XmlChSRange {
        range: 0xead..=0xeb0,
    },
    XmlChSRange {
        range: 0xeb2..=0xeb3,
    },
    XmlChSRange {
        range: 0xebd..=0xebd,
    },
    XmlChSRange {
        range: 0xec0..=0xec4,
    },
    XmlChSRange {
        range: 0xec6..=0xec6,
    },
    XmlChSRange {
        range: 0xedc..=0xedd,
    },
    XmlChSRange {
        range: 0xf00..=0xf00,
    },
    XmlChSRange {
        range: 0xf40..=0xf47,
    },
    XmlChSRange {
        range: 0xf49..=0xf6a,
    },
    XmlChSRange {
        range: 0xf88..=0xf8b,
    },
    XmlChSRange {
        range: 0x1000..=0x1021,
    },
    XmlChSRange {
        range: 0x1023..=0x1027,
    },
    XmlChSRange {
        range: 0x1029..=0x102a,
    },
    XmlChSRange {
        range: 0x1050..=0x1055,
    },
    XmlChSRange {
        range: 0x10a0..=0x10c5,
    },
    XmlChSRange {
        range: 0x10d0..=0x10f8,
    },
    XmlChSRange {
        range: 0x1100..=0x1159,
    },
    XmlChSRange {
        range: 0x115f..=0x11a2,
    },
    XmlChSRange {
        range: 0x11a8..=0x11f9,
    },
    XmlChSRange {
        range: 0x1200..=0x1206,
    },
    XmlChSRange {
        range: 0x1208..=0x1246,
    },
    XmlChSRange {
        range: 0x1248..=0x1248,
    },
    XmlChSRange {
        range: 0x124a..=0x124d,
    },
    XmlChSRange {
        range: 0x1250..=0x1256,
    },
    XmlChSRange {
        range: 0x1258..=0x1258,
    },
    XmlChSRange {
        range: 0x125a..=0x125d,
    },
    XmlChSRange {
        range: 0x1260..=0x1286,
    },
    XmlChSRange {
        range: 0x1288..=0x1288,
    },
    XmlChSRange {
        range: 0x128a..=0x128d,
    },
    XmlChSRange {
        range: 0x1290..=0x12ae,
    },
    XmlChSRange {
        range: 0x12b0..=0x12b0,
    },
    XmlChSRange {
        range: 0x12b2..=0x12b5,
    },
    XmlChSRange {
        range: 0x12b8..=0x12be,
    },
    XmlChSRange {
        range: 0x12c0..=0x12c0,
    },
    XmlChSRange {
        range: 0x12c2..=0x12c5,
    },
    XmlChSRange {
        range: 0x12c8..=0x12ce,
    },
    XmlChSRange {
        range: 0x12d0..=0x12d6,
    },
    XmlChSRange {
        range: 0x12d8..=0x12ee,
    },
    XmlChSRange {
        range: 0x12f0..=0x130e,
    },
    XmlChSRange {
        range: 0x1310..=0x1310,
    },
    XmlChSRange {
        range: 0x1312..=0x1315,
    },
    XmlChSRange {
        range: 0x1318..=0x131e,
    },
    XmlChSRange {
        range: 0x1320..=0x1346,
    },
    XmlChSRange {
        range: 0x1348..=0x135a,
    },
    XmlChSRange {
        range: 0x13a0..=0x13f4,
    },
    XmlChSRange {
        range: 0x1401..=0x166c,
    },
    XmlChSRange {
        range: 0x166f..=0x1676,
    },
    XmlChSRange {
        range: 0x1681..=0x169a,
    },
    XmlChSRange {
        range: 0x16a0..=0x16ea,
    },
    XmlChSRange {
        range: 0x1700..=0x170c,
    },
    XmlChSRange {
        range: 0x170e..=0x1711,
    },
    XmlChSRange {
        range: 0x1720..=0x1731,
    },
    XmlChSRange {
        range: 0x1740..=0x1751,
    },
    XmlChSRange {
        range: 0x1760..=0x176c,
    },
    XmlChSRange {
        range: 0x176e..=0x1770,
    },
    XmlChSRange {
        range: 0x1780..=0x17b3,
    },
    XmlChSRange {
        range: 0x17d7..=0x17d7,
    },
    XmlChSRange {
        range: 0x17dc..=0x17dc,
    },
    XmlChSRange {
        range: 0x1820..=0x1877,
    },
    XmlChSRange {
        range: 0x1880..=0x18a8,
    },
    XmlChSRange {
        range: 0x1900..=0x191c,
    },
    XmlChSRange {
        range: 0x1950..=0x196d,
    },
    XmlChSRange {
        range: 0x1970..=0x1974,
    },
    XmlChSRange {
        range: 0x1d00..=0x1d6b,
    },
    XmlChSRange {
        range: 0x1e00..=0x1e9b,
    },
    XmlChSRange {
        range: 0x1ea0..=0x1ef9,
    },
    XmlChSRange {
        range: 0x1f00..=0x1f15,
    },
    XmlChSRange {
        range: 0x1f18..=0x1f1d,
    },
    XmlChSRange {
        range: 0x1f20..=0x1f45,
    },
    XmlChSRange {
        range: 0x1f48..=0x1f4d,
    },
    XmlChSRange {
        range: 0x1f50..=0x1f57,
    },
    XmlChSRange {
        range: 0x1f59..=0x1f59,
    },
    XmlChSRange {
        range: 0x1f5b..=0x1f5b,
    },
    XmlChSRange {
        range: 0x1f5d..=0x1f5d,
    },
    XmlChSRange {
        range: 0x1f5f..=0x1f7d,
    },
    XmlChSRange {
        range: 0x1f80..=0x1fb4,
    },
    XmlChSRange {
        range: 0x1fb6..=0x1fbc,
    },
    XmlChSRange {
        range: 0x1fbe..=0x1fbe,
    },
    XmlChSRange {
        range: 0x1fc2..=0x1fc4,
    },
    XmlChSRange {
        range: 0x1fc6..=0x1fcc,
    },
    XmlChSRange {
        range: 0x1fd0..=0x1fd3,
    },
    XmlChSRange {
        range: 0x1fd6..=0x1fdb,
    },
    XmlChSRange {
        range: 0x1fe0..=0x1fec,
    },
    XmlChSRange {
        range: 0x1ff2..=0x1ff4,
    },
    XmlChSRange {
        range: 0x1ff6..=0x1ffc,
    },
    XmlChSRange {
        range: 0x2071..=0x2071,
    },
    XmlChSRange {
        range: 0x207f..=0x207f,
    },
    XmlChSRange {
        range: 0x2102..=0x2102,
    },
    XmlChSRange {
        range: 0x2107..=0x2107,
    },
    XmlChSRange {
        range: 0x210a..=0x2113,
    },
    XmlChSRange {
        range: 0x2115..=0x2115,
    },
    XmlChSRange {
        range: 0x2119..=0x211d,
    },
    XmlChSRange {
        range: 0x2124..=0x2124,
    },
    XmlChSRange {
        range: 0x2126..=0x2126,
    },
    XmlChSRange {
        range: 0x2128..=0x2128,
    },
    XmlChSRange {
        range: 0x212a..=0x212d,
    },
    XmlChSRange {
        range: 0x212f..=0x2131,
    },
    XmlChSRange {
        range: 0x2133..=0x2139,
    },
    XmlChSRange {
        range: 0x213d..=0x213f,
    },
    XmlChSRange {
        range: 0x2145..=0x2149,
    },
    XmlChSRange {
        range: 0x3005..=0x3006,
    },
    XmlChSRange {
        range: 0x3031..=0x3035,
    },
    XmlChSRange {
        range: 0x303b..=0x303c,
    },
    XmlChSRange {
        range: 0x3041..=0x3096,
    },
    XmlChSRange {
        range: 0x309d..=0x309f,
    },
    XmlChSRange {
        range: 0x30a1..=0x30fa,
    },
    XmlChSRange {
        range: 0x30fc..=0x30ff,
    },
    XmlChSRange {
        range: 0x3105..=0x312c,
    },
    XmlChSRange {
        range: 0x3131..=0x318e,
    },
    XmlChSRange {
        range: 0x31a0..=0x31b7,
    },
    XmlChSRange {
        range: 0x31f0..=0x31ff,
    },
    XmlChSRange {
        range: 0x3400..=0x3400,
    },
    XmlChSRange {
        range: 0x4db5..=0x4db5,
    },
    XmlChSRange {
        range: 0x4e00..=0x4e00,
    },
    XmlChSRange {
        range: 0x9fa5..=0x9fa5,
    },
    XmlChSRange {
        range: 0xa000..=0xa48c,
    },
    XmlChSRange {
        range: 0xac00..=0xac00,
    },
    XmlChSRange {
        range: 0xd7a3..=0xd7a3,
    },
    XmlChSRange {
        range: 0xf900..=0xfa2d,
    },
    XmlChSRange {
        range: 0xfa30..=0xfa6a,
    },
    XmlChSRange {
        range: 0xfb00..=0xfb06,
    },
    XmlChSRange {
        range: 0xfb13..=0xfb17,
    },
    XmlChSRange {
        range: 0xfb1d..=0xfb1d,
    },
    XmlChSRange {
        range: 0xfb1f..=0xfb28,
    },
    XmlChSRange {
        range: 0xfb2a..=0xfb36,
    },
    XmlChSRange {
        range: 0xfb38..=0xfb3c,
    },
    XmlChSRange {
        range: 0xfb3e..=0xfb3e,
    },
    XmlChSRange {
        range: 0xfb40..=0xfb41,
    },
    XmlChSRange {
        range: 0xfb43..=0xfb44,
    },
    XmlChSRange {
        range: 0xfb46..=0xfbb1,
    },
    XmlChSRange {
        range: 0xfbd3..=0xfd3d,
    },
    XmlChSRange {
        range: 0xfd50..=0xfd8f,
    },
    XmlChSRange {
        range: 0xfd92..=0xfdc7,
    },
    XmlChSRange {
        range: 0xfdf0..=0xfdfb,
    },
    XmlChSRange {
        range: 0xfe70..=0xfe74,
    },
    XmlChSRange {
        range: 0xfe76..=0xfefc,
    },
    XmlChSRange {
        range: 0xff21..=0xff3a,
    },
    XmlChSRange {
        range: 0xff41..=0xff5a,
    },
    XmlChSRange {
        range: 0xff66..=0xffbe,
    },
    XmlChSRange {
        range: 0xffc2..=0xffc7,
    },
    XmlChSRange {
        range: 0xffca..=0xffcf,
    },
    XmlChSRange {
        range: 0xffd2..=0xffd7,
    },
    XmlChSRange {
        range: 0xffda..=0xffdc,
    },
];

const XML_LL: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x10000..=0x1000b,
    },
    XmlChLRange {
        range: 0x1000d..=0x10026,
    },
    XmlChLRange {
        range: 0x10028..=0x1003a,
    },
    XmlChLRange {
        range: 0x1003c..=0x1003d,
    },
    XmlChLRange {
        range: 0x1003f..=0x1004d,
    },
    XmlChLRange {
        range: 0x10050..=0x1005d,
    },
    XmlChLRange {
        range: 0x10080..=0x100fa,
    },
    XmlChLRange {
        range: 0x10300..=0x1031e,
    },
    XmlChLRange {
        range: 0x10330..=0x10349,
    },
    XmlChLRange {
        range: 0x10380..=0x1039d,
    },
    XmlChLRange {
        range: 0x10400..=0x1049d,
    },
    XmlChLRange {
        range: 0x10800..=0x10805,
    },
    XmlChLRange {
        range: 0x10808..=0x10808,
    },
    XmlChLRange {
        range: 0x1080a..=0x10835,
    },
    XmlChLRange {
        range: 0x10837..=0x10838,
    },
    XmlChLRange {
        range: 0x1083c..=0x1083c,
    },
    XmlChLRange {
        range: 0x1083f..=0x1083f,
    },
    XmlChLRange {
        range: 0x1d400..=0x1d454,
    },
    XmlChLRange {
        range: 0x1d456..=0x1d49c,
    },
    XmlChLRange {
        range: 0x1d49e..=0x1d49f,
    },
    XmlChLRange {
        range: 0x1d4a2..=0x1d4a2,
    },
    XmlChLRange {
        range: 0x1d4a5..=0x1d4a6,
    },
    XmlChLRange {
        range: 0x1d4a9..=0x1d4ac,
    },
    XmlChLRange {
        range: 0x1d4ae..=0x1d4b9,
    },
    XmlChLRange {
        range: 0x1d4bb..=0x1d4bb,
    },
    XmlChLRange {
        range: 0x1d4bd..=0x1d4c3,
    },
    XmlChLRange {
        range: 0x1d4c5..=0x1d505,
    },
    XmlChLRange {
        range: 0x1d507..=0x1d50a,
    },
    XmlChLRange {
        range: 0x1d50d..=0x1d514,
    },
    XmlChLRange {
        range: 0x1d516..=0x1d51c,
    },
    XmlChLRange {
        range: 0x1d51e..=0x1d539,
    },
    XmlChLRange {
        range: 0x1d53b..=0x1d53e,
    },
    XmlChLRange {
        range: 0x1d540..=0x1d544,
    },
    XmlChLRange {
        range: 0x1d546..=0x1d546,
    },
    XmlChLRange {
        range: 0x1d54a..=0x1d550,
    },
    XmlChLRange {
        range: 0x1d552..=0x1d6a3,
    },
    XmlChLRange {
        range: 0x1d6a8..=0x1d6c0,
    },
    XmlChLRange {
        range: 0x1d6c2..=0x1d6da,
    },
    XmlChLRange {
        range: 0x1d6dc..=0x1d6fa,
    },
    XmlChLRange {
        range: 0x1d6fc..=0x1d714,
    },
    XmlChLRange {
        range: 0x1d716..=0x1d734,
    },
    XmlChLRange {
        range: 0x1d736..=0x1d74e,
    },
    XmlChLRange {
        range: 0x1d750..=0x1d76e,
    },
    XmlChLRange {
        range: 0x1d770..=0x1d788,
    },
    XmlChLRange {
        range: 0x1d78a..=0x1d7a8,
    },
    XmlChLRange {
        range: 0x1d7aa..=0x1d7c2,
    },
    XmlChLRange {
        range: 0x1d7c4..=0x1d7c9,
    },
    XmlChLRange {
        range: 0x20000..=0x20000,
    },
    XmlChLRange {
        range: 0x2a6d6..=0x2a6d6,
    },
    XmlChLRange {
        range: 0x2f800..=0x2fa1d,
    },
];

const XML_LG: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_LS,
    long_range: XML_LL,
};

const XML_LL_S: &[XmlChSRange] = &[
    XmlChSRange { range: 0x61..=0x7a },
    XmlChSRange { range: 0xaa..=0xaa },
    XmlChSRange { range: 0xb5..=0xb5 },
    XmlChSRange { range: 0xba..=0xba },
    XmlChSRange { range: 0xdf..=0xf6 },
    XmlChSRange { range: 0xf8..=0xff },
    XmlChSRange {
        range: 0x101..=0x101,
    },
    XmlChSRange {
        range: 0x103..=0x103,
    },
    XmlChSRange {
        range: 0x105..=0x105,
    },
    XmlChSRange {
        range: 0x107..=0x107,
    },
    XmlChSRange {
        range: 0x109..=0x109,
    },
    XmlChSRange {
        range: 0x10b..=0x10b,
    },
    XmlChSRange {
        range: 0x10d..=0x10d,
    },
    XmlChSRange {
        range: 0x10f..=0x10f,
    },
    XmlChSRange {
        range: 0x111..=0x111,
    },
    XmlChSRange {
        range: 0x113..=0x113,
    },
    XmlChSRange {
        range: 0x115..=0x115,
    },
    XmlChSRange {
        range: 0x117..=0x117,
    },
    XmlChSRange {
        range: 0x119..=0x119,
    },
    XmlChSRange {
        range: 0x11b..=0x11b,
    },
    XmlChSRange {
        range: 0x11d..=0x11d,
    },
    XmlChSRange {
        range: 0x11f..=0x11f,
    },
    XmlChSRange {
        range: 0x121..=0x121,
    },
    XmlChSRange {
        range: 0x123..=0x123,
    },
    XmlChSRange {
        range: 0x125..=0x125,
    },
    XmlChSRange {
        range: 0x127..=0x127,
    },
    XmlChSRange {
        range: 0x129..=0x129,
    },
    XmlChSRange {
        range: 0x12b..=0x12b,
    },
    XmlChSRange {
        range: 0x12d..=0x12d,
    },
    XmlChSRange {
        range: 0x12f..=0x12f,
    },
    XmlChSRange {
        range: 0x131..=0x131,
    },
    XmlChSRange {
        range: 0x133..=0x133,
    },
    XmlChSRange {
        range: 0x135..=0x135,
    },
    XmlChSRange {
        range: 0x137..=0x138,
    },
    XmlChSRange {
        range: 0x13a..=0x13a,
    },
    XmlChSRange {
        range: 0x13c..=0x13c,
    },
    XmlChSRange {
        range: 0x13e..=0x13e,
    },
    XmlChSRange {
        range: 0x140..=0x140,
    },
    XmlChSRange {
        range: 0x142..=0x142,
    },
    XmlChSRange {
        range: 0x144..=0x144,
    },
    XmlChSRange {
        range: 0x146..=0x146,
    },
    XmlChSRange {
        range: 0x148..=0x149,
    },
    XmlChSRange {
        range: 0x14b..=0x14b,
    },
    XmlChSRange {
        range: 0x14d..=0x14d,
    },
    XmlChSRange {
        range: 0x14f..=0x14f,
    },
    XmlChSRange {
        range: 0x151..=0x151,
    },
    XmlChSRange {
        range: 0x153..=0x153,
    },
    XmlChSRange {
        range: 0x155..=0x155,
    },
    XmlChSRange {
        range: 0x157..=0x157,
    },
    XmlChSRange {
        range: 0x159..=0x159,
    },
    XmlChSRange {
        range: 0x15b..=0x15b,
    },
    XmlChSRange {
        range: 0x15d..=0x15d,
    },
    XmlChSRange {
        range: 0x15f..=0x15f,
    },
    XmlChSRange {
        range: 0x161..=0x161,
    },
    XmlChSRange {
        range: 0x163..=0x163,
    },
    XmlChSRange {
        range: 0x165..=0x165,
    },
    XmlChSRange {
        range: 0x167..=0x167,
    },
    XmlChSRange {
        range: 0x169..=0x169,
    },
    XmlChSRange {
        range: 0x16b..=0x16b,
    },
    XmlChSRange {
        range: 0x16d..=0x16d,
    },
    XmlChSRange {
        range: 0x16f..=0x16f,
    },
    XmlChSRange {
        range: 0x171..=0x171,
    },
    XmlChSRange {
        range: 0x173..=0x173,
    },
    XmlChSRange {
        range: 0x175..=0x175,
    },
    XmlChSRange {
        range: 0x177..=0x177,
    },
    XmlChSRange {
        range: 0x17a..=0x17a,
    },
    XmlChSRange {
        range: 0x17c..=0x17c,
    },
    XmlChSRange {
        range: 0x17e..=0x180,
    },
    XmlChSRange {
        range: 0x183..=0x183,
    },
    XmlChSRange {
        range: 0x185..=0x185,
    },
    XmlChSRange {
        range: 0x188..=0x188,
    },
    XmlChSRange {
        range: 0x18c..=0x18d,
    },
    XmlChSRange {
        range: 0x192..=0x192,
    },
    XmlChSRange {
        range: 0x195..=0x195,
    },
    XmlChSRange {
        range: 0x199..=0x19b,
    },
    XmlChSRange {
        range: 0x19e..=0x19e,
    },
    XmlChSRange {
        range: 0x1a1..=0x1a1,
    },
    XmlChSRange {
        range: 0x1a3..=0x1a3,
    },
    XmlChSRange {
        range: 0x1a5..=0x1a5,
    },
    XmlChSRange {
        range: 0x1a8..=0x1a8,
    },
    XmlChSRange {
        range: 0x1aa..=0x1ab,
    },
    XmlChSRange {
        range: 0x1ad..=0x1ad,
    },
    XmlChSRange {
        range: 0x1b0..=0x1b0,
    },
    XmlChSRange {
        range: 0x1b4..=0x1b4,
    },
    XmlChSRange {
        range: 0x1b6..=0x1b6,
    },
    XmlChSRange {
        range: 0x1b9..=0x1ba,
    },
    XmlChSRange {
        range: 0x1bd..=0x1bf,
    },
    XmlChSRange {
        range: 0x1c6..=0x1c6,
    },
    XmlChSRange {
        range: 0x1c9..=0x1c9,
    },
    XmlChSRange {
        range: 0x1cc..=0x1cc,
    },
    XmlChSRange {
        range: 0x1ce..=0x1ce,
    },
    XmlChSRange {
        range: 0x1d0..=0x1d0,
    },
    XmlChSRange {
        range: 0x1d2..=0x1d2,
    },
    XmlChSRange {
        range: 0x1d4..=0x1d4,
    },
    XmlChSRange {
        range: 0x1d6..=0x1d6,
    },
    XmlChSRange {
        range: 0x1d8..=0x1d8,
    },
    XmlChSRange {
        range: 0x1da..=0x1da,
    },
    XmlChSRange {
        range: 0x1dc..=0x1dd,
    },
    XmlChSRange {
        range: 0x1df..=0x1df,
    },
    XmlChSRange {
        range: 0x1e1..=0x1e1,
    },
    XmlChSRange {
        range: 0x1e3..=0x1e3,
    },
    XmlChSRange {
        range: 0x1e5..=0x1e5,
    },
    XmlChSRange {
        range: 0x1e7..=0x1e7,
    },
    XmlChSRange {
        range: 0x1e9..=0x1e9,
    },
    XmlChSRange {
        range: 0x1eb..=0x1eb,
    },
    XmlChSRange {
        range: 0x1ed..=0x1ed,
    },
    XmlChSRange {
        range: 0x1ef..=0x1f0,
    },
    XmlChSRange {
        range: 0x1f3..=0x1f3,
    },
    XmlChSRange {
        range: 0x1f5..=0x1f5,
    },
    XmlChSRange {
        range: 0x1f9..=0x1f9,
    },
    XmlChSRange {
        range: 0x1fb..=0x1fb,
    },
    XmlChSRange {
        range: 0x1fd..=0x1fd,
    },
    XmlChSRange {
        range: 0x1ff..=0x1ff,
    },
    XmlChSRange {
        range: 0x201..=0x201,
    },
    XmlChSRange {
        range: 0x203..=0x203,
    },
    XmlChSRange {
        range: 0x205..=0x205,
    },
    XmlChSRange {
        range: 0x207..=0x207,
    },
    XmlChSRange {
        range: 0x209..=0x209,
    },
    XmlChSRange {
        range: 0x20b..=0x20b,
    },
    XmlChSRange {
        range: 0x20d..=0x20d,
    },
    XmlChSRange {
        range: 0x20f..=0x20f,
    },
    XmlChSRange {
        range: 0x211..=0x211,
    },
    XmlChSRange {
        range: 0x213..=0x213,
    },
    XmlChSRange {
        range: 0x215..=0x215,
    },
    XmlChSRange {
        range: 0x217..=0x217,
    },
    XmlChSRange {
        range: 0x219..=0x219,
    },
    XmlChSRange {
        range: 0x21b..=0x21b,
    },
    XmlChSRange {
        range: 0x21d..=0x21d,
    },
    XmlChSRange {
        range: 0x21f..=0x21f,
    },
    XmlChSRange {
        range: 0x221..=0x221,
    },
    XmlChSRange {
        range: 0x223..=0x223,
    },
    XmlChSRange {
        range: 0x225..=0x225,
    },
    XmlChSRange {
        range: 0x227..=0x227,
    },
    XmlChSRange {
        range: 0x229..=0x229,
    },
    XmlChSRange {
        range: 0x22b..=0x22b,
    },
    XmlChSRange {
        range: 0x22d..=0x22d,
    },
    XmlChSRange {
        range: 0x22f..=0x22f,
    },
    XmlChSRange {
        range: 0x231..=0x231,
    },
    XmlChSRange {
        range: 0x233..=0x236,
    },
    XmlChSRange {
        range: 0x250..=0x2af,
    },
    XmlChSRange {
        range: 0x390..=0x390,
    },
    XmlChSRange {
        range: 0x3ac..=0x3ce,
    },
    XmlChSRange {
        range: 0x3d0..=0x3d1,
    },
    XmlChSRange {
        range: 0x3d5..=0x3d7,
    },
    XmlChSRange {
        range: 0x3d9..=0x3d9,
    },
    XmlChSRange {
        range: 0x3db..=0x3db,
    },
    XmlChSRange {
        range: 0x3dd..=0x3dd,
    },
    XmlChSRange {
        range: 0x3df..=0x3df,
    },
    XmlChSRange {
        range: 0x3e1..=0x3e1,
    },
    XmlChSRange {
        range: 0x3e3..=0x3e3,
    },
    XmlChSRange {
        range: 0x3e5..=0x3e5,
    },
    XmlChSRange {
        range: 0x3e7..=0x3e7,
    },
    XmlChSRange {
        range: 0x3e9..=0x3e9,
    },
    XmlChSRange {
        range: 0x3eb..=0x3eb,
    },
    XmlChSRange {
        range: 0x3ed..=0x3ed,
    },
    XmlChSRange {
        range: 0x3ef..=0x3f3,
    },
    XmlChSRange {
        range: 0x3f5..=0x3f5,
    },
    XmlChSRange {
        range: 0x3f8..=0x3f8,
    },
    XmlChSRange {
        range: 0x3fb..=0x3fb,
    },
    XmlChSRange {
        range: 0x430..=0x45f,
    },
    XmlChSRange {
        range: 0x461..=0x461,
    },
    XmlChSRange {
        range: 0x463..=0x463,
    },
    XmlChSRange {
        range: 0x465..=0x465,
    },
    XmlChSRange {
        range: 0x467..=0x467,
    },
    XmlChSRange {
        range: 0x469..=0x469,
    },
    XmlChSRange {
        range: 0x46b..=0x46b,
    },
    XmlChSRange {
        range: 0x46d..=0x46d,
    },
    XmlChSRange {
        range: 0x46f..=0x46f,
    },
    XmlChSRange {
        range: 0x471..=0x471,
    },
    XmlChSRange {
        range: 0x473..=0x473,
    },
    XmlChSRange {
        range: 0x475..=0x475,
    },
    XmlChSRange {
        range: 0x477..=0x477,
    },
    XmlChSRange {
        range: 0x479..=0x479,
    },
    XmlChSRange {
        range: 0x47b..=0x47b,
    },
    XmlChSRange {
        range: 0x47d..=0x47d,
    },
    XmlChSRange {
        range: 0x47f..=0x47f,
    },
    XmlChSRange {
        range: 0x481..=0x481,
    },
    XmlChSRange {
        range: 0x48b..=0x48b,
    },
    XmlChSRange {
        range: 0x48d..=0x48d,
    },
    XmlChSRange {
        range: 0x48f..=0x48f,
    },
    XmlChSRange {
        range: 0x491..=0x491,
    },
    XmlChSRange {
        range: 0x493..=0x493,
    },
    XmlChSRange {
        range: 0x495..=0x495,
    },
    XmlChSRange {
        range: 0x497..=0x497,
    },
    XmlChSRange {
        range: 0x499..=0x499,
    },
    XmlChSRange {
        range: 0x49b..=0x49b,
    },
    XmlChSRange {
        range: 0x49d..=0x49d,
    },
    XmlChSRange {
        range: 0x49f..=0x49f,
    },
    XmlChSRange {
        range: 0x4a1..=0x4a1,
    },
    XmlChSRange {
        range: 0x4a3..=0x4a3,
    },
    XmlChSRange {
        range: 0x4a5..=0x4a5,
    },
    XmlChSRange {
        range: 0x4a7..=0x4a7,
    },
    XmlChSRange {
        range: 0x4a9..=0x4a9,
    },
    XmlChSRange {
        range: 0x4ab..=0x4ab,
    },
    XmlChSRange {
        range: 0x4ad..=0x4ad,
    },
    XmlChSRange {
        range: 0x4af..=0x4af,
    },
    XmlChSRange {
        range: 0x4b1..=0x4b1,
    },
    XmlChSRange {
        range: 0x4b3..=0x4b3,
    },
    XmlChSRange {
        range: 0x4b5..=0x4b5,
    },
    XmlChSRange {
        range: 0x4b7..=0x4b7,
    },
    XmlChSRange {
        range: 0x4b9..=0x4b9,
    },
    XmlChSRange {
        range: 0x4bb..=0x4bb,
    },
    XmlChSRange {
        range: 0x4bd..=0x4bd,
    },
    XmlChSRange {
        range: 0x4bf..=0x4bf,
    },
    XmlChSRange {
        range: 0x4c2..=0x4c2,
    },
    XmlChSRange {
        range: 0x4c4..=0x4c4,
    },
    XmlChSRange {
        range: 0x4c6..=0x4c6,
    },
    XmlChSRange {
        range: 0x4c8..=0x4c8,
    },
    XmlChSRange {
        range: 0x4ca..=0x4ca,
    },
    XmlChSRange {
        range: 0x4cc..=0x4cc,
    },
    XmlChSRange {
        range: 0x4ce..=0x4ce,
    },
    XmlChSRange {
        range: 0x4d1..=0x4d1,
    },
    XmlChSRange {
        range: 0x4d3..=0x4d3,
    },
    XmlChSRange {
        range: 0x4d5..=0x4d5,
    },
    XmlChSRange {
        range: 0x4d7..=0x4d7,
    },
    XmlChSRange {
        range: 0x4d9..=0x4d9,
    },
    XmlChSRange {
        range: 0x4db..=0x4db,
    },
    XmlChSRange {
        range: 0x4dd..=0x4dd,
    },
    XmlChSRange {
        range: 0x4df..=0x4df,
    },
    XmlChSRange {
        range: 0x4e1..=0x4e1,
    },
    XmlChSRange {
        range: 0x4e3..=0x4e3,
    },
    XmlChSRange {
        range: 0x4e5..=0x4e5,
    },
    XmlChSRange {
        range: 0x4e7..=0x4e7,
    },
    XmlChSRange {
        range: 0x4e9..=0x4e9,
    },
    XmlChSRange {
        range: 0x4eb..=0x4eb,
    },
    XmlChSRange {
        range: 0x4ed..=0x4ed,
    },
    XmlChSRange {
        range: 0x4ef..=0x4ef,
    },
    XmlChSRange {
        range: 0x4f1..=0x4f1,
    },
    XmlChSRange {
        range: 0x4f3..=0x4f3,
    },
    XmlChSRange {
        range: 0x4f5..=0x4f5,
    },
    XmlChSRange {
        range: 0x4f9..=0x4f9,
    },
    XmlChSRange {
        range: 0x501..=0x501,
    },
    XmlChSRange {
        range: 0x503..=0x503,
    },
    XmlChSRange {
        range: 0x505..=0x505,
    },
    XmlChSRange {
        range: 0x507..=0x507,
    },
    XmlChSRange {
        range: 0x509..=0x509,
    },
    XmlChSRange {
        range: 0x50b..=0x50b,
    },
    XmlChSRange {
        range: 0x50d..=0x50d,
    },
    XmlChSRange {
        range: 0x50f..=0x50f,
    },
    XmlChSRange {
        range: 0x561..=0x587,
    },
    XmlChSRange {
        range: 0x1d00..=0x1d2b,
    },
    XmlChSRange {
        range: 0x1d62..=0x1d6b,
    },
    XmlChSRange {
        range: 0x1e01..=0x1e01,
    },
    XmlChSRange {
        range: 0x1e03..=0x1e03,
    },
    XmlChSRange {
        range: 0x1e05..=0x1e05,
    },
    XmlChSRange {
        range: 0x1e07..=0x1e07,
    },
    XmlChSRange {
        range: 0x1e09..=0x1e09,
    },
    XmlChSRange {
        range: 0x1e0b..=0x1e0b,
    },
    XmlChSRange {
        range: 0x1e0d..=0x1e0d,
    },
    XmlChSRange {
        range: 0x1e0f..=0x1e0f,
    },
    XmlChSRange {
        range: 0x1e11..=0x1e11,
    },
    XmlChSRange {
        range: 0x1e13..=0x1e13,
    },
    XmlChSRange {
        range: 0x1e15..=0x1e15,
    },
    XmlChSRange {
        range: 0x1e17..=0x1e17,
    },
    XmlChSRange {
        range: 0x1e19..=0x1e19,
    },
    XmlChSRange {
        range: 0x1e1b..=0x1e1b,
    },
    XmlChSRange {
        range: 0x1e1d..=0x1e1d,
    },
    XmlChSRange {
        range: 0x1e1f..=0x1e1f,
    },
    XmlChSRange {
        range: 0x1e21..=0x1e21,
    },
    XmlChSRange {
        range: 0x1e23..=0x1e23,
    },
    XmlChSRange {
        range: 0x1e25..=0x1e25,
    },
    XmlChSRange {
        range: 0x1e27..=0x1e27,
    },
    XmlChSRange {
        range: 0x1e29..=0x1e29,
    },
    XmlChSRange {
        range: 0x1e2b..=0x1e2b,
    },
    XmlChSRange {
        range: 0x1e2d..=0x1e2d,
    },
    XmlChSRange {
        range: 0x1e2f..=0x1e2f,
    },
    XmlChSRange {
        range: 0x1e31..=0x1e31,
    },
    XmlChSRange {
        range: 0x1e33..=0x1e33,
    },
    XmlChSRange {
        range: 0x1e35..=0x1e35,
    },
    XmlChSRange {
        range: 0x1e37..=0x1e37,
    },
    XmlChSRange {
        range: 0x1e39..=0x1e39,
    },
    XmlChSRange {
        range: 0x1e3b..=0x1e3b,
    },
    XmlChSRange {
        range: 0x1e3d..=0x1e3d,
    },
    XmlChSRange {
        range: 0x1e3f..=0x1e3f,
    },
    XmlChSRange {
        range: 0x1e41..=0x1e41,
    },
    XmlChSRange {
        range: 0x1e43..=0x1e43,
    },
    XmlChSRange {
        range: 0x1e45..=0x1e45,
    },
    XmlChSRange {
        range: 0x1e47..=0x1e47,
    },
    XmlChSRange {
        range: 0x1e49..=0x1e49,
    },
    XmlChSRange {
        range: 0x1e4b..=0x1e4b,
    },
    XmlChSRange {
        range: 0x1e4d..=0x1e4d,
    },
    XmlChSRange {
        range: 0x1e4f..=0x1e4f,
    },
    XmlChSRange {
        range: 0x1e51..=0x1e51,
    },
    XmlChSRange {
        range: 0x1e53..=0x1e53,
    },
    XmlChSRange {
        range: 0x1e55..=0x1e55,
    },
    XmlChSRange {
        range: 0x1e57..=0x1e57,
    },
    XmlChSRange {
        range: 0x1e59..=0x1e59,
    },
    XmlChSRange {
        range: 0x1e5b..=0x1e5b,
    },
    XmlChSRange {
        range: 0x1e5d..=0x1e5d,
    },
    XmlChSRange {
        range: 0x1e5f..=0x1e5f,
    },
    XmlChSRange {
        range: 0x1e61..=0x1e61,
    },
    XmlChSRange {
        range: 0x1e63..=0x1e63,
    },
    XmlChSRange {
        range: 0x1e65..=0x1e65,
    },
    XmlChSRange {
        range: 0x1e67..=0x1e67,
    },
    XmlChSRange {
        range: 0x1e69..=0x1e69,
    },
    XmlChSRange {
        range: 0x1e6b..=0x1e6b,
    },
    XmlChSRange {
        range: 0x1e6d..=0x1e6d,
    },
    XmlChSRange {
        range: 0x1e6f..=0x1e6f,
    },
    XmlChSRange {
        range: 0x1e71..=0x1e71,
    },
    XmlChSRange {
        range: 0x1e73..=0x1e73,
    },
    XmlChSRange {
        range: 0x1e75..=0x1e75,
    },
    XmlChSRange {
        range: 0x1e77..=0x1e77,
    },
    XmlChSRange {
        range: 0x1e79..=0x1e79,
    },
    XmlChSRange {
        range: 0x1e7b..=0x1e7b,
    },
    XmlChSRange {
        range: 0x1e7d..=0x1e7d,
    },
    XmlChSRange {
        range: 0x1e7f..=0x1e7f,
    },
    XmlChSRange {
        range: 0x1e81..=0x1e81,
    },
    XmlChSRange {
        range: 0x1e83..=0x1e83,
    },
    XmlChSRange {
        range: 0x1e85..=0x1e85,
    },
    XmlChSRange {
        range: 0x1e87..=0x1e87,
    },
    XmlChSRange {
        range: 0x1e89..=0x1e89,
    },
    XmlChSRange {
        range: 0x1e8b..=0x1e8b,
    },
    XmlChSRange {
        range: 0x1e8d..=0x1e8d,
    },
    XmlChSRange {
        range: 0x1e8f..=0x1e8f,
    },
    XmlChSRange {
        range: 0x1e91..=0x1e91,
    },
    XmlChSRange {
        range: 0x1e93..=0x1e93,
    },
    XmlChSRange {
        range: 0x1e95..=0x1e9b,
    },
    XmlChSRange {
        range: 0x1ea1..=0x1ea1,
    },
    XmlChSRange {
        range: 0x1ea3..=0x1ea3,
    },
    XmlChSRange {
        range: 0x1ea5..=0x1ea5,
    },
    XmlChSRange {
        range: 0x1ea7..=0x1ea7,
    },
    XmlChSRange {
        range: 0x1ea9..=0x1ea9,
    },
    XmlChSRange {
        range: 0x1eab..=0x1eab,
    },
    XmlChSRange {
        range: 0x1ead..=0x1ead,
    },
    XmlChSRange {
        range: 0x1eaf..=0x1eaf,
    },
    XmlChSRange {
        range: 0x1eb1..=0x1eb1,
    },
    XmlChSRange {
        range: 0x1eb3..=0x1eb3,
    },
    XmlChSRange {
        range: 0x1eb5..=0x1eb5,
    },
    XmlChSRange {
        range: 0x1eb7..=0x1eb7,
    },
    XmlChSRange {
        range: 0x1eb9..=0x1eb9,
    },
    XmlChSRange {
        range: 0x1ebb..=0x1ebb,
    },
    XmlChSRange {
        range: 0x1ebd..=0x1ebd,
    },
    XmlChSRange {
        range: 0x1ebf..=0x1ebf,
    },
    XmlChSRange {
        range: 0x1ec1..=0x1ec1,
    },
    XmlChSRange {
        range: 0x1ec3..=0x1ec3,
    },
    XmlChSRange {
        range: 0x1ec5..=0x1ec5,
    },
    XmlChSRange {
        range: 0x1ec7..=0x1ec7,
    },
    XmlChSRange {
        range: 0x1ec9..=0x1ec9,
    },
    XmlChSRange {
        range: 0x1ecb..=0x1ecb,
    },
    XmlChSRange {
        range: 0x1ecd..=0x1ecd,
    },
    XmlChSRange {
        range: 0x1ecf..=0x1ecf,
    },
    XmlChSRange {
        range: 0x1ed1..=0x1ed1,
    },
    XmlChSRange {
        range: 0x1ed3..=0x1ed3,
    },
    XmlChSRange {
        range: 0x1ed5..=0x1ed5,
    },
    XmlChSRange {
        range: 0x1ed7..=0x1ed7,
    },
    XmlChSRange {
        range: 0x1ed9..=0x1ed9,
    },
    XmlChSRange {
        range: 0x1edb..=0x1edb,
    },
    XmlChSRange {
        range: 0x1edd..=0x1edd,
    },
    XmlChSRange {
        range: 0x1edf..=0x1edf,
    },
    XmlChSRange {
        range: 0x1ee1..=0x1ee1,
    },
    XmlChSRange {
        range: 0x1ee3..=0x1ee3,
    },
    XmlChSRange {
        range: 0x1ee5..=0x1ee5,
    },
    XmlChSRange {
        range: 0x1ee7..=0x1ee7,
    },
    XmlChSRange {
        range: 0x1ee9..=0x1ee9,
    },
    XmlChSRange {
        range: 0x1eeb..=0x1eeb,
    },
    XmlChSRange {
        range: 0x1eed..=0x1eed,
    },
    XmlChSRange {
        range: 0x1eef..=0x1eef,
    },
    XmlChSRange {
        range: 0x1ef1..=0x1ef1,
    },
    XmlChSRange {
        range: 0x1ef3..=0x1ef3,
    },
    XmlChSRange {
        range: 0x1ef5..=0x1ef5,
    },
    XmlChSRange {
        range: 0x1ef7..=0x1ef7,
    },
    XmlChSRange {
        range: 0x1ef9..=0x1ef9,
    },
    XmlChSRange {
        range: 0x1f00..=0x1f07,
    },
    XmlChSRange {
        range: 0x1f10..=0x1f15,
    },
    XmlChSRange {
        range: 0x1f20..=0x1f27,
    },
    XmlChSRange {
        range: 0x1f30..=0x1f37,
    },
    XmlChSRange {
        range: 0x1f40..=0x1f45,
    },
    XmlChSRange {
        range: 0x1f50..=0x1f57,
    },
    XmlChSRange {
        range: 0x1f60..=0x1f67,
    },
    XmlChSRange {
        range: 0x1f70..=0x1f7d,
    },
    XmlChSRange {
        range: 0x1f80..=0x1f87,
    },
    XmlChSRange {
        range: 0x1f90..=0x1f97,
    },
    XmlChSRange {
        range: 0x1fa0..=0x1fa7,
    },
    XmlChSRange {
        range: 0x1fb0..=0x1fb4,
    },
    XmlChSRange {
        range: 0x1fb6..=0x1fb7,
    },
    XmlChSRange {
        range: 0x1fbe..=0x1fbe,
    },
    XmlChSRange {
        range: 0x1fc2..=0x1fc4,
    },
    XmlChSRange {
        range: 0x1fc6..=0x1fc7,
    },
    XmlChSRange {
        range: 0x1fd0..=0x1fd3,
    },
    XmlChSRange {
        range: 0x1fd6..=0x1fd7,
    },
    XmlChSRange {
        range: 0x1fe0..=0x1fe7,
    },
    XmlChSRange {
        range: 0x1ff2..=0x1ff4,
    },
    XmlChSRange {
        range: 0x1ff6..=0x1ff7,
    },
    XmlChSRange {
        range: 0x2071..=0x2071,
    },
    XmlChSRange {
        range: 0x207f..=0x207f,
    },
    XmlChSRange {
        range: 0x210a..=0x210a,
    },
    XmlChSRange {
        range: 0x210e..=0x210f,
    },
    XmlChSRange {
        range: 0x2113..=0x2113,
    },
    XmlChSRange {
        range: 0x212f..=0x212f,
    },
    XmlChSRange {
        range: 0x2134..=0x2134,
    },
    XmlChSRange {
        range: 0x2139..=0x2139,
    },
    XmlChSRange {
        range: 0x213d..=0x213d,
    },
    XmlChSRange {
        range: 0x2146..=0x2149,
    },
    XmlChSRange {
        range: 0xfb00..=0xfb06,
    },
    XmlChSRange {
        range: 0xfb13..=0xfb17,
    },
    XmlChSRange {
        range: 0xff41..=0xff5a,
    },
];

const XML_LL_L: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x10428..=0x1044f,
    },
    XmlChLRange {
        range: 0x1d41a..=0x1d433,
    },
    XmlChLRange {
        range: 0x1d44e..=0x1d454,
    },
    XmlChLRange {
        range: 0x1d456..=0x1d467,
    },
    XmlChLRange {
        range: 0x1d482..=0x1d49b,
    },
    XmlChLRange {
        range: 0x1d4b6..=0x1d4b9,
    },
    XmlChLRange {
        range: 0x1d4bb..=0x1d4bb,
    },
    XmlChLRange {
        range: 0x1d4bd..=0x1d4c3,
    },
    XmlChLRange {
        range: 0x1d4c5..=0x1d4cf,
    },
    XmlChLRange {
        range: 0x1d4ea..=0x1d503,
    },
    XmlChLRange {
        range: 0x1d51e..=0x1d537,
    },
    XmlChLRange {
        range: 0x1d552..=0x1d56b,
    },
    XmlChLRange {
        range: 0x1d586..=0x1d59f,
    },
    XmlChLRange {
        range: 0x1d5ba..=0x1d5d3,
    },
    XmlChLRange {
        range: 0x1d5ee..=0x1d607,
    },
    XmlChLRange {
        range: 0x1d622..=0x1d63b,
    },
    XmlChLRange {
        range: 0x1d656..=0x1d66f,
    },
    XmlChLRange {
        range: 0x1d68a..=0x1d6a3,
    },
    XmlChLRange {
        range: 0x1d6c2..=0x1d6da,
    },
    XmlChLRange {
        range: 0x1d6dc..=0x1d6e1,
    },
    XmlChLRange {
        range: 0x1d6fc..=0x1d714,
    },
    XmlChLRange {
        range: 0x1d716..=0x1d71b,
    },
    XmlChLRange {
        range: 0x1d736..=0x1d74e,
    },
    XmlChLRange {
        range: 0x1d750..=0x1d755,
    },
    XmlChLRange {
        range: 0x1d770..=0x1d788,
    },
    XmlChLRange {
        range: 0x1d78a..=0x1d78f,
    },
    XmlChLRange {
        range: 0x1d7aa..=0x1d7c2,
    },
    XmlChLRange {
        range: 0x1d7c4..=0x1d7c9,
    },
];

const XML_LL_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_LL_S,
    long_range: XML_LL_L,
};

const XML_LM_S: &[XmlChSRange] = &[
    XmlChSRange {
        range: 0x2b0..=0x2c1,
    },
    XmlChSRange {
        range: 0x2c6..=0x2d1,
    },
    XmlChSRange {
        range: 0x2e0..=0x2e4,
    },
    XmlChSRange {
        range: 0x2ee..=0x2ee,
    },
    XmlChSRange {
        range: 0x37a..=0x37a,
    },
    XmlChSRange {
        range: 0x559..=0x559,
    },
    XmlChSRange {
        range: 0x640..=0x640,
    },
    XmlChSRange {
        range: 0x6e5..=0x6e6,
    },
    XmlChSRange {
        range: 0xe46..=0xe46,
    },
    XmlChSRange {
        range: 0xec6..=0xec6,
    },
    XmlChSRange {
        range: 0x17d7..=0x17d7,
    },
    XmlChSRange {
        range: 0x1843..=0x1843,
    },
    XmlChSRange {
        range: 0x1d2c..=0x1d61,
    },
    XmlChSRange {
        range: 0x3005..=0x3005,
    },
    XmlChSRange {
        range: 0x3031..=0x3035,
    },
    XmlChSRange {
        range: 0x303b..=0x303b,
    },
    XmlChSRange {
        range: 0x309d..=0x309e,
    },
    XmlChSRange {
        range: 0x30fc..=0x30fe,
    },
    XmlChSRange {
        range: 0xff70..=0xff70,
    },
    XmlChSRange {
        range: 0xff9e..=0xff9f,
    },
];

const XML_LM_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_LM_S,
    long_range: &[],
};

const XML_LO_S: &[XmlChSRange] = &[
    XmlChSRange {
        range: 0x1bb..=0x1bb,
    },
    XmlChSRange {
        range: 0x1c0..=0x1c3,
    },
    XmlChSRange {
        range: 0x5d0..=0x5ea,
    },
    XmlChSRange {
        range: 0x5f0..=0x5f2,
    },
    XmlChSRange {
        range: 0x621..=0x63a,
    },
    XmlChSRange {
        range: 0x641..=0x64a,
    },
    XmlChSRange {
        range: 0x66e..=0x66f,
    },
    XmlChSRange {
        range: 0x671..=0x6d3,
    },
    XmlChSRange {
        range: 0x6d5..=0x6d5,
    },
    XmlChSRange {
        range: 0x6ee..=0x6ef,
    },
    XmlChSRange {
        range: 0x6fa..=0x6fc,
    },
    XmlChSRange {
        range: 0x6ff..=0x6ff,
    },
    XmlChSRange {
        range: 0x710..=0x710,
    },
    XmlChSRange {
        range: 0x712..=0x72f,
    },
    XmlChSRange {
        range: 0x74d..=0x74f,
    },
    XmlChSRange {
        range: 0x780..=0x7a5,
    },
    XmlChSRange {
        range: 0x7b1..=0x7b1,
    },
    XmlChSRange {
        range: 0x904..=0x939,
    },
    XmlChSRange {
        range: 0x93d..=0x93d,
    },
    XmlChSRange {
        range: 0x950..=0x950,
    },
    XmlChSRange {
        range: 0x958..=0x961,
    },
    XmlChSRange {
        range: 0x985..=0x98c,
    },
    XmlChSRange {
        range: 0x98f..=0x990,
    },
    XmlChSRange {
        range: 0x993..=0x9a8,
    },
    XmlChSRange {
        range: 0x9aa..=0x9b0,
    },
    XmlChSRange {
        range: 0x9b2..=0x9b2,
    },
    XmlChSRange {
        range: 0x9b6..=0x9b9,
    },
    XmlChSRange {
        range: 0x9bd..=0x9bd,
    },
    XmlChSRange {
        range: 0x9dc..=0x9dd,
    },
    XmlChSRange {
        range: 0x9df..=0x9e1,
    },
    XmlChSRange {
        range: 0x9f0..=0x9f1,
    },
    XmlChSRange {
        range: 0xa05..=0xa0a,
    },
    XmlChSRange {
        range: 0xa0f..=0xa10,
    },
    XmlChSRange {
        range: 0xa13..=0xa28,
    },
    XmlChSRange {
        range: 0xa2a..=0xa30,
    },
    XmlChSRange {
        range: 0xa32..=0xa33,
    },
    XmlChSRange {
        range: 0xa35..=0xa36,
    },
    XmlChSRange {
        range: 0xa38..=0xa39,
    },
    XmlChSRange {
        range: 0xa59..=0xa5c,
    },
    XmlChSRange {
        range: 0xa5e..=0xa5e,
    },
    XmlChSRange {
        range: 0xa72..=0xa74,
    },
    XmlChSRange {
        range: 0xa85..=0xa8d,
    },
    XmlChSRange {
        range: 0xa8f..=0xa91,
    },
    XmlChSRange {
        range: 0xa93..=0xaa8,
    },
    XmlChSRange {
        range: 0xaaa..=0xab0,
    },
    XmlChSRange {
        range: 0xab2..=0xab3,
    },
    XmlChSRange {
        range: 0xab5..=0xab9,
    },
    XmlChSRange {
        range: 0xabd..=0xabd,
    },
    XmlChSRange {
        range: 0xad0..=0xad0,
    },
    XmlChSRange {
        range: 0xae0..=0xae1,
    },
    XmlChSRange {
        range: 0xb05..=0xb0c,
    },
    XmlChSRange {
        range: 0xb0f..=0xb10,
    },
    XmlChSRange {
        range: 0xb13..=0xb28,
    },
    XmlChSRange {
        range: 0xb2a..=0xb30,
    },
    XmlChSRange {
        range: 0xb32..=0xb33,
    },
    XmlChSRange {
        range: 0xb35..=0xb39,
    },
    XmlChSRange {
        range: 0xb3d..=0xb3d,
    },
    XmlChSRange {
        range: 0xb5c..=0xb5d,
    },
    XmlChSRange {
        range: 0xb5f..=0xb61,
    },
    XmlChSRange {
        range: 0xb71..=0xb71,
    },
    XmlChSRange {
        range: 0xb83..=0xb83,
    },
    XmlChSRange {
        range: 0xb85..=0xb8a,
    },
    XmlChSRange {
        range: 0xb8e..=0xb90,
    },
    XmlChSRange {
        range: 0xb92..=0xb95,
    },
    XmlChSRange {
        range: 0xb99..=0xb9a,
    },
    XmlChSRange {
        range: 0xb9c..=0xb9c,
    },
    XmlChSRange {
        range: 0xb9e..=0xb9f,
    },
    XmlChSRange {
        range: 0xba3..=0xba4,
    },
    XmlChSRange {
        range: 0xba8..=0xbaa,
    },
    XmlChSRange {
        range: 0xbae..=0xbb5,
    },
    XmlChSRange {
        range: 0xbb7..=0xbb9,
    },
    XmlChSRange {
        range: 0xc05..=0xc0c,
    },
    XmlChSRange {
        range: 0xc0e..=0xc10,
    },
    XmlChSRange {
        range: 0xc12..=0xc28,
    },
    XmlChSRange {
        range: 0xc2a..=0xc33,
    },
    XmlChSRange {
        range: 0xc35..=0xc39,
    },
    XmlChSRange {
        range: 0xc60..=0xc61,
    },
    XmlChSRange {
        range: 0xc85..=0xc8c,
    },
    XmlChSRange {
        range: 0xc8e..=0xc90,
    },
    XmlChSRange {
        range: 0xc92..=0xca8,
    },
    XmlChSRange {
        range: 0xcaa..=0xcb3,
    },
    XmlChSRange {
        range: 0xcb5..=0xcb9,
    },
    XmlChSRange {
        range: 0xcbd..=0xcbd,
    },
    XmlChSRange {
        range: 0xcde..=0xcde,
    },
    XmlChSRange {
        range: 0xce0..=0xce1,
    },
    XmlChSRange {
        range: 0xd05..=0xd0c,
    },
    XmlChSRange {
        range: 0xd0e..=0xd10,
    },
    XmlChSRange {
        range: 0xd12..=0xd28,
    },
    XmlChSRange {
        range: 0xd2a..=0xd39,
    },
    XmlChSRange {
        range: 0xd60..=0xd61,
    },
    XmlChSRange {
        range: 0xd85..=0xd96,
    },
    XmlChSRange {
        range: 0xd9a..=0xdb1,
    },
    XmlChSRange {
        range: 0xdb3..=0xdbb,
    },
    XmlChSRange {
        range: 0xdbd..=0xdbd,
    },
    XmlChSRange {
        range: 0xdc0..=0xdc6,
    },
    XmlChSRange {
        range: 0xe01..=0xe30,
    },
    XmlChSRange {
        range: 0xe32..=0xe33,
    },
    XmlChSRange {
        range: 0xe40..=0xe45,
    },
    XmlChSRange {
        range: 0xe81..=0xe82,
    },
    XmlChSRange {
        range: 0xe84..=0xe84,
    },
    XmlChSRange {
        range: 0xe87..=0xe88,
    },
    XmlChSRange {
        range: 0xe8a..=0xe8a,
    },
    XmlChSRange {
        range: 0xe8d..=0xe8d,
    },
    XmlChSRange {
        range: 0xe94..=0xe97,
    },
    XmlChSRange {
        range: 0xe99..=0xe9f,
    },
    XmlChSRange {
        range: 0xea1..=0xea3,
    },
    XmlChSRange {
        range: 0xea5..=0xea5,
    },
    XmlChSRange {
        range: 0xea7..=0xea7,
    },
    XmlChSRange {
        range: 0xeaa..=0xeab,
    },
    XmlChSRange {
        range: 0xead..=0xeb0,
    },
    XmlChSRange {
        range: 0xeb2..=0xeb3,
    },
    XmlChSRange {
        range: 0xebd..=0xebd,
    },
    XmlChSRange {
        range: 0xec0..=0xec4,
    },
    XmlChSRange {
        range: 0xedc..=0xedd,
    },
    XmlChSRange {
        range: 0xf00..=0xf00,
    },
    XmlChSRange {
        range: 0xf40..=0xf47,
    },
    XmlChSRange {
        range: 0xf49..=0xf6a,
    },
    XmlChSRange {
        range: 0xf88..=0xf8b,
    },
    XmlChSRange {
        range: 0x1000..=0x1021,
    },
    XmlChSRange {
        range: 0x1023..=0x1027,
    },
    XmlChSRange {
        range: 0x1029..=0x102a,
    },
    XmlChSRange {
        range: 0x1050..=0x1055,
    },
    XmlChSRange {
        range: 0x10d0..=0x10f8,
    },
    XmlChSRange {
        range: 0x1100..=0x1159,
    },
    XmlChSRange {
        range: 0x115f..=0x11a2,
    },
    XmlChSRange {
        range: 0x11a8..=0x11f9,
    },
    XmlChSRange {
        range: 0x1200..=0x1206,
    },
    XmlChSRange {
        range: 0x1208..=0x1246,
    },
    XmlChSRange {
        range: 0x1248..=0x1248,
    },
    XmlChSRange {
        range: 0x124a..=0x124d,
    },
    XmlChSRange {
        range: 0x1250..=0x1256,
    },
    XmlChSRange {
        range: 0x1258..=0x1258,
    },
    XmlChSRange {
        range: 0x125a..=0x125d,
    },
    XmlChSRange {
        range: 0x1260..=0x1286,
    },
    XmlChSRange {
        range: 0x1288..=0x1288,
    },
    XmlChSRange {
        range: 0x128a..=0x128d,
    },
    XmlChSRange {
        range: 0x1290..=0x12ae,
    },
    XmlChSRange {
        range: 0x12b0..=0x12b0,
    },
    XmlChSRange {
        range: 0x12b2..=0x12b5,
    },
    XmlChSRange {
        range: 0x12b8..=0x12be,
    },
    XmlChSRange {
        range: 0x12c0..=0x12c0,
    },
    XmlChSRange {
        range: 0x12c2..=0x12c5,
    },
    XmlChSRange {
        range: 0x12c8..=0x12ce,
    },
    XmlChSRange {
        range: 0x12d0..=0x12d6,
    },
    XmlChSRange {
        range: 0x12d8..=0x12ee,
    },
    XmlChSRange {
        range: 0x12f0..=0x130e,
    },
    XmlChSRange {
        range: 0x1310..=0x1310,
    },
    XmlChSRange {
        range: 0x1312..=0x1315,
    },
    XmlChSRange {
        range: 0x1318..=0x131e,
    },
    XmlChSRange {
        range: 0x1320..=0x1346,
    },
    XmlChSRange {
        range: 0x1348..=0x135a,
    },
    XmlChSRange {
        range: 0x13a0..=0x13f4,
    },
    XmlChSRange {
        range: 0x1401..=0x166c,
    },
    XmlChSRange {
        range: 0x166f..=0x1676,
    },
    XmlChSRange {
        range: 0x1681..=0x169a,
    },
    XmlChSRange {
        range: 0x16a0..=0x16ea,
    },
    XmlChSRange {
        range: 0x1700..=0x170c,
    },
    XmlChSRange {
        range: 0x170e..=0x1711,
    },
    XmlChSRange {
        range: 0x1720..=0x1731,
    },
    XmlChSRange {
        range: 0x1740..=0x1751,
    },
    XmlChSRange {
        range: 0x1760..=0x176c,
    },
    XmlChSRange {
        range: 0x176e..=0x1770,
    },
    XmlChSRange {
        range: 0x1780..=0x17b3,
    },
    XmlChSRange {
        range: 0x17dc..=0x17dc,
    },
    XmlChSRange {
        range: 0x1820..=0x1842,
    },
    XmlChSRange {
        range: 0x1844..=0x1877,
    },
    XmlChSRange {
        range: 0x1880..=0x18a8,
    },
    XmlChSRange {
        range: 0x1900..=0x191c,
    },
    XmlChSRange {
        range: 0x1950..=0x196d,
    },
    XmlChSRange {
        range: 0x1970..=0x1974,
    },
    XmlChSRange {
        range: 0x2135..=0x2138,
    },
    XmlChSRange {
        range: 0x3006..=0x3006,
    },
    XmlChSRange {
        range: 0x303c..=0x303c,
    },
    XmlChSRange {
        range: 0x3041..=0x3096,
    },
    XmlChSRange {
        range: 0x309f..=0x309f,
    },
    XmlChSRange {
        range: 0x30a1..=0x30fa,
    },
    XmlChSRange {
        range: 0x30ff..=0x30ff,
    },
    XmlChSRange {
        range: 0x3105..=0x312c,
    },
    XmlChSRange {
        range: 0x3131..=0x318e,
    },
    XmlChSRange {
        range: 0x31a0..=0x31b7,
    },
    XmlChSRange {
        range: 0x31f0..=0x31ff,
    },
    XmlChSRange {
        range: 0x3400..=0x3400,
    },
    XmlChSRange {
        range: 0x4db5..=0x4db5,
    },
    XmlChSRange {
        range: 0x4e00..=0x4e00,
    },
    XmlChSRange {
        range: 0x9fa5..=0x9fa5,
    },
    XmlChSRange {
        range: 0xa000..=0xa48c,
    },
    XmlChSRange {
        range: 0xac00..=0xac00,
    },
    XmlChSRange {
        range: 0xd7a3..=0xd7a3,
    },
    XmlChSRange {
        range: 0xf900..=0xfa2d,
    },
    XmlChSRange {
        range: 0xfa30..=0xfa6a,
    },
    XmlChSRange {
        range: 0xfb1d..=0xfb1d,
    },
    XmlChSRange {
        range: 0xfb1f..=0xfb28,
    },
    XmlChSRange {
        range: 0xfb2a..=0xfb36,
    },
    XmlChSRange {
        range: 0xfb38..=0xfb3c,
    },
    XmlChSRange {
        range: 0xfb3e..=0xfb3e,
    },
    XmlChSRange {
        range: 0xfb40..=0xfb41,
    },
    XmlChSRange {
        range: 0xfb43..=0xfb44,
    },
    XmlChSRange {
        range: 0xfb46..=0xfbb1,
    },
    XmlChSRange {
        range: 0xfbd3..=0xfd3d,
    },
    XmlChSRange {
        range: 0xfd50..=0xfd8f,
    },
    XmlChSRange {
        range: 0xfd92..=0xfdc7,
    },
    XmlChSRange {
        range: 0xfdf0..=0xfdfb,
    },
    XmlChSRange {
        range: 0xfe70..=0xfe74,
    },
    XmlChSRange {
        range: 0xfe76..=0xfefc,
    },
    XmlChSRange {
        range: 0xff66..=0xff6f,
    },
    XmlChSRange {
        range: 0xff71..=0xff9d,
    },
    XmlChSRange {
        range: 0xffa0..=0xffbe,
    },
    XmlChSRange {
        range: 0xffc2..=0xffc7,
    },
    XmlChSRange {
        range: 0xffca..=0xffcf,
    },
    XmlChSRange {
        range: 0xffd2..=0xffd7,
    },
    XmlChSRange {
        range: 0xffda..=0xffdc,
    },
];

const XML_LO_L: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x10000..=0x1000b,
    },
    XmlChLRange {
        range: 0x1000d..=0x10026,
    },
    XmlChLRange {
        range: 0x10028..=0x1003a,
    },
    XmlChLRange {
        range: 0x1003c..=0x1003d,
    },
    XmlChLRange {
        range: 0x1003f..=0x1004d,
    },
    XmlChLRange {
        range: 0x10050..=0x1005d,
    },
    XmlChLRange {
        range: 0x10080..=0x100fa,
    },
    XmlChLRange {
        range: 0x10300..=0x1031e,
    },
    XmlChLRange {
        range: 0x10330..=0x10349,
    },
    XmlChLRange {
        range: 0x10380..=0x1039d,
    },
    XmlChLRange {
        range: 0x10450..=0x1049d,
    },
    XmlChLRange {
        range: 0x10800..=0x10805,
    },
    XmlChLRange {
        range: 0x10808..=0x10808,
    },
    XmlChLRange {
        range: 0x1080a..=0x10835,
    },
    XmlChLRange {
        range: 0x10837..=0x10838,
    },
    XmlChLRange {
        range: 0x1083c..=0x1083c,
    },
    XmlChLRange {
        range: 0x1083f..=0x1083f,
    },
    XmlChLRange {
        range: 0x20000..=0x20000,
    },
    XmlChLRange {
        range: 0x2a6d6..=0x2a6d6,
    },
    XmlChLRange {
        range: 0x2f800..=0x2fa1d,
    },
];

const XML_LO_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_LO_S,
    long_range: XML_LO_L,
};

const XML_LT_S: &[XmlChSRange] = &[
    XmlChSRange {
        range: 0x1c5..=0x1c5,
    },
    XmlChSRange {
        range: 0x1c8..=0x1c8,
    },
    XmlChSRange {
        range: 0x1cb..=0x1cb,
    },
    XmlChSRange {
        range: 0x1f2..=0x1f2,
    },
    XmlChSRange {
        range: 0x1f88..=0x1f8f,
    },
    XmlChSRange {
        range: 0x1f98..=0x1f9f,
    },
    XmlChSRange {
        range: 0x1fa8..=0x1faf,
    },
    XmlChSRange {
        range: 0x1fbc..=0x1fbc,
    },
    XmlChSRange {
        range: 0x1fcc..=0x1fcc,
    },
    XmlChSRange {
        range: 0x1ffc..=0x1ffc,
    },
];

const XML_LT_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_LT_S,
    long_range: &[],
};

const XML_LU_S: &[XmlChSRange] = &[
    XmlChSRange { range: 0x41..=0x5a },
    XmlChSRange { range: 0xc0..=0xd6 },
    XmlChSRange { range: 0xd8..=0xde },
    XmlChSRange {
        range: 0x100..=0x100,
    },
    XmlChSRange {
        range: 0x102..=0x102,
    },
    XmlChSRange {
        range: 0x104..=0x104,
    },
    XmlChSRange {
        range: 0x106..=0x106,
    },
    XmlChSRange {
        range: 0x108..=0x108,
    },
    XmlChSRange {
        range: 0x10a..=0x10a,
    },
    XmlChSRange {
        range: 0x10c..=0x10c,
    },
    XmlChSRange {
        range: 0x10e..=0x10e,
    },
    XmlChSRange {
        range: 0x110..=0x110,
    },
    XmlChSRange {
        range: 0x112..=0x112,
    },
    XmlChSRange {
        range: 0x114..=0x114,
    },
    XmlChSRange {
        range: 0x116..=0x116,
    },
    XmlChSRange {
        range: 0x118..=0x118,
    },
    XmlChSRange {
        range: 0x11a..=0x11a,
    },
    XmlChSRange {
        range: 0x11c..=0x11c,
    },
    XmlChSRange {
        range: 0x11e..=0x11e,
    },
    XmlChSRange {
        range: 0x120..=0x120,
    },
    XmlChSRange {
        range: 0x122..=0x122,
    },
    XmlChSRange {
        range: 0x124..=0x124,
    },
    XmlChSRange {
        range: 0x126..=0x126,
    },
    XmlChSRange {
        range: 0x128..=0x128,
    },
    XmlChSRange {
        range: 0x12a..=0x12a,
    },
    XmlChSRange {
        range: 0x12c..=0x12c,
    },
    XmlChSRange {
        range: 0x12e..=0x12e,
    },
    XmlChSRange {
        range: 0x130..=0x130,
    },
    XmlChSRange {
        range: 0x132..=0x132,
    },
    XmlChSRange {
        range: 0x134..=0x134,
    },
    XmlChSRange {
        range: 0x136..=0x136,
    },
    XmlChSRange {
        range: 0x139..=0x139,
    },
    XmlChSRange {
        range: 0x13b..=0x13b,
    },
    XmlChSRange {
        range: 0x13d..=0x13d,
    },
    XmlChSRange {
        range: 0x13f..=0x13f,
    },
    XmlChSRange {
        range: 0x141..=0x141,
    },
    XmlChSRange {
        range: 0x143..=0x143,
    },
    XmlChSRange {
        range: 0x145..=0x145,
    },
    XmlChSRange {
        range: 0x147..=0x147,
    },
    XmlChSRange {
        range: 0x14a..=0x14a,
    },
    XmlChSRange {
        range: 0x14c..=0x14c,
    },
    XmlChSRange {
        range: 0x14e..=0x14e,
    },
    XmlChSRange {
        range: 0x150..=0x150,
    },
    XmlChSRange {
        range: 0x152..=0x152,
    },
    XmlChSRange {
        range: 0x154..=0x154,
    },
    XmlChSRange {
        range: 0x156..=0x156,
    },
    XmlChSRange {
        range: 0x158..=0x158,
    },
    XmlChSRange {
        range: 0x15a..=0x15a,
    },
    XmlChSRange {
        range: 0x15c..=0x15c,
    },
    XmlChSRange {
        range: 0x15e..=0x15e,
    },
    XmlChSRange {
        range: 0x160..=0x160,
    },
    XmlChSRange {
        range: 0x162..=0x162,
    },
    XmlChSRange {
        range: 0x164..=0x164,
    },
    XmlChSRange {
        range: 0x166..=0x166,
    },
    XmlChSRange {
        range: 0x168..=0x168,
    },
    XmlChSRange {
        range: 0x16a..=0x16a,
    },
    XmlChSRange {
        range: 0x16c..=0x16c,
    },
    XmlChSRange {
        range: 0x16e..=0x16e,
    },
    XmlChSRange {
        range: 0x170..=0x170,
    },
    XmlChSRange {
        range: 0x172..=0x172,
    },
    XmlChSRange {
        range: 0x174..=0x174,
    },
    XmlChSRange {
        range: 0x176..=0x176,
    },
    XmlChSRange {
        range: 0x178..=0x179,
    },
    XmlChSRange {
        range: 0x17b..=0x17b,
    },
    XmlChSRange {
        range: 0x17d..=0x17d,
    },
    XmlChSRange {
        range: 0x181..=0x182,
    },
    XmlChSRange {
        range: 0x184..=0x184,
    },
    XmlChSRange {
        range: 0x186..=0x187,
    },
    XmlChSRange {
        range: 0x189..=0x18b,
    },
    XmlChSRange {
        range: 0x18e..=0x191,
    },
    XmlChSRange {
        range: 0x193..=0x194,
    },
    XmlChSRange {
        range: 0x196..=0x198,
    },
    XmlChSRange {
        range: 0x19c..=0x19d,
    },
    XmlChSRange {
        range: 0x19f..=0x1a0,
    },
    XmlChSRange {
        range: 0x1a2..=0x1a2,
    },
    XmlChSRange {
        range: 0x1a4..=0x1a4,
    },
    XmlChSRange {
        range: 0x1a6..=0x1a7,
    },
    XmlChSRange {
        range: 0x1a9..=0x1a9,
    },
    XmlChSRange {
        range: 0x1ac..=0x1ac,
    },
    XmlChSRange {
        range: 0x1ae..=0x1af,
    },
    XmlChSRange {
        range: 0x1b1..=0x1b3,
    },
    XmlChSRange {
        range: 0x1b5..=0x1b5,
    },
    XmlChSRange {
        range: 0x1b7..=0x1b8,
    },
    XmlChSRange {
        range: 0x1bc..=0x1bc,
    },
    XmlChSRange {
        range: 0x1c4..=0x1c4,
    },
    XmlChSRange {
        range: 0x1c7..=0x1c7,
    },
    XmlChSRange {
        range: 0x1ca..=0x1ca,
    },
    XmlChSRange {
        range: 0x1cd..=0x1cd,
    },
    XmlChSRange {
        range: 0x1cf..=0x1cf,
    },
    XmlChSRange {
        range: 0x1d1..=0x1d1,
    },
    XmlChSRange {
        range: 0x1d3..=0x1d3,
    },
    XmlChSRange {
        range: 0x1d5..=0x1d5,
    },
    XmlChSRange {
        range: 0x1d7..=0x1d7,
    },
    XmlChSRange {
        range: 0x1d9..=0x1d9,
    },
    XmlChSRange {
        range: 0x1db..=0x1db,
    },
    XmlChSRange {
        range: 0x1de..=0x1de,
    },
    XmlChSRange {
        range: 0x1e0..=0x1e0,
    },
    XmlChSRange {
        range: 0x1e2..=0x1e2,
    },
    XmlChSRange {
        range: 0x1e4..=0x1e4,
    },
    XmlChSRange {
        range: 0x1e6..=0x1e6,
    },
    XmlChSRange {
        range: 0x1e8..=0x1e8,
    },
    XmlChSRange {
        range: 0x1ea..=0x1ea,
    },
    XmlChSRange {
        range: 0x1ec..=0x1ec,
    },
    XmlChSRange {
        range: 0x1ee..=0x1ee,
    },
    XmlChSRange {
        range: 0x1f1..=0x1f1,
    },
    XmlChSRange {
        range: 0x1f4..=0x1f4,
    },
    XmlChSRange {
        range: 0x1f6..=0x1f8,
    },
    XmlChSRange {
        range: 0x1fa..=0x1fa,
    },
    XmlChSRange {
        range: 0x1fc..=0x1fc,
    },
    XmlChSRange {
        range: 0x1fe..=0x1fe,
    },
    XmlChSRange {
        range: 0x200..=0x200,
    },
    XmlChSRange {
        range: 0x202..=0x202,
    },
    XmlChSRange {
        range: 0x204..=0x204,
    },
    XmlChSRange {
        range: 0x206..=0x206,
    },
    XmlChSRange {
        range: 0x208..=0x208,
    },
    XmlChSRange {
        range: 0x20a..=0x20a,
    },
    XmlChSRange {
        range: 0x20c..=0x20c,
    },
    XmlChSRange {
        range: 0x20e..=0x20e,
    },
    XmlChSRange {
        range: 0x210..=0x210,
    },
    XmlChSRange {
        range: 0x212..=0x212,
    },
    XmlChSRange {
        range: 0x214..=0x214,
    },
    XmlChSRange {
        range: 0x216..=0x216,
    },
    XmlChSRange {
        range: 0x218..=0x218,
    },
    XmlChSRange {
        range: 0x21a..=0x21a,
    },
    XmlChSRange {
        range: 0x21c..=0x21c,
    },
    XmlChSRange {
        range: 0x21e..=0x21e,
    },
    XmlChSRange {
        range: 0x220..=0x220,
    },
    XmlChSRange {
        range: 0x222..=0x222,
    },
    XmlChSRange {
        range: 0x224..=0x224,
    },
    XmlChSRange {
        range: 0x226..=0x226,
    },
    XmlChSRange {
        range: 0x228..=0x228,
    },
    XmlChSRange {
        range: 0x22a..=0x22a,
    },
    XmlChSRange {
        range: 0x22c..=0x22c,
    },
    XmlChSRange {
        range: 0x22e..=0x22e,
    },
    XmlChSRange {
        range: 0x230..=0x230,
    },
    XmlChSRange {
        range: 0x232..=0x232,
    },
    XmlChSRange {
        range: 0x386..=0x386,
    },
    XmlChSRange {
        range: 0x388..=0x38a,
    },
    XmlChSRange {
        range: 0x38c..=0x38c,
    },
    XmlChSRange {
        range: 0x38e..=0x38f,
    },
    XmlChSRange {
        range: 0x391..=0x3a1,
    },
    XmlChSRange {
        range: 0x3a3..=0x3ab,
    },
    XmlChSRange {
        range: 0x3d2..=0x3d4,
    },
    XmlChSRange {
        range: 0x3d8..=0x3d8,
    },
    XmlChSRange {
        range: 0x3da..=0x3da,
    },
    XmlChSRange {
        range: 0x3dc..=0x3dc,
    },
    XmlChSRange {
        range: 0x3de..=0x3de,
    },
    XmlChSRange {
        range: 0x3e0..=0x3e0,
    },
    XmlChSRange {
        range: 0x3e2..=0x3e2,
    },
    XmlChSRange {
        range: 0x3e4..=0x3e4,
    },
    XmlChSRange {
        range: 0x3e6..=0x3e6,
    },
    XmlChSRange {
        range: 0x3e8..=0x3e8,
    },
    XmlChSRange {
        range: 0x3ea..=0x3ea,
    },
    XmlChSRange {
        range: 0x3ec..=0x3ec,
    },
    XmlChSRange {
        range: 0x3ee..=0x3ee,
    },
    XmlChSRange {
        range: 0x3f4..=0x3f4,
    },
    XmlChSRange {
        range: 0x3f7..=0x3f7,
    },
    XmlChSRange {
        range: 0x3f9..=0x3fa,
    },
    XmlChSRange {
        range: 0x400..=0x42f,
    },
    XmlChSRange {
        range: 0x460..=0x460,
    },
    XmlChSRange {
        range: 0x462..=0x462,
    },
    XmlChSRange {
        range: 0x464..=0x464,
    },
    XmlChSRange {
        range: 0x466..=0x466,
    },
    XmlChSRange {
        range: 0x468..=0x468,
    },
    XmlChSRange {
        range: 0x46a..=0x46a,
    },
    XmlChSRange {
        range: 0x46c..=0x46c,
    },
    XmlChSRange {
        range: 0x46e..=0x46e,
    },
    XmlChSRange {
        range: 0x470..=0x470,
    },
    XmlChSRange {
        range: 0x472..=0x472,
    },
    XmlChSRange {
        range: 0x474..=0x474,
    },
    XmlChSRange {
        range: 0x476..=0x476,
    },
    XmlChSRange {
        range: 0x478..=0x478,
    },
    XmlChSRange {
        range: 0x47a..=0x47a,
    },
    XmlChSRange {
        range: 0x47c..=0x47c,
    },
    XmlChSRange {
        range: 0x47e..=0x47e,
    },
    XmlChSRange {
        range: 0x480..=0x480,
    },
    XmlChSRange {
        range: 0x48a..=0x48a,
    },
    XmlChSRange {
        range: 0x48c..=0x48c,
    },
    XmlChSRange {
        range: 0x48e..=0x48e,
    },
    XmlChSRange {
        range: 0x490..=0x490,
    },
    XmlChSRange {
        range: 0x492..=0x492,
    },
    XmlChSRange {
        range: 0x494..=0x494,
    },
    XmlChSRange {
        range: 0x496..=0x496,
    },
    XmlChSRange {
        range: 0x498..=0x498,
    },
    XmlChSRange {
        range: 0x49a..=0x49a,
    },
    XmlChSRange {
        range: 0x49c..=0x49c,
    },
    XmlChSRange {
        range: 0x49e..=0x49e,
    },
    XmlChSRange {
        range: 0x4a0..=0x4a0,
    },
    XmlChSRange {
        range: 0x4a2..=0x4a2,
    },
    XmlChSRange {
        range: 0x4a4..=0x4a4,
    },
    XmlChSRange {
        range: 0x4a6..=0x4a6,
    },
    XmlChSRange {
        range: 0x4a8..=0x4a8,
    },
    XmlChSRange {
        range: 0x4aa..=0x4aa,
    },
    XmlChSRange {
        range: 0x4ac..=0x4ac,
    },
    XmlChSRange {
        range: 0x4ae..=0x4ae,
    },
    XmlChSRange {
        range: 0x4b0..=0x4b0,
    },
    XmlChSRange {
        range: 0x4b2..=0x4b2,
    },
    XmlChSRange {
        range: 0x4b4..=0x4b4,
    },
    XmlChSRange {
        range: 0x4b6..=0x4b6,
    },
    XmlChSRange {
        range: 0x4b8..=0x4b8,
    },
    XmlChSRange {
        range: 0x4ba..=0x4ba,
    },
    XmlChSRange {
        range: 0x4bc..=0x4bc,
    },
    XmlChSRange {
        range: 0x4be..=0x4be,
    },
    XmlChSRange {
        range: 0x4c0..=0x4c1,
    },
    XmlChSRange {
        range: 0x4c3..=0x4c3,
    },
    XmlChSRange {
        range: 0x4c5..=0x4c5,
    },
    XmlChSRange {
        range: 0x4c7..=0x4c7,
    },
    XmlChSRange {
        range: 0x4c9..=0x4c9,
    },
    XmlChSRange {
        range: 0x4cb..=0x4cb,
    },
    XmlChSRange {
        range: 0x4cd..=0x4cd,
    },
    XmlChSRange {
        range: 0x4d0..=0x4d0,
    },
    XmlChSRange {
        range: 0x4d2..=0x4d2,
    },
    XmlChSRange {
        range: 0x4d4..=0x4d4,
    },
    XmlChSRange {
        range: 0x4d6..=0x4d6,
    },
    XmlChSRange {
        range: 0x4d8..=0x4d8,
    },
    XmlChSRange {
        range: 0x4da..=0x4da,
    },
    XmlChSRange {
        range: 0x4dc..=0x4dc,
    },
    XmlChSRange {
        range: 0x4de..=0x4de,
    },
    XmlChSRange {
        range: 0x4e0..=0x4e0,
    },
    XmlChSRange {
        range: 0x4e2..=0x4e2,
    },
    XmlChSRange {
        range: 0x4e4..=0x4e4,
    },
    XmlChSRange {
        range: 0x4e6..=0x4e6,
    },
    XmlChSRange {
        range: 0x4e8..=0x4e8,
    },
    XmlChSRange {
        range: 0x4ea..=0x4ea,
    },
    XmlChSRange {
        range: 0x4ec..=0x4ec,
    },
    XmlChSRange {
        range: 0x4ee..=0x4ee,
    },
    XmlChSRange {
        range: 0x4f0..=0x4f0,
    },
    XmlChSRange {
        range: 0x4f2..=0x4f2,
    },
    XmlChSRange {
        range: 0x4f4..=0x4f4,
    },
    XmlChSRange {
        range: 0x4f8..=0x4f8,
    },
    XmlChSRange {
        range: 0x500..=0x500,
    },
    XmlChSRange {
        range: 0x502..=0x502,
    },
    XmlChSRange {
        range: 0x504..=0x504,
    },
    XmlChSRange {
        range: 0x506..=0x506,
    },
    XmlChSRange {
        range: 0x508..=0x508,
    },
    XmlChSRange {
        range: 0x50a..=0x50a,
    },
    XmlChSRange {
        range: 0x50c..=0x50c,
    },
    XmlChSRange {
        range: 0x50e..=0x50e,
    },
    XmlChSRange {
        range: 0x531..=0x556,
    },
    XmlChSRange {
        range: 0x10a0..=0x10c5,
    },
    XmlChSRange {
        range: 0x1e00..=0x1e00,
    },
    XmlChSRange {
        range: 0x1e02..=0x1e02,
    },
    XmlChSRange {
        range: 0x1e04..=0x1e04,
    },
    XmlChSRange {
        range: 0x1e06..=0x1e06,
    },
    XmlChSRange {
        range: 0x1e08..=0x1e08,
    },
    XmlChSRange {
        range: 0x1e0a..=0x1e0a,
    },
    XmlChSRange {
        range: 0x1e0c..=0x1e0c,
    },
    XmlChSRange {
        range: 0x1e0e..=0x1e0e,
    },
    XmlChSRange {
        range: 0x1e10..=0x1e10,
    },
    XmlChSRange {
        range: 0x1e12..=0x1e12,
    },
    XmlChSRange {
        range: 0x1e14..=0x1e14,
    },
    XmlChSRange {
        range: 0x1e16..=0x1e16,
    },
    XmlChSRange {
        range: 0x1e18..=0x1e18,
    },
    XmlChSRange {
        range: 0x1e1a..=0x1e1a,
    },
    XmlChSRange {
        range: 0x1e1c..=0x1e1c,
    },
    XmlChSRange {
        range: 0x1e1e..=0x1e1e,
    },
    XmlChSRange {
        range: 0x1e20..=0x1e20,
    },
    XmlChSRange {
        range: 0x1e22..=0x1e22,
    },
    XmlChSRange {
        range: 0x1e24..=0x1e24,
    },
    XmlChSRange {
        range: 0x1e26..=0x1e26,
    },
    XmlChSRange {
        range: 0x1e28..=0x1e28,
    },
    XmlChSRange {
        range: 0x1e2a..=0x1e2a,
    },
    XmlChSRange {
        range: 0x1e2c..=0x1e2c,
    },
    XmlChSRange {
        range: 0x1e2e..=0x1e2e,
    },
    XmlChSRange {
        range: 0x1e30..=0x1e30,
    },
    XmlChSRange {
        range: 0x1e32..=0x1e32,
    },
    XmlChSRange {
        range: 0x1e34..=0x1e34,
    },
    XmlChSRange {
        range: 0x1e36..=0x1e36,
    },
    XmlChSRange {
        range: 0x1e38..=0x1e38,
    },
    XmlChSRange {
        range: 0x1e3a..=0x1e3a,
    },
    XmlChSRange {
        range: 0x1e3c..=0x1e3c,
    },
    XmlChSRange {
        range: 0x1e3e..=0x1e3e,
    },
    XmlChSRange {
        range: 0x1e40..=0x1e40,
    },
    XmlChSRange {
        range: 0x1e42..=0x1e42,
    },
    XmlChSRange {
        range: 0x1e44..=0x1e44,
    },
    XmlChSRange {
        range: 0x1e46..=0x1e46,
    },
    XmlChSRange {
        range: 0x1e48..=0x1e48,
    },
    XmlChSRange {
        range: 0x1e4a..=0x1e4a,
    },
    XmlChSRange {
        range: 0x1e4c..=0x1e4c,
    },
    XmlChSRange {
        range: 0x1e4e..=0x1e4e,
    },
    XmlChSRange {
        range: 0x1e50..=0x1e50,
    },
    XmlChSRange {
        range: 0x1e52..=0x1e52,
    },
    XmlChSRange {
        range: 0x1e54..=0x1e54,
    },
    XmlChSRange {
        range: 0x1e56..=0x1e56,
    },
    XmlChSRange {
        range: 0x1e58..=0x1e58,
    },
    XmlChSRange {
        range: 0x1e5a..=0x1e5a,
    },
    XmlChSRange {
        range: 0x1e5c..=0x1e5c,
    },
    XmlChSRange {
        range: 0x1e5e..=0x1e5e,
    },
    XmlChSRange {
        range: 0x1e60..=0x1e60,
    },
    XmlChSRange {
        range: 0x1e62..=0x1e62,
    },
    XmlChSRange {
        range: 0x1e64..=0x1e64,
    },
    XmlChSRange {
        range: 0x1e66..=0x1e66,
    },
    XmlChSRange {
        range: 0x1e68..=0x1e68,
    },
    XmlChSRange {
        range: 0x1e6a..=0x1e6a,
    },
    XmlChSRange {
        range: 0x1e6c..=0x1e6c,
    },
    XmlChSRange {
        range: 0x1e6e..=0x1e6e,
    },
    XmlChSRange {
        range: 0x1e70..=0x1e70,
    },
    XmlChSRange {
        range: 0x1e72..=0x1e72,
    },
    XmlChSRange {
        range: 0x1e74..=0x1e74,
    },
    XmlChSRange {
        range: 0x1e76..=0x1e76,
    },
    XmlChSRange {
        range: 0x1e78..=0x1e78,
    },
    XmlChSRange {
        range: 0x1e7a..=0x1e7a,
    },
    XmlChSRange {
        range: 0x1e7c..=0x1e7c,
    },
    XmlChSRange {
        range: 0x1e7e..=0x1e7e,
    },
    XmlChSRange {
        range: 0x1e80..=0x1e80,
    },
    XmlChSRange {
        range: 0x1e82..=0x1e82,
    },
    XmlChSRange {
        range: 0x1e84..=0x1e84,
    },
    XmlChSRange {
        range: 0x1e86..=0x1e86,
    },
    XmlChSRange {
        range: 0x1e88..=0x1e88,
    },
    XmlChSRange {
        range: 0x1e8a..=0x1e8a,
    },
    XmlChSRange {
        range: 0x1e8c..=0x1e8c,
    },
    XmlChSRange {
        range: 0x1e8e..=0x1e8e,
    },
    XmlChSRange {
        range: 0x1e90..=0x1e90,
    },
    XmlChSRange {
        range: 0x1e92..=0x1e92,
    },
    XmlChSRange {
        range: 0x1e94..=0x1e94,
    },
    XmlChSRange {
        range: 0x1ea0..=0x1ea0,
    },
    XmlChSRange {
        range: 0x1ea2..=0x1ea2,
    },
    XmlChSRange {
        range: 0x1ea4..=0x1ea4,
    },
    XmlChSRange {
        range: 0x1ea6..=0x1ea6,
    },
    XmlChSRange {
        range: 0x1ea8..=0x1ea8,
    },
    XmlChSRange {
        range: 0x1eaa..=0x1eaa,
    },
    XmlChSRange {
        range: 0x1eac..=0x1eac,
    },
    XmlChSRange {
        range: 0x1eae..=0x1eae,
    },
    XmlChSRange {
        range: 0x1eb0..=0x1eb0,
    },
    XmlChSRange {
        range: 0x1eb2..=0x1eb2,
    },
    XmlChSRange {
        range: 0x1eb4..=0x1eb4,
    },
    XmlChSRange {
        range: 0x1eb6..=0x1eb6,
    },
    XmlChSRange {
        range: 0x1eb8..=0x1eb8,
    },
    XmlChSRange {
        range: 0x1eba..=0x1eba,
    },
    XmlChSRange {
        range: 0x1ebc..=0x1ebc,
    },
    XmlChSRange {
        range: 0x1ebe..=0x1ebe,
    },
    XmlChSRange {
        range: 0x1ec0..=0x1ec0,
    },
    XmlChSRange {
        range: 0x1ec2..=0x1ec2,
    },
    XmlChSRange {
        range: 0x1ec4..=0x1ec4,
    },
    XmlChSRange {
        range: 0x1ec6..=0x1ec6,
    },
    XmlChSRange {
        range: 0x1ec8..=0x1ec8,
    },
    XmlChSRange {
        range: 0x1eca..=0x1eca,
    },
    XmlChSRange {
        range: 0x1ecc..=0x1ecc,
    },
    XmlChSRange {
        range: 0x1ece..=0x1ece,
    },
    XmlChSRange {
        range: 0x1ed0..=0x1ed0,
    },
    XmlChSRange {
        range: 0x1ed2..=0x1ed2,
    },
    XmlChSRange {
        range: 0x1ed4..=0x1ed4,
    },
    XmlChSRange {
        range: 0x1ed6..=0x1ed6,
    },
    XmlChSRange {
        range: 0x1ed8..=0x1ed8,
    },
    XmlChSRange {
        range: 0x1eda..=0x1eda,
    },
    XmlChSRange {
        range: 0x1edc..=0x1edc,
    },
    XmlChSRange {
        range: 0x1ede..=0x1ede,
    },
    XmlChSRange {
        range: 0x1ee0..=0x1ee0,
    },
    XmlChSRange {
        range: 0x1ee2..=0x1ee2,
    },
    XmlChSRange {
        range: 0x1ee4..=0x1ee4,
    },
    XmlChSRange {
        range: 0x1ee6..=0x1ee6,
    },
    XmlChSRange {
        range: 0x1ee8..=0x1ee8,
    },
    XmlChSRange {
        range: 0x1eea..=0x1eea,
    },
    XmlChSRange {
        range: 0x1eec..=0x1eec,
    },
    XmlChSRange {
        range: 0x1eee..=0x1eee,
    },
    XmlChSRange {
        range: 0x1ef0..=0x1ef0,
    },
    XmlChSRange {
        range: 0x1ef2..=0x1ef2,
    },
    XmlChSRange {
        range: 0x1ef4..=0x1ef4,
    },
    XmlChSRange {
        range: 0x1ef6..=0x1ef6,
    },
    XmlChSRange {
        range: 0x1ef8..=0x1ef8,
    },
    XmlChSRange {
        range: 0x1f08..=0x1f0f,
    },
    XmlChSRange {
        range: 0x1f18..=0x1f1d,
    },
    XmlChSRange {
        range: 0x1f28..=0x1f2f,
    },
    XmlChSRange {
        range: 0x1f38..=0x1f3f,
    },
    XmlChSRange {
        range: 0x1f48..=0x1f4d,
    },
    XmlChSRange {
        range: 0x1f59..=0x1f59,
    },
    XmlChSRange {
        range: 0x1f5b..=0x1f5b,
    },
    XmlChSRange {
        range: 0x1f5d..=0x1f5d,
    },
    XmlChSRange {
        range: 0x1f5f..=0x1f5f,
    },
    XmlChSRange {
        range: 0x1f68..=0x1f6f,
    },
    XmlChSRange {
        range: 0x1fb8..=0x1fbb,
    },
    XmlChSRange {
        range: 0x1fc8..=0x1fcb,
    },
    XmlChSRange {
        range: 0x1fd8..=0x1fdb,
    },
    XmlChSRange {
        range: 0x1fe8..=0x1fec,
    },
    XmlChSRange {
        range: 0x1ff8..=0x1ffb,
    },
    XmlChSRange {
        range: 0x2102..=0x2102,
    },
    XmlChSRange {
        range: 0x2107..=0x2107,
    },
    XmlChSRange {
        range: 0x210b..=0x210d,
    },
    XmlChSRange {
        range: 0x2110..=0x2112,
    },
    XmlChSRange {
        range: 0x2115..=0x2115,
    },
    XmlChSRange {
        range: 0x2119..=0x211d,
    },
    XmlChSRange {
        range: 0x2124..=0x2124,
    },
    XmlChSRange {
        range: 0x2126..=0x2126,
    },
    XmlChSRange {
        range: 0x2128..=0x2128,
    },
    XmlChSRange {
        range: 0x212a..=0x212d,
    },
    XmlChSRange {
        range: 0x2130..=0x2131,
    },
    XmlChSRange {
        range: 0x2133..=0x2133,
    },
    XmlChSRange {
        range: 0x213e..=0x213f,
    },
    XmlChSRange {
        range: 0x2145..=0x2145,
    },
    XmlChSRange {
        range: 0xff21..=0xff3a,
    },
];

const XML_LU_L: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x10400..=0x10427,
    },
    XmlChLRange {
        range: 0x1d400..=0x1d419,
    },
    XmlChLRange {
        range: 0x1d434..=0x1d44d,
    },
    XmlChLRange {
        range: 0x1d468..=0x1d481,
    },
    XmlChLRange {
        range: 0x1d49c..=0x1d49c,
    },
    XmlChLRange {
        range: 0x1d49e..=0x1d49f,
    },
    XmlChLRange {
        range: 0x1d4a2..=0x1d4a2,
    },
    XmlChLRange {
        range: 0x1d4a5..=0x1d4a6,
    },
    XmlChLRange {
        range: 0x1d4a9..=0x1d4ac,
    },
    XmlChLRange {
        range: 0x1d4ae..=0x1d4b5,
    },
    XmlChLRange {
        range: 0x1d4d0..=0x1d4e9,
    },
    XmlChLRange {
        range: 0x1d504..=0x1d505,
    },
    XmlChLRange {
        range: 0x1d507..=0x1d50a,
    },
    XmlChLRange {
        range: 0x1d50d..=0x1d514,
    },
    XmlChLRange {
        range: 0x1d516..=0x1d51c,
    },
    XmlChLRange {
        range: 0x1d538..=0x1d539,
    },
    XmlChLRange {
        range: 0x1d53b..=0x1d53e,
    },
    XmlChLRange {
        range: 0x1d540..=0x1d544,
    },
    XmlChLRange {
        range: 0x1d546..=0x1d546,
    },
    XmlChLRange {
        range: 0x1d54a..=0x1d550,
    },
    XmlChLRange {
        range: 0x1d56c..=0x1d585,
    },
    XmlChLRange {
        range: 0x1d5a0..=0x1d5b9,
    },
    XmlChLRange {
        range: 0x1d5d4..=0x1d5ed,
    },
    XmlChLRange {
        range: 0x1d608..=0x1d621,
    },
    XmlChLRange {
        range: 0x1d63c..=0x1d655,
    },
    XmlChLRange {
        range: 0x1d670..=0x1d689,
    },
    XmlChLRange {
        range: 0x1d6a8..=0x1d6c0,
    },
    XmlChLRange {
        range: 0x1d6e2..=0x1d6fa,
    },
    XmlChLRange {
        range: 0x1d71c..=0x1d734,
    },
    XmlChLRange {
        range: 0x1d756..=0x1d76e,
    },
    XmlChLRange {
        range: 0x1d790..=0x1d7a8,
    },
];

const XML_LU_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_LU_S,
    long_range: XML_LU_L,
};

const XML_MS: &[XmlChSRange] = &[
    XmlChSRange {
        range: 0x300..=0x357,
    },
    XmlChSRange {
        range: 0x35d..=0x36f,
    },
    XmlChSRange {
        range: 0x483..=0x486,
    },
    XmlChSRange {
        range: 0x488..=0x489,
    },
    XmlChSRange {
        range: 0x591..=0x5a1,
    },
    XmlChSRange {
        range: 0x5a3..=0x5b9,
    },
    XmlChSRange {
        range: 0x5bb..=0x5bd,
    },
    XmlChSRange {
        range: 0x5bf..=0x5bf,
    },
    XmlChSRange {
        range: 0x5c1..=0x5c2,
    },
    XmlChSRange {
        range: 0x5c4..=0x5c4,
    },
    XmlChSRange {
        range: 0x610..=0x615,
    },
    XmlChSRange {
        range: 0x64b..=0x658,
    },
    XmlChSRange {
        range: 0x670..=0x670,
    },
    XmlChSRange {
        range: 0x6d6..=0x6dc,
    },
    XmlChSRange {
        range: 0x6de..=0x6e4,
    },
    XmlChSRange {
        range: 0x6e7..=0x6e8,
    },
    XmlChSRange {
        range: 0x6ea..=0x6ed,
    },
    XmlChSRange {
        range: 0x711..=0x711,
    },
    XmlChSRange {
        range: 0x730..=0x74a,
    },
    XmlChSRange {
        range: 0x7a6..=0x7b0,
    },
    XmlChSRange {
        range: 0x901..=0x903,
    },
    XmlChSRange {
        range: 0x93c..=0x93c,
    },
    XmlChSRange {
        range: 0x93e..=0x94d,
    },
    XmlChSRange {
        range: 0x951..=0x954,
    },
    XmlChSRange {
        range: 0x962..=0x963,
    },
    XmlChSRange {
        range: 0x981..=0x983,
    },
    XmlChSRange {
        range: 0x9bc..=0x9bc,
    },
    XmlChSRange {
        range: 0x9be..=0x9c4,
    },
    XmlChSRange {
        range: 0x9c7..=0x9c8,
    },
    XmlChSRange {
        range: 0x9cb..=0x9cd,
    },
    XmlChSRange {
        range: 0x9d7..=0x9d7,
    },
    XmlChSRange {
        range: 0x9e2..=0x9e3,
    },
    XmlChSRange {
        range: 0xa01..=0xa03,
    },
    XmlChSRange {
        range: 0xa3c..=0xa3c,
    },
    XmlChSRange {
        range: 0xa3e..=0xa42,
    },
    XmlChSRange {
        range: 0xa47..=0xa48,
    },
    XmlChSRange {
        range: 0xa4b..=0xa4d,
    },
    XmlChSRange {
        range: 0xa70..=0xa71,
    },
    XmlChSRange {
        range: 0xa81..=0xa83,
    },
    XmlChSRange {
        range: 0xabc..=0xabc,
    },
    XmlChSRange {
        range: 0xabe..=0xac5,
    },
    XmlChSRange {
        range: 0xac7..=0xac9,
    },
    XmlChSRange {
        range: 0xacb..=0xacd,
    },
    XmlChSRange {
        range: 0xae2..=0xae3,
    },
    XmlChSRange {
        range: 0xb01..=0xb03,
    },
    XmlChSRange {
        range: 0xb3c..=0xb3c,
    },
    XmlChSRange {
        range: 0xb3e..=0xb43,
    },
    XmlChSRange {
        range: 0xb47..=0xb48,
    },
    XmlChSRange {
        range: 0xb4b..=0xb4d,
    },
    XmlChSRange {
        range: 0xb56..=0xb57,
    },
    XmlChSRange {
        range: 0xb82..=0xb82,
    },
    XmlChSRange {
        range: 0xbbe..=0xbc2,
    },
    XmlChSRange {
        range: 0xbc6..=0xbc8,
    },
    XmlChSRange {
        range: 0xbca..=0xbcd,
    },
    XmlChSRange {
        range: 0xbd7..=0xbd7,
    },
    XmlChSRange {
        range: 0xc01..=0xc03,
    },
    XmlChSRange {
        range: 0xc3e..=0xc44,
    },
    XmlChSRange {
        range: 0xc46..=0xc48,
    },
    XmlChSRange {
        range: 0xc4a..=0xc4d,
    },
    XmlChSRange {
        range: 0xc55..=0xc56,
    },
    XmlChSRange {
        range: 0xc82..=0xc83,
    },
    XmlChSRange {
        range: 0xcbc..=0xcbc,
    },
    XmlChSRange {
        range: 0xcbe..=0xcc4,
    },
    XmlChSRange {
        range: 0xcc6..=0xcc8,
    },
    XmlChSRange {
        range: 0xcca..=0xccd,
    },
    XmlChSRange {
        range: 0xcd5..=0xcd6,
    },
    XmlChSRange {
        range: 0xd02..=0xd03,
    },
    XmlChSRange {
        range: 0xd3e..=0xd43,
    },
    XmlChSRange {
        range: 0xd46..=0xd48,
    },
    XmlChSRange {
        range: 0xd4a..=0xd4d,
    },
    XmlChSRange {
        range: 0xd57..=0xd57,
    },
    XmlChSRange {
        range: 0xd82..=0xd83,
    },
    XmlChSRange {
        range: 0xdca..=0xdca,
    },
    XmlChSRange {
        range: 0xdcf..=0xdd4,
    },
    XmlChSRange {
        range: 0xdd6..=0xdd6,
    },
    XmlChSRange {
        range: 0xdd8..=0xddf,
    },
    XmlChSRange {
        range: 0xdf2..=0xdf3,
    },
    XmlChSRange {
        range: 0xe31..=0xe31,
    },
    XmlChSRange {
        range: 0xe34..=0xe3a,
    },
    XmlChSRange {
        range: 0xe47..=0xe4e,
    },
    XmlChSRange {
        range: 0xeb1..=0xeb1,
    },
    XmlChSRange {
        range: 0xeb4..=0xeb9,
    },
    XmlChSRange {
        range: 0xebb..=0xebc,
    },
    XmlChSRange {
        range: 0xec8..=0xecd,
    },
    XmlChSRange {
        range: 0xf18..=0xf19,
    },
    XmlChSRange {
        range: 0xf35..=0xf35,
    },
    XmlChSRange {
        range: 0xf37..=0xf37,
    },
    XmlChSRange {
        range: 0xf39..=0xf39,
    },
    XmlChSRange {
        range: 0xf3e..=0xf3f,
    },
    XmlChSRange {
        range: 0xf71..=0xf84,
    },
    XmlChSRange {
        range: 0xf86..=0xf87,
    },
    XmlChSRange {
        range: 0xf90..=0xf97,
    },
    XmlChSRange {
        range: 0xf99..=0xfbc,
    },
    XmlChSRange {
        range: 0xfc6..=0xfc6,
    },
    XmlChSRange {
        range: 0x102c..=0x1032,
    },
    XmlChSRange {
        range: 0x1036..=0x1039,
    },
    XmlChSRange {
        range: 0x1056..=0x1059,
    },
    XmlChSRange {
        range: 0x1712..=0x1714,
    },
    XmlChSRange {
        range: 0x1732..=0x1734,
    },
    XmlChSRange {
        range: 0x1752..=0x1753,
    },
    XmlChSRange {
        range: 0x1772..=0x1773,
    },
    XmlChSRange {
        range: 0x17b6..=0x17d3,
    },
    XmlChSRange {
        range: 0x17dd..=0x17dd,
    },
    XmlChSRange {
        range: 0x180b..=0x180d,
    },
    XmlChSRange {
        range: 0x18a9..=0x18a9,
    },
    XmlChSRange {
        range: 0x1920..=0x192b,
    },
    XmlChSRange {
        range: 0x1930..=0x193b,
    },
    XmlChSRange {
        range: 0x20d0..=0x20ea,
    },
    XmlChSRange {
        range: 0x302a..=0x302f,
    },
    XmlChSRange {
        range: 0x3099..=0x309a,
    },
    XmlChSRange {
        range: 0xfb1e..=0xfb1e,
    },
    XmlChSRange {
        range: 0xfe00..=0xfe0f,
    },
    XmlChSRange {
        range: 0xfe20..=0xfe23,
    },
];

const XML_ML: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x1d165..=0x1d169,
    },
    XmlChLRange {
        range: 0x1d16d..=0x1d172,
    },
    XmlChLRange {
        range: 0x1d17b..=0x1d182,
    },
    XmlChLRange {
        range: 0x1d185..=0x1d18b,
    },
    XmlChLRange {
        range: 0x1d1aa..=0x1d1ad,
    },
    XmlChLRange {
        range: 0xe0100..=0xe01ef,
    },
];

const XML_MG: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_MS,
    long_range: XML_ML,
};

const XML_MC_S: &[XmlChSRange] = &[
    XmlChSRange {
        range: 0x903..=0x903,
    },
    XmlChSRange {
        range: 0x93e..=0x940,
    },
    XmlChSRange {
        range: 0x949..=0x94c,
    },
    XmlChSRange {
        range: 0x982..=0x983,
    },
    XmlChSRange {
        range: 0x9be..=0x9c0,
    },
    XmlChSRange {
        range: 0x9c7..=0x9c8,
    },
    XmlChSRange {
        range: 0x9cb..=0x9cc,
    },
    XmlChSRange {
        range: 0x9d7..=0x9d7,
    },
    XmlChSRange {
        range: 0xa03..=0xa03,
    },
    XmlChSRange {
        range: 0xa3e..=0xa40,
    },
    XmlChSRange {
        range: 0xa83..=0xa83,
    },
    XmlChSRange {
        range: 0xabe..=0xac0,
    },
    XmlChSRange {
        range: 0xac9..=0xac9,
    },
    XmlChSRange {
        range: 0xacb..=0xacc,
    },
    XmlChSRange {
        range: 0xb02..=0xb03,
    },
    XmlChSRange {
        range: 0xb3e..=0xb3e,
    },
    XmlChSRange {
        range: 0xb40..=0xb40,
    },
    XmlChSRange {
        range: 0xb47..=0xb48,
    },
    XmlChSRange {
        range: 0xb4b..=0xb4c,
    },
    XmlChSRange {
        range: 0xb57..=0xb57,
    },
    XmlChSRange {
        range: 0xbbe..=0xbbf,
    },
    XmlChSRange {
        range: 0xbc1..=0xbc2,
    },
    XmlChSRange {
        range: 0xbc6..=0xbc8,
    },
    XmlChSRange {
        range: 0xbca..=0xbcc,
    },
    XmlChSRange {
        range: 0xbd7..=0xbd7,
    },
    XmlChSRange {
        range: 0xc01..=0xc03,
    },
    XmlChSRange {
        range: 0xc41..=0xc44,
    },
    XmlChSRange {
        range: 0xc82..=0xc83,
    },
    XmlChSRange {
        range: 0xcbe..=0xcbe,
    },
    XmlChSRange {
        range: 0xcc0..=0xcc4,
    },
    XmlChSRange {
        range: 0xcc7..=0xcc8,
    },
    XmlChSRange {
        range: 0xcca..=0xccb,
    },
    XmlChSRange {
        range: 0xcd5..=0xcd6,
    },
    XmlChSRange {
        range: 0xd02..=0xd03,
    },
    XmlChSRange {
        range: 0xd3e..=0xd40,
    },
    XmlChSRange {
        range: 0xd46..=0xd48,
    },
    XmlChSRange {
        range: 0xd4a..=0xd4c,
    },
    XmlChSRange {
        range: 0xd57..=0xd57,
    },
    XmlChSRange {
        range: 0xd82..=0xd83,
    },
    XmlChSRange {
        range: 0xdcf..=0xdd1,
    },
    XmlChSRange {
        range: 0xdd8..=0xddf,
    },
    XmlChSRange {
        range: 0xdf2..=0xdf3,
    },
    XmlChSRange {
        range: 0xf3e..=0xf3f,
    },
    XmlChSRange {
        range: 0xf7f..=0xf7f,
    },
    XmlChSRange {
        range: 0x102c..=0x102c,
    },
    XmlChSRange {
        range: 0x1031..=0x1031,
    },
    XmlChSRange {
        range: 0x1038..=0x1038,
    },
    XmlChSRange {
        range: 0x1056..=0x1057,
    },
    XmlChSRange {
        range: 0x17b6..=0x17b6,
    },
    XmlChSRange {
        range: 0x17be..=0x17c5,
    },
    XmlChSRange {
        range: 0x17c7..=0x17c8,
    },
    XmlChSRange {
        range: 0x1923..=0x1926,
    },
    XmlChSRange {
        range: 0x1929..=0x192b,
    },
    XmlChSRange {
        range: 0x1930..=0x1931,
    },
    XmlChSRange {
        range: 0x1933..=0x1938,
    },
];

const XML_MC_L: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x1d165..=0x1d166,
    },
    XmlChLRange {
        range: 0x1d16d..=0x1d172,
    },
];

const XML_MC_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_MC_S,
    long_range: XML_MC_L,
};

const XML_MN_S: &[XmlChSRange] = &[
    XmlChSRange {
        range: 0x300..=0x357,
    },
    XmlChSRange {
        range: 0x35d..=0x36f,
    },
    XmlChSRange {
        range: 0x483..=0x486,
    },
    XmlChSRange {
        range: 0x591..=0x5a1,
    },
    XmlChSRange {
        range: 0x5a3..=0x5b9,
    },
    XmlChSRange {
        range: 0x5bb..=0x5bd,
    },
    XmlChSRange {
        range: 0x5bf..=0x5bf,
    },
    XmlChSRange {
        range: 0x5c1..=0x5c2,
    },
    XmlChSRange {
        range: 0x5c4..=0x5c4,
    },
    XmlChSRange {
        range: 0x610..=0x615,
    },
    XmlChSRange {
        range: 0x64b..=0x658,
    },
    XmlChSRange {
        range: 0x670..=0x670,
    },
    XmlChSRange {
        range: 0x6d6..=0x6dc,
    },
    XmlChSRange {
        range: 0x6df..=0x6e4,
    },
    XmlChSRange {
        range: 0x6e7..=0x6e8,
    },
    XmlChSRange {
        range: 0x6ea..=0x6ed,
    },
    XmlChSRange {
        range: 0x711..=0x711,
    },
    XmlChSRange {
        range: 0x730..=0x74a,
    },
    XmlChSRange {
        range: 0x7a6..=0x7b0,
    },
    XmlChSRange {
        range: 0x901..=0x902,
    },
    XmlChSRange {
        range: 0x93c..=0x93c,
    },
    XmlChSRange {
        range: 0x941..=0x948,
    },
    XmlChSRange {
        range: 0x94d..=0x94d,
    },
    XmlChSRange {
        range: 0x951..=0x954,
    },
    XmlChSRange {
        range: 0x962..=0x963,
    },
    XmlChSRange {
        range: 0x981..=0x981,
    },
    XmlChSRange {
        range: 0x9bc..=0x9bc,
    },
    XmlChSRange {
        range: 0x9c1..=0x9c4,
    },
    XmlChSRange {
        range: 0x9cd..=0x9cd,
    },
    XmlChSRange {
        range: 0x9e2..=0x9e3,
    },
    XmlChSRange {
        range: 0xa01..=0xa02,
    },
    XmlChSRange {
        range: 0xa3c..=0xa3c,
    },
    XmlChSRange {
        range: 0xa41..=0xa42,
    },
    XmlChSRange {
        range: 0xa47..=0xa48,
    },
    XmlChSRange {
        range: 0xa4b..=0xa4d,
    },
    XmlChSRange {
        range: 0xa70..=0xa71,
    },
    XmlChSRange {
        range: 0xa81..=0xa82,
    },
    XmlChSRange {
        range: 0xabc..=0xabc,
    },
    XmlChSRange {
        range: 0xac1..=0xac5,
    },
    XmlChSRange {
        range: 0xac7..=0xac8,
    },
    XmlChSRange {
        range: 0xacd..=0xacd,
    },
    XmlChSRange {
        range: 0xae2..=0xae3,
    },
    XmlChSRange {
        range: 0xb01..=0xb01,
    },
    XmlChSRange {
        range: 0xb3c..=0xb3c,
    },
    XmlChSRange {
        range: 0xb3f..=0xb3f,
    },
    XmlChSRange {
        range: 0xb41..=0xb43,
    },
    XmlChSRange {
        range: 0xb4d..=0xb4d,
    },
    XmlChSRange {
        range: 0xb56..=0xb56,
    },
    XmlChSRange {
        range: 0xb82..=0xb82,
    },
    XmlChSRange {
        range: 0xbc0..=0xbc0,
    },
    XmlChSRange {
        range: 0xbcd..=0xbcd,
    },
    XmlChSRange {
        range: 0xc3e..=0xc40,
    },
    XmlChSRange {
        range: 0xc46..=0xc48,
    },
    XmlChSRange {
        range: 0xc4a..=0xc4d,
    },
    XmlChSRange {
        range: 0xc55..=0xc56,
    },
    XmlChSRange {
        range: 0xcbc..=0xcbc,
    },
    XmlChSRange {
        range: 0xcbf..=0xcbf,
    },
    XmlChSRange {
        range: 0xcc6..=0xcc6,
    },
    XmlChSRange {
        range: 0xccc..=0xccd,
    },
    XmlChSRange {
        range: 0xd41..=0xd43,
    },
    XmlChSRange {
        range: 0xd4d..=0xd4d,
    },
    XmlChSRange {
        range: 0xdca..=0xdca,
    },
    XmlChSRange {
        range: 0xdd2..=0xdd4,
    },
    XmlChSRange {
        range: 0xdd6..=0xdd6,
    },
    XmlChSRange {
        range: 0xe31..=0xe31,
    },
    XmlChSRange {
        range: 0xe34..=0xe3a,
    },
    XmlChSRange {
        range: 0xe47..=0xe4e,
    },
    XmlChSRange {
        range: 0xeb1..=0xeb1,
    },
    XmlChSRange {
        range: 0xeb4..=0xeb9,
    },
    XmlChSRange {
        range: 0xebb..=0xebc,
    },
    XmlChSRange {
        range: 0xec8..=0xecd,
    },
    XmlChSRange {
        range: 0xf18..=0xf19,
    },
    XmlChSRange {
        range: 0xf35..=0xf35,
    },
    XmlChSRange {
        range: 0xf37..=0xf37,
    },
    XmlChSRange {
        range: 0xf39..=0xf39,
    },
    XmlChSRange {
        range: 0xf71..=0xf7e,
    },
    XmlChSRange {
        range: 0xf80..=0xf84,
    },
    XmlChSRange {
        range: 0xf86..=0xf87,
    },
    XmlChSRange {
        range: 0xf90..=0xf97,
    },
    XmlChSRange {
        range: 0xf99..=0xfbc,
    },
    XmlChSRange {
        range: 0xfc6..=0xfc6,
    },
    XmlChSRange {
        range: 0x102d..=0x1030,
    },
    XmlChSRange {
        range: 0x1032..=0x1032,
    },
    XmlChSRange {
        range: 0x1036..=0x1037,
    },
    XmlChSRange {
        range: 0x1039..=0x1039,
    },
    XmlChSRange {
        range: 0x1058..=0x1059,
    },
    XmlChSRange {
        range: 0x1712..=0x1714,
    },
    XmlChSRange {
        range: 0x1732..=0x1734,
    },
    XmlChSRange {
        range: 0x1752..=0x1753,
    },
    XmlChSRange {
        range: 0x1772..=0x1773,
    },
    XmlChSRange {
        range: 0x17b7..=0x17bd,
    },
    XmlChSRange {
        range: 0x17c6..=0x17c6,
    },
    XmlChSRange {
        range: 0x17c9..=0x17d3,
    },
    XmlChSRange {
        range: 0x17dd..=0x17dd,
    },
    XmlChSRange {
        range: 0x180b..=0x180d,
    },
    XmlChSRange {
        range: 0x18a9..=0x18a9,
    },
    XmlChSRange {
        range: 0x1920..=0x1922,
    },
    XmlChSRange {
        range: 0x1927..=0x1928,
    },
    XmlChSRange {
        range: 0x1932..=0x1932,
    },
    XmlChSRange {
        range: 0x1939..=0x193b,
    },
    XmlChSRange {
        range: 0x20d0..=0x20dc,
    },
    XmlChSRange {
        range: 0x20e1..=0x20e1,
    },
    XmlChSRange {
        range: 0x20e5..=0x20ea,
    },
    XmlChSRange {
        range: 0x302a..=0x302f,
    },
    XmlChSRange {
        range: 0x3099..=0x309a,
    },
    XmlChSRange {
        range: 0xfb1e..=0xfb1e,
    },
    XmlChSRange {
        range: 0xfe00..=0xfe0f,
    },
    XmlChSRange {
        range: 0xfe20..=0xfe23,
    },
];

const XML_MN_L: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x1d167..=0x1d169,
    },
    XmlChLRange {
        range: 0x1d17b..=0x1d182,
    },
    XmlChLRange {
        range: 0x1d185..=0x1d18b,
    },
    XmlChLRange {
        range: 0x1d1aa..=0x1d1ad,
    },
    XmlChLRange {
        range: 0xe0100..=0xe01ef,
    },
];

const XML_MN_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_MN_S,
    long_range: XML_MN_L,
};

const XML_NS: &[XmlChSRange] = &[
    XmlChSRange { range: 0x30..=0x39 },
    XmlChSRange { range: 0xb2..=0xb3 },
    XmlChSRange { range: 0xb9..=0xb9 },
    XmlChSRange { range: 0xbc..=0xbe },
    XmlChSRange {
        range: 0x660..=0x669,
    },
    XmlChSRange {
        range: 0x6f0..=0x6f9,
    },
    XmlChSRange {
        range: 0x966..=0x96f,
    },
    XmlChSRange {
        range: 0x9e6..=0x9ef,
    },
    XmlChSRange {
        range: 0x9f4..=0x9f9,
    },
    XmlChSRange {
        range: 0xa66..=0xa6f,
    },
    XmlChSRange {
        range: 0xae6..=0xaef,
    },
    XmlChSRange {
        range: 0xb66..=0xb6f,
    },
    XmlChSRange {
        range: 0xbe7..=0xbf2,
    },
    XmlChSRange {
        range: 0xc66..=0xc6f,
    },
    XmlChSRange {
        range: 0xce6..=0xcef,
    },
    XmlChSRange {
        range: 0xd66..=0xd6f,
    },
    XmlChSRange {
        range: 0xe50..=0xe59,
    },
    XmlChSRange {
        range: 0xed0..=0xed9,
    },
    XmlChSRange {
        range: 0xf20..=0xf33,
    },
    XmlChSRange {
        range: 0x1040..=0x1049,
    },
    XmlChSRange {
        range: 0x1369..=0x137c,
    },
    XmlChSRange {
        range: 0x16ee..=0x16f0,
    },
    XmlChSRange {
        range: 0x17e0..=0x17e9,
    },
    XmlChSRange {
        range: 0x17f0..=0x17f9,
    },
    XmlChSRange {
        range: 0x1810..=0x1819,
    },
    XmlChSRange {
        range: 0x1946..=0x194f,
    },
    XmlChSRange {
        range: 0x2070..=0x2070,
    },
    XmlChSRange {
        range: 0x2074..=0x2079,
    },
    XmlChSRange {
        range: 0x2080..=0x2089,
    },
    XmlChSRange {
        range: 0x2153..=0x2183,
    },
    XmlChSRange {
        range: 0x2460..=0x249b,
    },
    XmlChSRange {
        range: 0x24ea..=0x24ff,
    },
    XmlChSRange {
        range: 0x2776..=0x2793,
    },
    XmlChSRange {
        range: 0x3007..=0x3007,
    },
    XmlChSRange {
        range: 0x3021..=0x3029,
    },
    XmlChSRange {
        range: 0x3038..=0x303a,
    },
    XmlChSRange {
        range: 0x3192..=0x3195,
    },
    XmlChSRange {
        range: 0x3220..=0x3229,
    },
    XmlChSRange {
        range: 0x3251..=0x325f,
    },
    XmlChSRange {
        range: 0x3280..=0x3289,
    },
    XmlChSRange {
        range: 0x32b1..=0x32bf,
    },
    XmlChSRange {
        range: 0xff10..=0xff19,
    },
];

const XML_NL: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x10107..=0x10133,
    },
    XmlChLRange {
        range: 0x10320..=0x10323,
    },
    XmlChLRange {
        range: 0x1034a..=0x1034a,
    },
    XmlChLRange {
        range: 0x104a0..=0x104a9,
    },
    XmlChLRange {
        range: 0x1d7ce..=0x1d7ff,
    },
];

const XML_NG: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_NS,
    long_range: XML_NL,
};

const XML_ND_S: &[XmlChSRange] = &[
    XmlChSRange { range: 0x30..=0x39 },
    XmlChSRange {
        range: 0x660..=0x669,
    },
    XmlChSRange {
        range: 0x6f0..=0x6f9,
    },
    XmlChSRange {
        range: 0x966..=0x96f,
    },
    XmlChSRange {
        range: 0x9e6..=0x9ef,
    },
    XmlChSRange {
        range: 0xa66..=0xa6f,
    },
    XmlChSRange {
        range: 0xae6..=0xaef,
    },
    XmlChSRange {
        range: 0xb66..=0xb6f,
    },
    XmlChSRange {
        range: 0xbe7..=0xbef,
    },
    XmlChSRange {
        range: 0xc66..=0xc6f,
    },
    XmlChSRange {
        range: 0xce6..=0xcef,
    },
    XmlChSRange {
        range: 0xd66..=0xd6f,
    },
    XmlChSRange {
        range: 0xe50..=0xe59,
    },
    XmlChSRange {
        range: 0xed0..=0xed9,
    },
    XmlChSRange {
        range: 0xf20..=0xf29,
    },
    XmlChSRange {
        range: 0x1040..=0x1049,
    },
    XmlChSRange {
        range: 0x1369..=0x1371,
    },
    XmlChSRange {
        range: 0x17e0..=0x17e9,
    },
    XmlChSRange {
        range: 0x1810..=0x1819,
    },
    XmlChSRange {
        range: 0x1946..=0x194f,
    },
    XmlChSRange {
        range: 0xff10..=0xff19,
    },
];

const XML_ND_L: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x104a0..=0x104a9,
    },
    XmlChLRange {
        range: 0x1d7ce..=0x1d7ff,
    },
];

const XML_ND_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_ND_S,
    long_range: XML_ND_L,
};

const XML_NO_S: &[XmlChSRange] = &[
    XmlChSRange { range: 0xb2..=0xb3 },
    XmlChSRange { range: 0xb9..=0xb9 },
    XmlChSRange { range: 0xbc..=0xbe },
    XmlChSRange {
        range: 0x9f4..=0x9f9,
    },
    XmlChSRange {
        range: 0xbf0..=0xbf2,
    },
    XmlChSRange {
        range: 0xf2a..=0xf33,
    },
    XmlChSRange {
        range: 0x1372..=0x137c,
    },
    XmlChSRange {
        range: 0x17f0..=0x17f9,
    },
    XmlChSRange {
        range: 0x2070..=0x2070,
    },
    XmlChSRange {
        range: 0x2074..=0x2079,
    },
    XmlChSRange {
        range: 0x2080..=0x2089,
    },
    XmlChSRange {
        range: 0x2153..=0x215f,
    },
    XmlChSRange {
        range: 0x2460..=0x249b,
    },
    XmlChSRange {
        range: 0x24ea..=0x24ff,
    },
    XmlChSRange {
        range: 0x2776..=0x2793,
    },
    XmlChSRange {
        range: 0x3192..=0x3195,
    },
    XmlChSRange {
        range: 0x3220..=0x3229,
    },
    XmlChSRange {
        range: 0x3251..=0x325f,
    },
    XmlChSRange {
        range: 0x3280..=0x3289,
    },
    XmlChSRange {
        range: 0x32b1..=0x32bf,
    },
];

const XML_NO_L: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x10107..=0x10133,
    },
    XmlChLRange {
        range: 0x10320..=0x10323,
    },
];

const XML_NO_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_NO_S,
    long_range: XML_NO_L,
};

const XML_PS: &[XmlChSRange] = &[
    XmlChSRange { range: 0x21..=0x23 },
    XmlChSRange { range: 0x25..=0x2a },
    XmlChSRange { range: 0x2c..=0x2f },
    XmlChSRange { range: 0x3a..=0x3b },
    XmlChSRange { range: 0x3f..=0x40 },
    XmlChSRange { range: 0x5b..=0x5d },
    XmlChSRange { range: 0x5f..=0x5f },
    XmlChSRange { range: 0x7b..=0x7b },
    XmlChSRange { range: 0x7d..=0x7d },
    XmlChSRange { range: 0xa1..=0xa1 },
    XmlChSRange { range: 0xab..=0xab },
    XmlChSRange { range: 0xb7..=0xb7 },
    XmlChSRange { range: 0xbb..=0xbb },
    XmlChSRange { range: 0xbf..=0xbf },
    XmlChSRange {
        range: 0x37e..=0x37e,
    },
    XmlChSRange {
        range: 0x387..=0x387,
    },
    XmlChSRange {
        range: 0x55a..=0x55f,
    },
    XmlChSRange {
        range: 0x589..=0x58a,
    },
    XmlChSRange {
        range: 0x5be..=0x5be,
    },
    XmlChSRange {
        range: 0x5c0..=0x5c0,
    },
    XmlChSRange {
        range: 0x5c3..=0x5c3,
    },
    XmlChSRange {
        range: 0x5f3..=0x5f4,
    },
    XmlChSRange {
        range: 0x60c..=0x60d,
    },
    XmlChSRange {
        range: 0x61b..=0x61b,
    },
    XmlChSRange {
        range: 0x61f..=0x61f,
    },
    XmlChSRange {
        range: 0x66a..=0x66d,
    },
    XmlChSRange {
        range: 0x6d4..=0x6d4,
    },
    XmlChSRange {
        range: 0x700..=0x70d,
    },
    XmlChSRange {
        range: 0x964..=0x965,
    },
    XmlChSRange {
        range: 0x970..=0x970,
    },
    XmlChSRange {
        range: 0xdf4..=0xdf4,
    },
    XmlChSRange {
        range: 0xe4f..=0xe4f,
    },
    XmlChSRange {
        range: 0xe5a..=0xe5b,
    },
    XmlChSRange {
        range: 0xf04..=0xf12,
    },
    XmlChSRange {
        range: 0xf3a..=0xf3d,
    },
    XmlChSRange {
        range: 0xf85..=0xf85,
    },
    XmlChSRange {
        range: 0x104a..=0x104f,
    },
    XmlChSRange {
        range: 0x10fb..=0x10fb,
    },
    XmlChSRange {
        range: 0x1361..=0x1368,
    },
    XmlChSRange {
        range: 0x166d..=0x166e,
    },
    XmlChSRange {
        range: 0x169b..=0x169c,
    },
    XmlChSRange {
        range: 0x16eb..=0x16ed,
    },
    XmlChSRange {
        range: 0x1735..=0x1736,
    },
    XmlChSRange {
        range: 0x17d4..=0x17d6,
    },
    XmlChSRange {
        range: 0x17d8..=0x17da,
    },
    XmlChSRange {
        range: 0x1800..=0x180a,
    },
    XmlChSRange {
        range: 0x1944..=0x1945,
    },
    XmlChSRange {
        range: 0x2010..=0x2027,
    },
    XmlChSRange {
        range: 0x2030..=0x2043,
    },
    XmlChSRange {
        range: 0x2045..=0x2051,
    },
    XmlChSRange {
        range: 0x2053..=0x2054,
    },
    XmlChSRange {
        range: 0x2057..=0x2057,
    },
    XmlChSRange {
        range: 0x207d..=0x207e,
    },
    XmlChSRange {
        range: 0x208d..=0x208e,
    },
    XmlChSRange {
        range: 0x2329..=0x232a,
    },
    XmlChSRange {
        range: 0x23b4..=0x23b6,
    },
    XmlChSRange {
        range: 0x2768..=0x2775,
    },
    XmlChSRange {
        range: 0x27e6..=0x27eb,
    },
    XmlChSRange {
        range: 0x2983..=0x2998,
    },
    XmlChSRange {
        range: 0x29d8..=0x29db,
    },
    XmlChSRange {
        range: 0x29fc..=0x29fd,
    },
    XmlChSRange {
        range: 0x3001..=0x3003,
    },
    XmlChSRange {
        range: 0x3008..=0x3011,
    },
    XmlChSRange {
        range: 0x3014..=0x301f,
    },
    XmlChSRange {
        range: 0x3030..=0x3030,
    },
    XmlChSRange {
        range: 0x303d..=0x303d,
    },
    XmlChSRange {
        range: 0x30a0..=0x30a0,
    },
    XmlChSRange {
        range: 0x30fb..=0x30fb,
    },
    XmlChSRange {
        range: 0xfd3e..=0xfd3f,
    },
    XmlChSRange {
        range: 0xfe30..=0xfe52,
    },
    XmlChSRange {
        range: 0xfe54..=0xfe61,
    },
    XmlChSRange {
        range: 0xfe63..=0xfe63,
    },
    XmlChSRange {
        range: 0xfe68..=0xfe68,
    },
    XmlChSRange {
        range: 0xfe6a..=0xfe6b,
    },
    XmlChSRange {
        range: 0xff01..=0xff03,
    },
    XmlChSRange {
        range: 0xff05..=0xff0a,
    },
    XmlChSRange {
        range: 0xff0c..=0xff0f,
    },
    XmlChSRange {
        range: 0xff1a..=0xff1b,
    },
    XmlChSRange {
        range: 0xff1f..=0xff20,
    },
    XmlChSRange {
        range: 0xff3b..=0xff3d,
    },
    XmlChSRange {
        range: 0xff3f..=0xff3f,
    },
    XmlChSRange {
        range: 0xff5b..=0xff5b,
    },
    XmlChSRange {
        range: 0xff5d..=0xff5d,
    },
    XmlChSRange {
        range: 0xff5f..=0xff65,
    },
];

const XML_PL: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x10100..=0x10101,
    },
    XmlChLRange {
        range: 0x1039f..=0x1039f,
    },
];

const XML_PG: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_PS,
    long_range: XML_PL,
};

const XML_PD_S: &[XmlChSRange] = &[
    XmlChSRange { range: 0x2d..=0x2d },
    XmlChSRange {
        range: 0x58a..=0x58a,
    },
    XmlChSRange {
        range: 0x1806..=0x1806,
    },
    XmlChSRange {
        range: 0x2010..=0x2015,
    },
    XmlChSRange {
        range: 0x301c..=0x301c,
    },
    XmlChSRange {
        range: 0x3030..=0x3030,
    },
    XmlChSRange {
        range: 0x30a0..=0x30a0,
    },
    XmlChSRange {
        range: 0xfe31..=0xfe32,
    },
    XmlChSRange {
        range: 0xfe58..=0xfe58,
    },
    XmlChSRange {
        range: 0xfe63..=0xfe63,
    },
    XmlChSRange {
        range: 0xff0d..=0xff0d,
    },
];

const XML_PD_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_PD_S,
    long_range: &[],
};

const XML_PE_S: &[XmlChSRange] = &[
    XmlChSRange { range: 0x29..=0x29 },
    XmlChSRange { range: 0x5d..=0x5d },
    XmlChSRange { range: 0x7d..=0x7d },
    XmlChSRange {
        range: 0xf3b..=0xf3b,
    },
    XmlChSRange {
        range: 0xf3d..=0xf3d,
    },
    XmlChSRange {
        range: 0x169c..=0x169c,
    },
    XmlChSRange {
        range: 0x2046..=0x2046,
    },
    XmlChSRange {
        range: 0x207e..=0x207e,
    },
    XmlChSRange {
        range: 0x208e..=0x208e,
    },
    XmlChSRange {
        range: 0x232a..=0x232a,
    },
    XmlChSRange {
        range: 0x23b5..=0x23b5,
    },
    XmlChSRange {
        range: 0x2769..=0x2769,
    },
    XmlChSRange {
        range: 0x276b..=0x276b,
    },
    XmlChSRange {
        range: 0x276d..=0x276d,
    },
    XmlChSRange {
        range: 0x276f..=0x276f,
    },
    XmlChSRange {
        range: 0x2771..=0x2771,
    },
    XmlChSRange {
        range: 0x2773..=0x2773,
    },
    XmlChSRange {
        range: 0x2775..=0x2775,
    },
    XmlChSRange {
        range: 0x27e7..=0x27e7,
    },
    XmlChSRange {
        range: 0x27e9..=0x27e9,
    },
    XmlChSRange {
        range: 0x27eb..=0x27eb,
    },
    XmlChSRange {
        range: 0x2984..=0x2984,
    },
    XmlChSRange {
        range: 0x2986..=0x2986,
    },
    XmlChSRange {
        range: 0x2988..=0x2988,
    },
    XmlChSRange {
        range: 0x298a..=0x298a,
    },
    XmlChSRange {
        range: 0x298c..=0x298c,
    },
    XmlChSRange {
        range: 0x298e..=0x298e,
    },
    XmlChSRange {
        range: 0x2990..=0x2990,
    },
    XmlChSRange {
        range: 0x2992..=0x2992,
    },
    XmlChSRange {
        range: 0x2994..=0x2994,
    },
    XmlChSRange {
        range: 0x2996..=0x2996,
    },
    XmlChSRange {
        range: 0x2998..=0x2998,
    },
    XmlChSRange {
        range: 0x29d9..=0x29d9,
    },
    XmlChSRange {
        range: 0x29db..=0x29db,
    },
    XmlChSRange {
        range: 0x29fd..=0x29fd,
    },
    XmlChSRange {
        range: 0x3009..=0x3009,
    },
    XmlChSRange {
        range: 0x300b..=0x300b,
    },
    XmlChSRange {
        range: 0x300d..=0x300d,
    },
    XmlChSRange {
        range: 0x300f..=0x300f,
    },
    XmlChSRange {
        range: 0x3011..=0x3011,
    },
    XmlChSRange {
        range: 0x3015..=0x3015,
    },
    XmlChSRange {
        range: 0x3017..=0x3017,
    },
    XmlChSRange {
        range: 0x3019..=0x3019,
    },
    XmlChSRange {
        range: 0x301b..=0x301b,
    },
    XmlChSRange {
        range: 0x301e..=0x301f,
    },
    XmlChSRange {
        range: 0xfd3f..=0xfd3f,
    },
    XmlChSRange {
        range: 0xfe36..=0xfe36,
    },
    XmlChSRange {
        range: 0xfe38..=0xfe38,
    },
    XmlChSRange {
        range: 0xfe3a..=0xfe3a,
    },
    XmlChSRange {
        range: 0xfe3c..=0xfe3c,
    },
    XmlChSRange {
        range: 0xfe3e..=0xfe3e,
    },
    XmlChSRange {
        range: 0xfe40..=0xfe40,
    },
    XmlChSRange {
        range: 0xfe42..=0xfe42,
    },
    XmlChSRange {
        range: 0xfe44..=0xfe44,
    },
    XmlChSRange {
        range: 0xfe48..=0xfe48,
    },
    XmlChSRange {
        range: 0xfe5a..=0xfe5a,
    },
    XmlChSRange {
        range: 0xfe5c..=0xfe5c,
    },
    XmlChSRange {
        range: 0xfe5e..=0xfe5e,
    },
    XmlChSRange {
        range: 0xff09..=0xff09,
    },
    XmlChSRange {
        range: 0xff3d..=0xff3d,
    },
    XmlChSRange {
        range: 0xff5d..=0xff5d,
    },
    XmlChSRange {
        range: 0xff60..=0xff60,
    },
    XmlChSRange {
        range: 0xff63..=0xff63,
    },
];

const XML_PE_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_PE_S,
    long_range: &[],
};

const XML_PO_S: &[XmlChSRange] = &[
    XmlChSRange { range: 0x21..=0x23 },
    XmlChSRange { range: 0x25..=0x27 },
    XmlChSRange { range: 0x2a..=0x2a },
    XmlChSRange { range: 0x2c..=0x2c },
    XmlChSRange { range: 0x2e..=0x2f },
    XmlChSRange { range: 0x3a..=0x3b },
    XmlChSRange { range: 0x3f..=0x40 },
    XmlChSRange { range: 0x5c..=0x5c },
    XmlChSRange { range: 0xa1..=0xa1 },
    XmlChSRange { range: 0xb7..=0xb7 },
    XmlChSRange { range: 0xbf..=0xbf },
    XmlChSRange {
        range: 0x37e..=0x37e,
    },
    XmlChSRange {
        range: 0x387..=0x387,
    },
    XmlChSRange {
        range: 0x55a..=0x55f,
    },
    XmlChSRange {
        range: 0x589..=0x589,
    },
    XmlChSRange {
        range: 0x5be..=0x5be,
    },
    XmlChSRange {
        range: 0x5c0..=0x5c0,
    },
    XmlChSRange {
        range: 0x5c3..=0x5c3,
    },
    XmlChSRange {
        range: 0x5f3..=0x5f4,
    },
    XmlChSRange {
        range: 0x60c..=0x60d,
    },
    XmlChSRange {
        range: 0x61b..=0x61b,
    },
    XmlChSRange {
        range: 0x61f..=0x61f,
    },
    XmlChSRange {
        range: 0x66a..=0x66d,
    },
    XmlChSRange {
        range: 0x6d4..=0x6d4,
    },
    XmlChSRange {
        range: 0x700..=0x70d,
    },
    XmlChSRange {
        range: 0x964..=0x965,
    },
    XmlChSRange {
        range: 0x970..=0x970,
    },
    XmlChSRange {
        range: 0xdf4..=0xdf4,
    },
    XmlChSRange {
        range: 0xe4f..=0xe4f,
    },
    XmlChSRange {
        range: 0xe5a..=0xe5b,
    },
    XmlChSRange {
        range: 0xf04..=0xf12,
    },
    XmlChSRange {
        range: 0xf85..=0xf85,
    },
    XmlChSRange {
        range: 0x104a..=0x104f,
    },
    XmlChSRange {
        range: 0x10fb..=0x10fb,
    },
    XmlChSRange {
        range: 0x1361..=0x1368,
    },
    XmlChSRange {
        range: 0x166d..=0x166e,
    },
    XmlChSRange {
        range: 0x16eb..=0x16ed,
    },
    XmlChSRange {
        range: 0x1735..=0x1736,
    },
    XmlChSRange {
        range: 0x17d4..=0x17d6,
    },
    XmlChSRange {
        range: 0x17d8..=0x17da,
    },
    XmlChSRange {
        range: 0x1800..=0x1805,
    },
    XmlChSRange {
        range: 0x1807..=0x180a,
    },
    XmlChSRange {
        range: 0x1944..=0x1945,
    },
    XmlChSRange {
        range: 0x2016..=0x2017,
    },
    XmlChSRange {
        range: 0x2020..=0x2027,
    },
    XmlChSRange {
        range: 0x2030..=0x2038,
    },
    XmlChSRange {
        range: 0x203b..=0x203e,
    },
    XmlChSRange {
        range: 0x2041..=0x2043,
    },
    XmlChSRange {
        range: 0x2047..=0x2051,
    },
    XmlChSRange {
        range: 0x2053..=0x2053,
    },
    XmlChSRange {
        range: 0x2057..=0x2057,
    },
    XmlChSRange {
        range: 0x23b6..=0x23b6,
    },
    XmlChSRange {
        range: 0x3001..=0x3003,
    },
    XmlChSRange {
        range: 0x303d..=0x303d,
    },
    XmlChSRange {
        range: 0xfe30..=0xfe30,
    },
    XmlChSRange {
        range: 0xfe45..=0xfe46,
    },
    XmlChSRange {
        range: 0xfe49..=0xfe4c,
    },
    XmlChSRange {
        range: 0xfe50..=0xfe52,
    },
    XmlChSRange {
        range: 0xfe54..=0xfe57,
    },
    XmlChSRange {
        range: 0xfe5f..=0xfe61,
    },
    XmlChSRange {
        range: 0xfe68..=0xfe68,
    },
    XmlChSRange {
        range: 0xfe6a..=0xfe6b,
    },
    XmlChSRange {
        range: 0xff01..=0xff03,
    },
    XmlChSRange {
        range: 0xff05..=0xff07,
    },
    XmlChSRange {
        range: 0xff0a..=0xff0a,
    },
    XmlChSRange {
        range: 0xff0c..=0xff0c,
    },
    XmlChSRange {
        range: 0xff0e..=0xff0f,
    },
    XmlChSRange {
        range: 0xff1a..=0xff1b,
    },
    XmlChSRange {
        range: 0xff1f..=0xff20,
    },
    XmlChSRange {
        range: 0xff3c..=0xff3c,
    },
    XmlChSRange {
        range: 0xff61..=0xff61,
    },
    XmlChSRange {
        range: 0xff64..=0xff64,
    },
];

const XML_PO_L: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x10100..=0x10101,
    },
    XmlChLRange {
        range: 0x1039f..=0x1039f,
    },
];

const XML_PO_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_PO_S,
    long_range: XML_PO_L,
};

const XML_PS_S: &[XmlChSRange] = &[
    XmlChSRange { range: 0x28..=0x28 },
    XmlChSRange { range: 0x5b..=0x5b },
    XmlChSRange { range: 0x7b..=0x7b },
    XmlChSRange {
        range: 0xf3a..=0xf3a,
    },
    XmlChSRange {
        range: 0xf3c..=0xf3c,
    },
    XmlChSRange {
        range: 0x169b..=0x169b,
    },
    XmlChSRange {
        range: 0x201a..=0x201a,
    },
    XmlChSRange {
        range: 0x201e..=0x201e,
    },
    XmlChSRange {
        range: 0x2045..=0x2045,
    },
    XmlChSRange {
        range: 0x207d..=0x207d,
    },
    XmlChSRange {
        range: 0x208d..=0x208d,
    },
    XmlChSRange {
        range: 0x2329..=0x2329,
    },
    XmlChSRange {
        range: 0x23b4..=0x23b4,
    },
    XmlChSRange {
        range: 0x2768..=0x2768,
    },
    XmlChSRange {
        range: 0x276a..=0x276a,
    },
    XmlChSRange {
        range: 0x276c..=0x276c,
    },
    XmlChSRange {
        range: 0x276e..=0x276e,
    },
    XmlChSRange {
        range: 0x2770..=0x2770,
    },
    XmlChSRange {
        range: 0x2772..=0x2772,
    },
    XmlChSRange {
        range: 0x2774..=0x2774,
    },
    XmlChSRange {
        range: 0x27e6..=0x27e6,
    },
    XmlChSRange {
        range: 0x27e8..=0x27e8,
    },
    XmlChSRange {
        range: 0x27ea..=0x27ea,
    },
    XmlChSRange {
        range: 0x2983..=0x2983,
    },
    XmlChSRange {
        range: 0x2985..=0x2985,
    },
    XmlChSRange {
        range: 0x2987..=0x2987,
    },
    XmlChSRange {
        range: 0x2989..=0x2989,
    },
    XmlChSRange {
        range: 0x298b..=0x298b,
    },
    XmlChSRange {
        range: 0x298d..=0x298d,
    },
    XmlChSRange {
        range: 0x298f..=0x298f,
    },
    XmlChSRange {
        range: 0x2991..=0x2991,
    },
    XmlChSRange {
        range: 0x2993..=0x2993,
    },
    XmlChSRange {
        range: 0x2995..=0x2995,
    },
    XmlChSRange {
        range: 0x2997..=0x2997,
    },
    XmlChSRange {
        range: 0x29d8..=0x29d8,
    },
    XmlChSRange {
        range: 0x29da..=0x29da,
    },
    XmlChSRange {
        range: 0x29fc..=0x29fc,
    },
    XmlChSRange {
        range: 0x3008..=0x3008,
    },
    XmlChSRange {
        range: 0x300a..=0x300a,
    },
    XmlChSRange {
        range: 0x300c..=0x300c,
    },
    XmlChSRange {
        range: 0x300e..=0x300e,
    },
    XmlChSRange {
        range: 0x3010..=0x3010,
    },
    XmlChSRange {
        range: 0x3014..=0x3014,
    },
    XmlChSRange {
        range: 0x3016..=0x3016,
    },
    XmlChSRange {
        range: 0x3018..=0x3018,
    },
    XmlChSRange {
        range: 0x301a..=0x301a,
    },
    XmlChSRange {
        range: 0x301d..=0x301d,
    },
    XmlChSRange {
        range: 0xfd3e..=0xfd3e,
    },
    XmlChSRange {
        range: 0xfe35..=0xfe35,
    },
    XmlChSRange {
        range: 0xfe37..=0xfe37,
    },
    XmlChSRange {
        range: 0xfe39..=0xfe39,
    },
    XmlChSRange {
        range: 0xfe3b..=0xfe3b,
    },
    XmlChSRange {
        range: 0xfe3d..=0xfe3d,
    },
    XmlChSRange {
        range: 0xfe3f..=0xfe3f,
    },
    XmlChSRange {
        range: 0xfe41..=0xfe41,
    },
    XmlChSRange {
        range: 0xfe43..=0xfe43,
    },
    XmlChSRange {
        range: 0xfe47..=0xfe47,
    },
    XmlChSRange {
        range: 0xfe59..=0xfe59,
    },
    XmlChSRange {
        range: 0xfe5b..=0xfe5b,
    },
    XmlChSRange {
        range: 0xfe5d..=0xfe5d,
    },
    XmlChSRange {
        range: 0xff08..=0xff08,
    },
    XmlChSRange {
        range: 0xff3b..=0xff3b,
    },
    XmlChSRange {
        range: 0xff5b..=0xff5b,
    },
    XmlChSRange {
        range: 0xff5f..=0xff5f,
    },
    XmlChSRange {
        range: 0xff62..=0xff62,
    },
];

const XML_PS_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_PS_S,
    long_range: &[],
};

const XML_SS: &[XmlChSRange] = &[
    XmlChSRange { range: 0x24..=0x24 },
    XmlChSRange { range: 0x2b..=0x2b },
    XmlChSRange { range: 0x3c..=0x3e },
    XmlChSRange { range: 0x5e..=0x5e },
    XmlChSRange { range: 0x60..=0x60 },
    XmlChSRange { range: 0x7c..=0x7c },
    XmlChSRange { range: 0x7e..=0x7e },
    XmlChSRange { range: 0xa2..=0xa9 },
    XmlChSRange { range: 0xac..=0xac },
    XmlChSRange { range: 0xae..=0xb1 },
    XmlChSRange { range: 0xb4..=0xb4 },
    XmlChSRange { range: 0xb6..=0xb6 },
    XmlChSRange { range: 0xb8..=0xb8 },
    XmlChSRange { range: 0xd7..=0xd7 },
    XmlChSRange { range: 0xf7..=0xf7 },
    XmlChSRange {
        range: 0x2c2..=0x2c5,
    },
    XmlChSRange {
        range: 0x2d2..=0x2df,
    },
    XmlChSRange {
        range: 0x2e5..=0x2ed,
    },
    XmlChSRange {
        range: 0x2ef..=0x2ff,
    },
    XmlChSRange {
        range: 0x374..=0x375,
    },
    XmlChSRange {
        range: 0x384..=0x385,
    },
    XmlChSRange {
        range: 0x3f6..=0x3f6,
    },
    XmlChSRange {
        range: 0x482..=0x482,
    },
    XmlChSRange {
        range: 0x60e..=0x60f,
    },
    XmlChSRange {
        range: 0x6e9..=0x6e9,
    },
    XmlChSRange {
        range: 0x6fd..=0x6fe,
    },
    XmlChSRange {
        range: 0x9f2..=0x9f3,
    },
    XmlChSRange {
        range: 0x9fa..=0x9fa,
    },
    XmlChSRange {
        range: 0xaf1..=0xaf1,
    },
    XmlChSRange {
        range: 0xb70..=0xb70,
    },
    XmlChSRange {
        range: 0xbf3..=0xbfa,
    },
    XmlChSRange {
        range: 0xe3f..=0xe3f,
    },
    XmlChSRange {
        range: 0xf01..=0xf03,
    },
    XmlChSRange {
        range: 0xf13..=0xf17,
    },
    XmlChSRange {
        range: 0xf1a..=0xf1f,
    },
    XmlChSRange {
        range: 0xf34..=0xf34,
    },
    XmlChSRange {
        range: 0xf36..=0xf36,
    },
    XmlChSRange {
        range: 0xf38..=0xf38,
    },
    XmlChSRange {
        range: 0xfbe..=0xfc5,
    },
    XmlChSRange {
        range: 0xfc7..=0xfcc,
    },
    XmlChSRange {
        range: 0xfcf..=0xfcf,
    },
    XmlChSRange {
        range: 0x17db..=0x17db,
    },
    XmlChSRange {
        range: 0x1940..=0x1940,
    },
    XmlChSRange {
        range: 0x19e0..=0x19ff,
    },
    XmlChSRange {
        range: 0x1fbd..=0x1fbd,
    },
    XmlChSRange {
        range: 0x1fbf..=0x1fc1,
    },
    XmlChSRange {
        range: 0x1fcd..=0x1fcf,
    },
    XmlChSRange {
        range: 0x1fdd..=0x1fdf,
    },
    XmlChSRange {
        range: 0x1fed..=0x1fef,
    },
    XmlChSRange {
        range: 0x1ffd..=0x1ffe,
    },
    XmlChSRange {
        range: 0x2044..=0x2044,
    },
    XmlChSRange {
        range: 0x2052..=0x2052,
    },
    XmlChSRange {
        range: 0x207a..=0x207c,
    },
    XmlChSRange {
        range: 0x208a..=0x208c,
    },
    XmlChSRange {
        range: 0x20a0..=0x20b1,
    },
    XmlChSRange {
        range: 0x2100..=0x2101,
    },
    XmlChSRange {
        range: 0x2103..=0x2106,
    },
    XmlChSRange {
        range: 0x2108..=0x2109,
    },
    XmlChSRange {
        range: 0x2114..=0x2114,
    },
    XmlChSRange {
        range: 0x2116..=0x2118,
    },
    XmlChSRange {
        range: 0x211e..=0x2123,
    },
    XmlChSRange {
        range: 0x2125..=0x2125,
    },
    XmlChSRange {
        range: 0x2127..=0x2127,
    },
    XmlChSRange {
        range: 0x2129..=0x2129,
    },
    XmlChSRange {
        range: 0x212e..=0x212e,
    },
    XmlChSRange {
        range: 0x2132..=0x2132,
    },
    XmlChSRange {
        range: 0x213a..=0x213b,
    },
    XmlChSRange {
        range: 0x2140..=0x2144,
    },
    XmlChSRange {
        range: 0x214a..=0x214b,
    },
    XmlChSRange {
        range: 0x2190..=0x2328,
    },
    XmlChSRange {
        range: 0x232b..=0x23b3,
    },
    XmlChSRange {
        range: 0x23b7..=0x23d0,
    },
    XmlChSRange {
        range: 0x2400..=0x2426,
    },
    XmlChSRange {
        range: 0x2440..=0x244a,
    },
    XmlChSRange {
        range: 0x249c..=0x24e9,
    },
    XmlChSRange {
        range: 0x2500..=0x2617,
    },
    XmlChSRange {
        range: 0x2619..=0x267d,
    },
    XmlChSRange {
        range: 0x2680..=0x2691,
    },
    XmlChSRange {
        range: 0x26a0..=0x26a1,
    },
    XmlChSRange {
        range: 0x2701..=0x2704,
    },
    XmlChSRange {
        range: 0x2706..=0x2709,
    },
    XmlChSRange {
        range: 0x270c..=0x2727,
    },
    XmlChSRange {
        range: 0x2729..=0x274b,
    },
    XmlChSRange {
        range: 0x274d..=0x274d,
    },
    XmlChSRange {
        range: 0x274f..=0x2752,
    },
    XmlChSRange {
        range: 0x2756..=0x2756,
    },
    XmlChSRange {
        range: 0x2758..=0x275e,
    },
    XmlChSRange {
        range: 0x2761..=0x2767,
    },
    XmlChSRange {
        range: 0x2794..=0x2794,
    },
    XmlChSRange {
        range: 0x2798..=0x27af,
    },
    XmlChSRange {
        range: 0x27b1..=0x27be,
    },
    XmlChSRange {
        range: 0x27d0..=0x27e5,
    },
    XmlChSRange {
        range: 0x27f0..=0x2982,
    },
    XmlChSRange {
        range: 0x2999..=0x29d7,
    },
    XmlChSRange {
        range: 0x29dc..=0x29fb,
    },
    XmlChSRange {
        range: 0x29fe..=0x2b0d,
    },
    XmlChSRange {
        range: 0x2e80..=0x2e99,
    },
    XmlChSRange {
        range: 0x2e9b..=0x2ef3,
    },
    XmlChSRange {
        range: 0x2f00..=0x2fd5,
    },
    XmlChSRange {
        range: 0x2ff0..=0x2ffb,
    },
    XmlChSRange {
        range: 0x3004..=0x3004,
    },
    XmlChSRange {
        range: 0x3012..=0x3013,
    },
    XmlChSRange {
        range: 0x3020..=0x3020,
    },
    XmlChSRange {
        range: 0x3036..=0x3037,
    },
    XmlChSRange {
        range: 0x303e..=0x303f,
    },
    XmlChSRange {
        range: 0x309b..=0x309c,
    },
    XmlChSRange {
        range: 0x3190..=0x3191,
    },
    XmlChSRange {
        range: 0x3196..=0x319f,
    },
    XmlChSRange {
        range: 0x3200..=0x321e,
    },
    XmlChSRange {
        range: 0x322a..=0x3243,
    },
    XmlChSRange {
        range: 0x3250..=0x3250,
    },
    XmlChSRange {
        range: 0x3260..=0x327d,
    },
    XmlChSRange {
        range: 0x327f..=0x327f,
    },
    XmlChSRange {
        range: 0x328a..=0x32b0,
    },
    XmlChSRange {
        range: 0x32c0..=0x32fe,
    },
    XmlChSRange {
        range: 0x3300..=0x33ff,
    },
    XmlChSRange {
        range: 0x4dc0..=0x4dff,
    },
    XmlChSRange {
        range: 0xa490..=0xa4c6,
    },
    XmlChSRange {
        range: 0xfb29..=0xfb29,
    },
    XmlChSRange {
        range: 0xfdfc..=0xfdfd,
    },
    XmlChSRange {
        range: 0xfe62..=0xfe62,
    },
    XmlChSRange {
        range: 0xfe64..=0xfe66,
    },
    XmlChSRange {
        range: 0xfe69..=0xfe69,
    },
    XmlChSRange {
        range: 0xff04..=0xff04,
    },
    XmlChSRange {
        range: 0xff0b..=0xff0b,
    },
    XmlChSRange {
        range: 0xff1c..=0xff1e,
    },
    XmlChSRange {
        range: 0xff3e..=0xff3e,
    },
    XmlChSRange {
        range: 0xff40..=0xff40,
    },
    XmlChSRange {
        range: 0xff5c..=0xff5c,
    },
    XmlChSRange {
        range: 0xff5e..=0xff5e,
    },
    XmlChSRange {
        range: 0xffe0..=0xffe6,
    },
    XmlChSRange {
        range: 0xffe8..=0xffee,
    },
    XmlChSRange {
        range: 0xfffc..=0xfffd,
    },
];

const XML_SL: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x10102..=0x10102,
    },
    XmlChLRange {
        range: 0x10137..=0x1013f,
    },
    XmlChLRange {
        range: 0x1d000..=0x1d0f5,
    },
    XmlChLRange {
        range: 0x1d100..=0x1d126,
    },
    XmlChLRange {
        range: 0x1d12a..=0x1d164,
    },
    XmlChLRange {
        range: 0x1d16a..=0x1d16c,
    },
    XmlChLRange {
        range: 0x1d183..=0x1d184,
    },
    XmlChLRange {
        range: 0x1d18c..=0x1d1a9,
    },
    XmlChLRange {
        range: 0x1d1ae..=0x1d1dd,
    },
    XmlChLRange {
        range: 0x1d300..=0x1d356,
    },
    XmlChLRange {
        range: 0x1d6c1..=0x1d6c1,
    },
    XmlChLRange {
        range: 0x1d6db..=0x1d6db,
    },
    XmlChLRange {
        range: 0x1d6fb..=0x1d6fb,
    },
    XmlChLRange {
        range: 0x1d715..=0x1d715,
    },
    XmlChLRange {
        range: 0x1d735..=0x1d735,
    },
    XmlChLRange {
        range: 0x1d74f..=0x1d74f,
    },
    XmlChLRange {
        range: 0x1d76f..=0x1d76f,
    },
    XmlChLRange {
        range: 0x1d789..=0x1d789,
    },
    XmlChLRange {
        range: 0x1d7a9..=0x1d7a9,
    },
    XmlChLRange {
        range: 0x1d7c3..=0x1d7c3,
    },
];

const XML_SG: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_SS,
    long_range: XML_SL,
};

const XML_SC_S: &[XmlChSRange] = &[
    XmlChSRange { range: 0x24..=0x24 },
    XmlChSRange { range: 0xa2..=0xa5 },
    XmlChSRange {
        range: 0x9f2..=0x9f3,
    },
    XmlChSRange {
        range: 0xaf1..=0xaf1,
    },
    XmlChSRange {
        range: 0xbf9..=0xbf9,
    },
    XmlChSRange {
        range: 0xe3f..=0xe3f,
    },
    XmlChSRange {
        range: 0x17db..=0x17db,
    },
    XmlChSRange {
        range: 0x20a0..=0x20b1,
    },
    XmlChSRange {
        range: 0xfdfc..=0xfdfc,
    },
    XmlChSRange {
        range: 0xfe69..=0xfe69,
    },
    XmlChSRange {
        range: 0xff04..=0xff04,
    },
    XmlChSRange {
        range: 0xffe0..=0xffe1,
    },
    XmlChSRange {
        range: 0xffe5..=0xffe6,
    },
];

const XML_SC_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_SC_S,
    long_range: &[],
};

const XML_SK_S: &[XmlChSRange] = &[
    XmlChSRange { range: 0x5e..=0x5e },
    XmlChSRange { range: 0x60..=0x60 },
    XmlChSRange { range: 0xa8..=0xa8 },
    XmlChSRange { range: 0xaf..=0xaf },
    XmlChSRange { range: 0xb4..=0xb4 },
    XmlChSRange { range: 0xb8..=0xb8 },
    XmlChSRange {
        range: 0x2c2..=0x2c5,
    },
    XmlChSRange {
        range: 0x2d2..=0x2df,
    },
    XmlChSRange {
        range: 0x2e5..=0x2ed,
    },
    XmlChSRange {
        range: 0x2ef..=0x2ff,
    },
    XmlChSRange {
        range: 0x374..=0x375,
    },
    XmlChSRange {
        range: 0x384..=0x385,
    },
    XmlChSRange {
        range: 0x1fbd..=0x1fbd,
    },
    XmlChSRange {
        range: 0x1fbf..=0x1fc1,
    },
    XmlChSRange {
        range: 0x1fcd..=0x1fcf,
    },
    XmlChSRange {
        range: 0x1fdd..=0x1fdf,
    },
    XmlChSRange {
        range: 0x1fed..=0x1fef,
    },
    XmlChSRange {
        range: 0x1ffd..=0x1ffe,
    },
    XmlChSRange {
        range: 0x309b..=0x309c,
    },
    XmlChSRange {
        range: 0xff3e..=0xff3e,
    },
    XmlChSRange {
        range: 0xff40..=0xff40,
    },
    XmlChSRange {
        range: 0xffe3..=0xffe3,
    },
];

const XML_SK_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_SK_S,
    long_range: &[],
};

const XML_SM_S: &[XmlChSRange] = &[
    XmlChSRange { range: 0x2b..=0x2b },
    XmlChSRange { range: 0x3c..=0x3e },
    XmlChSRange { range: 0x7c..=0x7c },
    XmlChSRange { range: 0x7e..=0x7e },
    XmlChSRange { range: 0xac..=0xac },
    XmlChSRange { range: 0xb1..=0xb1 },
    XmlChSRange { range: 0xd7..=0xd7 },
    XmlChSRange { range: 0xf7..=0xf7 },
    XmlChSRange {
        range: 0x3f6..=0x3f6,
    },
    XmlChSRange {
        range: 0x2044..=0x2044,
    },
    XmlChSRange {
        range: 0x2052..=0x2052,
    },
    XmlChSRange {
        range: 0x207a..=0x207c,
    },
    XmlChSRange {
        range: 0x208a..=0x208c,
    },
    XmlChSRange {
        range: 0x2140..=0x2144,
    },
    XmlChSRange {
        range: 0x214b..=0x214b,
    },
    XmlChSRange {
        range: 0x2190..=0x2194,
    },
    XmlChSRange {
        range: 0x219a..=0x219b,
    },
    XmlChSRange {
        range: 0x21a0..=0x21a0,
    },
    XmlChSRange {
        range: 0x21a3..=0x21a3,
    },
    XmlChSRange {
        range: 0x21a6..=0x21a6,
    },
    XmlChSRange {
        range: 0x21ae..=0x21ae,
    },
    XmlChSRange {
        range: 0x21ce..=0x21cf,
    },
    XmlChSRange {
        range: 0x21d2..=0x21d2,
    },
    XmlChSRange {
        range: 0x21d4..=0x21d4,
    },
    XmlChSRange {
        range: 0x21f4..=0x22ff,
    },
    XmlChSRange {
        range: 0x2308..=0x230b,
    },
    XmlChSRange {
        range: 0x2320..=0x2321,
    },
    XmlChSRange {
        range: 0x237c..=0x237c,
    },
    XmlChSRange {
        range: 0x239b..=0x23b3,
    },
    XmlChSRange {
        range: 0x25b7..=0x25b7,
    },
    XmlChSRange {
        range: 0x25c1..=0x25c1,
    },
    XmlChSRange {
        range: 0x25f8..=0x25ff,
    },
    XmlChSRange {
        range: 0x266f..=0x266f,
    },
    XmlChSRange {
        range: 0x27d0..=0x27e5,
    },
    XmlChSRange {
        range: 0x27f0..=0x27ff,
    },
    XmlChSRange {
        range: 0x2900..=0x2982,
    },
    XmlChSRange {
        range: 0x2999..=0x29d7,
    },
    XmlChSRange {
        range: 0x29dc..=0x29fb,
    },
    XmlChSRange {
        range: 0x29fe..=0x2aff,
    },
    XmlChSRange {
        range: 0xfb29..=0xfb29,
    },
    XmlChSRange {
        range: 0xfe62..=0xfe62,
    },
    XmlChSRange {
        range: 0xfe64..=0xfe66,
    },
    XmlChSRange {
        range: 0xff0b..=0xff0b,
    },
    XmlChSRange {
        range: 0xff1c..=0xff1e,
    },
    XmlChSRange {
        range: 0xff5c..=0xff5c,
    },
    XmlChSRange {
        range: 0xff5e..=0xff5e,
    },
    XmlChSRange {
        range: 0xffe2..=0xffe2,
    },
    XmlChSRange {
        range: 0xffe9..=0xffec,
    },
];

const XML_SM_L: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x1d6c1..=0x1d6c1,
    },
    XmlChLRange {
        range: 0x1d6db..=0x1d6db,
    },
    XmlChLRange {
        range: 0x1d6fb..=0x1d6fb,
    },
    XmlChLRange {
        range: 0x1d715..=0x1d715,
    },
    XmlChLRange {
        range: 0x1d735..=0x1d735,
    },
    XmlChLRange {
        range: 0x1d74f..=0x1d74f,
    },
    XmlChLRange {
        range: 0x1d76f..=0x1d76f,
    },
    XmlChLRange {
        range: 0x1d789..=0x1d789,
    },
    XmlChLRange {
        range: 0x1d7a9..=0x1d7a9,
    },
    XmlChLRange {
        range: 0x1d7c3..=0x1d7c3,
    },
];

const XML_SM_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_SM_S,
    long_range: XML_SM_L,
};

const XML_SO_S: &[XmlChSRange] = &[
    XmlChSRange { range: 0xa6..=0xa7 },
    XmlChSRange { range: 0xa9..=0xa9 },
    XmlChSRange { range: 0xae..=0xae },
    XmlChSRange { range: 0xb0..=0xb0 },
    XmlChSRange { range: 0xb6..=0xb6 },
    XmlChSRange {
        range: 0x482..=0x482,
    },
    XmlChSRange {
        range: 0x60e..=0x60f,
    },
    XmlChSRange {
        range: 0x6e9..=0x6e9,
    },
    XmlChSRange {
        range: 0x6fd..=0x6fe,
    },
    XmlChSRange {
        range: 0x9fa..=0x9fa,
    },
    XmlChSRange {
        range: 0xb70..=0xb70,
    },
    XmlChSRange {
        range: 0xbf3..=0xbf8,
    },
    XmlChSRange {
        range: 0xbfa..=0xbfa,
    },
    XmlChSRange {
        range: 0xf01..=0xf03,
    },
    XmlChSRange {
        range: 0xf13..=0xf17,
    },
    XmlChSRange {
        range: 0xf1a..=0xf1f,
    },
    XmlChSRange {
        range: 0xf34..=0xf34,
    },
    XmlChSRange {
        range: 0xf36..=0xf36,
    },
    XmlChSRange {
        range: 0xf38..=0xf38,
    },
    XmlChSRange {
        range: 0xfbe..=0xfc5,
    },
    XmlChSRange {
        range: 0xfc7..=0xfcc,
    },
    XmlChSRange {
        range: 0xfcf..=0xfcf,
    },
    XmlChSRange {
        range: 0x1940..=0x1940,
    },
    XmlChSRange {
        range: 0x19e0..=0x19ff,
    },
    XmlChSRange {
        range: 0x2100..=0x2101,
    },
    XmlChSRange {
        range: 0x2103..=0x2106,
    },
    XmlChSRange {
        range: 0x2108..=0x2109,
    },
    XmlChSRange {
        range: 0x2114..=0x2114,
    },
    XmlChSRange {
        range: 0x2116..=0x2118,
    },
    XmlChSRange {
        range: 0x211e..=0x2123,
    },
    XmlChSRange {
        range: 0x2125..=0x2125,
    },
    XmlChSRange {
        range: 0x2127..=0x2127,
    },
    XmlChSRange {
        range: 0x2129..=0x2129,
    },
    XmlChSRange {
        range: 0x212e..=0x212e,
    },
    XmlChSRange {
        range: 0x2132..=0x2132,
    },
    XmlChSRange {
        range: 0x213a..=0x213b,
    },
    XmlChSRange {
        range: 0x214a..=0x214a,
    },
    XmlChSRange {
        range: 0x2195..=0x2199,
    },
    XmlChSRange {
        range: 0x219c..=0x219f,
    },
    XmlChSRange {
        range: 0x21a1..=0x21a2,
    },
    XmlChSRange {
        range: 0x21a4..=0x21a5,
    },
    XmlChSRange {
        range: 0x21a7..=0x21ad,
    },
    XmlChSRange {
        range: 0x21af..=0x21cd,
    },
    XmlChSRange {
        range: 0x21d0..=0x21d1,
    },
    XmlChSRange {
        range: 0x21d3..=0x21d3,
    },
    XmlChSRange {
        range: 0x21d5..=0x21f3,
    },
    XmlChSRange {
        range: 0x2300..=0x2307,
    },
    XmlChSRange {
        range: 0x230c..=0x231f,
    },
    XmlChSRange {
        range: 0x2322..=0x2328,
    },
    XmlChSRange {
        range: 0x232b..=0x237b,
    },
    XmlChSRange {
        range: 0x237d..=0x239a,
    },
    XmlChSRange {
        range: 0x23b7..=0x23d0,
    },
    XmlChSRange {
        range: 0x2400..=0x2426,
    },
    XmlChSRange {
        range: 0x2440..=0x244a,
    },
    XmlChSRange {
        range: 0x249c..=0x24e9,
    },
    XmlChSRange {
        range: 0x2500..=0x25b6,
    },
    XmlChSRange {
        range: 0x25b8..=0x25c0,
    },
    XmlChSRange {
        range: 0x25c2..=0x25f7,
    },
    XmlChSRange {
        range: 0x2600..=0x2617,
    },
    XmlChSRange {
        range: 0x2619..=0x266e,
    },
    XmlChSRange {
        range: 0x2670..=0x267d,
    },
    XmlChSRange {
        range: 0x2680..=0x2691,
    },
    XmlChSRange {
        range: 0x26a0..=0x26a1,
    },
    XmlChSRange {
        range: 0x2701..=0x2704,
    },
    XmlChSRange {
        range: 0x2706..=0x2709,
    },
    XmlChSRange {
        range: 0x270c..=0x2727,
    },
    XmlChSRange {
        range: 0x2729..=0x274b,
    },
    XmlChSRange {
        range: 0x274d..=0x274d,
    },
    XmlChSRange {
        range: 0x274f..=0x2752,
    },
    XmlChSRange {
        range: 0x2756..=0x2756,
    },
    XmlChSRange {
        range: 0x2758..=0x275e,
    },
    XmlChSRange {
        range: 0x2761..=0x2767,
    },
    XmlChSRange {
        range: 0x2794..=0x2794,
    },
    XmlChSRange {
        range: 0x2798..=0x27af,
    },
    XmlChSRange {
        range: 0x27b1..=0x27be,
    },
    XmlChSRange {
        range: 0x2800..=0x28ff,
    },
    XmlChSRange {
        range: 0x2b00..=0x2b0d,
    },
    XmlChSRange {
        range: 0x2e80..=0x2e99,
    },
    XmlChSRange {
        range: 0x2e9b..=0x2ef3,
    },
    XmlChSRange {
        range: 0x2f00..=0x2fd5,
    },
    XmlChSRange {
        range: 0x2ff0..=0x2ffb,
    },
    XmlChSRange {
        range: 0x3004..=0x3004,
    },
    XmlChSRange {
        range: 0x3012..=0x3013,
    },
    XmlChSRange {
        range: 0x3020..=0x3020,
    },
    XmlChSRange {
        range: 0x3036..=0x3037,
    },
    XmlChSRange {
        range: 0x303e..=0x303f,
    },
    XmlChSRange {
        range: 0x3190..=0x3191,
    },
    XmlChSRange {
        range: 0x3196..=0x319f,
    },
    XmlChSRange {
        range: 0x3200..=0x321e,
    },
    XmlChSRange {
        range: 0x322a..=0x3243,
    },
    XmlChSRange {
        range: 0x3250..=0x3250,
    },
    XmlChSRange {
        range: 0x3260..=0x327d,
    },
    XmlChSRange {
        range: 0x327f..=0x327f,
    },
    XmlChSRange {
        range: 0x328a..=0x32b0,
    },
    XmlChSRange {
        range: 0x32c0..=0x32fe,
    },
    XmlChSRange {
        range: 0x3300..=0x33ff,
    },
    XmlChSRange {
        range: 0x4dc0..=0x4dff,
    },
    XmlChSRange {
        range: 0xa490..=0xa4c6,
    },
    XmlChSRange {
        range: 0xfdfd..=0xfdfd,
    },
    XmlChSRange {
        range: 0xffe4..=0xffe4,
    },
    XmlChSRange {
        range: 0xffe8..=0xffe8,
    },
    XmlChSRange {
        range: 0xffed..=0xffee,
    },
    XmlChSRange {
        range: 0xfffc..=0xfffd,
    },
];

const XML_SO_L: &[XmlChLRange] = &[
    XmlChLRange {
        range: 0x10102..=0x10102,
    },
    XmlChLRange {
        range: 0x10137..=0x1013f,
    },
    XmlChLRange {
        range: 0x1d000..=0x1d0f5,
    },
    XmlChLRange {
        range: 0x1d100..=0x1d126,
    },
    XmlChLRange {
        range: 0x1d12a..=0x1d164,
    },
    XmlChLRange {
        range: 0x1d16a..=0x1d16c,
    },
    XmlChLRange {
        range: 0x1d183..=0x1d184,
    },
    XmlChLRange {
        range: 0x1d18c..=0x1d1a9,
    },
    XmlChLRange {
        range: 0x1d1ae..=0x1d1dd,
    },
    XmlChLRange {
        range: 0x1d300..=0x1d356,
    },
];

const XML_SO_G: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_SO_S,
    long_range: XML_SO_L,
};

const XML_ZS: &[XmlChSRange] = &[
    XmlChSRange { range: 0x20..=0x20 },
    XmlChSRange { range: 0xa0..=0xa0 },
    XmlChSRange {
        range: 0x1680..=0x1680,
    },
    XmlChSRange {
        range: 0x180e..=0x180e,
    },
    XmlChSRange {
        range: 0x2000..=0x200a,
    },
    XmlChSRange {
        range: 0x2028..=0x2029,
    },
    XmlChSRange {
        range: 0x202f..=0x202f,
    },
    XmlChSRange {
        range: 0x205f..=0x205f,
    },
    XmlChSRange {
        range: 0x3000..=0x3000,
    },
];

const XML_ZG: XmlChRangeGroup = XmlChRangeGroup {
    short_range: XML_ZS,
    long_range: &[],
};

const XML_UNICODE_BLOCK_TBL: XmlUnicodeNameTable = XmlUnicodeNameTable {
    table: XML_UNICODE_BLOCKS,
    numentries: 128,
};
const XML_UNICODE_CAT_TBL: XmlUnicodeNameTable = XmlUnicodeNameTable {
    table: XML_UNICODE_CATS,
    numentries: 36,
};

/// Check whether the character is part of AegeanNumbers UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_aegean_numbers(code: i32) -> i32 {
    (0x10100..=0x1013F).contains(&code) as i32
}

/// Check whether the character is part of AlphabeticPresentationForms UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_alphabetic_presentation_forms(code: i32) -> i32 {
    (0xFB00..=0xFB4F).contains(&code) as i32
}

/// Check whether the character is part of Arabic UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_arabic(code: i32) -> i32 {
    (0x0600..=0x06FF).contains(&code) as i32
}

/// Check whether the character is part of ArabicPresentationForms-A UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_arabic_presentation_forms_a(code: i32) -> i32 {
    (0xFB50..=0xFDFF).contains(&code) as i32
}

/// Check whether the character is part of ArabicPresentationForms-B UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_arabic_presentation_forms_b(code: i32) -> i32 {
    (0xFE70..=0xFEFF).contains(&code) as i32
}

/// Check whether the character is part of Armenian UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_armenian(code: i32) -> i32 {
    (0x0530..=0x058F).contains(&code) as i32
}

/// Check whether the character is part of Arrows UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_arrows(code: i32) -> i32 {
    (0x2190..=0x21FF).contains(&code) as i32
}

/// Check whether the character is part of BasicLatin UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_basic_latin(code: i32) -> i32 {
    (0x0000..=0x007F).contains(&code) as i32
}

/// Check whether the character is part of Bengali UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_bengali(code: i32) -> i32 {
    (0x0980..=0x09FF).contains(&code) as i32
}

/// Check whether the character is part of BlockElements UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_block_elements(code: i32) -> i32 {
    (0x2580..=0x259F).contains(&code) as i32
}

/// Check whether the character is part of Bopomofo UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_bopomofo(code: i32) -> i32 {
    (0x3100..=0x312F).contains(&code) as i32
}

/// Check whether the character is part of BopomofoExtended UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_bopomofo_extended(code: i32) -> i32 {
    (0x31A0..=0x31BF).contains(&code) as i32
}

/// Check whether the character is part of BoxDrawing UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_box_drawing(code: i32) -> i32 {
    (0x2500..=0x257F).contains(&code) as i32
}

/// Check whether the character is part of BraillePatterns UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_braille_patterns(code: i32) -> i32 {
    (0x2800..=0x28FF).contains(&code) as i32
}

/// Check whether the character is part of Buhid UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_buhid(code: i32) -> i32 {
    (0x1740..=0x175F).contains(&code) as i32
}

/// Check whether the character is part of ByzantineMusicalSymbols UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_byzantine_musical_symbols(code: i32) -> i32 {
    (0x1D000..=0x1D0FF).contains(&code) as i32
}

/// Check whether the character is part of CJKCompatibility UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cjk_compatibility(code: i32) -> i32 {
    (0x3300..=0x33FF).contains(&code) as i32
}

/// Check whether the character is part of CJKCompatibilityForms UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cjk_compatibility_forms(code: i32) -> i32 {
    (0xFE30..=0xFE4F).contains(&code) as i32
}

/// Check whether the character is part of CJKCompatibilityIdeographs UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cjk_compatibility_ideographs(code: i32) -> i32 {
    (0xF900..=0xFAFF).contains(&code) as i32
}

/// Check whether the character is part of CJKCompatibilityIdeographsSupplement UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cjk_compatibility_ideographs_supplement(code: i32) -> i32 {
    (0x2F800..=0x2FA1F).contains(&code) as i32
}

/// Check whether the character is part of CJKRadicalsSupplement UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cjk_radicals_supplement(code: i32) -> i32 {
    (0x2E80..=0x2EFF).contains(&code) as i32
}

/// Check whether the character is part of CJKSymbolsandPunctuation UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cjk_symbolsand_punctuation(code: i32) -> i32 {
    (0x3000..=0x303F).contains(&code) as i32
}

/// Check whether the character is part of CJKUnifiedIdeographs UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cjk_unified_ideographs(code: i32) -> i32 {
    (0x4E00..=0x9FFF).contains(&code) as i32
}

/// Check whether the character is part of CJKUnifiedIdeographsExtensionA UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cjk_unified_ideographs_extension_a(code: i32) -> i32 {
    (0x3400..=0x4DBF).contains(&code) as i32
}

/// Check whether the character is part of CJKUnifiedIdeographsExtensionB UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cjk_unified_ideographs_extension_b(code: i32) -> i32 {
    (0x20000..=0x2A6DF).contains(&code) as i32
}

/// Check whether the character is part of Cherokee UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cherokee(code: i32) -> i32 {
    (0x13A0..=0x13FF).contains(&code) as i32
}

/// Check whether the character is part of CombiningDiacriticalMarks UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_combining_diacritical_marks(code: i32) -> i32 {
    (0x0300..=0x036F).contains(&code) as i32
}

/// Check whether the character is part of CombiningDiacriticalMarksforSymbols UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_combining_diacritical_marksfor_symbols(code: i32) -> i32 {
    (0x20D0..=0x20FF).contains(&code) as i32
}

/// Check whether the character is part of CombiningHalfMarks UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_combining_half_marks(code: i32) -> i32 {
    (0xFE20..=0xFE2F).contains(&code) as i32
}

/// Check whether the character is part of CombiningMarksforSymbols UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_combining_marksfor_symbols(code: i32) -> i32 {
    (0x20D0..=0x20FF).contains(&code) as i32
}

/// Check whether the character is part of ControlPictures UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_control_pictures(code: i32) -> i32 {
    (0x2400..=0x243F).contains(&code) as i32
}

/// Check whether the character is part of CurrencySymbols UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_currency_symbols(code: i32) -> i32 {
    (0x20A0..=0x20CF).contains(&code) as i32
}

/// Check whether the character is part of CypriotSyllabary UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cypriot_syllabary(code: i32) -> i32 {
    (0x10800..=0x1083F).contains(&code) as i32
}

/// Check whether the character is part of Cyrillic UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cyrillic(code: i32) -> i32 {
    (0x0400..=0x04FF).contains(&code) as i32
}

/// Check whether the character is part of CyrillicSupplement UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cyrillic_supplement(code: i32) -> i32 {
    (0x0500..=0x052F).contains(&code) as i32
}

/// Check whether the character is part of Deseret UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_deseret(code: i32) -> i32 {
    (0x10400..=0x1044F).contains(&code) as i32
}

/// Check whether the character is part of Devanagari UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_devanagari(code: i32) -> i32 {
    (0x0900..=0x097F).contains(&code) as i32
}

/// Check whether the character is part of Dingbats UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_dingbats(code: i32) -> i32 {
    (0x2700..=0x27BF).contains(&code) as i32
}

/// Check whether the character is part of EnclosedAlphanumerics UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_enclosed_alphanumerics(code: i32) -> i32 {
    (0x2460..=0x24FF).contains(&code) as i32
}

/// Check whether the character is part of EnclosedCJKLettersandMonths UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_enclosed_cjk_lettersand_months(code: i32) -> i32 {
    (0x3200..=0x32FF).contains(&code) as i32
}

/// Check whether the character is part of Ethiopic UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_ethiopic(code: i32) -> i32 {
    (0x1200..=0x137F).contains(&code) as i32
}

/// Check whether the character is part of GeneralPunctuation UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_general_punctuation(code: i32) -> i32 {
    (0x2000..=0x206F).contains(&code) as i32
}

/// Check whether the character is part of GeometricShapes UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_geometric_shapes(code: i32) -> i32 {
    (0x25A0..=0x25FF).contains(&code) as i32
}

/// Check whether the character is part of Georgian UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_georgian(code: i32) -> i32 {
    (0x10A0..=0x10FF).contains(&code) as i32
}

/// Check whether the character is part of Gothic UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_gothic(code: i32) -> i32 {
    (0x10330..=0x1034F).contains(&code) as i32
}

/// Check whether the character is part of Greek UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_greek(code: i32) -> i32 {
    (0x0370..=0x03FF).contains(&code) as i32
}

/// Check whether the character is part of GreekExtended UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_greek_extended(code: i32) -> i32 {
    (0x1F00..=0x1FFF).contains(&code) as i32
}

/// Check whether the character is part of GreekandCoptic UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_greekand_coptic(code: i32) -> i32 {
    (0x0370..=0x03FF).contains(&code) as i32
}

/// Check whether the character is part of Gujarati UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_gujarati(code: i32) -> i32 {
    (0x0A80..=0x0AFF).contains(&code) as i32
}

/// Check whether the character is part of Gurmukhi UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_gurmukhi(code: i32) -> i32 {
    (0x0A00..=0x0A7F).contains(&code) as i32
}

/// Check whether the character is part of HalfwidthandFullwidthForms UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_halfwidthand_fullwidth_forms(code: i32) -> i32 {
    (0xFF00..=0xFFEF).contains(&code) as i32
}

/// Check whether the character is part of HangulCompatibilityJamo UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_hangul_compatibility_jamo(code: i32) -> i32 {
    (0x3130..=0x318F).contains(&code) as i32
}

/// Check whether the character is part of HangulJamo UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_hangul_jamo(code: i32) -> i32 {
    (0x1100..=0x11FF).contains(&code) as i32
}

/// Check whether the character is part of HangulSyllables UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_hangul_syllables(code: i32) -> i32 {
    (0xAC00..=0xD7AF).contains(&code) as i32
}

/// Check whether the character is part of Hanunoo UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_hanunoo(code: i32) -> i32 {
    (0x1720..=0x173F).contains(&code) as i32
}

/// Check whether the character is part of Hebrew UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_hebrew(code: i32) -> i32 {
    (0x0590..=0x05FF).contains(&code) as i32
}

/// Check whether the character is part of HighPrivateUseSurrogates UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_high_private_use_surrogates(code: i32) -> i32 {
    (0xDB80..=0xDBFF).contains(&code) as i32
}

/// Check whether the character is part of HighSurrogates UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_high_surrogates(code: i32) -> i32 {
    (0xD800..=0xDB7F).contains(&code) as i32
}

/// Check whether the character is part of Hiragana UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_hiragana(code: i32) -> i32 {
    (0x3040..=0x309F).contains(&code) as i32
}

/// Check whether the character is part of IPAExtensions UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_ipa_extensions(code: i32) -> i32 {
    (0x0250..=0x02AF).contains(&code) as i32
}

/// Check whether the character is part of IdeographicDescriptionCharacters UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_ideographic_description_characters(code: i32) -> i32 {
    (0x2FF0..=0x2FFF).contains(&code) as i32
}

/// Check whether the character is part of Kanbun UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_kanbun(code: i32) -> i32 {
    (0x3190..=0x319F).contains(&code) as i32
}

/// Check whether the character is part of KangxiRadicals UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_kangxi_radicals(code: i32) -> i32 {
    (0x2F00..=0x2FDF).contains(&code) as i32
}

/// Check whether the character is part of Kannada UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_kannada(code: i32) -> i32 {
    (0x0C80..=0x0CFF).contains(&code) as i32
}

/// Check whether the character is part of Katakana UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_katakana(code: i32) -> i32 {
    (0x30A0..=0x30FF).contains(&code) as i32
}

/// Check whether the character is part of KatakanaPhoneticExtensions UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_katakana_phonetic_extensions(code: i32) -> i32 {
    (0x31F0..=0x31FF).contains(&code) as i32
}

/// Check whether the character is part of Khmer UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_khmer(code: i32) -> i32 {
    (0x1780..=0x17FF).contains(&code) as i32
}

/// Check whether the character is part of KhmerSymbols UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_khmer_symbols(code: i32) -> i32 {
    (0x19E0..=0x19FF).contains(&code) as i32
}

/// Check whether the character is part of Lao UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_lao(code: i32) -> i32 {
    (0x0E80..=0x0EFF).contains(&code) as i32
}

/// Check whether the character is part of Latin-1Supplement UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_latin1_supplement(code: i32) -> i32 {
    (0x0080..=0x00FF).contains(&code) as i32
}

/// Check whether the character is part of LatinExtended-A UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_latin_extended_a(code: i32) -> i32 {
    (0x0100..=0x017F).contains(&code) as i32
}

/// Check whether the character is part of LatinExtended-B UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_latin_extended_b(code: i32) -> i32 {
    (0x0180..=0x024F).contains(&code) as i32
}

/// Check whether the character is part of LatinExtendedAdditional UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_latin_extended_additional(code: i32) -> i32 {
    (0x1E00..=0x1EFF).contains(&code) as i32
}

/// Check whether the character is part of LetterlikeSymbols UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_letterlike_symbols(code: i32) -> i32 {
    (0x2100..=0x214F).contains(&code) as i32
}

/// Check whether the character is part of Limbu UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_limbu(code: i32) -> i32 {
    (0x1900..=0x194F).contains(&code) as i32
}

/// Check whether the character is part of LinearBIdeograms UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_linear_bideograms(code: i32) -> i32 {
    (0x10080..=0x100FF).contains(&code) as i32
}

/// Check whether the character is part of LinearBSyllabary UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_linear_bsyllabary(code: i32) -> i32 {
    (0x10000..=0x1007F).contains(&code) as i32
}

/// Check whether the character is part of LowSurrogates UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_low_surrogates(code: i32) -> i32 {
    (0xDC00..=0xDFFF).contains(&code) as i32
}

/// Check whether the character is part of Malayalam UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_malayalam(code: i32) -> i32 {
    (0x0D00..=0x0D7F).contains(&code) as i32
}

/// Check whether the character is part of MathematicalAlphanumericSymbols UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_mathematical_alphanumeric_symbols(code: i32) -> i32 {
    (0x1D400..=0x1D7FF).contains(&code) as i32
}

/// Check whether the character is part of MathematicalOperators UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_mathematical_operators(code: i32) -> i32 {
    (0x2200..=0x22FF).contains(&code) as i32
}

/// Check whether the character is part of MiscellaneousMathematicalSymbols-A UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_miscellaneous_mathematical_symbols_a(code: i32) -> i32 {
    (0x27C0..=0x27EF).contains(&code) as i32
}

/// Check whether the character is part of MiscellaneousMathematicalSymbols-B UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_miscellaneous_mathematical_symbols_b(code: i32) -> i32 {
    (0x2980..=0x29FF).contains(&code) as i32
}

/// Check whether the character is part of MiscellaneousSymbols UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_miscellaneous_symbols(code: i32) -> i32 {
    (0x2600..=0x26FF).contains(&code) as i32
}

/// Check whether the character is part of MiscellaneousSymbolsandArrows UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_miscellaneous_symbolsand_arrows(code: i32) -> i32 {
    (0x2B00..=0x2BFF).contains(&code) as i32
}

/// Check whether the character is part of MiscellaneousTechnical UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_miscellaneous_technical(code: i32) -> i32 {
    (0x2300..=0x23FF).contains(&code) as i32
}

/// Check whether the character is part of Mongolian UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_mongolian(code: i32) -> i32 {
    (0x1800..=0x18AF).contains(&code) as i32
}

/// Check whether the character is part of MusicalSymbols UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_musical_symbols(code: i32) -> i32 {
    (0x1D100..=0x1D1FF).contains(&code) as i32
}

/// Check whether the character is part of Myanmar UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_myanmar(code: i32) -> i32 {
    (0x1000..=0x109F).contains(&code) as i32
}

/// Check whether the character is part of NumberForms UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_number_forms(code: i32) -> i32 {
    (0x2150..=0x218F).contains(&code) as i32
}

/// Check whether the character is part of Ogham UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_ogham(code: i32) -> i32 {
    (0x1680..=0x169F).contains(&code) as i32
}

/// Check whether the character is part of OldItalic UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_old_italic(code: i32) -> i32 {
    (0x10300..=0x1032F).contains(&code) as i32
}

/// Check whether the character is part of OpticalCharacterRecognition UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_optical_character_recognition(code: i32) -> i32 {
    (0x2440..=0x245F).contains(&code) as i32
}

/// Check whether the character is part of Oriya UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_oriya(code: i32) -> i32 {
    (0x0B00..=0x0B7F).contains(&code) as i32
}

/// Check whether the character is part of Osmanya UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_osmanya(code: i32) -> i32 {
    (0x10480..=0x104AF).contains(&code) as i32
}

/// Check whether the character is part of PhoneticExtensions UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_phonetic_extensions(code: i32) -> i32 {
    (0x1D00..=0x1D7F).contains(&code) as i32
}

/// Check whether the character is part of PrivateUse UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_private_use(code: i32) -> i32 {
    ((0xE000..=0xF8FF).contains(&code)
        || (0xF0000..=0xFFFFF).contains(&code)
        || (0x100000..=0x10FFFF).contains(&code)) as i32
}

/// Check whether the character is part of PrivateUseArea UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_private_use_area(code: i32) -> i32 {
    (0xE000..=0xF8FF).contains(&code) as i32
}

/// Check whether the character is part of Runic UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_runic(code: i32) -> i32 {
    (0x16A0..=0x16FF).contains(&code) as i32
}

/// Check whether the character is part of Shavian UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_shavian(code: i32) -> i32 {
    (0x10450..=0x1047F).contains(&code) as i32
}

/// Check whether the character is part of Sinhala UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_sinhala(code: i32) -> i32 {
    (0x0D80..=0x0DFF).contains(&code) as i32
}

/// Check whether the character is part of SmallFormVariants UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_small_form_variants(code: i32) -> i32 {
    (0xFE50..=0xFE6F).contains(&code) as i32
}

/// Check whether the character is part of SpacingModifierLetters UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_spacing_modifier_letters(code: i32) -> i32 {
    (0x02B0..=0x02FF).contains(&code) as i32
}

/// Check whether the character is part of Specials UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_specials(code: i32) -> i32 {
    (0xFFF0..=0xFFFF).contains(&code) as i32
}

/// Check whether the character is part of SuperscriptsandSubscripts UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_superscriptsand_subscripts(code: i32) -> i32 {
    (0x2070..=0x209F).contains(&code) as i32
}

/// Check whether the character is part of SupplementalArrows-A UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_supplemental_arrows_a(code: i32) -> i32 {
    (0x27F0..=0x27FF).contains(&code) as i32
}

/// Check whether the character is part of SupplementalArrows-B UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_supplemental_arrows_b(code: i32) -> i32 {
    (0x2900..=0x297F).contains(&code) as i32
}

/// Check whether the character is part of SupplementalMathematicalOperators UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_supplemental_mathematical_operators(code: i32) -> i32 {
    (0x2A00..=0x2AFF).contains(&code) as i32
}

/// Check whether the character is part of SupplementaryPrivateUseArea-A UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_supplementary_private_use_area_a(code: i32) -> i32 {
    (0xF0000..=0xFFFFF).contains(&code) as i32
}

/// Check whether the character is part of SupplementaryPrivateUseArea-B UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_supplementary_private_use_area_b(code: i32) -> i32 {
    (0x100000..=0x10FFFF).contains(&code) as i32
}

/// Check whether the character is part of Syriac UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_syriac(code: i32) -> i32 {
    (0x0700..=0x074F).contains(&code) as i32
}

/// Check whether the character is part of Tagalog UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_tagalog(code: i32) -> i32 {
    (0x1700..=0x171F).contains(&code) as i32
}

/// Check whether the character is part of Tagbanwa UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_tagbanwa(code: i32) -> i32 {
    (0x1760..=0x177F).contains(&code) as i32
}

/// Check whether the character is part of Tags UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_tags(code: i32) -> i32 {
    (0xE0000..=0xE007F).contains(&code) as i32
}

/// Check whether the character is part of TaiLe UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_tai_le(code: i32) -> i32 {
    (0x1950..=0x197F).contains(&code) as i32
}

/// Check whether the character is part of TaiXuanJingSymbols UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_tai_xuan_jing_symbols(code: i32) -> i32 {
    (0x1D300..=0x1D35F).contains(&code) as i32
}

/// Check whether the character is part of Tamil UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_tamil(code: i32) -> i32 {
    (0x0B80..=0x0BFF).contains(&code) as i32
}

/// Check whether the character is part of Telugu UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_telugu(code: i32) -> i32 {
    (0x0C00..=0x0C7F).contains(&code) as i32
}

/// Check whether the character is part of Thaana UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_thaana(code: i32) -> i32 {
    (0x0780..=0x07BF).contains(&code) as i32
}

/// Check whether the character is part of Thai UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_thai(code: i32) -> i32 {
    (0x0E00..=0x0E7F).contains(&code) as i32
}

/// Check whether the character is part of Tibetan UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_tibetan(code: i32) -> i32 {
    (0x0F00..=0x0FFF).contains(&code) as i32
}

/// Check whether the character is part of Ugaritic UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_ugaritic(code: i32) -> i32 {
    (0x10380..=0x1039F).contains(&code) as i32
}

/// Check whether the character is part of UnifiedCanadianAboriginalSyllabics UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_unified_canadian_aboriginal_syllabics(code: i32) -> i32 {
    (0x1400..=0x167F).contains(&code) as i32
}

/// Check whether the character is part of VariationSelectors UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_variation_selectors(code: i32) -> i32 {
    (0xFE00..=0xFE0F).contains(&code) as i32
}

/// Check whether the character is part of VariationSelectorsSupplement UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_variation_selectors_supplement(code: i32) -> i32 {
    (0xE0100..=0xE01EF).contains(&code) as i32
}

/// Check whether the character is part of YiRadicals UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_yi_radicals(code: i32) -> i32 {
    (0xA490..=0xA4CF).contains(&code) as i32
}

/// Check whether the character is part of YiSyllables UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_yi_syllables(code: i32) -> i32 {
    (0xA000..=0xA48F).contains(&code) as i32
}

/// Check whether the character is part of YijingHexagramSymbols UCS Block
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_yijing_hexagram_symbols(code: i32) -> i32 {
    (0x4DC0..=0x4DFF).contains(&code) as i32
}

/// Check whether the character is part of the UCS Block
///
/// Returns 1 if true, 0 if false and -1 on unknown block
pub unsafe extern "C" fn xml_ucs_is_block(code: i32, block: *const c_char) -> i32 {
    if let Some(func) = xml_unicode_lookup(&XML_UNICODE_BLOCK_TBL, block) {
        func(code)
    } else {
        -1
    }
}

/// Check whether the character is part of C UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_c(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_CG) as i32
}

/// Check whether the character is part of Cc UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_cc(code: i32) -> i32 {
    ((0x0..=0x1f).contains(&code) || (0x7f..=0x9f).contains(&code)) as i32
}

/// Check whether the character is part of Cf UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_cf(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_CF_G) as i32
}

/// Check whether the character is part of Co UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_co(code: i32) -> i32 {
    ((code == 0xe000)
        || (code == 0xf8ff)
        || (code == 0xf0000)
        || (code == 0xffffd)
        || (code == 0x100000)
        || (code == 0x10fffd)) as i32
}

/// Check whether the character is part of Cs UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_cs(code: i32) -> i32 {
    ((code == 0xd800)
        || (0xdb7f..=0xdb80).contains(&code)
        || (0xdbff..=0xdc00).contains(&code)
        || (code == 0xdfff)) as i32
}

/// Check whether the character is part of L UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_l(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_LG) as i32
}

/// Check whether the character is part of Ll UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_ll(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_LL_G) as i32
}

/// Check whether the character is part of Lm UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_lm(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_LM_G) as i32
}

/// Check whether the character is part of Lo UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_lo(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_LO_G) as i32
}

/// Check whether the character is part of Lt UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_lt(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_LT_G) as i32
}

/// Check whether the character is part of Lu UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_lu(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_LU_G) as i32
}

/// Check whether the character is part of M UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_m(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_MG) as i32
}

/// Check whether the character is part of Mc UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_mc(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_MC_G) as i32
}

/// Check whether the character is part of Me UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_me(code: i32) -> i32 {
    ((0x488..=0x489).contains(&code)
        || (code == 0x6de)
        || (0x20dd..=0x20e0).contains(&code)
        || (0x20e2..=0x20e4).contains(&code)) as i32
}

/// Check whether the character is part of Mn UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_mn(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_MN_G) as i32
}

/// Check whether the character is part of N UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_n(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_NG) as i32
}

/// Check whether the character is part of Nd UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_nd(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_ND_G) as i32
}

/// Check whether the character is part of Nl UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_nl(code: i32) -> i32 {
    ((0x16ee..=0x16f0).contains(&code)
        || (0x2160..=0x2183).contains(&code)
        || (code == 0x3007)
        || (0x3021..=0x3029).contains(&code)
        || (0x3038..=0x303a).contains(&code)
        || (code == 0x1034a)) as i32
}

/// Check whether the character is part of No UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_no(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_NO_G) as i32
}

/// Check whether the character is part of P UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_p(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_PG) as i32
}

/// Check whether the character is part of Pc UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_pc(code: i32) -> i32 {
    ((code == 0x5f)
        || (0x203f..=0x2040).contains(&code)
        || (code == 0x2054)
        || (code == 0x30fb)
        || (0xfe33..=0xfe34).contains(&code)
        || (0xfe4d..=0xfe4f).contains(&code)
        || (code == 0xff3f)
        || (code == 0xff65)) as i32
}

/// Check whether the character is part of Pd UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_pd(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_PD_G) as i32
}

/// Check whether the character is part of Pe UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_pe(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_PE_G) as i32
}

/// Check whether the character is part of Pf UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_pf(code: i32) -> i32 {
    ((code == 0xbb) || (code == 0x2019) || (code == 0x201d) || (code == 0x203a)) as i32
}

/// Check whether the character is part of Pi UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_pi(code: i32) -> i32 {
    ((code == 0xab)
        || (code == 0x2018)
        || (0x201b..=0x201c).contains(&code)
        || (code == 0x201f)
        || (code == 0x2039)) as i32
}

/// Check whether the character is part of Po UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_po(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_PO_G) as i32
}

/// Check whether the character is part of Ps UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_ps(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_PS_G) as i32
}

/// Check whether the character is part of S UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_s(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_SG) as i32
}

/// Check whether the character is part of Sc UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_sc(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_SC_G) as i32
}

/// Check whether the character is part of Sk UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_sk(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_SK_G) as i32
}

/// Check whether the character is part of Sm UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_sm(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_SM_G) as i32
}

/// Check whether the character is part of So UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_so(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_SO_G) as i32
}

/// Check whether the character is part of Z UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_z(code: i32) -> i32 {
    xml_char_in_range(code as u32, &XML_ZG) as i32
}

/// Check whether the character is part of Zl UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_zl(code: i32) -> i32 {
    (code == 0x2028) as i32
}

/// Check whether the character is part of Zp UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_zp(code: i32) -> i32 {
    (code == 0x2029) as i32
}

/// Check whether the character is part of Zs UCS Category
///
/// Returns 1 if true 0 otherwise
pub unsafe extern "C" fn xml_ucs_is_cat_zs(code: i32) -> i32 {
    ((code == 0x20)
        || (code == 0xa0)
        || (code == 0x1680)
        || (code == 0x180e)
        || (0x2000..=0x200a).contains(&code)
        || (code == 0x202f)
        || (code == 0x205f)
        || (code == 0x3000)) as i32
}

/// Check whether the character is part of the UCS Category
///
/// Returns 1 if true, 0 if false and -1 on unknown category
pub unsafe extern "C" fn xml_ucs_is_cat(code: i32, cat: *const c_char) -> i32 {
    if let Some(func) = xml_unicode_lookup(&XML_UNICODE_CAT_TBL, cat) {
        func(code)
    } else {
        -1
    }
}

/// Binary table lookup for user-supplied name
///
/// Returns pointer to range function if found, otherwise NULL
#[doc(alias = "xmlUnicodeLookup")]
unsafe extern "C" fn xml_unicode_lookup(
    tptr: *const XmlUnicodeNameTable,
    tname: *const c_char,
) -> Option<XmlIntFunc> {
    let mut low: i32;
    let mut high: i32;
    let mut mid: i32;
    let mut cmp: i32;

    if tptr.is_null() || tname.is_null() {
        return None;
    }

    low = 0;
    high = (*tptr).numentries - 1;
    let sptr: *const XmlUnicodeRange = (*tptr).table;
    while low <= high {
        mid = (low + high) / 2;
        let res = {
            cmp = strcmp(tname, (*sptr.add(mid as usize)).rangename);
            cmp == 0
        };
        if res {
            return Some((*sptr.add(mid as usize)).func);
        }
        if cmp < 0 {
            high = mid - 1;
        } else {
            low = mid + 1;
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_ucsis_aegean_numbers() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_aegean_numbers(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsAegeanNumbers",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsAegeanNumbers()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_alphabetic_presentation_forms() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_alphabetic_presentation_forms(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsAlphabeticPresentationForms",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsAlphabeticPresentationForms()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_arabic() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_arabic(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsArabic",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsArabic()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_arabic_presentation_forms_a() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_arabic_presentation_forms_a(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsArabicPresentationFormsA",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsArabicPresentationFormsA()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_arabic_presentation_forms_b() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_arabic_presentation_forms_b(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsArabicPresentationFormsB",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsArabicPresentationFormsB()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_armenian() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_armenian(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsArmenian",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsArmenian()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_arrows() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_arrows(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsArrows",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsArrows()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_basic_latin() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_basic_latin(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsBasicLatin",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsBasicLatin()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_bengali() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_bengali(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsBengali",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsBengali()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_block() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                for n_block in 0..GEN_NB_CONST_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let code = gen_int(n_code, 0);
                    let block = gen_const_char_ptr(n_block, 1);

                    let ret_val = xml_ucs_is_block(code, block);
                    desret_int(ret_val);
                    des_int(n_code, code, 0);
                    des_const_char_ptr(n_block, block, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlUCSIsBlock",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsBlock()");
                        eprint!(" {}", n_code);
                        eprintln!(" {}", n_block);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_block_elements() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_block_elements(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsBlockElements",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsBlockElements()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_bopomofo() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_bopomofo(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsBopomofo",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsBopomofo()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_bopomofo_extended() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_bopomofo_extended(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsBopomofoExtended",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsBopomofoExtended()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_box_drawing() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_box_drawing(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsBoxDrawing",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsBoxDrawing()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_braille_patterns() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_braille_patterns(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsBraillePatterns",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsBraillePatterns()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_buhid() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_buhid(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsBuhid",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsBuhid()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_byzantine_musical_symbols() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_byzantine_musical_symbols(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsByzantineMusicalSymbols",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsByzantineMusicalSymbols()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cjkcompatibility() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cjk_compatibility(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCJKCompatibility",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsCJKCompatibility()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cjkcompatibility_forms() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cjk_compatibility_forms(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCJKCompatibilityForms",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsCJKCompatibilityForms()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cjkcompatibility_ideographs() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cjk_compatibility_ideographs(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCJKCompatibilityIdeographs",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsCJKCompatibilityIdeographs()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cjkcompatibility_ideographs_supplement() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cjk_compatibility_ideographs_supplement(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCJKCompatibilityIdeographsSupplement",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsCJKCompatibilityIdeographsSupplement()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cjkradicals_supplement() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cjk_radicals_supplement(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCJKRadicalsSupplement",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsCJKRadicalsSupplement()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cjksymbolsand_punctuation() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cjk_symbolsand_punctuation(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCJKSymbolsandPunctuation",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsCJKSymbolsandPunctuation()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cjkunified_ideographs() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cjk_unified_ideographs(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCJKUnifiedIdeographs",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsCJKUnifiedIdeographs()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cjkunified_ideographs_extension_a() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cjk_unified_ideographs_extension_a(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCJKUnifiedIdeographsExtensionA",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsCJKUnifiedIdeographsExtensionA()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cjkunified_ideographs_extension_b() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cjk_unified_ideographs_extension_b(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCJKUnifiedIdeographsExtensionB",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsCJKUnifiedIdeographsExtensionB()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                for n_cat in 0..GEN_NB_CONST_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let code = gen_int(n_code, 0);
                    let cat = gen_const_char_ptr(n_cat, 1);

                    let ret_val = xml_ucs_is_cat(code, cat);
                    desret_int(ret_val);
                    des_int(n_code, code, 0);
                    des_const_char_ptr(n_cat, cat, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlUCSIsCat",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCat()");
                        eprint!(" {}", n_code);
                        eprintln!(" {}", n_cat);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_c() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_c(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatC",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatC()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_cc() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_cc(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatCc",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatCc()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_cf() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_cf(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatCf",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatCf()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_co() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_co(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatCo",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatCo()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_cs() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_cs(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatCs",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatCs()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_l() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_l(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatL",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatL()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_ll() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_ll(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatLl",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatLl()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_lm() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_lm(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatLm",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatLm()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_lo() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_lo(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatLo",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatLo()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_lt() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_lt(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatLt",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatLt()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_lu() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_lu(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatLu",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatLu()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_m() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_m(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatM",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatM()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_mc() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_mc(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatMc",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatMc()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_me() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_me(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatMe",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatMe()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_mn() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_mn(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatMn",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatMn()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_n() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_n(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatN",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatN()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_nd() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_nd(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatNd",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatNd()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_nl() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_nl(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatNl",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatNl()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_no() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_no(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatNo",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatNo()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_p() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_p(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatP",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatP()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_pc() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_pc(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatPc",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatPc()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_pd() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_pd(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatPd",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatPd()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_pe() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_pe(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatPe",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatPe()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_pf() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_pf(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatPf",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatPf()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_pi() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_pi(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatPi",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatPi()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_po() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_po(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatPo",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatPo()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_ps() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_ps(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatPs",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatPs()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_s() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_s(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatS",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatS()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_sc() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_sc(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatSc",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatSc()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_sk() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_sk(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatSk",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatSk()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_sm() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_sm(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatSm",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatSm()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_so() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_so(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatSo",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatSo()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_z() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_z(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatZ",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatZ()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_zl() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_zl(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatZl",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatZl()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_zp() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_zp(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatZp",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatZp()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cat_zs() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cat_zs(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCatZs",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCatZs()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cherokee() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cherokee(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCherokee",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCherokee()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_combining_diacritical_marks() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_combining_diacritical_marks(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCombiningDiacriticalMarks",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsCombiningDiacriticalMarks()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_combining_diacritical_marksfor_symbols() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_combining_diacritical_marksfor_symbols(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCombiningDiacriticalMarksforSymbols",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsCombiningDiacriticalMarksforSymbols()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_combining_half_marks() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_combining_half_marks(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCombiningHalfMarks",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsCombiningHalfMarks()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_combining_marksfor_symbols() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_combining_marksfor_symbols(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCombiningMarksforSymbols",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsCombiningMarksforSymbols()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_control_pictures() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_control_pictures(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsControlPictures",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsControlPictures()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_currency_symbols() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_currency_symbols(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCurrencySymbols",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsCurrencySymbols()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cypriot_syllabary() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cypriot_syllabary(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCypriotSyllabary",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsCypriotSyllabary()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cyrillic() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cyrillic(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCyrillic",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsCyrillic()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_cyrillic_supplement() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_cyrillic_supplement(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsCyrillicSupplement",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsCyrillicSupplement()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_deseret() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_deseret(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsDeseret",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsDeseret()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_devanagari() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_devanagari(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsDevanagari",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsDevanagari()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_dingbats() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_dingbats(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsDingbats",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsDingbats()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_enclosed_alphanumerics() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_enclosed_alphanumerics(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsEnclosedAlphanumerics",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsEnclosedAlphanumerics()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_enclosed_cjklettersand_months() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_enclosed_cjk_lettersand_months(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsEnclosedCJKLettersandMonths",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsEnclosedCJKLettersandMonths()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_ethiopic() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_ethiopic(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsEthiopic",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsEthiopic()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_general_punctuation() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_general_punctuation(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsGeneralPunctuation",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsGeneralPunctuation()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_geometric_shapes() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_geometric_shapes(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsGeometricShapes",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsGeometricShapes()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_georgian() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_georgian(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsGeorgian",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsGeorgian()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_gothic() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_gothic(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsGothic",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsGothic()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_greek() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_greek(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsGreek",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsGreek()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_greek_extended() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_greek_extended(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsGreekExtended",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsGreekExtended()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_greekand_coptic() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_greekand_coptic(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsGreekandCoptic",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsGreekandCoptic()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_gujarati() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_gujarati(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsGujarati",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsGujarati()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_gurmukhi() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_gurmukhi(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsGurmukhi",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsGurmukhi()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_halfwidthand_fullwidth_forms() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_halfwidthand_fullwidth_forms(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsHalfwidthandFullwidthForms",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsHalfwidthandFullwidthForms()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_hangul_compatibility_jamo() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_hangul_compatibility_jamo(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsHangulCompatibilityJamo",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsHangulCompatibilityJamo()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_hangul_jamo() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_hangul_jamo(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsHangulJamo",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsHangulJamo()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_hangul_syllables() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_hangul_syllables(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsHangulSyllables",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsHangulSyllables()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_hanunoo() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_hanunoo(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsHanunoo",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsHanunoo()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_hebrew() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_hebrew(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsHebrew",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsHebrew()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_high_private_use_surrogates() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_high_private_use_surrogates(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsHighPrivateUseSurrogates",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsHighPrivateUseSurrogates()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_high_surrogates() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_high_surrogates(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsHighSurrogates",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsHighSurrogates()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_hiragana() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_hiragana(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsHiragana",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsHiragana()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_ipaextensions() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_ipa_extensions(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsIPAExtensions",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsIPAExtensions()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_ideographic_description_characters() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_ideographic_description_characters(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsIdeographicDescriptionCharacters",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsIdeographicDescriptionCharacters()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_kanbun() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_kanbun(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsKanbun",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsKanbun()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_kangxi_radicals() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_kangxi_radicals(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsKangxiRadicals",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsKangxiRadicals()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_kannada() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_kannada(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsKannada",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsKannada()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_katakana() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_katakana(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsKatakana",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsKatakana()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_katakana_phonetic_extensions() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_katakana_phonetic_extensions(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsKatakanaPhoneticExtensions",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsKatakanaPhoneticExtensions()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_khmer() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_khmer(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsKhmer",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsKhmer()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_khmer_symbols() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_khmer_symbols(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsKhmerSymbols",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsKhmerSymbols()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_lao() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_lao(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsLao",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsLao()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_latin1_supplement() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_latin1_supplement(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsLatin1Supplement",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsLatin1Supplement()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_latin_extended_a() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_latin_extended_a(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsLatinExtendedA",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsLatinExtendedA()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_latin_extended_additional() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_latin_extended_additional(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsLatinExtendedAdditional",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsLatinExtendedAdditional()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_latin_extended_b() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_latin_extended_b(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsLatinExtendedB",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsLatinExtendedB()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_letterlike_symbols() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_letterlike_symbols(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsLetterlikeSymbols",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsLetterlikeSymbols()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_limbu() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_limbu(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsLimbu",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsLimbu()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_linear_bideograms() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_linear_bideograms(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsLinearBIdeograms",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsLinearBIdeograms()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_linear_bsyllabary() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_linear_bsyllabary(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsLinearBSyllabary",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsLinearBSyllabary()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_low_surrogates() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_low_surrogates(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsLowSurrogates",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsLowSurrogates()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_malayalam() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_malayalam(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsMalayalam",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsMalayalam()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_mathematical_alphanumeric_symbols() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_mathematical_alphanumeric_symbols(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsMathematicalAlphanumericSymbols",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsMathematicalAlphanumericSymbols()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_mathematical_operators() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_mathematical_operators(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsMathematicalOperators",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsMathematicalOperators()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_miscellaneous_mathematical_symbols_a() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_miscellaneous_mathematical_symbols_a(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsMiscellaneousMathematicalSymbolsA",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsMiscellaneousMathematicalSymbolsA()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_miscellaneous_mathematical_symbols_b() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_miscellaneous_mathematical_symbols_b(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsMiscellaneousMathematicalSymbolsB",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsMiscellaneousMathematicalSymbolsB()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_miscellaneous_symbols() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_miscellaneous_symbols(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsMiscellaneousSymbols",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsMiscellaneousSymbols()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_miscellaneous_symbolsand_arrows() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_miscellaneous_symbolsand_arrows(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsMiscellaneousSymbolsandArrows",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsMiscellaneousSymbolsandArrows()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_miscellaneous_technical() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_miscellaneous_technical(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsMiscellaneousTechnical",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsMiscellaneousTechnical()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_mongolian() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_mongolian(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsMongolian",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsMongolian()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_musical_symbols() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_musical_symbols(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsMusicalSymbols",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsMusicalSymbols()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_myanmar() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_myanmar(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsMyanmar",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsMyanmar()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_number_forms() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_number_forms(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsNumberForms",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsNumberForms()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_ogham() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_ogham(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsOgham",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsOgham()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_old_italic() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_old_italic(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsOldItalic",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsOldItalic()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_optical_character_recognition() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_optical_character_recognition(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsOpticalCharacterRecognition",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsOpticalCharacterRecognition()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_oriya() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_oriya(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsOriya",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsOriya()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_osmanya() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_osmanya(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsOsmanya",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsOsmanya()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_phonetic_extensions() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_phonetic_extensions(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsPhoneticExtensions",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsPhoneticExtensions()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_private_use() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_private_use(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsPrivateUse",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsPrivateUse()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_private_use_area() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_private_use_area(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsPrivateUseArea",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsPrivateUseArea()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_runic() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_runic(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsRunic",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsRunic()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_shavian() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_shavian(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsShavian",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsShavian()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_sinhala() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_sinhala(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsSinhala",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsSinhala()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_small_form_variants() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_small_form_variants(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsSmallFormVariants",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsSmallFormVariants()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_spacing_modifier_letters() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_spacing_modifier_letters(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsSpacingModifierLetters",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsSpacingModifierLetters()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_specials() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_specials(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsSpecials",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsSpecials()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_superscriptsand_subscripts() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_superscriptsand_subscripts(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsSuperscriptsandSubscripts",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsSuperscriptsandSubscripts()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_supplemental_arrows_a() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_supplemental_arrows_a(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsSupplementalArrowsA",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsSupplementalArrowsA()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_supplemental_arrows_b() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_supplemental_arrows_b(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsSupplementalArrowsB",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsSupplementalArrowsB()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_supplemental_mathematical_operators() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_supplemental_mathematical_operators(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsSupplementalMathematicalOperators",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsSupplementalMathematicalOperators()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_supplementary_private_use_area_a() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_supplementary_private_use_area_a(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsSupplementaryPrivateUseAreaA",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsSupplementaryPrivateUseAreaA()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_supplementary_private_use_area_b() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_supplementary_private_use_area_b(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsSupplementaryPrivateUseAreaB",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsSupplementaryPrivateUseAreaB()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_syriac() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_syriac(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsSyriac",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsSyriac()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_tagalog() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_tagalog(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsTagalog",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsTagalog()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_tagbanwa() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_tagbanwa(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsTagbanwa",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsTagbanwa()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_tags() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_tags(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsTags",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsTags()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_tai_le() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_tai_le(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsTaiLe",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsTaiLe()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_tai_xuan_jing_symbols() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_tai_xuan_jing_symbols(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsTaiXuanJingSymbols",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsTaiXuanJingSymbols()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_tamil() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_tamil(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsTamil",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsTamil()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_telugu() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_telugu(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsTelugu",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsTelugu()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_thaana() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_thaana(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsThaana",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsThaana()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_thai() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_thai(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsThai",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsThai()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_tibetan() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_tibetan(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsTibetan",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsTibetan()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_ugaritic() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_ugaritic(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsUgaritic",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlUCSIsUgaritic()");
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_unified_canadian_aboriginal_syllabics() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_unified_canadian_aboriginal_syllabics(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsUnifiedCanadianAboriginalSyllabics",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsUnifiedCanadianAboriginalSyllabics()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_variation_selectors() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_variation_selectors(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsVariationSelectors",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsVariationSelectors()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_variation_selectors_supplement() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_variation_selectors_supplement(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsVariationSelectorsSupplement",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsVariationSelectorsSupplement()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_yi_radicals() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_yi_radicals(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsYiRadicals",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsYiRadicals()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_yi_syllables() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_yi_syllables(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsYiSyllables",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsYiSyllables()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }

    #[test]
    fn test_xml_ucsis_yijing_hexagram_symbols() {
        #[cfg(feature = "libxml_unicode")]
        unsafe {
            let mut leaks = 0;

            for n_code in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let code = gen_int(n_code, 0);

                let ret_val = xml_ucs_is_yijing_hexagram_symbols(code);
                desret_int(ret_val);
                des_int(n_code, code, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlUCSIsYijingHexagramSymbols",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlUCSIsYijingHexagramSymbols()"
                    );
                    eprintln!(" {}", n_code);
                }
            }
        }
    }
}
