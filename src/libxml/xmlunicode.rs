//! Provide methods and data structures for handling Unicode characters.  
//! This module is based on `libxml/xmlunicode.h`, `xmlunicode.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::ffi::{c_char, c_int, c_uint};

use libc::strcmp;

use super::chvalid::{xml_char_in_range, XmlChLRange, XmlChRangeGroup, XmlChSRange};

pub type XmlIntFunc = unsafe extern "C" fn(c_int) -> c_int; /* just to keep one's mind untwisted */

#[repr(C)]
pub struct XmlUnicodeRange {
    rangename: *const c_char,
    func: XmlIntFunc,
}

#[repr(C)]
pub struct XmlUnicodeNameTable {
    table: *const XmlUnicodeRange,
    numentries: c_int,
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
    XmlChSRange {
        low: 0x0,
        high: 0x1f,
    },
    XmlChSRange {
        low: 0x7f,
        high: 0x9f,
    },
    XmlChSRange {
        low: 0xad,
        high: 0xad,
    },
    XmlChSRange {
        low: 0x600,
        high: 0x603,
    },
    XmlChSRange {
        low: 0x6dd,
        high: 0x6dd,
    },
    XmlChSRange {
        low: 0x70f,
        high: 0x70f,
    },
    XmlChSRange {
        low: 0x17b4,
        high: 0x17b5,
    },
    XmlChSRange {
        low: 0x200b,
        high: 0x200f,
    },
    XmlChSRange {
        low: 0x202a,
        high: 0x202e,
    },
    XmlChSRange {
        low: 0x2060,
        high: 0x2063,
    },
    XmlChSRange {
        low: 0x206a,
        high: 0x206f,
    },
    XmlChSRange {
        low: 0xd800,
        high: 0xd800,
    },
    XmlChSRange {
        low: 0xdb7f,
        high: 0xdb80,
    },
    XmlChSRange {
        low: 0xdbff,
        high: 0xdc00,
    },
    XmlChSRange {
        low: 0xdfff,
        high: 0xe000,
    },
    XmlChSRange {
        low: 0xf8ff,
        high: 0xf8ff,
    },
    XmlChSRange {
        low: 0xfeff,
        high: 0xfeff,
    },
    XmlChSRange {
        low: 0xfff9,
        high: 0xfffb,
    },
];

const XML_CL: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x1d173,
        high: 0x1d17a,
    },
    XmlChLRange {
        low: 0xe0001,
        high: 0xe0001,
    },
    XmlChLRange {
        low: 0xe0020,
        high: 0xe007f,
    },
    XmlChLRange {
        low: 0xf0000,
        high: 0xf0000,
    },
    XmlChLRange {
        low: 0xffffd,
        high: 0xffffd,
    },
    XmlChLRange {
        low: 0x100000,
        high: 0x100000,
    },
    XmlChLRange {
        low: 0x10fffd,
        high: 0x10fffd,
    },
];

const XML_CG: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 18,
    nb_long_range: 7,
    short_range: XML_CS,
    long_range: XML_CL,
};

const XML_CF_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0xad,
        high: 0xad,
    },
    XmlChSRange {
        low: 0x600,
        high: 0x603,
    },
    XmlChSRange {
        low: 0x6dd,
        high: 0x6dd,
    },
    XmlChSRange {
        low: 0x70f,
        high: 0x70f,
    },
    XmlChSRange {
        low: 0x17b4,
        high: 0x17b5,
    },
    XmlChSRange {
        low: 0x200b,
        high: 0x200f,
    },
    XmlChSRange {
        low: 0x202a,
        high: 0x202e,
    },
    XmlChSRange {
        low: 0x2060,
        high: 0x2063,
    },
    XmlChSRange {
        low: 0x206a,
        high: 0x206f,
    },
    XmlChSRange {
        low: 0xfeff,
        high: 0xfeff,
    },
    XmlChSRange {
        low: 0xfff9,
        high: 0xfffb,
    },
];

const XML_CF_L: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x1d173,
        high: 0x1d17a,
    },
    XmlChLRange {
        low: 0xe0001,
        high: 0xe0001,
    },
    XmlChLRange {
        low: 0xe0020,
        high: 0xe007f,
    },
];

const XML_CF_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 11,
    nb_long_range: 3,
    short_range: XML_CF_S,
    long_range: XML_CF_L,
};

const XML_LS: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x41,
        high: 0x5a,
    },
    XmlChSRange {
        low: 0x61,
        high: 0x7a,
    },
    XmlChSRange {
        low: 0xaa,
        high: 0xaa,
    },
    XmlChSRange {
        low: 0xb5,
        high: 0xb5,
    },
    XmlChSRange {
        low: 0xba,
        high: 0xba,
    },
    XmlChSRange {
        low: 0xc0,
        high: 0xd6,
    },
    XmlChSRange {
        low: 0xd8,
        high: 0xf6,
    },
    XmlChSRange {
        low: 0xf8,
        high: 0x236,
    },
    XmlChSRange {
        low: 0x250,
        high: 0x2c1,
    },
    XmlChSRange {
        low: 0x2c6,
        high: 0x2d1,
    },
    XmlChSRange {
        low: 0x2e0,
        high: 0x2e4,
    },
    XmlChSRange {
        low: 0x2ee,
        high: 0x2ee,
    },
    XmlChSRange {
        low: 0x37a,
        high: 0x37a,
    },
    XmlChSRange {
        low: 0x386,
        high: 0x386,
    },
    XmlChSRange {
        low: 0x388,
        high: 0x38a,
    },
    XmlChSRange {
        low: 0x38c,
        high: 0x38c,
    },
    XmlChSRange {
        low: 0x38e,
        high: 0x3a1,
    },
    XmlChSRange {
        low: 0x3a3,
        high: 0x3ce,
    },
    XmlChSRange {
        low: 0x3d0,
        high: 0x3f5,
    },
    XmlChSRange {
        low: 0x3f7,
        high: 0x3fb,
    },
    XmlChSRange {
        low: 0x400,
        high: 0x481,
    },
    XmlChSRange {
        low: 0x48a,
        high: 0x4ce,
    },
    XmlChSRange {
        low: 0x4d0,
        high: 0x4f5,
    },
    XmlChSRange {
        low: 0x4f8,
        high: 0x4f9,
    },
    XmlChSRange {
        low: 0x500,
        high: 0x50f,
    },
    XmlChSRange {
        low: 0x531,
        high: 0x556,
    },
    XmlChSRange {
        low: 0x559,
        high: 0x559,
    },
    XmlChSRange {
        low: 0x561,
        high: 0x587,
    },
    XmlChSRange {
        low: 0x5d0,
        high: 0x5ea,
    },
    XmlChSRange {
        low: 0x5f0,
        high: 0x5f2,
    },
    XmlChSRange {
        low: 0x621,
        high: 0x63a,
    },
    XmlChSRange {
        low: 0x640,
        high: 0x64a,
    },
    XmlChSRange {
        low: 0x66e,
        high: 0x66f,
    },
    XmlChSRange {
        low: 0x671,
        high: 0x6d3,
    },
    XmlChSRange {
        low: 0x6d5,
        high: 0x6d5,
    },
    XmlChSRange {
        low: 0x6e5,
        high: 0x6e6,
    },
    XmlChSRange {
        low: 0x6ee,
        high: 0x6ef,
    },
    XmlChSRange {
        low: 0x6fa,
        high: 0x6fc,
    },
    XmlChSRange {
        low: 0x6ff,
        high: 0x6ff,
    },
    XmlChSRange {
        low: 0x710,
        high: 0x710,
    },
    XmlChSRange {
        low: 0x712,
        high: 0x72f,
    },
    XmlChSRange {
        low: 0x74d,
        high: 0x74f,
    },
    XmlChSRange {
        low: 0x780,
        high: 0x7a5,
    },
    XmlChSRange {
        low: 0x7b1,
        high: 0x7b1,
    },
    XmlChSRange {
        low: 0x904,
        high: 0x939,
    },
    XmlChSRange {
        low: 0x93d,
        high: 0x93d,
    },
    XmlChSRange {
        low: 0x950,
        high: 0x950,
    },
    XmlChSRange {
        low: 0x958,
        high: 0x961,
    },
    XmlChSRange {
        low: 0x985,
        high: 0x98c,
    },
    XmlChSRange {
        low: 0x98f,
        high: 0x990,
    },
    XmlChSRange {
        low: 0x993,
        high: 0x9a8,
    },
    XmlChSRange {
        low: 0x9aa,
        high: 0x9b0,
    },
    XmlChSRange {
        low: 0x9b2,
        high: 0x9b2,
    },
    XmlChSRange {
        low: 0x9b6,
        high: 0x9b9,
    },
    XmlChSRange {
        low: 0x9bd,
        high: 0x9bd,
    },
    XmlChSRange {
        low: 0x9dc,
        high: 0x9dd,
    },
    XmlChSRange {
        low: 0x9df,
        high: 0x9e1,
    },
    XmlChSRange {
        low: 0x9f0,
        high: 0x9f1,
    },
    XmlChSRange {
        low: 0xa05,
        high: 0xa0a,
    },
    XmlChSRange {
        low: 0xa0f,
        high: 0xa10,
    },
    XmlChSRange {
        low: 0xa13,
        high: 0xa28,
    },
    XmlChSRange {
        low: 0xa2a,
        high: 0xa30,
    },
    XmlChSRange {
        low: 0xa32,
        high: 0xa33,
    },
    XmlChSRange {
        low: 0xa35,
        high: 0xa36,
    },
    XmlChSRange {
        low: 0xa38,
        high: 0xa39,
    },
    XmlChSRange {
        low: 0xa59,
        high: 0xa5c,
    },
    XmlChSRange {
        low: 0xa5e,
        high: 0xa5e,
    },
    XmlChSRange {
        low: 0xa72,
        high: 0xa74,
    },
    XmlChSRange {
        low: 0xa85,
        high: 0xa8d,
    },
    XmlChSRange {
        low: 0xa8f,
        high: 0xa91,
    },
    XmlChSRange {
        low: 0xa93,
        high: 0xaa8,
    },
    XmlChSRange {
        low: 0xaaa,
        high: 0xab0,
    },
    XmlChSRange {
        low: 0xab2,
        high: 0xab3,
    },
    XmlChSRange {
        low: 0xab5,
        high: 0xab9,
    },
    XmlChSRange {
        low: 0xabd,
        high: 0xabd,
    },
    XmlChSRange {
        low: 0xad0,
        high: 0xad0,
    },
    XmlChSRange {
        low: 0xae0,
        high: 0xae1,
    },
    XmlChSRange {
        low: 0xb05,
        high: 0xb0c,
    },
    XmlChSRange {
        low: 0xb0f,
        high: 0xb10,
    },
    XmlChSRange {
        low: 0xb13,
        high: 0xb28,
    },
    XmlChSRange {
        low: 0xb2a,
        high: 0xb30,
    },
    XmlChSRange {
        low: 0xb32,
        high: 0xb33,
    },
    XmlChSRange {
        low: 0xb35,
        high: 0xb39,
    },
    XmlChSRange {
        low: 0xb3d,
        high: 0xb3d,
    },
    XmlChSRange {
        low: 0xb5c,
        high: 0xb5d,
    },
    XmlChSRange {
        low: 0xb5f,
        high: 0xb61,
    },
    XmlChSRange {
        low: 0xb71,
        high: 0xb71,
    },
    XmlChSRange {
        low: 0xb83,
        high: 0xb83,
    },
    XmlChSRange {
        low: 0xb85,
        high: 0xb8a,
    },
    XmlChSRange {
        low: 0xb8e,
        high: 0xb90,
    },
    XmlChSRange {
        low: 0xb92,
        high: 0xb95,
    },
    XmlChSRange {
        low: 0xb99,
        high: 0xb9a,
    },
    XmlChSRange {
        low: 0xb9c,
        high: 0xb9c,
    },
    XmlChSRange {
        low: 0xb9e,
        high: 0xb9f,
    },
    XmlChSRange {
        low: 0xba3,
        high: 0xba4,
    },
    XmlChSRange {
        low: 0xba8,
        high: 0xbaa,
    },
    XmlChSRange {
        low: 0xbae,
        high: 0xbb5,
    },
    XmlChSRange {
        low: 0xbb7,
        high: 0xbb9,
    },
    XmlChSRange {
        low: 0xc05,
        high: 0xc0c,
    },
    XmlChSRange {
        low: 0xc0e,
        high: 0xc10,
    },
    XmlChSRange {
        low: 0xc12,
        high: 0xc28,
    },
    XmlChSRange {
        low: 0xc2a,
        high: 0xc33,
    },
    XmlChSRange {
        low: 0xc35,
        high: 0xc39,
    },
    XmlChSRange {
        low: 0xc60,
        high: 0xc61,
    },
    XmlChSRange {
        low: 0xc85,
        high: 0xc8c,
    },
    XmlChSRange {
        low: 0xc8e,
        high: 0xc90,
    },
    XmlChSRange {
        low: 0xc92,
        high: 0xca8,
    },
    XmlChSRange {
        low: 0xcaa,
        high: 0xcb3,
    },
    XmlChSRange {
        low: 0xcb5,
        high: 0xcb9,
    },
    XmlChSRange {
        low: 0xcbd,
        high: 0xcbd,
    },
    XmlChSRange {
        low: 0xcde,
        high: 0xcde,
    },
    XmlChSRange {
        low: 0xce0,
        high: 0xce1,
    },
    XmlChSRange {
        low: 0xd05,
        high: 0xd0c,
    },
    XmlChSRange {
        low: 0xd0e,
        high: 0xd10,
    },
    XmlChSRange {
        low: 0xd12,
        high: 0xd28,
    },
    XmlChSRange {
        low: 0xd2a,
        high: 0xd39,
    },
    XmlChSRange {
        low: 0xd60,
        high: 0xd61,
    },
    XmlChSRange {
        low: 0xd85,
        high: 0xd96,
    },
    XmlChSRange {
        low: 0xd9a,
        high: 0xdb1,
    },
    XmlChSRange {
        low: 0xdb3,
        high: 0xdbb,
    },
    XmlChSRange {
        low: 0xdbd,
        high: 0xdbd,
    },
    XmlChSRange {
        low: 0xdc0,
        high: 0xdc6,
    },
    XmlChSRange {
        low: 0xe01,
        high: 0xe30,
    },
    XmlChSRange {
        low: 0xe32,
        high: 0xe33,
    },
    XmlChSRange {
        low: 0xe40,
        high: 0xe46,
    },
    XmlChSRange {
        low: 0xe81,
        high: 0xe82,
    },
    XmlChSRange {
        low: 0xe84,
        high: 0xe84,
    },
    XmlChSRange {
        low: 0xe87,
        high: 0xe88,
    },
    XmlChSRange {
        low: 0xe8a,
        high: 0xe8a,
    },
    XmlChSRange {
        low: 0xe8d,
        high: 0xe8d,
    },
    XmlChSRange {
        low: 0xe94,
        high: 0xe97,
    },
    XmlChSRange {
        low: 0xe99,
        high: 0xe9f,
    },
    XmlChSRange {
        low: 0xea1,
        high: 0xea3,
    },
    XmlChSRange {
        low: 0xea5,
        high: 0xea5,
    },
    XmlChSRange {
        low: 0xea7,
        high: 0xea7,
    },
    XmlChSRange {
        low: 0xeaa,
        high: 0xeab,
    },
    XmlChSRange {
        low: 0xead,
        high: 0xeb0,
    },
    XmlChSRange {
        low: 0xeb2,
        high: 0xeb3,
    },
    XmlChSRange {
        low: 0xebd,
        high: 0xebd,
    },
    XmlChSRange {
        low: 0xec0,
        high: 0xec4,
    },
    XmlChSRange {
        low: 0xec6,
        high: 0xec6,
    },
    XmlChSRange {
        low: 0xedc,
        high: 0xedd,
    },
    XmlChSRange {
        low: 0xf00,
        high: 0xf00,
    },
    XmlChSRange {
        low: 0xf40,
        high: 0xf47,
    },
    XmlChSRange {
        low: 0xf49,
        high: 0xf6a,
    },
    XmlChSRange {
        low: 0xf88,
        high: 0xf8b,
    },
    XmlChSRange {
        low: 0x1000,
        high: 0x1021,
    },
    XmlChSRange {
        low: 0x1023,
        high: 0x1027,
    },
    XmlChSRange {
        low: 0x1029,
        high: 0x102a,
    },
    XmlChSRange {
        low: 0x1050,
        high: 0x1055,
    },
    XmlChSRange {
        low: 0x10a0,
        high: 0x10c5,
    },
    XmlChSRange {
        low: 0x10d0,
        high: 0x10f8,
    },
    XmlChSRange {
        low: 0x1100,
        high: 0x1159,
    },
    XmlChSRange {
        low: 0x115f,
        high: 0x11a2,
    },
    XmlChSRange {
        low: 0x11a8,
        high: 0x11f9,
    },
    XmlChSRange {
        low: 0x1200,
        high: 0x1206,
    },
    XmlChSRange {
        low: 0x1208,
        high: 0x1246,
    },
    XmlChSRange {
        low: 0x1248,
        high: 0x1248,
    },
    XmlChSRange {
        low: 0x124a,
        high: 0x124d,
    },
    XmlChSRange {
        low: 0x1250,
        high: 0x1256,
    },
    XmlChSRange {
        low: 0x1258,
        high: 0x1258,
    },
    XmlChSRange {
        low: 0x125a,
        high: 0x125d,
    },
    XmlChSRange {
        low: 0x1260,
        high: 0x1286,
    },
    XmlChSRange {
        low: 0x1288,
        high: 0x1288,
    },
    XmlChSRange {
        low: 0x128a,
        high: 0x128d,
    },
    XmlChSRange {
        low: 0x1290,
        high: 0x12ae,
    },
    XmlChSRange {
        low: 0x12b0,
        high: 0x12b0,
    },
    XmlChSRange {
        low: 0x12b2,
        high: 0x12b5,
    },
    XmlChSRange {
        low: 0x12b8,
        high: 0x12be,
    },
    XmlChSRange {
        low: 0x12c0,
        high: 0x12c0,
    },
    XmlChSRange {
        low: 0x12c2,
        high: 0x12c5,
    },
    XmlChSRange {
        low: 0x12c8,
        high: 0x12ce,
    },
    XmlChSRange {
        low: 0x12d0,
        high: 0x12d6,
    },
    XmlChSRange {
        low: 0x12d8,
        high: 0x12ee,
    },
    XmlChSRange {
        low: 0x12f0,
        high: 0x130e,
    },
    XmlChSRange {
        low: 0x1310,
        high: 0x1310,
    },
    XmlChSRange {
        low: 0x1312,
        high: 0x1315,
    },
    XmlChSRange {
        low: 0x1318,
        high: 0x131e,
    },
    XmlChSRange {
        low: 0x1320,
        high: 0x1346,
    },
    XmlChSRange {
        low: 0x1348,
        high: 0x135a,
    },
    XmlChSRange {
        low: 0x13a0,
        high: 0x13f4,
    },
    XmlChSRange {
        low: 0x1401,
        high: 0x166c,
    },
    XmlChSRange {
        low: 0x166f,
        high: 0x1676,
    },
    XmlChSRange {
        low: 0x1681,
        high: 0x169a,
    },
    XmlChSRange {
        low: 0x16a0,
        high: 0x16ea,
    },
    XmlChSRange {
        low: 0x1700,
        high: 0x170c,
    },
    XmlChSRange {
        low: 0x170e,
        high: 0x1711,
    },
    XmlChSRange {
        low: 0x1720,
        high: 0x1731,
    },
    XmlChSRange {
        low: 0x1740,
        high: 0x1751,
    },
    XmlChSRange {
        low: 0x1760,
        high: 0x176c,
    },
    XmlChSRange {
        low: 0x176e,
        high: 0x1770,
    },
    XmlChSRange {
        low: 0x1780,
        high: 0x17b3,
    },
    XmlChSRange {
        low: 0x17d7,
        high: 0x17d7,
    },
    XmlChSRange {
        low: 0x17dc,
        high: 0x17dc,
    },
    XmlChSRange {
        low: 0x1820,
        high: 0x1877,
    },
    XmlChSRange {
        low: 0x1880,
        high: 0x18a8,
    },
    XmlChSRange {
        low: 0x1900,
        high: 0x191c,
    },
    XmlChSRange {
        low: 0x1950,
        high: 0x196d,
    },
    XmlChSRange {
        low: 0x1970,
        high: 0x1974,
    },
    XmlChSRange {
        low: 0x1d00,
        high: 0x1d6b,
    },
    XmlChSRange {
        low: 0x1e00,
        high: 0x1e9b,
    },
    XmlChSRange {
        low: 0x1ea0,
        high: 0x1ef9,
    },
    XmlChSRange {
        low: 0x1f00,
        high: 0x1f15,
    },
    XmlChSRange {
        low: 0x1f18,
        high: 0x1f1d,
    },
    XmlChSRange {
        low: 0x1f20,
        high: 0x1f45,
    },
    XmlChSRange {
        low: 0x1f48,
        high: 0x1f4d,
    },
    XmlChSRange {
        low: 0x1f50,
        high: 0x1f57,
    },
    XmlChSRange {
        low: 0x1f59,
        high: 0x1f59,
    },
    XmlChSRange {
        low: 0x1f5b,
        high: 0x1f5b,
    },
    XmlChSRange {
        low: 0x1f5d,
        high: 0x1f5d,
    },
    XmlChSRange {
        low: 0x1f5f,
        high: 0x1f7d,
    },
    XmlChSRange {
        low: 0x1f80,
        high: 0x1fb4,
    },
    XmlChSRange {
        low: 0x1fb6,
        high: 0x1fbc,
    },
    XmlChSRange {
        low: 0x1fbe,
        high: 0x1fbe,
    },
    XmlChSRange {
        low: 0x1fc2,
        high: 0x1fc4,
    },
    XmlChSRange {
        low: 0x1fc6,
        high: 0x1fcc,
    },
    XmlChSRange {
        low: 0x1fd0,
        high: 0x1fd3,
    },
    XmlChSRange {
        low: 0x1fd6,
        high: 0x1fdb,
    },
    XmlChSRange {
        low: 0x1fe0,
        high: 0x1fec,
    },
    XmlChSRange {
        low: 0x1ff2,
        high: 0x1ff4,
    },
    XmlChSRange {
        low: 0x1ff6,
        high: 0x1ffc,
    },
    XmlChSRange {
        low: 0x2071,
        high: 0x2071,
    },
    XmlChSRange {
        low: 0x207f,
        high: 0x207f,
    },
    XmlChSRange {
        low: 0x2102,
        high: 0x2102,
    },
    XmlChSRange {
        low: 0x2107,
        high: 0x2107,
    },
    XmlChSRange {
        low: 0x210a,
        high: 0x2113,
    },
    XmlChSRange {
        low: 0x2115,
        high: 0x2115,
    },
    XmlChSRange {
        low: 0x2119,
        high: 0x211d,
    },
    XmlChSRange {
        low: 0x2124,
        high: 0x2124,
    },
    XmlChSRange {
        low: 0x2126,
        high: 0x2126,
    },
    XmlChSRange {
        low: 0x2128,
        high: 0x2128,
    },
    XmlChSRange {
        low: 0x212a,
        high: 0x212d,
    },
    XmlChSRange {
        low: 0x212f,
        high: 0x2131,
    },
    XmlChSRange {
        low: 0x2133,
        high: 0x2139,
    },
    XmlChSRange {
        low: 0x213d,
        high: 0x213f,
    },
    XmlChSRange {
        low: 0x2145,
        high: 0x2149,
    },
    XmlChSRange {
        low: 0x3005,
        high: 0x3006,
    },
    XmlChSRange {
        low: 0x3031,
        high: 0x3035,
    },
    XmlChSRange {
        low: 0x303b,
        high: 0x303c,
    },
    XmlChSRange {
        low: 0x3041,
        high: 0x3096,
    },
    XmlChSRange {
        low: 0x309d,
        high: 0x309f,
    },
    XmlChSRange {
        low: 0x30a1,
        high: 0x30fa,
    },
    XmlChSRange {
        low: 0x30fc,
        high: 0x30ff,
    },
    XmlChSRange {
        low: 0x3105,
        high: 0x312c,
    },
    XmlChSRange {
        low: 0x3131,
        high: 0x318e,
    },
    XmlChSRange {
        low: 0x31a0,
        high: 0x31b7,
    },
    XmlChSRange {
        low: 0x31f0,
        high: 0x31ff,
    },
    XmlChSRange {
        low: 0x3400,
        high: 0x3400,
    },
    XmlChSRange {
        low: 0x4db5,
        high: 0x4db5,
    },
    XmlChSRange {
        low: 0x4e00,
        high: 0x4e00,
    },
    XmlChSRange {
        low: 0x9fa5,
        high: 0x9fa5,
    },
    XmlChSRange {
        low: 0xa000,
        high: 0xa48c,
    },
    XmlChSRange {
        low: 0xac00,
        high: 0xac00,
    },
    XmlChSRange {
        low: 0xd7a3,
        high: 0xd7a3,
    },
    XmlChSRange {
        low: 0xf900,
        high: 0xfa2d,
    },
    XmlChSRange {
        low: 0xfa30,
        high: 0xfa6a,
    },
    XmlChSRange {
        low: 0xfb00,
        high: 0xfb06,
    },
    XmlChSRange {
        low: 0xfb13,
        high: 0xfb17,
    },
    XmlChSRange {
        low: 0xfb1d,
        high: 0xfb1d,
    },
    XmlChSRange {
        low: 0xfb1f,
        high: 0xfb28,
    },
    XmlChSRange {
        low: 0xfb2a,
        high: 0xfb36,
    },
    XmlChSRange {
        low: 0xfb38,
        high: 0xfb3c,
    },
    XmlChSRange {
        low: 0xfb3e,
        high: 0xfb3e,
    },
    XmlChSRange {
        low: 0xfb40,
        high: 0xfb41,
    },
    XmlChSRange {
        low: 0xfb43,
        high: 0xfb44,
    },
    XmlChSRange {
        low: 0xfb46,
        high: 0xfbb1,
    },
    XmlChSRange {
        low: 0xfbd3,
        high: 0xfd3d,
    },
    XmlChSRange {
        low: 0xfd50,
        high: 0xfd8f,
    },
    XmlChSRange {
        low: 0xfd92,
        high: 0xfdc7,
    },
    XmlChSRange {
        low: 0xfdf0,
        high: 0xfdfb,
    },
    XmlChSRange {
        low: 0xfe70,
        high: 0xfe74,
    },
    XmlChSRange {
        low: 0xfe76,
        high: 0xfefc,
    },
    XmlChSRange {
        low: 0xff21,
        high: 0xff3a,
    },
    XmlChSRange {
        low: 0xff41,
        high: 0xff5a,
    },
    XmlChSRange {
        low: 0xff66,
        high: 0xffbe,
    },
    XmlChSRange {
        low: 0xffc2,
        high: 0xffc7,
    },
    XmlChSRange {
        low: 0xffca,
        high: 0xffcf,
    },
    XmlChSRange {
        low: 0xffd2,
        high: 0xffd7,
    },
    XmlChSRange {
        low: 0xffda,
        high: 0xffdc,
    },
];

const XML_LL: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x10000,
        high: 0x1000b,
    },
    XmlChLRange {
        low: 0x1000d,
        high: 0x10026,
    },
    XmlChLRange {
        low: 0x10028,
        high: 0x1003a,
    },
    XmlChLRange {
        low: 0x1003c,
        high: 0x1003d,
    },
    XmlChLRange {
        low: 0x1003f,
        high: 0x1004d,
    },
    XmlChLRange {
        low: 0x10050,
        high: 0x1005d,
    },
    XmlChLRange {
        low: 0x10080,
        high: 0x100fa,
    },
    XmlChLRange {
        low: 0x10300,
        high: 0x1031e,
    },
    XmlChLRange {
        low: 0x10330,
        high: 0x10349,
    },
    XmlChLRange {
        low: 0x10380,
        high: 0x1039d,
    },
    XmlChLRange {
        low: 0x10400,
        high: 0x1049d,
    },
    XmlChLRange {
        low: 0x10800,
        high: 0x10805,
    },
    XmlChLRange {
        low: 0x10808,
        high: 0x10808,
    },
    XmlChLRange {
        low: 0x1080a,
        high: 0x10835,
    },
    XmlChLRange {
        low: 0x10837,
        high: 0x10838,
    },
    XmlChLRange {
        low: 0x1083c,
        high: 0x1083c,
    },
    XmlChLRange {
        low: 0x1083f,
        high: 0x1083f,
    },
    XmlChLRange {
        low: 0x1d400,
        high: 0x1d454,
    },
    XmlChLRange {
        low: 0x1d456,
        high: 0x1d49c,
    },
    XmlChLRange {
        low: 0x1d49e,
        high: 0x1d49f,
    },
    XmlChLRange {
        low: 0x1d4a2,
        high: 0x1d4a2,
    },
    XmlChLRange {
        low: 0x1d4a5,
        high: 0x1d4a6,
    },
    XmlChLRange {
        low: 0x1d4a9,
        high: 0x1d4ac,
    },
    XmlChLRange {
        low: 0x1d4ae,
        high: 0x1d4b9,
    },
    XmlChLRange {
        low: 0x1d4bb,
        high: 0x1d4bb,
    },
    XmlChLRange {
        low: 0x1d4bd,
        high: 0x1d4c3,
    },
    XmlChLRange {
        low: 0x1d4c5,
        high: 0x1d505,
    },
    XmlChLRange {
        low: 0x1d507,
        high: 0x1d50a,
    },
    XmlChLRange {
        low: 0x1d50d,
        high: 0x1d514,
    },
    XmlChLRange {
        low: 0x1d516,
        high: 0x1d51c,
    },
    XmlChLRange {
        low: 0x1d51e,
        high: 0x1d539,
    },
    XmlChLRange {
        low: 0x1d53b,
        high: 0x1d53e,
    },
    XmlChLRange {
        low: 0x1d540,
        high: 0x1d544,
    },
    XmlChLRange {
        low: 0x1d546,
        high: 0x1d546,
    },
    XmlChLRange {
        low: 0x1d54a,
        high: 0x1d550,
    },
    XmlChLRange {
        low: 0x1d552,
        high: 0x1d6a3,
    },
    XmlChLRange {
        low: 0x1d6a8,
        high: 0x1d6c0,
    },
    XmlChLRange {
        low: 0x1d6c2,
        high: 0x1d6da,
    },
    XmlChLRange {
        low: 0x1d6dc,
        high: 0x1d6fa,
    },
    XmlChLRange {
        low: 0x1d6fc,
        high: 0x1d714,
    },
    XmlChLRange {
        low: 0x1d716,
        high: 0x1d734,
    },
    XmlChLRange {
        low: 0x1d736,
        high: 0x1d74e,
    },
    XmlChLRange {
        low: 0x1d750,
        high: 0x1d76e,
    },
    XmlChLRange {
        low: 0x1d770,
        high: 0x1d788,
    },
    XmlChLRange {
        low: 0x1d78a,
        high: 0x1d7a8,
    },
    XmlChLRange {
        low: 0x1d7aa,
        high: 0x1d7c2,
    },
    XmlChLRange {
        low: 0x1d7c4,
        high: 0x1d7c9,
    },
    XmlChLRange {
        low: 0x20000,
        high: 0x20000,
    },
    XmlChLRange {
        low: 0x2a6d6,
        high: 0x2a6d6,
    },
    XmlChLRange {
        low: 0x2f800,
        high: 0x2fa1d,
    },
];

const XML_LG: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 279,
    nb_long_range: 50,
    short_range: XML_LS,
    long_range: XML_LL,
};

const XML_LL_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x61,
        high: 0x7a,
    },
    XmlChSRange {
        low: 0xaa,
        high: 0xaa,
    },
    XmlChSRange {
        low: 0xb5,
        high: 0xb5,
    },
    XmlChSRange {
        low: 0xba,
        high: 0xba,
    },
    XmlChSRange {
        low: 0xdf,
        high: 0xf6,
    },
    XmlChSRange {
        low: 0xf8,
        high: 0xff,
    },
    XmlChSRange {
        low: 0x101,
        high: 0x101,
    },
    XmlChSRange {
        low: 0x103,
        high: 0x103,
    },
    XmlChSRange {
        low: 0x105,
        high: 0x105,
    },
    XmlChSRange {
        low: 0x107,
        high: 0x107,
    },
    XmlChSRange {
        low: 0x109,
        high: 0x109,
    },
    XmlChSRange {
        low: 0x10b,
        high: 0x10b,
    },
    XmlChSRange {
        low: 0x10d,
        high: 0x10d,
    },
    XmlChSRange {
        low: 0x10f,
        high: 0x10f,
    },
    XmlChSRange {
        low: 0x111,
        high: 0x111,
    },
    XmlChSRange {
        low: 0x113,
        high: 0x113,
    },
    XmlChSRange {
        low: 0x115,
        high: 0x115,
    },
    XmlChSRange {
        low: 0x117,
        high: 0x117,
    },
    XmlChSRange {
        low: 0x119,
        high: 0x119,
    },
    XmlChSRange {
        low: 0x11b,
        high: 0x11b,
    },
    XmlChSRange {
        low: 0x11d,
        high: 0x11d,
    },
    XmlChSRange {
        low: 0x11f,
        high: 0x11f,
    },
    XmlChSRange {
        low: 0x121,
        high: 0x121,
    },
    XmlChSRange {
        low: 0x123,
        high: 0x123,
    },
    XmlChSRange {
        low: 0x125,
        high: 0x125,
    },
    XmlChSRange {
        low: 0x127,
        high: 0x127,
    },
    XmlChSRange {
        low: 0x129,
        high: 0x129,
    },
    XmlChSRange {
        low: 0x12b,
        high: 0x12b,
    },
    XmlChSRange {
        low: 0x12d,
        high: 0x12d,
    },
    XmlChSRange {
        low: 0x12f,
        high: 0x12f,
    },
    XmlChSRange {
        low: 0x131,
        high: 0x131,
    },
    XmlChSRange {
        low: 0x133,
        high: 0x133,
    },
    XmlChSRange {
        low: 0x135,
        high: 0x135,
    },
    XmlChSRange {
        low: 0x137,
        high: 0x138,
    },
    XmlChSRange {
        low: 0x13a,
        high: 0x13a,
    },
    XmlChSRange {
        low: 0x13c,
        high: 0x13c,
    },
    XmlChSRange {
        low: 0x13e,
        high: 0x13e,
    },
    XmlChSRange {
        low: 0x140,
        high: 0x140,
    },
    XmlChSRange {
        low: 0x142,
        high: 0x142,
    },
    XmlChSRange {
        low: 0x144,
        high: 0x144,
    },
    XmlChSRange {
        low: 0x146,
        high: 0x146,
    },
    XmlChSRange {
        low: 0x148,
        high: 0x149,
    },
    XmlChSRange {
        low: 0x14b,
        high: 0x14b,
    },
    XmlChSRange {
        low: 0x14d,
        high: 0x14d,
    },
    XmlChSRange {
        low: 0x14f,
        high: 0x14f,
    },
    XmlChSRange {
        low: 0x151,
        high: 0x151,
    },
    XmlChSRange {
        low: 0x153,
        high: 0x153,
    },
    XmlChSRange {
        low: 0x155,
        high: 0x155,
    },
    XmlChSRange {
        low: 0x157,
        high: 0x157,
    },
    XmlChSRange {
        low: 0x159,
        high: 0x159,
    },
    XmlChSRange {
        low: 0x15b,
        high: 0x15b,
    },
    XmlChSRange {
        low: 0x15d,
        high: 0x15d,
    },
    XmlChSRange {
        low: 0x15f,
        high: 0x15f,
    },
    XmlChSRange {
        low: 0x161,
        high: 0x161,
    },
    XmlChSRange {
        low: 0x163,
        high: 0x163,
    },
    XmlChSRange {
        low: 0x165,
        high: 0x165,
    },
    XmlChSRange {
        low: 0x167,
        high: 0x167,
    },
    XmlChSRange {
        low: 0x169,
        high: 0x169,
    },
    XmlChSRange {
        low: 0x16b,
        high: 0x16b,
    },
    XmlChSRange {
        low: 0x16d,
        high: 0x16d,
    },
    XmlChSRange {
        low: 0x16f,
        high: 0x16f,
    },
    XmlChSRange {
        low: 0x171,
        high: 0x171,
    },
    XmlChSRange {
        low: 0x173,
        high: 0x173,
    },
    XmlChSRange {
        low: 0x175,
        high: 0x175,
    },
    XmlChSRange {
        low: 0x177,
        high: 0x177,
    },
    XmlChSRange {
        low: 0x17a,
        high: 0x17a,
    },
    XmlChSRange {
        low: 0x17c,
        high: 0x17c,
    },
    XmlChSRange {
        low: 0x17e,
        high: 0x180,
    },
    XmlChSRange {
        low: 0x183,
        high: 0x183,
    },
    XmlChSRange {
        low: 0x185,
        high: 0x185,
    },
    XmlChSRange {
        low: 0x188,
        high: 0x188,
    },
    XmlChSRange {
        low: 0x18c,
        high: 0x18d,
    },
    XmlChSRange {
        low: 0x192,
        high: 0x192,
    },
    XmlChSRange {
        low: 0x195,
        high: 0x195,
    },
    XmlChSRange {
        low: 0x199,
        high: 0x19b,
    },
    XmlChSRange {
        low: 0x19e,
        high: 0x19e,
    },
    XmlChSRange {
        low: 0x1a1,
        high: 0x1a1,
    },
    XmlChSRange {
        low: 0x1a3,
        high: 0x1a3,
    },
    XmlChSRange {
        low: 0x1a5,
        high: 0x1a5,
    },
    XmlChSRange {
        low: 0x1a8,
        high: 0x1a8,
    },
    XmlChSRange {
        low: 0x1aa,
        high: 0x1ab,
    },
    XmlChSRange {
        low: 0x1ad,
        high: 0x1ad,
    },
    XmlChSRange {
        low: 0x1b0,
        high: 0x1b0,
    },
    XmlChSRange {
        low: 0x1b4,
        high: 0x1b4,
    },
    XmlChSRange {
        low: 0x1b6,
        high: 0x1b6,
    },
    XmlChSRange {
        low: 0x1b9,
        high: 0x1ba,
    },
    XmlChSRange {
        low: 0x1bd,
        high: 0x1bf,
    },
    XmlChSRange {
        low: 0x1c6,
        high: 0x1c6,
    },
    XmlChSRange {
        low: 0x1c9,
        high: 0x1c9,
    },
    XmlChSRange {
        low: 0x1cc,
        high: 0x1cc,
    },
    XmlChSRange {
        low: 0x1ce,
        high: 0x1ce,
    },
    XmlChSRange {
        low: 0x1d0,
        high: 0x1d0,
    },
    XmlChSRange {
        low: 0x1d2,
        high: 0x1d2,
    },
    XmlChSRange {
        low: 0x1d4,
        high: 0x1d4,
    },
    XmlChSRange {
        low: 0x1d6,
        high: 0x1d6,
    },
    XmlChSRange {
        low: 0x1d8,
        high: 0x1d8,
    },
    XmlChSRange {
        low: 0x1da,
        high: 0x1da,
    },
    XmlChSRange {
        low: 0x1dc,
        high: 0x1dd,
    },
    XmlChSRange {
        low: 0x1df,
        high: 0x1df,
    },
    XmlChSRange {
        low: 0x1e1,
        high: 0x1e1,
    },
    XmlChSRange {
        low: 0x1e3,
        high: 0x1e3,
    },
    XmlChSRange {
        low: 0x1e5,
        high: 0x1e5,
    },
    XmlChSRange {
        low: 0x1e7,
        high: 0x1e7,
    },
    XmlChSRange {
        low: 0x1e9,
        high: 0x1e9,
    },
    XmlChSRange {
        low: 0x1eb,
        high: 0x1eb,
    },
    XmlChSRange {
        low: 0x1ed,
        high: 0x1ed,
    },
    XmlChSRange {
        low: 0x1ef,
        high: 0x1f0,
    },
    XmlChSRange {
        low: 0x1f3,
        high: 0x1f3,
    },
    XmlChSRange {
        low: 0x1f5,
        high: 0x1f5,
    },
    XmlChSRange {
        low: 0x1f9,
        high: 0x1f9,
    },
    XmlChSRange {
        low: 0x1fb,
        high: 0x1fb,
    },
    XmlChSRange {
        low: 0x1fd,
        high: 0x1fd,
    },
    XmlChSRange {
        low: 0x1ff,
        high: 0x1ff,
    },
    XmlChSRange {
        low: 0x201,
        high: 0x201,
    },
    XmlChSRange {
        low: 0x203,
        high: 0x203,
    },
    XmlChSRange {
        low: 0x205,
        high: 0x205,
    },
    XmlChSRange {
        low: 0x207,
        high: 0x207,
    },
    XmlChSRange {
        low: 0x209,
        high: 0x209,
    },
    XmlChSRange {
        low: 0x20b,
        high: 0x20b,
    },
    XmlChSRange {
        low: 0x20d,
        high: 0x20d,
    },
    XmlChSRange {
        low: 0x20f,
        high: 0x20f,
    },
    XmlChSRange {
        low: 0x211,
        high: 0x211,
    },
    XmlChSRange {
        low: 0x213,
        high: 0x213,
    },
    XmlChSRange {
        low: 0x215,
        high: 0x215,
    },
    XmlChSRange {
        low: 0x217,
        high: 0x217,
    },
    XmlChSRange {
        low: 0x219,
        high: 0x219,
    },
    XmlChSRange {
        low: 0x21b,
        high: 0x21b,
    },
    XmlChSRange {
        low: 0x21d,
        high: 0x21d,
    },
    XmlChSRange {
        low: 0x21f,
        high: 0x21f,
    },
    XmlChSRange {
        low: 0x221,
        high: 0x221,
    },
    XmlChSRange {
        low: 0x223,
        high: 0x223,
    },
    XmlChSRange {
        low: 0x225,
        high: 0x225,
    },
    XmlChSRange {
        low: 0x227,
        high: 0x227,
    },
    XmlChSRange {
        low: 0x229,
        high: 0x229,
    },
    XmlChSRange {
        low: 0x22b,
        high: 0x22b,
    },
    XmlChSRange {
        low: 0x22d,
        high: 0x22d,
    },
    XmlChSRange {
        low: 0x22f,
        high: 0x22f,
    },
    XmlChSRange {
        low: 0x231,
        high: 0x231,
    },
    XmlChSRange {
        low: 0x233,
        high: 0x236,
    },
    XmlChSRange {
        low: 0x250,
        high: 0x2af,
    },
    XmlChSRange {
        low: 0x390,
        high: 0x390,
    },
    XmlChSRange {
        low: 0x3ac,
        high: 0x3ce,
    },
    XmlChSRange {
        low: 0x3d0,
        high: 0x3d1,
    },
    XmlChSRange {
        low: 0x3d5,
        high: 0x3d7,
    },
    XmlChSRange {
        low: 0x3d9,
        high: 0x3d9,
    },
    XmlChSRange {
        low: 0x3db,
        high: 0x3db,
    },
    XmlChSRange {
        low: 0x3dd,
        high: 0x3dd,
    },
    XmlChSRange {
        low: 0x3df,
        high: 0x3df,
    },
    XmlChSRange {
        low: 0x3e1,
        high: 0x3e1,
    },
    XmlChSRange {
        low: 0x3e3,
        high: 0x3e3,
    },
    XmlChSRange {
        low: 0x3e5,
        high: 0x3e5,
    },
    XmlChSRange {
        low: 0x3e7,
        high: 0x3e7,
    },
    XmlChSRange {
        low: 0x3e9,
        high: 0x3e9,
    },
    XmlChSRange {
        low: 0x3eb,
        high: 0x3eb,
    },
    XmlChSRange {
        low: 0x3ed,
        high: 0x3ed,
    },
    XmlChSRange {
        low: 0x3ef,
        high: 0x3f3,
    },
    XmlChSRange {
        low: 0x3f5,
        high: 0x3f5,
    },
    XmlChSRange {
        low: 0x3f8,
        high: 0x3f8,
    },
    XmlChSRange {
        low: 0x3fb,
        high: 0x3fb,
    },
    XmlChSRange {
        low: 0x430,
        high: 0x45f,
    },
    XmlChSRange {
        low: 0x461,
        high: 0x461,
    },
    XmlChSRange {
        low: 0x463,
        high: 0x463,
    },
    XmlChSRange {
        low: 0x465,
        high: 0x465,
    },
    XmlChSRange {
        low: 0x467,
        high: 0x467,
    },
    XmlChSRange {
        low: 0x469,
        high: 0x469,
    },
    XmlChSRange {
        low: 0x46b,
        high: 0x46b,
    },
    XmlChSRange {
        low: 0x46d,
        high: 0x46d,
    },
    XmlChSRange {
        low: 0x46f,
        high: 0x46f,
    },
    XmlChSRange {
        low: 0x471,
        high: 0x471,
    },
    XmlChSRange {
        low: 0x473,
        high: 0x473,
    },
    XmlChSRange {
        low: 0x475,
        high: 0x475,
    },
    XmlChSRange {
        low: 0x477,
        high: 0x477,
    },
    XmlChSRange {
        low: 0x479,
        high: 0x479,
    },
    XmlChSRange {
        low: 0x47b,
        high: 0x47b,
    },
    XmlChSRange {
        low: 0x47d,
        high: 0x47d,
    },
    XmlChSRange {
        low: 0x47f,
        high: 0x47f,
    },
    XmlChSRange {
        low: 0x481,
        high: 0x481,
    },
    XmlChSRange {
        low: 0x48b,
        high: 0x48b,
    },
    XmlChSRange {
        low: 0x48d,
        high: 0x48d,
    },
    XmlChSRange {
        low: 0x48f,
        high: 0x48f,
    },
    XmlChSRange {
        low: 0x491,
        high: 0x491,
    },
    XmlChSRange {
        low: 0x493,
        high: 0x493,
    },
    XmlChSRange {
        low: 0x495,
        high: 0x495,
    },
    XmlChSRange {
        low: 0x497,
        high: 0x497,
    },
    XmlChSRange {
        low: 0x499,
        high: 0x499,
    },
    XmlChSRange {
        low: 0x49b,
        high: 0x49b,
    },
    XmlChSRange {
        low: 0x49d,
        high: 0x49d,
    },
    XmlChSRange {
        low: 0x49f,
        high: 0x49f,
    },
    XmlChSRange {
        low: 0x4a1,
        high: 0x4a1,
    },
    XmlChSRange {
        low: 0x4a3,
        high: 0x4a3,
    },
    XmlChSRange {
        low: 0x4a5,
        high: 0x4a5,
    },
    XmlChSRange {
        low: 0x4a7,
        high: 0x4a7,
    },
    XmlChSRange {
        low: 0x4a9,
        high: 0x4a9,
    },
    XmlChSRange {
        low: 0x4ab,
        high: 0x4ab,
    },
    XmlChSRange {
        low: 0x4ad,
        high: 0x4ad,
    },
    XmlChSRange {
        low: 0x4af,
        high: 0x4af,
    },
    XmlChSRange {
        low: 0x4b1,
        high: 0x4b1,
    },
    XmlChSRange {
        low: 0x4b3,
        high: 0x4b3,
    },
    XmlChSRange {
        low: 0x4b5,
        high: 0x4b5,
    },
    XmlChSRange {
        low: 0x4b7,
        high: 0x4b7,
    },
    XmlChSRange {
        low: 0x4b9,
        high: 0x4b9,
    },
    XmlChSRange {
        low: 0x4bb,
        high: 0x4bb,
    },
    XmlChSRange {
        low: 0x4bd,
        high: 0x4bd,
    },
    XmlChSRange {
        low: 0x4bf,
        high: 0x4bf,
    },
    XmlChSRange {
        low: 0x4c2,
        high: 0x4c2,
    },
    XmlChSRange {
        low: 0x4c4,
        high: 0x4c4,
    },
    XmlChSRange {
        low: 0x4c6,
        high: 0x4c6,
    },
    XmlChSRange {
        low: 0x4c8,
        high: 0x4c8,
    },
    XmlChSRange {
        low: 0x4ca,
        high: 0x4ca,
    },
    XmlChSRange {
        low: 0x4cc,
        high: 0x4cc,
    },
    XmlChSRange {
        low: 0x4ce,
        high: 0x4ce,
    },
    XmlChSRange {
        low: 0x4d1,
        high: 0x4d1,
    },
    XmlChSRange {
        low: 0x4d3,
        high: 0x4d3,
    },
    XmlChSRange {
        low: 0x4d5,
        high: 0x4d5,
    },
    XmlChSRange {
        low: 0x4d7,
        high: 0x4d7,
    },
    XmlChSRange {
        low: 0x4d9,
        high: 0x4d9,
    },
    XmlChSRange {
        low: 0x4db,
        high: 0x4db,
    },
    XmlChSRange {
        low: 0x4dd,
        high: 0x4dd,
    },
    XmlChSRange {
        low: 0x4df,
        high: 0x4df,
    },
    XmlChSRange {
        low: 0x4e1,
        high: 0x4e1,
    },
    XmlChSRange {
        low: 0x4e3,
        high: 0x4e3,
    },
    XmlChSRange {
        low: 0x4e5,
        high: 0x4e5,
    },
    XmlChSRange {
        low: 0x4e7,
        high: 0x4e7,
    },
    XmlChSRange {
        low: 0x4e9,
        high: 0x4e9,
    },
    XmlChSRange {
        low: 0x4eb,
        high: 0x4eb,
    },
    XmlChSRange {
        low: 0x4ed,
        high: 0x4ed,
    },
    XmlChSRange {
        low: 0x4ef,
        high: 0x4ef,
    },
    XmlChSRange {
        low: 0x4f1,
        high: 0x4f1,
    },
    XmlChSRange {
        low: 0x4f3,
        high: 0x4f3,
    },
    XmlChSRange {
        low: 0x4f5,
        high: 0x4f5,
    },
    XmlChSRange {
        low: 0x4f9,
        high: 0x4f9,
    },
    XmlChSRange {
        low: 0x501,
        high: 0x501,
    },
    XmlChSRange {
        low: 0x503,
        high: 0x503,
    },
    XmlChSRange {
        low: 0x505,
        high: 0x505,
    },
    XmlChSRange {
        low: 0x507,
        high: 0x507,
    },
    XmlChSRange {
        low: 0x509,
        high: 0x509,
    },
    XmlChSRange {
        low: 0x50b,
        high: 0x50b,
    },
    XmlChSRange {
        low: 0x50d,
        high: 0x50d,
    },
    XmlChSRange {
        low: 0x50f,
        high: 0x50f,
    },
    XmlChSRange {
        low: 0x561,
        high: 0x587,
    },
    XmlChSRange {
        low: 0x1d00,
        high: 0x1d2b,
    },
    XmlChSRange {
        low: 0x1d62,
        high: 0x1d6b,
    },
    XmlChSRange {
        low: 0x1e01,
        high: 0x1e01,
    },
    XmlChSRange {
        low: 0x1e03,
        high: 0x1e03,
    },
    XmlChSRange {
        low: 0x1e05,
        high: 0x1e05,
    },
    XmlChSRange {
        low: 0x1e07,
        high: 0x1e07,
    },
    XmlChSRange {
        low: 0x1e09,
        high: 0x1e09,
    },
    XmlChSRange {
        low: 0x1e0b,
        high: 0x1e0b,
    },
    XmlChSRange {
        low: 0x1e0d,
        high: 0x1e0d,
    },
    XmlChSRange {
        low: 0x1e0f,
        high: 0x1e0f,
    },
    XmlChSRange {
        low: 0x1e11,
        high: 0x1e11,
    },
    XmlChSRange {
        low: 0x1e13,
        high: 0x1e13,
    },
    XmlChSRange {
        low: 0x1e15,
        high: 0x1e15,
    },
    XmlChSRange {
        low: 0x1e17,
        high: 0x1e17,
    },
    XmlChSRange {
        low: 0x1e19,
        high: 0x1e19,
    },
    XmlChSRange {
        low: 0x1e1b,
        high: 0x1e1b,
    },
    XmlChSRange {
        low: 0x1e1d,
        high: 0x1e1d,
    },
    XmlChSRange {
        low: 0x1e1f,
        high: 0x1e1f,
    },
    XmlChSRange {
        low: 0x1e21,
        high: 0x1e21,
    },
    XmlChSRange {
        low: 0x1e23,
        high: 0x1e23,
    },
    XmlChSRange {
        low: 0x1e25,
        high: 0x1e25,
    },
    XmlChSRange {
        low: 0x1e27,
        high: 0x1e27,
    },
    XmlChSRange {
        low: 0x1e29,
        high: 0x1e29,
    },
    XmlChSRange {
        low: 0x1e2b,
        high: 0x1e2b,
    },
    XmlChSRange {
        low: 0x1e2d,
        high: 0x1e2d,
    },
    XmlChSRange {
        low: 0x1e2f,
        high: 0x1e2f,
    },
    XmlChSRange {
        low: 0x1e31,
        high: 0x1e31,
    },
    XmlChSRange {
        low: 0x1e33,
        high: 0x1e33,
    },
    XmlChSRange {
        low: 0x1e35,
        high: 0x1e35,
    },
    XmlChSRange {
        low: 0x1e37,
        high: 0x1e37,
    },
    XmlChSRange {
        low: 0x1e39,
        high: 0x1e39,
    },
    XmlChSRange {
        low: 0x1e3b,
        high: 0x1e3b,
    },
    XmlChSRange {
        low: 0x1e3d,
        high: 0x1e3d,
    },
    XmlChSRange {
        low: 0x1e3f,
        high: 0x1e3f,
    },
    XmlChSRange {
        low: 0x1e41,
        high: 0x1e41,
    },
    XmlChSRange {
        low: 0x1e43,
        high: 0x1e43,
    },
    XmlChSRange {
        low: 0x1e45,
        high: 0x1e45,
    },
    XmlChSRange {
        low: 0x1e47,
        high: 0x1e47,
    },
    XmlChSRange {
        low: 0x1e49,
        high: 0x1e49,
    },
    XmlChSRange {
        low: 0x1e4b,
        high: 0x1e4b,
    },
    XmlChSRange {
        low: 0x1e4d,
        high: 0x1e4d,
    },
    XmlChSRange {
        low: 0x1e4f,
        high: 0x1e4f,
    },
    XmlChSRange {
        low: 0x1e51,
        high: 0x1e51,
    },
    XmlChSRange {
        low: 0x1e53,
        high: 0x1e53,
    },
    XmlChSRange {
        low: 0x1e55,
        high: 0x1e55,
    },
    XmlChSRange {
        low: 0x1e57,
        high: 0x1e57,
    },
    XmlChSRange {
        low: 0x1e59,
        high: 0x1e59,
    },
    XmlChSRange {
        low: 0x1e5b,
        high: 0x1e5b,
    },
    XmlChSRange {
        low: 0x1e5d,
        high: 0x1e5d,
    },
    XmlChSRange {
        low: 0x1e5f,
        high: 0x1e5f,
    },
    XmlChSRange {
        low: 0x1e61,
        high: 0x1e61,
    },
    XmlChSRange {
        low: 0x1e63,
        high: 0x1e63,
    },
    XmlChSRange {
        low: 0x1e65,
        high: 0x1e65,
    },
    XmlChSRange {
        low: 0x1e67,
        high: 0x1e67,
    },
    XmlChSRange {
        low: 0x1e69,
        high: 0x1e69,
    },
    XmlChSRange {
        low: 0x1e6b,
        high: 0x1e6b,
    },
    XmlChSRange {
        low: 0x1e6d,
        high: 0x1e6d,
    },
    XmlChSRange {
        low: 0x1e6f,
        high: 0x1e6f,
    },
    XmlChSRange {
        low: 0x1e71,
        high: 0x1e71,
    },
    XmlChSRange {
        low: 0x1e73,
        high: 0x1e73,
    },
    XmlChSRange {
        low: 0x1e75,
        high: 0x1e75,
    },
    XmlChSRange {
        low: 0x1e77,
        high: 0x1e77,
    },
    XmlChSRange {
        low: 0x1e79,
        high: 0x1e79,
    },
    XmlChSRange {
        low: 0x1e7b,
        high: 0x1e7b,
    },
    XmlChSRange {
        low: 0x1e7d,
        high: 0x1e7d,
    },
    XmlChSRange {
        low: 0x1e7f,
        high: 0x1e7f,
    },
    XmlChSRange {
        low: 0x1e81,
        high: 0x1e81,
    },
    XmlChSRange {
        low: 0x1e83,
        high: 0x1e83,
    },
    XmlChSRange {
        low: 0x1e85,
        high: 0x1e85,
    },
    XmlChSRange {
        low: 0x1e87,
        high: 0x1e87,
    },
    XmlChSRange {
        low: 0x1e89,
        high: 0x1e89,
    },
    XmlChSRange {
        low: 0x1e8b,
        high: 0x1e8b,
    },
    XmlChSRange {
        low: 0x1e8d,
        high: 0x1e8d,
    },
    XmlChSRange {
        low: 0x1e8f,
        high: 0x1e8f,
    },
    XmlChSRange {
        low: 0x1e91,
        high: 0x1e91,
    },
    XmlChSRange {
        low: 0x1e93,
        high: 0x1e93,
    },
    XmlChSRange {
        low: 0x1e95,
        high: 0x1e9b,
    },
    XmlChSRange {
        low: 0x1ea1,
        high: 0x1ea1,
    },
    XmlChSRange {
        low: 0x1ea3,
        high: 0x1ea3,
    },
    XmlChSRange {
        low: 0x1ea5,
        high: 0x1ea5,
    },
    XmlChSRange {
        low: 0x1ea7,
        high: 0x1ea7,
    },
    XmlChSRange {
        low: 0x1ea9,
        high: 0x1ea9,
    },
    XmlChSRange {
        low: 0x1eab,
        high: 0x1eab,
    },
    XmlChSRange {
        low: 0x1ead,
        high: 0x1ead,
    },
    XmlChSRange {
        low: 0x1eaf,
        high: 0x1eaf,
    },
    XmlChSRange {
        low: 0x1eb1,
        high: 0x1eb1,
    },
    XmlChSRange {
        low: 0x1eb3,
        high: 0x1eb3,
    },
    XmlChSRange {
        low: 0x1eb5,
        high: 0x1eb5,
    },
    XmlChSRange {
        low: 0x1eb7,
        high: 0x1eb7,
    },
    XmlChSRange {
        low: 0x1eb9,
        high: 0x1eb9,
    },
    XmlChSRange {
        low: 0x1ebb,
        high: 0x1ebb,
    },
    XmlChSRange {
        low: 0x1ebd,
        high: 0x1ebd,
    },
    XmlChSRange {
        low: 0x1ebf,
        high: 0x1ebf,
    },
    XmlChSRange {
        low: 0x1ec1,
        high: 0x1ec1,
    },
    XmlChSRange {
        low: 0x1ec3,
        high: 0x1ec3,
    },
    XmlChSRange {
        low: 0x1ec5,
        high: 0x1ec5,
    },
    XmlChSRange {
        low: 0x1ec7,
        high: 0x1ec7,
    },
    XmlChSRange {
        low: 0x1ec9,
        high: 0x1ec9,
    },
    XmlChSRange {
        low: 0x1ecb,
        high: 0x1ecb,
    },
    XmlChSRange {
        low: 0x1ecd,
        high: 0x1ecd,
    },
    XmlChSRange {
        low: 0x1ecf,
        high: 0x1ecf,
    },
    XmlChSRange {
        low: 0x1ed1,
        high: 0x1ed1,
    },
    XmlChSRange {
        low: 0x1ed3,
        high: 0x1ed3,
    },
    XmlChSRange {
        low: 0x1ed5,
        high: 0x1ed5,
    },
    XmlChSRange {
        low: 0x1ed7,
        high: 0x1ed7,
    },
    XmlChSRange {
        low: 0x1ed9,
        high: 0x1ed9,
    },
    XmlChSRange {
        low: 0x1edb,
        high: 0x1edb,
    },
    XmlChSRange {
        low: 0x1edd,
        high: 0x1edd,
    },
    XmlChSRange {
        low: 0x1edf,
        high: 0x1edf,
    },
    XmlChSRange {
        low: 0x1ee1,
        high: 0x1ee1,
    },
    XmlChSRange {
        low: 0x1ee3,
        high: 0x1ee3,
    },
    XmlChSRange {
        low: 0x1ee5,
        high: 0x1ee5,
    },
    XmlChSRange {
        low: 0x1ee7,
        high: 0x1ee7,
    },
    XmlChSRange {
        low: 0x1ee9,
        high: 0x1ee9,
    },
    XmlChSRange {
        low: 0x1eeb,
        high: 0x1eeb,
    },
    XmlChSRange {
        low: 0x1eed,
        high: 0x1eed,
    },
    XmlChSRange {
        low: 0x1eef,
        high: 0x1eef,
    },
    XmlChSRange {
        low: 0x1ef1,
        high: 0x1ef1,
    },
    XmlChSRange {
        low: 0x1ef3,
        high: 0x1ef3,
    },
    XmlChSRange {
        low: 0x1ef5,
        high: 0x1ef5,
    },
    XmlChSRange {
        low: 0x1ef7,
        high: 0x1ef7,
    },
    XmlChSRange {
        low: 0x1ef9,
        high: 0x1ef9,
    },
    XmlChSRange {
        low: 0x1f00,
        high: 0x1f07,
    },
    XmlChSRange {
        low: 0x1f10,
        high: 0x1f15,
    },
    XmlChSRange {
        low: 0x1f20,
        high: 0x1f27,
    },
    XmlChSRange {
        low: 0x1f30,
        high: 0x1f37,
    },
    XmlChSRange {
        low: 0x1f40,
        high: 0x1f45,
    },
    XmlChSRange {
        low: 0x1f50,
        high: 0x1f57,
    },
    XmlChSRange {
        low: 0x1f60,
        high: 0x1f67,
    },
    XmlChSRange {
        low: 0x1f70,
        high: 0x1f7d,
    },
    XmlChSRange {
        low: 0x1f80,
        high: 0x1f87,
    },
    XmlChSRange {
        low: 0x1f90,
        high: 0x1f97,
    },
    XmlChSRange {
        low: 0x1fa0,
        high: 0x1fa7,
    },
    XmlChSRange {
        low: 0x1fb0,
        high: 0x1fb4,
    },
    XmlChSRange {
        low: 0x1fb6,
        high: 0x1fb7,
    },
    XmlChSRange {
        low: 0x1fbe,
        high: 0x1fbe,
    },
    XmlChSRange {
        low: 0x1fc2,
        high: 0x1fc4,
    },
    XmlChSRange {
        low: 0x1fc6,
        high: 0x1fc7,
    },
    XmlChSRange {
        low: 0x1fd0,
        high: 0x1fd3,
    },
    XmlChSRange {
        low: 0x1fd6,
        high: 0x1fd7,
    },
    XmlChSRange {
        low: 0x1fe0,
        high: 0x1fe7,
    },
    XmlChSRange {
        low: 0x1ff2,
        high: 0x1ff4,
    },
    XmlChSRange {
        low: 0x1ff6,
        high: 0x1ff7,
    },
    XmlChSRange {
        low: 0x2071,
        high: 0x2071,
    },
    XmlChSRange {
        low: 0x207f,
        high: 0x207f,
    },
    XmlChSRange {
        low: 0x210a,
        high: 0x210a,
    },
    XmlChSRange {
        low: 0x210e,
        high: 0x210f,
    },
    XmlChSRange {
        low: 0x2113,
        high: 0x2113,
    },
    XmlChSRange {
        low: 0x212f,
        high: 0x212f,
    },
    XmlChSRange {
        low: 0x2134,
        high: 0x2134,
    },
    XmlChSRange {
        low: 0x2139,
        high: 0x2139,
    },
    XmlChSRange {
        low: 0x213d,
        high: 0x213d,
    },
    XmlChSRange {
        low: 0x2146,
        high: 0x2149,
    },
    XmlChSRange {
        low: 0xfb00,
        high: 0xfb06,
    },
    XmlChSRange {
        low: 0xfb13,
        high: 0xfb17,
    },
    XmlChSRange {
        low: 0xff41,
        high: 0xff5a,
    },
];

const XML_LL_L: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x10428,
        high: 0x1044f,
    },
    XmlChLRange {
        low: 0x1d41a,
        high: 0x1d433,
    },
    XmlChLRange {
        low: 0x1d44e,
        high: 0x1d454,
    },
    XmlChLRange {
        low: 0x1d456,
        high: 0x1d467,
    },
    XmlChLRange {
        low: 0x1d482,
        high: 0x1d49b,
    },
    XmlChLRange {
        low: 0x1d4b6,
        high: 0x1d4b9,
    },
    XmlChLRange {
        low: 0x1d4bb,
        high: 0x1d4bb,
    },
    XmlChLRange {
        low: 0x1d4bd,
        high: 0x1d4c3,
    },
    XmlChLRange {
        low: 0x1d4c5,
        high: 0x1d4cf,
    },
    XmlChLRange {
        low: 0x1d4ea,
        high: 0x1d503,
    },
    XmlChLRange {
        low: 0x1d51e,
        high: 0x1d537,
    },
    XmlChLRange {
        low: 0x1d552,
        high: 0x1d56b,
    },
    XmlChLRange {
        low: 0x1d586,
        high: 0x1d59f,
    },
    XmlChLRange {
        low: 0x1d5ba,
        high: 0x1d5d3,
    },
    XmlChLRange {
        low: 0x1d5ee,
        high: 0x1d607,
    },
    XmlChLRange {
        low: 0x1d622,
        high: 0x1d63b,
    },
    XmlChLRange {
        low: 0x1d656,
        high: 0x1d66f,
    },
    XmlChLRange {
        low: 0x1d68a,
        high: 0x1d6a3,
    },
    XmlChLRange {
        low: 0x1d6c2,
        high: 0x1d6da,
    },
    XmlChLRange {
        low: 0x1d6dc,
        high: 0x1d6e1,
    },
    XmlChLRange {
        low: 0x1d6fc,
        high: 0x1d714,
    },
    XmlChLRange {
        low: 0x1d716,
        high: 0x1d71b,
    },
    XmlChLRange {
        low: 0x1d736,
        high: 0x1d74e,
    },
    XmlChLRange {
        low: 0x1d750,
        high: 0x1d755,
    },
    XmlChLRange {
        low: 0x1d770,
        high: 0x1d788,
    },
    XmlChLRange {
        low: 0x1d78a,
        high: 0x1d78f,
    },
    XmlChLRange {
        low: 0x1d7aa,
        high: 0x1d7c2,
    },
    XmlChLRange {
        low: 0x1d7c4,
        high: 0x1d7c9,
    },
];

const XML_LL_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 396,
    nb_long_range: 28,
    short_range: XML_LL_S,
    long_range: XML_LL_L,
};

const XML_LM_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x2b0,
        high: 0x2c1,
    },
    XmlChSRange {
        low: 0x2c6,
        high: 0x2d1,
    },
    XmlChSRange {
        low: 0x2e0,
        high: 0x2e4,
    },
    XmlChSRange {
        low: 0x2ee,
        high: 0x2ee,
    },
    XmlChSRange {
        low: 0x37a,
        high: 0x37a,
    },
    XmlChSRange {
        low: 0x559,
        high: 0x559,
    },
    XmlChSRange {
        low: 0x640,
        high: 0x640,
    },
    XmlChSRange {
        low: 0x6e5,
        high: 0x6e6,
    },
    XmlChSRange {
        low: 0xe46,
        high: 0xe46,
    },
    XmlChSRange {
        low: 0xec6,
        high: 0xec6,
    },
    XmlChSRange {
        low: 0x17d7,
        high: 0x17d7,
    },
    XmlChSRange {
        low: 0x1843,
        high: 0x1843,
    },
    XmlChSRange {
        low: 0x1d2c,
        high: 0x1d61,
    },
    XmlChSRange {
        low: 0x3005,
        high: 0x3005,
    },
    XmlChSRange {
        low: 0x3031,
        high: 0x3035,
    },
    XmlChSRange {
        low: 0x303b,
        high: 0x303b,
    },
    XmlChSRange {
        low: 0x309d,
        high: 0x309e,
    },
    XmlChSRange {
        low: 0x30fc,
        high: 0x30fe,
    },
    XmlChSRange {
        low: 0xff70,
        high: 0xff70,
    },
    XmlChSRange {
        low: 0xff9e,
        high: 0xff9f,
    },
];

const XML_LM_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 20,
    nb_long_range: 0,
    short_range: XML_LM_S,
    long_range: &[],
};

const XML_LO_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x1bb,
        high: 0x1bb,
    },
    XmlChSRange {
        low: 0x1c0,
        high: 0x1c3,
    },
    XmlChSRange {
        low: 0x5d0,
        high: 0x5ea,
    },
    XmlChSRange {
        low: 0x5f0,
        high: 0x5f2,
    },
    XmlChSRange {
        low: 0x621,
        high: 0x63a,
    },
    XmlChSRange {
        low: 0x641,
        high: 0x64a,
    },
    XmlChSRange {
        low: 0x66e,
        high: 0x66f,
    },
    XmlChSRange {
        low: 0x671,
        high: 0x6d3,
    },
    XmlChSRange {
        low: 0x6d5,
        high: 0x6d5,
    },
    XmlChSRange {
        low: 0x6ee,
        high: 0x6ef,
    },
    XmlChSRange {
        low: 0x6fa,
        high: 0x6fc,
    },
    XmlChSRange {
        low: 0x6ff,
        high: 0x6ff,
    },
    XmlChSRange {
        low: 0x710,
        high: 0x710,
    },
    XmlChSRange {
        low: 0x712,
        high: 0x72f,
    },
    XmlChSRange {
        low: 0x74d,
        high: 0x74f,
    },
    XmlChSRange {
        low: 0x780,
        high: 0x7a5,
    },
    XmlChSRange {
        low: 0x7b1,
        high: 0x7b1,
    },
    XmlChSRange {
        low: 0x904,
        high: 0x939,
    },
    XmlChSRange {
        low: 0x93d,
        high: 0x93d,
    },
    XmlChSRange {
        low: 0x950,
        high: 0x950,
    },
    XmlChSRange {
        low: 0x958,
        high: 0x961,
    },
    XmlChSRange {
        low: 0x985,
        high: 0x98c,
    },
    XmlChSRange {
        low: 0x98f,
        high: 0x990,
    },
    XmlChSRange {
        low: 0x993,
        high: 0x9a8,
    },
    XmlChSRange {
        low: 0x9aa,
        high: 0x9b0,
    },
    XmlChSRange {
        low: 0x9b2,
        high: 0x9b2,
    },
    XmlChSRange {
        low: 0x9b6,
        high: 0x9b9,
    },
    XmlChSRange {
        low: 0x9bd,
        high: 0x9bd,
    },
    XmlChSRange {
        low: 0x9dc,
        high: 0x9dd,
    },
    XmlChSRange {
        low: 0x9df,
        high: 0x9e1,
    },
    XmlChSRange {
        low: 0x9f0,
        high: 0x9f1,
    },
    XmlChSRange {
        low: 0xa05,
        high: 0xa0a,
    },
    XmlChSRange {
        low: 0xa0f,
        high: 0xa10,
    },
    XmlChSRange {
        low: 0xa13,
        high: 0xa28,
    },
    XmlChSRange {
        low: 0xa2a,
        high: 0xa30,
    },
    XmlChSRange {
        low: 0xa32,
        high: 0xa33,
    },
    XmlChSRange {
        low: 0xa35,
        high: 0xa36,
    },
    XmlChSRange {
        low: 0xa38,
        high: 0xa39,
    },
    XmlChSRange {
        low: 0xa59,
        high: 0xa5c,
    },
    XmlChSRange {
        low: 0xa5e,
        high: 0xa5e,
    },
    XmlChSRange {
        low: 0xa72,
        high: 0xa74,
    },
    XmlChSRange {
        low: 0xa85,
        high: 0xa8d,
    },
    XmlChSRange {
        low: 0xa8f,
        high: 0xa91,
    },
    XmlChSRange {
        low: 0xa93,
        high: 0xaa8,
    },
    XmlChSRange {
        low: 0xaaa,
        high: 0xab0,
    },
    XmlChSRange {
        low: 0xab2,
        high: 0xab3,
    },
    XmlChSRange {
        low: 0xab5,
        high: 0xab9,
    },
    XmlChSRange {
        low: 0xabd,
        high: 0xabd,
    },
    XmlChSRange {
        low: 0xad0,
        high: 0xad0,
    },
    XmlChSRange {
        low: 0xae0,
        high: 0xae1,
    },
    XmlChSRange {
        low: 0xb05,
        high: 0xb0c,
    },
    XmlChSRange {
        low: 0xb0f,
        high: 0xb10,
    },
    XmlChSRange {
        low: 0xb13,
        high: 0xb28,
    },
    XmlChSRange {
        low: 0xb2a,
        high: 0xb30,
    },
    XmlChSRange {
        low: 0xb32,
        high: 0xb33,
    },
    XmlChSRange {
        low: 0xb35,
        high: 0xb39,
    },
    XmlChSRange {
        low: 0xb3d,
        high: 0xb3d,
    },
    XmlChSRange {
        low: 0xb5c,
        high: 0xb5d,
    },
    XmlChSRange {
        low: 0xb5f,
        high: 0xb61,
    },
    XmlChSRange {
        low: 0xb71,
        high: 0xb71,
    },
    XmlChSRange {
        low: 0xb83,
        high: 0xb83,
    },
    XmlChSRange {
        low: 0xb85,
        high: 0xb8a,
    },
    XmlChSRange {
        low: 0xb8e,
        high: 0xb90,
    },
    XmlChSRange {
        low: 0xb92,
        high: 0xb95,
    },
    XmlChSRange {
        low: 0xb99,
        high: 0xb9a,
    },
    XmlChSRange {
        low: 0xb9c,
        high: 0xb9c,
    },
    XmlChSRange {
        low: 0xb9e,
        high: 0xb9f,
    },
    XmlChSRange {
        low: 0xba3,
        high: 0xba4,
    },
    XmlChSRange {
        low: 0xba8,
        high: 0xbaa,
    },
    XmlChSRange {
        low: 0xbae,
        high: 0xbb5,
    },
    XmlChSRange {
        low: 0xbb7,
        high: 0xbb9,
    },
    XmlChSRange {
        low: 0xc05,
        high: 0xc0c,
    },
    XmlChSRange {
        low: 0xc0e,
        high: 0xc10,
    },
    XmlChSRange {
        low: 0xc12,
        high: 0xc28,
    },
    XmlChSRange {
        low: 0xc2a,
        high: 0xc33,
    },
    XmlChSRange {
        low: 0xc35,
        high: 0xc39,
    },
    XmlChSRange {
        low: 0xc60,
        high: 0xc61,
    },
    XmlChSRange {
        low: 0xc85,
        high: 0xc8c,
    },
    XmlChSRange {
        low: 0xc8e,
        high: 0xc90,
    },
    XmlChSRange {
        low: 0xc92,
        high: 0xca8,
    },
    XmlChSRange {
        low: 0xcaa,
        high: 0xcb3,
    },
    XmlChSRange {
        low: 0xcb5,
        high: 0xcb9,
    },
    XmlChSRange {
        low: 0xcbd,
        high: 0xcbd,
    },
    XmlChSRange {
        low: 0xcde,
        high: 0xcde,
    },
    XmlChSRange {
        low: 0xce0,
        high: 0xce1,
    },
    XmlChSRange {
        low: 0xd05,
        high: 0xd0c,
    },
    XmlChSRange {
        low: 0xd0e,
        high: 0xd10,
    },
    XmlChSRange {
        low: 0xd12,
        high: 0xd28,
    },
    XmlChSRange {
        low: 0xd2a,
        high: 0xd39,
    },
    XmlChSRange {
        low: 0xd60,
        high: 0xd61,
    },
    XmlChSRange {
        low: 0xd85,
        high: 0xd96,
    },
    XmlChSRange {
        low: 0xd9a,
        high: 0xdb1,
    },
    XmlChSRange {
        low: 0xdb3,
        high: 0xdbb,
    },
    XmlChSRange {
        low: 0xdbd,
        high: 0xdbd,
    },
    XmlChSRange {
        low: 0xdc0,
        high: 0xdc6,
    },
    XmlChSRange {
        low: 0xe01,
        high: 0xe30,
    },
    XmlChSRange {
        low: 0xe32,
        high: 0xe33,
    },
    XmlChSRange {
        low: 0xe40,
        high: 0xe45,
    },
    XmlChSRange {
        low: 0xe81,
        high: 0xe82,
    },
    XmlChSRange {
        low: 0xe84,
        high: 0xe84,
    },
    XmlChSRange {
        low: 0xe87,
        high: 0xe88,
    },
    XmlChSRange {
        low: 0xe8a,
        high: 0xe8a,
    },
    XmlChSRange {
        low: 0xe8d,
        high: 0xe8d,
    },
    XmlChSRange {
        low: 0xe94,
        high: 0xe97,
    },
    XmlChSRange {
        low: 0xe99,
        high: 0xe9f,
    },
    XmlChSRange {
        low: 0xea1,
        high: 0xea3,
    },
    XmlChSRange {
        low: 0xea5,
        high: 0xea5,
    },
    XmlChSRange {
        low: 0xea7,
        high: 0xea7,
    },
    XmlChSRange {
        low: 0xeaa,
        high: 0xeab,
    },
    XmlChSRange {
        low: 0xead,
        high: 0xeb0,
    },
    XmlChSRange {
        low: 0xeb2,
        high: 0xeb3,
    },
    XmlChSRange {
        low: 0xebd,
        high: 0xebd,
    },
    XmlChSRange {
        low: 0xec0,
        high: 0xec4,
    },
    XmlChSRange {
        low: 0xedc,
        high: 0xedd,
    },
    XmlChSRange {
        low: 0xf00,
        high: 0xf00,
    },
    XmlChSRange {
        low: 0xf40,
        high: 0xf47,
    },
    XmlChSRange {
        low: 0xf49,
        high: 0xf6a,
    },
    XmlChSRange {
        low: 0xf88,
        high: 0xf8b,
    },
    XmlChSRange {
        low: 0x1000,
        high: 0x1021,
    },
    XmlChSRange {
        low: 0x1023,
        high: 0x1027,
    },
    XmlChSRange {
        low: 0x1029,
        high: 0x102a,
    },
    XmlChSRange {
        low: 0x1050,
        high: 0x1055,
    },
    XmlChSRange {
        low: 0x10d0,
        high: 0x10f8,
    },
    XmlChSRange {
        low: 0x1100,
        high: 0x1159,
    },
    XmlChSRange {
        low: 0x115f,
        high: 0x11a2,
    },
    XmlChSRange {
        low: 0x11a8,
        high: 0x11f9,
    },
    XmlChSRange {
        low: 0x1200,
        high: 0x1206,
    },
    XmlChSRange {
        low: 0x1208,
        high: 0x1246,
    },
    XmlChSRange {
        low: 0x1248,
        high: 0x1248,
    },
    XmlChSRange {
        low: 0x124a,
        high: 0x124d,
    },
    XmlChSRange {
        low: 0x1250,
        high: 0x1256,
    },
    XmlChSRange {
        low: 0x1258,
        high: 0x1258,
    },
    XmlChSRange {
        low: 0x125a,
        high: 0x125d,
    },
    XmlChSRange {
        low: 0x1260,
        high: 0x1286,
    },
    XmlChSRange {
        low: 0x1288,
        high: 0x1288,
    },
    XmlChSRange {
        low: 0x128a,
        high: 0x128d,
    },
    XmlChSRange {
        low: 0x1290,
        high: 0x12ae,
    },
    XmlChSRange {
        low: 0x12b0,
        high: 0x12b0,
    },
    XmlChSRange {
        low: 0x12b2,
        high: 0x12b5,
    },
    XmlChSRange {
        low: 0x12b8,
        high: 0x12be,
    },
    XmlChSRange {
        low: 0x12c0,
        high: 0x12c0,
    },
    XmlChSRange {
        low: 0x12c2,
        high: 0x12c5,
    },
    XmlChSRange {
        low: 0x12c8,
        high: 0x12ce,
    },
    XmlChSRange {
        low: 0x12d0,
        high: 0x12d6,
    },
    XmlChSRange {
        low: 0x12d8,
        high: 0x12ee,
    },
    XmlChSRange {
        low: 0x12f0,
        high: 0x130e,
    },
    XmlChSRange {
        low: 0x1310,
        high: 0x1310,
    },
    XmlChSRange {
        low: 0x1312,
        high: 0x1315,
    },
    XmlChSRange {
        low: 0x1318,
        high: 0x131e,
    },
    XmlChSRange {
        low: 0x1320,
        high: 0x1346,
    },
    XmlChSRange {
        low: 0x1348,
        high: 0x135a,
    },
    XmlChSRange {
        low: 0x13a0,
        high: 0x13f4,
    },
    XmlChSRange {
        low: 0x1401,
        high: 0x166c,
    },
    XmlChSRange {
        low: 0x166f,
        high: 0x1676,
    },
    XmlChSRange {
        low: 0x1681,
        high: 0x169a,
    },
    XmlChSRange {
        low: 0x16a0,
        high: 0x16ea,
    },
    XmlChSRange {
        low: 0x1700,
        high: 0x170c,
    },
    XmlChSRange {
        low: 0x170e,
        high: 0x1711,
    },
    XmlChSRange {
        low: 0x1720,
        high: 0x1731,
    },
    XmlChSRange {
        low: 0x1740,
        high: 0x1751,
    },
    XmlChSRange {
        low: 0x1760,
        high: 0x176c,
    },
    XmlChSRange {
        low: 0x176e,
        high: 0x1770,
    },
    XmlChSRange {
        low: 0x1780,
        high: 0x17b3,
    },
    XmlChSRange {
        low: 0x17dc,
        high: 0x17dc,
    },
    XmlChSRange {
        low: 0x1820,
        high: 0x1842,
    },
    XmlChSRange {
        low: 0x1844,
        high: 0x1877,
    },
    XmlChSRange {
        low: 0x1880,
        high: 0x18a8,
    },
    XmlChSRange {
        low: 0x1900,
        high: 0x191c,
    },
    XmlChSRange {
        low: 0x1950,
        high: 0x196d,
    },
    XmlChSRange {
        low: 0x1970,
        high: 0x1974,
    },
    XmlChSRange {
        low: 0x2135,
        high: 0x2138,
    },
    XmlChSRange {
        low: 0x3006,
        high: 0x3006,
    },
    XmlChSRange {
        low: 0x303c,
        high: 0x303c,
    },
    XmlChSRange {
        low: 0x3041,
        high: 0x3096,
    },
    XmlChSRange {
        low: 0x309f,
        high: 0x309f,
    },
    XmlChSRange {
        low: 0x30a1,
        high: 0x30fa,
    },
    XmlChSRange {
        low: 0x30ff,
        high: 0x30ff,
    },
    XmlChSRange {
        low: 0x3105,
        high: 0x312c,
    },
    XmlChSRange {
        low: 0x3131,
        high: 0x318e,
    },
    XmlChSRange {
        low: 0x31a0,
        high: 0x31b7,
    },
    XmlChSRange {
        low: 0x31f0,
        high: 0x31ff,
    },
    XmlChSRange {
        low: 0x3400,
        high: 0x3400,
    },
    XmlChSRange {
        low: 0x4db5,
        high: 0x4db5,
    },
    XmlChSRange {
        low: 0x4e00,
        high: 0x4e00,
    },
    XmlChSRange {
        low: 0x9fa5,
        high: 0x9fa5,
    },
    XmlChSRange {
        low: 0xa000,
        high: 0xa48c,
    },
    XmlChSRange {
        low: 0xac00,
        high: 0xac00,
    },
    XmlChSRange {
        low: 0xd7a3,
        high: 0xd7a3,
    },
    XmlChSRange {
        low: 0xf900,
        high: 0xfa2d,
    },
    XmlChSRange {
        low: 0xfa30,
        high: 0xfa6a,
    },
    XmlChSRange {
        low: 0xfb1d,
        high: 0xfb1d,
    },
    XmlChSRange {
        low: 0xfb1f,
        high: 0xfb28,
    },
    XmlChSRange {
        low: 0xfb2a,
        high: 0xfb36,
    },
    XmlChSRange {
        low: 0xfb38,
        high: 0xfb3c,
    },
    XmlChSRange {
        low: 0xfb3e,
        high: 0xfb3e,
    },
    XmlChSRange {
        low: 0xfb40,
        high: 0xfb41,
    },
    XmlChSRange {
        low: 0xfb43,
        high: 0xfb44,
    },
    XmlChSRange {
        low: 0xfb46,
        high: 0xfbb1,
    },
    XmlChSRange {
        low: 0xfbd3,
        high: 0xfd3d,
    },
    XmlChSRange {
        low: 0xfd50,
        high: 0xfd8f,
    },
    XmlChSRange {
        low: 0xfd92,
        high: 0xfdc7,
    },
    XmlChSRange {
        low: 0xfdf0,
        high: 0xfdfb,
    },
    XmlChSRange {
        low: 0xfe70,
        high: 0xfe74,
    },
    XmlChSRange {
        low: 0xfe76,
        high: 0xfefc,
    },
    XmlChSRange {
        low: 0xff66,
        high: 0xff6f,
    },
    XmlChSRange {
        low: 0xff71,
        high: 0xff9d,
    },
    XmlChSRange {
        low: 0xffa0,
        high: 0xffbe,
    },
    XmlChSRange {
        low: 0xffc2,
        high: 0xffc7,
    },
    XmlChSRange {
        low: 0xffca,
        high: 0xffcf,
    },
    XmlChSRange {
        low: 0xffd2,
        high: 0xffd7,
    },
    XmlChSRange {
        low: 0xffda,
        high: 0xffdc,
    },
];

const XML_LO_L: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x10000,
        high: 0x1000b,
    },
    XmlChLRange {
        low: 0x1000d,
        high: 0x10026,
    },
    XmlChLRange {
        low: 0x10028,
        high: 0x1003a,
    },
    XmlChLRange {
        low: 0x1003c,
        high: 0x1003d,
    },
    XmlChLRange {
        low: 0x1003f,
        high: 0x1004d,
    },
    XmlChLRange {
        low: 0x10050,
        high: 0x1005d,
    },
    XmlChLRange {
        low: 0x10080,
        high: 0x100fa,
    },
    XmlChLRange {
        low: 0x10300,
        high: 0x1031e,
    },
    XmlChLRange {
        low: 0x10330,
        high: 0x10349,
    },
    XmlChLRange {
        low: 0x10380,
        high: 0x1039d,
    },
    XmlChLRange {
        low: 0x10450,
        high: 0x1049d,
    },
    XmlChLRange {
        low: 0x10800,
        high: 0x10805,
    },
    XmlChLRange {
        low: 0x10808,
        high: 0x10808,
    },
    XmlChLRange {
        low: 0x1080a,
        high: 0x10835,
    },
    XmlChLRange {
        low: 0x10837,
        high: 0x10838,
    },
    XmlChLRange {
        low: 0x1083c,
        high: 0x1083c,
    },
    XmlChLRange {
        low: 0x1083f,
        high: 0x1083f,
    },
    XmlChLRange {
        low: 0x20000,
        high: 0x20000,
    },
    XmlChLRange {
        low: 0x2a6d6,
        high: 0x2a6d6,
    },
    XmlChLRange {
        low: 0x2f800,
        high: 0x2fa1d,
    },
];

const XML_LO_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 211,
    nb_long_range: 20,
    short_range: XML_LO_S,
    long_range: XML_LO_L,
};

const XML_LT_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x1c5,
        high: 0x1c5,
    },
    XmlChSRange {
        low: 0x1c8,
        high: 0x1c8,
    },
    XmlChSRange {
        low: 0x1cb,
        high: 0x1cb,
    },
    XmlChSRange {
        low: 0x1f2,
        high: 0x1f2,
    },
    XmlChSRange {
        low: 0x1f88,
        high: 0x1f8f,
    },
    XmlChSRange {
        low: 0x1f98,
        high: 0x1f9f,
    },
    XmlChSRange {
        low: 0x1fa8,
        high: 0x1faf,
    },
    XmlChSRange {
        low: 0x1fbc,
        high: 0x1fbc,
    },
    XmlChSRange {
        low: 0x1fcc,
        high: 0x1fcc,
    },
    XmlChSRange {
        low: 0x1ffc,
        high: 0x1ffc,
    },
];

const XML_LT_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 10,
    nb_long_range: 0,
    short_range: XML_LT_S,
    long_range: &[],
};

const XML_LU_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x41,
        high: 0x5a,
    },
    XmlChSRange {
        low: 0xc0,
        high: 0xd6,
    },
    XmlChSRange {
        low: 0xd8,
        high: 0xde,
    },
    XmlChSRange {
        low: 0x100,
        high: 0x100,
    },
    XmlChSRange {
        low: 0x102,
        high: 0x102,
    },
    XmlChSRange {
        low: 0x104,
        high: 0x104,
    },
    XmlChSRange {
        low: 0x106,
        high: 0x106,
    },
    XmlChSRange {
        low: 0x108,
        high: 0x108,
    },
    XmlChSRange {
        low: 0x10a,
        high: 0x10a,
    },
    XmlChSRange {
        low: 0x10c,
        high: 0x10c,
    },
    XmlChSRange {
        low: 0x10e,
        high: 0x10e,
    },
    XmlChSRange {
        low: 0x110,
        high: 0x110,
    },
    XmlChSRange {
        low: 0x112,
        high: 0x112,
    },
    XmlChSRange {
        low: 0x114,
        high: 0x114,
    },
    XmlChSRange {
        low: 0x116,
        high: 0x116,
    },
    XmlChSRange {
        low: 0x118,
        high: 0x118,
    },
    XmlChSRange {
        low: 0x11a,
        high: 0x11a,
    },
    XmlChSRange {
        low: 0x11c,
        high: 0x11c,
    },
    XmlChSRange {
        low: 0x11e,
        high: 0x11e,
    },
    XmlChSRange {
        low: 0x120,
        high: 0x120,
    },
    XmlChSRange {
        low: 0x122,
        high: 0x122,
    },
    XmlChSRange {
        low: 0x124,
        high: 0x124,
    },
    XmlChSRange {
        low: 0x126,
        high: 0x126,
    },
    XmlChSRange {
        low: 0x128,
        high: 0x128,
    },
    XmlChSRange {
        low: 0x12a,
        high: 0x12a,
    },
    XmlChSRange {
        low: 0x12c,
        high: 0x12c,
    },
    XmlChSRange {
        low: 0x12e,
        high: 0x12e,
    },
    XmlChSRange {
        low: 0x130,
        high: 0x130,
    },
    XmlChSRange {
        low: 0x132,
        high: 0x132,
    },
    XmlChSRange {
        low: 0x134,
        high: 0x134,
    },
    XmlChSRange {
        low: 0x136,
        high: 0x136,
    },
    XmlChSRange {
        low: 0x139,
        high: 0x139,
    },
    XmlChSRange {
        low: 0x13b,
        high: 0x13b,
    },
    XmlChSRange {
        low: 0x13d,
        high: 0x13d,
    },
    XmlChSRange {
        low: 0x13f,
        high: 0x13f,
    },
    XmlChSRange {
        low: 0x141,
        high: 0x141,
    },
    XmlChSRange {
        low: 0x143,
        high: 0x143,
    },
    XmlChSRange {
        low: 0x145,
        high: 0x145,
    },
    XmlChSRange {
        low: 0x147,
        high: 0x147,
    },
    XmlChSRange {
        low: 0x14a,
        high: 0x14a,
    },
    XmlChSRange {
        low: 0x14c,
        high: 0x14c,
    },
    XmlChSRange {
        low: 0x14e,
        high: 0x14e,
    },
    XmlChSRange {
        low: 0x150,
        high: 0x150,
    },
    XmlChSRange {
        low: 0x152,
        high: 0x152,
    },
    XmlChSRange {
        low: 0x154,
        high: 0x154,
    },
    XmlChSRange {
        low: 0x156,
        high: 0x156,
    },
    XmlChSRange {
        low: 0x158,
        high: 0x158,
    },
    XmlChSRange {
        low: 0x15a,
        high: 0x15a,
    },
    XmlChSRange {
        low: 0x15c,
        high: 0x15c,
    },
    XmlChSRange {
        low: 0x15e,
        high: 0x15e,
    },
    XmlChSRange {
        low: 0x160,
        high: 0x160,
    },
    XmlChSRange {
        low: 0x162,
        high: 0x162,
    },
    XmlChSRange {
        low: 0x164,
        high: 0x164,
    },
    XmlChSRange {
        low: 0x166,
        high: 0x166,
    },
    XmlChSRange {
        low: 0x168,
        high: 0x168,
    },
    XmlChSRange {
        low: 0x16a,
        high: 0x16a,
    },
    XmlChSRange {
        low: 0x16c,
        high: 0x16c,
    },
    XmlChSRange {
        low: 0x16e,
        high: 0x16e,
    },
    XmlChSRange {
        low: 0x170,
        high: 0x170,
    },
    XmlChSRange {
        low: 0x172,
        high: 0x172,
    },
    XmlChSRange {
        low: 0x174,
        high: 0x174,
    },
    XmlChSRange {
        low: 0x176,
        high: 0x176,
    },
    XmlChSRange {
        low: 0x178,
        high: 0x179,
    },
    XmlChSRange {
        low: 0x17b,
        high: 0x17b,
    },
    XmlChSRange {
        low: 0x17d,
        high: 0x17d,
    },
    XmlChSRange {
        low: 0x181,
        high: 0x182,
    },
    XmlChSRange {
        low: 0x184,
        high: 0x184,
    },
    XmlChSRange {
        low: 0x186,
        high: 0x187,
    },
    XmlChSRange {
        low: 0x189,
        high: 0x18b,
    },
    XmlChSRange {
        low: 0x18e,
        high: 0x191,
    },
    XmlChSRange {
        low: 0x193,
        high: 0x194,
    },
    XmlChSRange {
        low: 0x196,
        high: 0x198,
    },
    XmlChSRange {
        low: 0x19c,
        high: 0x19d,
    },
    XmlChSRange {
        low: 0x19f,
        high: 0x1a0,
    },
    XmlChSRange {
        low: 0x1a2,
        high: 0x1a2,
    },
    XmlChSRange {
        low: 0x1a4,
        high: 0x1a4,
    },
    XmlChSRange {
        low: 0x1a6,
        high: 0x1a7,
    },
    XmlChSRange {
        low: 0x1a9,
        high: 0x1a9,
    },
    XmlChSRange {
        low: 0x1ac,
        high: 0x1ac,
    },
    XmlChSRange {
        low: 0x1ae,
        high: 0x1af,
    },
    XmlChSRange {
        low: 0x1b1,
        high: 0x1b3,
    },
    XmlChSRange {
        low: 0x1b5,
        high: 0x1b5,
    },
    XmlChSRange {
        low: 0x1b7,
        high: 0x1b8,
    },
    XmlChSRange {
        low: 0x1bc,
        high: 0x1bc,
    },
    XmlChSRange {
        low: 0x1c4,
        high: 0x1c4,
    },
    XmlChSRange {
        low: 0x1c7,
        high: 0x1c7,
    },
    XmlChSRange {
        low: 0x1ca,
        high: 0x1ca,
    },
    XmlChSRange {
        low: 0x1cd,
        high: 0x1cd,
    },
    XmlChSRange {
        low: 0x1cf,
        high: 0x1cf,
    },
    XmlChSRange {
        low: 0x1d1,
        high: 0x1d1,
    },
    XmlChSRange {
        low: 0x1d3,
        high: 0x1d3,
    },
    XmlChSRange {
        low: 0x1d5,
        high: 0x1d5,
    },
    XmlChSRange {
        low: 0x1d7,
        high: 0x1d7,
    },
    XmlChSRange {
        low: 0x1d9,
        high: 0x1d9,
    },
    XmlChSRange {
        low: 0x1db,
        high: 0x1db,
    },
    XmlChSRange {
        low: 0x1de,
        high: 0x1de,
    },
    XmlChSRange {
        low: 0x1e0,
        high: 0x1e0,
    },
    XmlChSRange {
        low: 0x1e2,
        high: 0x1e2,
    },
    XmlChSRange {
        low: 0x1e4,
        high: 0x1e4,
    },
    XmlChSRange {
        low: 0x1e6,
        high: 0x1e6,
    },
    XmlChSRange {
        low: 0x1e8,
        high: 0x1e8,
    },
    XmlChSRange {
        low: 0x1ea,
        high: 0x1ea,
    },
    XmlChSRange {
        low: 0x1ec,
        high: 0x1ec,
    },
    XmlChSRange {
        low: 0x1ee,
        high: 0x1ee,
    },
    XmlChSRange {
        low: 0x1f1,
        high: 0x1f1,
    },
    XmlChSRange {
        low: 0x1f4,
        high: 0x1f4,
    },
    XmlChSRange {
        low: 0x1f6,
        high: 0x1f8,
    },
    XmlChSRange {
        low: 0x1fa,
        high: 0x1fa,
    },
    XmlChSRange {
        low: 0x1fc,
        high: 0x1fc,
    },
    XmlChSRange {
        low: 0x1fe,
        high: 0x1fe,
    },
    XmlChSRange {
        low: 0x200,
        high: 0x200,
    },
    XmlChSRange {
        low: 0x202,
        high: 0x202,
    },
    XmlChSRange {
        low: 0x204,
        high: 0x204,
    },
    XmlChSRange {
        low: 0x206,
        high: 0x206,
    },
    XmlChSRange {
        low: 0x208,
        high: 0x208,
    },
    XmlChSRange {
        low: 0x20a,
        high: 0x20a,
    },
    XmlChSRange {
        low: 0x20c,
        high: 0x20c,
    },
    XmlChSRange {
        low: 0x20e,
        high: 0x20e,
    },
    XmlChSRange {
        low: 0x210,
        high: 0x210,
    },
    XmlChSRange {
        low: 0x212,
        high: 0x212,
    },
    XmlChSRange {
        low: 0x214,
        high: 0x214,
    },
    XmlChSRange {
        low: 0x216,
        high: 0x216,
    },
    XmlChSRange {
        low: 0x218,
        high: 0x218,
    },
    XmlChSRange {
        low: 0x21a,
        high: 0x21a,
    },
    XmlChSRange {
        low: 0x21c,
        high: 0x21c,
    },
    XmlChSRange {
        low: 0x21e,
        high: 0x21e,
    },
    XmlChSRange {
        low: 0x220,
        high: 0x220,
    },
    XmlChSRange {
        low: 0x222,
        high: 0x222,
    },
    XmlChSRange {
        low: 0x224,
        high: 0x224,
    },
    XmlChSRange {
        low: 0x226,
        high: 0x226,
    },
    XmlChSRange {
        low: 0x228,
        high: 0x228,
    },
    XmlChSRange {
        low: 0x22a,
        high: 0x22a,
    },
    XmlChSRange {
        low: 0x22c,
        high: 0x22c,
    },
    XmlChSRange {
        low: 0x22e,
        high: 0x22e,
    },
    XmlChSRange {
        low: 0x230,
        high: 0x230,
    },
    XmlChSRange {
        low: 0x232,
        high: 0x232,
    },
    XmlChSRange {
        low: 0x386,
        high: 0x386,
    },
    XmlChSRange {
        low: 0x388,
        high: 0x38a,
    },
    XmlChSRange {
        low: 0x38c,
        high: 0x38c,
    },
    XmlChSRange {
        low: 0x38e,
        high: 0x38f,
    },
    XmlChSRange {
        low: 0x391,
        high: 0x3a1,
    },
    XmlChSRange {
        low: 0x3a3,
        high: 0x3ab,
    },
    XmlChSRange {
        low: 0x3d2,
        high: 0x3d4,
    },
    XmlChSRange {
        low: 0x3d8,
        high: 0x3d8,
    },
    XmlChSRange {
        low: 0x3da,
        high: 0x3da,
    },
    XmlChSRange {
        low: 0x3dc,
        high: 0x3dc,
    },
    XmlChSRange {
        low: 0x3de,
        high: 0x3de,
    },
    XmlChSRange {
        low: 0x3e0,
        high: 0x3e0,
    },
    XmlChSRange {
        low: 0x3e2,
        high: 0x3e2,
    },
    XmlChSRange {
        low: 0x3e4,
        high: 0x3e4,
    },
    XmlChSRange {
        low: 0x3e6,
        high: 0x3e6,
    },
    XmlChSRange {
        low: 0x3e8,
        high: 0x3e8,
    },
    XmlChSRange {
        low: 0x3ea,
        high: 0x3ea,
    },
    XmlChSRange {
        low: 0x3ec,
        high: 0x3ec,
    },
    XmlChSRange {
        low: 0x3ee,
        high: 0x3ee,
    },
    XmlChSRange {
        low: 0x3f4,
        high: 0x3f4,
    },
    XmlChSRange {
        low: 0x3f7,
        high: 0x3f7,
    },
    XmlChSRange {
        low: 0x3f9,
        high: 0x3fa,
    },
    XmlChSRange {
        low: 0x400,
        high: 0x42f,
    },
    XmlChSRange {
        low: 0x460,
        high: 0x460,
    },
    XmlChSRange {
        low: 0x462,
        high: 0x462,
    },
    XmlChSRange {
        low: 0x464,
        high: 0x464,
    },
    XmlChSRange {
        low: 0x466,
        high: 0x466,
    },
    XmlChSRange {
        low: 0x468,
        high: 0x468,
    },
    XmlChSRange {
        low: 0x46a,
        high: 0x46a,
    },
    XmlChSRange {
        low: 0x46c,
        high: 0x46c,
    },
    XmlChSRange {
        low: 0x46e,
        high: 0x46e,
    },
    XmlChSRange {
        low: 0x470,
        high: 0x470,
    },
    XmlChSRange {
        low: 0x472,
        high: 0x472,
    },
    XmlChSRange {
        low: 0x474,
        high: 0x474,
    },
    XmlChSRange {
        low: 0x476,
        high: 0x476,
    },
    XmlChSRange {
        low: 0x478,
        high: 0x478,
    },
    XmlChSRange {
        low: 0x47a,
        high: 0x47a,
    },
    XmlChSRange {
        low: 0x47c,
        high: 0x47c,
    },
    XmlChSRange {
        low: 0x47e,
        high: 0x47e,
    },
    XmlChSRange {
        low: 0x480,
        high: 0x480,
    },
    XmlChSRange {
        low: 0x48a,
        high: 0x48a,
    },
    XmlChSRange {
        low: 0x48c,
        high: 0x48c,
    },
    XmlChSRange {
        low: 0x48e,
        high: 0x48e,
    },
    XmlChSRange {
        low: 0x490,
        high: 0x490,
    },
    XmlChSRange {
        low: 0x492,
        high: 0x492,
    },
    XmlChSRange {
        low: 0x494,
        high: 0x494,
    },
    XmlChSRange {
        low: 0x496,
        high: 0x496,
    },
    XmlChSRange {
        low: 0x498,
        high: 0x498,
    },
    XmlChSRange {
        low: 0x49a,
        high: 0x49a,
    },
    XmlChSRange {
        low: 0x49c,
        high: 0x49c,
    },
    XmlChSRange {
        low: 0x49e,
        high: 0x49e,
    },
    XmlChSRange {
        low: 0x4a0,
        high: 0x4a0,
    },
    XmlChSRange {
        low: 0x4a2,
        high: 0x4a2,
    },
    XmlChSRange {
        low: 0x4a4,
        high: 0x4a4,
    },
    XmlChSRange {
        low: 0x4a6,
        high: 0x4a6,
    },
    XmlChSRange {
        low: 0x4a8,
        high: 0x4a8,
    },
    XmlChSRange {
        low: 0x4aa,
        high: 0x4aa,
    },
    XmlChSRange {
        low: 0x4ac,
        high: 0x4ac,
    },
    XmlChSRange {
        low: 0x4ae,
        high: 0x4ae,
    },
    XmlChSRange {
        low: 0x4b0,
        high: 0x4b0,
    },
    XmlChSRange {
        low: 0x4b2,
        high: 0x4b2,
    },
    XmlChSRange {
        low: 0x4b4,
        high: 0x4b4,
    },
    XmlChSRange {
        low: 0x4b6,
        high: 0x4b6,
    },
    XmlChSRange {
        low: 0x4b8,
        high: 0x4b8,
    },
    XmlChSRange {
        low: 0x4ba,
        high: 0x4ba,
    },
    XmlChSRange {
        low: 0x4bc,
        high: 0x4bc,
    },
    XmlChSRange {
        low: 0x4be,
        high: 0x4be,
    },
    XmlChSRange {
        low: 0x4c0,
        high: 0x4c1,
    },
    XmlChSRange {
        low: 0x4c3,
        high: 0x4c3,
    },
    XmlChSRange {
        low: 0x4c5,
        high: 0x4c5,
    },
    XmlChSRange {
        low: 0x4c7,
        high: 0x4c7,
    },
    XmlChSRange {
        low: 0x4c9,
        high: 0x4c9,
    },
    XmlChSRange {
        low: 0x4cb,
        high: 0x4cb,
    },
    XmlChSRange {
        low: 0x4cd,
        high: 0x4cd,
    },
    XmlChSRange {
        low: 0x4d0,
        high: 0x4d0,
    },
    XmlChSRange {
        low: 0x4d2,
        high: 0x4d2,
    },
    XmlChSRange {
        low: 0x4d4,
        high: 0x4d4,
    },
    XmlChSRange {
        low: 0x4d6,
        high: 0x4d6,
    },
    XmlChSRange {
        low: 0x4d8,
        high: 0x4d8,
    },
    XmlChSRange {
        low: 0x4da,
        high: 0x4da,
    },
    XmlChSRange {
        low: 0x4dc,
        high: 0x4dc,
    },
    XmlChSRange {
        low: 0x4de,
        high: 0x4de,
    },
    XmlChSRange {
        low: 0x4e0,
        high: 0x4e0,
    },
    XmlChSRange {
        low: 0x4e2,
        high: 0x4e2,
    },
    XmlChSRange {
        low: 0x4e4,
        high: 0x4e4,
    },
    XmlChSRange {
        low: 0x4e6,
        high: 0x4e6,
    },
    XmlChSRange {
        low: 0x4e8,
        high: 0x4e8,
    },
    XmlChSRange {
        low: 0x4ea,
        high: 0x4ea,
    },
    XmlChSRange {
        low: 0x4ec,
        high: 0x4ec,
    },
    XmlChSRange {
        low: 0x4ee,
        high: 0x4ee,
    },
    XmlChSRange {
        low: 0x4f0,
        high: 0x4f0,
    },
    XmlChSRange {
        low: 0x4f2,
        high: 0x4f2,
    },
    XmlChSRange {
        low: 0x4f4,
        high: 0x4f4,
    },
    XmlChSRange {
        low: 0x4f8,
        high: 0x4f8,
    },
    XmlChSRange {
        low: 0x500,
        high: 0x500,
    },
    XmlChSRange {
        low: 0x502,
        high: 0x502,
    },
    XmlChSRange {
        low: 0x504,
        high: 0x504,
    },
    XmlChSRange {
        low: 0x506,
        high: 0x506,
    },
    XmlChSRange {
        low: 0x508,
        high: 0x508,
    },
    XmlChSRange {
        low: 0x50a,
        high: 0x50a,
    },
    XmlChSRange {
        low: 0x50c,
        high: 0x50c,
    },
    XmlChSRange {
        low: 0x50e,
        high: 0x50e,
    },
    XmlChSRange {
        low: 0x531,
        high: 0x556,
    },
    XmlChSRange {
        low: 0x10a0,
        high: 0x10c5,
    },
    XmlChSRange {
        low: 0x1e00,
        high: 0x1e00,
    },
    XmlChSRange {
        low: 0x1e02,
        high: 0x1e02,
    },
    XmlChSRange {
        low: 0x1e04,
        high: 0x1e04,
    },
    XmlChSRange {
        low: 0x1e06,
        high: 0x1e06,
    },
    XmlChSRange {
        low: 0x1e08,
        high: 0x1e08,
    },
    XmlChSRange {
        low: 0x1e0a,
        high: 0x1e0a,
    },
    XmlChSRange {
        low: 0x1e0c,
        high: 0x1e0c,
    },
    XmlChSRange {
        low: 0x1e0e,
        high: 0x1e0e,
    },
    XmlChSRange {
        low: 0x1e10,
        high: 0x1e10,
    },
    XmlChSRange {
        low: 0x1e12,
        high: 0x1e12,
    },
    XmlChSRange {
        low: 0x1e14,
        high: 0x1e14,
    },
    XmlChSRange {
        low: 0x1e16,
        high: 0x1e16,
    },
    XmlChSRange {
        low: 0x1e18,
        high: 0x1e18,
    },
    XmlChSRange {
        low: 0x1e1a,
        high: 0x1e1a,
    },
    XmlChSRange {
        low: 0x1e1c,
        high: 0x1e1c,
    },
    XmlChSRange {
        low: 0x1e1e,
        high: 0x1e1e,
    },
    XmlChSRange {
        low: 0x1e20,
        high: 0x1e20,
    },
    XmlChSRange {
        low: 0x1e22,
        high: 0x1e22,
    },
    XmlChSRange {
        low: 0x1e24,
        high: 0x1e24,
    },
    XmlChSRange {
        low: 0x1e26,
        high: 0x1e26,
    },
    XmlChSRange {
        low: 0x1e28,
        high: 0x1e28,
    },
    XmlChSRange {
        low: 0x1e2a,
        high: 0x1e2a,
    },
    XmlChSRange {
        low: 0x1e2c,
        high: 0x1e2c,
    },
    XmlChSRange {
        low: 0x1e2e,
        high: 0x1e2e,
    },
    XmlChSRange {
        low: 0x1e30,
        high: 0x1e30,
    },
    XmlChSRange {
        low: 0x1e32,
        high: 0x1e32,
    },
    XmlChSRange {
        low: 0x1e34,
        high: 0x1e34,
    },
    XmlChSRange {
        low: 0x1e36,
        high: 0x1e36,
    },
    XmlChSRange {
        low: 0x1e38,
        high: 0x1e38,
    },
    XmlChSRange {
        low: 0x1e3a,
        high: 0x1e3a,
    },
    XmlChSRange {
        low: 0x1e3c,
        high: 0x1e3c,
    },
    XmlChSRange {
        low: 0x1e3e,
        high: 0x1e3e,
    },
    XmlChSRange {
        low: 0x1e40,
        high: 0x1e40,
    },
    XmlChSRange {
        low: 0x1e42,
        high: 0x1e42,
    },
    XmlChSRange {
        low: 0x1e44,
        high: 0x1e44,
    },
    XmlChSRange {
        low: 0x1e46,
        high: 0x1e46,
    },
    XmlChSRange {
        low: 0x1e48,
        high: 0x1e48,
    },
    XmlChSRange {
        low: 0x1e4a,
        high: 0x1e4a,
    },
    XmlChSRange {
        low: 0x1e4c,
        high: 0x1e4c,
    },
    XmlChSRange {
        low: 0x1e4e,
        high: 0x1e4e,
    },
    XmlChSRange {
        low: 0x1e50,
        high: 0x1e50,
    },
    XmlChSRange {
        low: 0x1e52,
        high: 0x1e52,
    },
    XmlChSRange {
        low: 0x1e54,
        high: 0x1e54,
    },
    XmlChSRange {
        low: 0x1e56,
        high: 0x1e56,
    },
    XmlChSRange {
        low: 0x1e58,
        high: 0x1e58,
    },
    XmlChSRange {
        low: 0x1e5a,
        high: 0x1e5a,
    },
    XmlChSRange {
        low: 0x1e5c,
        high: 0x1e5c,
    },
    XmlChSRange {
        low: 0x1e5e,
        high: 0x1e5e,
    },
    XmlChSRange {
        low: 0x1e60,
        high: 0x1e60,
    },
    XmlChSRange {
        low: 0x1e62,
        high: 0x1e62,
    },
    XmlChSRange {
        low: 0x1e64,
        high: 0x1e64,
    },
    XmlChSRange {
        low: 0x1e66,
        high: 0x1e66,
    },
    XmlChSRange {
        low: 0x1e68,
        high: 0x1e68,
    },
    XmlChSRange {
        low: 0x1e6a,
        high: 0x1e6a,
    },
    XmlChSRange {
        low: 0x1e6c,
        high: 0x1e6c,
    },
    XmlChSRange {
        low: 0x1e6e,
        high: 0x1e6e,
    },
    XmlChSRange {
        low: 0x1e70,
        high: 0x1e70,
    },
    XmlChSRange {
        low: 0x1e72,
        high: 0x1e72,
    },
    XmlChSRange {
        low: 0x1e74,
        high: 0x1e74,
    },
    XmlChSRange {
        low: 0x1e76,
        high: 0x1e76,
    },
    XmlChSRange {
        low: 0x1e78,
        high: 0x1e78,
    },
    XmlChSRange {
        low: 0x1e7a,
        high: 0x1e7a,
    },
    XmlChSRange {
        low: 0x1e7c,
        high: 0x1e7c,
    },
    XmlChSRange {
        low: 0x1e7e,
        high: 0x1e7e,
    },
    XmlChSRange {
        low: 0x1e80,
        high: 0x1e80,
    },
    XmlChSRange {
        low: 0x1e82,
        high: 0x1e82,
    },
    XmlChSRange {
        low: 0x1e84,
        high: 0x1e84,
    },
    XmlChSRange {
        low: 0x1e86,
        high: 0x1e86,
    },
    XmlChSRange {
        low: 0x1e88,
        high: 0x1e88,
    },
    XmlChSRange {
        low: 0x1e8a,
        high: 0x1e8a,
    },
    XmlChSRange {
        low: 0x1e8c,
        high: 0x1e8c,
    },
    XmlChSRange {
        low: 0x1e8e,
        high: 0x1e8e,
    },
    XmlChSRange {
        low: 0x1e90,
        high: 0x1e90,
    },
    XmlChSRange {
        low: 0x1e92,
        high: 0x1e92,
    },
    XmlChSRange {
        low: 0x1e94,
        high: 0x1e94,
    },
    XmlChSRange {
        low: 0x1ea0,
        high: 0x1ea0,
    },
    XmlChSRange {
        low: 0x1ea2,
        high: 0x1ea2,
    },
    XmlChSRange {
        low: 0x1ea4,
        high: 0x1ea4,
    },
    XmlChSRange {
        low: 0x1ea6,
        high: 0x1ea6,
    },
    XmlChSRange {
        low: 0x1ea8,
        high: 0x1ea8,
    },
    XmlChSRange {
        low: 0x1eaa,
        high: 0x1eaa,
    },
    XmlChSRange {
        low: 0x1eac,
        high: 0x1eac,
    },
    XmlChSRange {
        low: 0x1eae,
        high: 0x1eae,
    },
    XmlChSRange {
        low: 0x1eb0,
        high: 0x1eb0,
    },
    XmlChSRange {
        low: 0x1eb2,
        high: 0x1eb2,
    },
    XmlChSRange {
        low: 0x1eb4,
        high: 0x1eb4,
    },
    XmlChSRange {
        low: 0x1eb6,
        high: 0x1eb6,
    },
    XmlChSRange {
        low: 0x1eb8,
        high: 0x1eb8,
    },
    XmlChSRange {
        low: 0x1eba,
        high: 0x1eba,
    },
    XmlChSRange {
        low: 0x1ebc,
        high: 0x1ebc,
    },
    XmlChSRange {
        low: 0x1ebe,
        high: 0x1ebe,
    },
    XmlChSRange {
        low: 0x1ec0,
        high: 0x1ec0,
    },
    XmlChSRange {
        low: 0x1ec2,
        high: 0x1ec2,
    },
    XmlChSRange {
        low: 0x1ec4,
        high: 0x1ec4,
    },
    XmlChSRange {
        low: 0x1ec6,
        high: 0x1ec6,
    },
    XmlChSRange {
        low: 0x1ec8,
        high: 0x1ec8,
    },
    XmlChSRange {
        low: 0x1eca,
        high: 0x1eca,
    },
    XmlChSRange {
        low: 0x1ecc,
        high: 0x1ecc,
    },
    XmlChSRange {
        low: 0x1ece,
        high: 0x1ece,
    },
    XmlChSRange {
        low: 0x1ed0,
        high: 0x1ed0,
    },
    XmlChSRange {
        low: 0x1ed2,
        high: 0x1ed2,
    },
    XmlChSRange {
        low: 0x1ed4,
        high: 0x1ed4,
    },
    XmlChSRange {
        low: 0x1ed6,
        high: 0x1ed6,
    },
    XmlChSRange {
        low: 0x1ed8,
        high: 0x1ed8,
    },
    XmlChSRange {
        low: 0x1eda,
        high: 0x1eda,
    },
    XmlChSRange {
        low: 0x1edc,
        high: 0x1edc,
    },
    XmlChSRange {
        low: 0x1ede,
        high: 0x1ede,
    },
    XmlChSRange {
        low: 0x1ee0,
        high: 0x1ee0,
    },
    XmlChSRange {
        low: 0x1ee2,
        high: 0x1ee2,
    },
    XmlChSRange {
        low: 0x1ee4,
        high: 0x1ee4,
    },
    XmlChSRange {
        low: 0x1ee6,
        high: 0x1ee6,
    },
    XmlChSRange {
        low: 0x1ee8,
        high: 0x1ee8,
    },
    XmlChSRange {
        low: 0x1eea,
        high: 0x1eea,
    },
    XmlChSRange {
        low: 0x1eec,
        high: 0x1eec,
    },
    XmlChSRange {
        low: 0x1eee,
        high: 0x1eee,
    },
    XmlChSRange {
        low: 0x1ef0,
        high: 0x1ef0,
    },
    XmlChSRange {
        low: 0x1ef2,
        high: 0x1ef2,
    },
    XmlChSRange {
        low: 0x1ef4,
        high: 0x1ef4,
    },
    XmlChSRange {
        low: 0x1ef6,
        high: 0x1ef6,
    },
    XmlChSRange {
        low: 0x1ef8,
        high: 0x1ef8,
    },
    XmlChSRange {
        low: 0x1f08,
        high: 0x1f0f,
    },
    XmlChSRange {
        low: 0x1f18,
        high: 0x1f1d,
    },
    XmlChSRange {
        low: 0x1f28,
        high: 0x1f2f,
    },
    XmlChSRange {
        low: 0x1f38,
        high: 0x1f3f,
    },
    XmlChSRange {
        low: 0x1f48,
        high: 0x1f4d,
    },
    XmlChSRange {
        low: 0x1f59,
        high: 0x1f59,
    },
    XmlChSRange {
        low: 0x1f5b,
        high: 0x1f5b,
    },
    XmlChSRange {
        low: 0x1f5d,
        high: 0x1f5d,
    },
    XmlChSRange {
        low: 0x1f5f,
        high: 0x1f5f,
    },
    XmlChSRange {
        low: 0x1f68,
        high: 0x1f6f,
    },
    XmlChSRange {
        low: 0x1fb8,
        high: 0x1fbb,
    },
    XmlChSRange {
        low: 0x1fc8,
        high: 0x1fcb,
    },
    XmlChSRange {
        low: 0x1fd8,
        high: 0x1fdb,
    },
    XmlChSRange {
        low: 0x1fe8,
        high: 0x1fec,
    },
    XmlChSRange {
        low: 0x1ff8,
        high: 0x1ffb,
    },
    XmlChSRange {
        low: 0x2102,
        high: 0x2102,
    },
    XmlChSRange {
        low: 0x2107,
        high: 0x2107,
    },
    XmlChSRange {
        low: 0x210b,
        high: 0x210d,
    },
    XmlChSRange {
        low: 0x2110,
        high: 0x2112,
    },
    XmlChSRange {
        low: 0x2115,
        high: 0x2115,
    },
    XmlChSRange {
        low: 0x2119,
        high: 0x211d,
    },
    XmlChSRange {
        low: 0x2124,
        high: 0x2124,
    },
    XmlChSRange {
        low: 0x2126,
        high: 0x2126,
    },
    XmlChSRange {
        low: 0x2128,
        high: 0x2128,
    },
    XmlChSRange {
        low: 0x212a,
        high: 0x212d,
    },
    XmlChSRange {
        low: 0x2130,
        high: 0x2131,
    },
    XmlChSRange {
        low: 0x2133,
        high: 0x2133,
    },
    XmlChSRange {
        low: 0x213e,
        high: 0x213f,
    },
    XmlChSRange {
        low: 0x2145,
        high: 0x2145,
    },
    XmlChSRange {
        low: 0xff21,
        high: 0xff3a,
    },
];

const XML_LU_L: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x10400,
        high: 0x10427,
    },
    XmlChLRange {
        low: 0x1d400,
        high: 0x1d419,
    },
    XmlChLRange {
        low: 0x1d434,
        high: 0x1d44d,
    },
    XmlChLRange {
        low: 0x1d468,
        high: 0x1d481,
    },
    XmlChLRange {
        low: 0x1d49c,
        high: 0x1d49c,
    },
    XmlChLRange {
        low: 0x1d49e,
        high: 0x1d49f,
    },
    XmlChLRange {
        low: 0x1d4a2,
        high: 0x1d4a2,
    },
    XmlChLRange {
        low: 0x1d4a5,
        high: 0x1d4a6,
    },
    XmlChLRange {
        low: 0x1d4a9,
        high: 0x1d4ac,
    },
    XmlChLRange {
        low: 0x1d4ae,
        high: 0x1d4b5,
    },
    XmlChLRange {
        low: 0x1d4d0,
        high: 0x1d4e9,
    },
    XmlChLRange {
        low: 0x1d504,
        high: 0x1d505,
    },
    XmlChLRange {
        low: 0x1d507,
        high: 0x1d50a,
    },
    XmlChLRange {
        low: 0x1d50d,
        high: 0x1d514,
    },
    XmlChLRange {
        low: 0x1d516,
        high: 0x1d51c,
    },
    XmlChLRange {
        low: 0x1d538,
        high: 0x1d539,
    },
    XmlChLRange {
        low: 0x1d53b,
        high: 0x1d53e,
    },
    XmlChLRange {
        low: 0x1d540,
        high: 0x1d544,
    },
    XmlChLRange {
        low: 0x1d546,
        high: 0x1d546,
    },
    XmlChLRange {
        low: 0x1d54a,
        high: 0x1d550,
    },
    XmlChLRange {
        low: 0x1d56c,
        high: 0x1d585,
    },
    XmlChLRange {
        low: 0x1d5a0,
        high: 0x1d5b9,
    },
    XmlChLRange {
        low: 0x1d5d4,
        high: 0x1d5ed,
    },
    XmlChLRange {
        low: 0x1d608,
        high: 0x1d621,
    },
    XmlChLRange {
        low: 0x1d63c,
        high: 0x1d655,
    },
    XmlChLRange {
        low: 0x1d670,
        high: 0x1d689,
    },
    XmlChLRange {
        low: 0x1d6a8,
        high: 0x1d6c0,
    },
    XmlChLRange {
        low: 0x1d6e2,
        high: 0x1d6fa,
    },
    XmlChLRange {
        low: 0x1d71c,
        high: 0x1d734,
    },
    XmlChLRange {
        low: 0x1d756,
        high: 0x1d76e,
    },
    XmlChLRange {
        low: 0x1d790,
        high: 0x1d7a8,
    },
];

const XML_LU_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 390,
    nb_long_range: 31,
    short_range: XML_LU_S,
    long_range: XML_LU_L,
};

const XML_MS: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x300,
        high: 0x357,
    },
    XmlChSRange {
        low: 0x35d,
        high: 0x36f,
    },
    XmlChSRange {
        low: 0x483,
        high: 0x486,
    },
    XmlChSRange {
        low: 0x488,
        high: 0x489,
    },
    XmlChSRange {
        low: 0x591,
        high: 0x5a1,
    },
    XmlChSRange {
        low: 0x5a3,
        high: 0x5b9,
    },
    XmlChSRange {
        low: 0x5bb,
        high: 0x5bd,
    },
    XmlChSRange {
        low: 0x5bf,
        high: 0x5bf,
    },
    XmlChSRange {
        low: 0x5c1,
        high: 0x5c2,
    },
    XmlChSRange {
        low: 0x5c4,
        high: 0x5c4,
    },
    XmlChSRange {
        low: 0x610,
        high: 0x615,
    },
    XmlChSRange {
        low: 0x64b,
        high: 0x658,
    },
    XmlChSRange {
        low: 0x670,
        high: 0x670,
    },
    XmlChSRange {
        low: 0x6d6,
        high: 0x6dc,
    },
    XmlChSRange {
        low: 0x6de,
        high: 0x6e4,
    },
    XmlChSRange {
        low: 0x6e7,
        high: 0x6e8,
    },
    XmlChSRange {
        low: 0x6ea,
        high: 0x6ed,
    },
    XmlChSRange {
        low: 0x711,
        high: 0x711,
    },
    XmlChSRange {
        low: 0x730,
        high: 0x74a,
    },
    XmlChSRange {
        low: 0x7a6,
        high: 0x7b0,
    },
    XmlChSRange {
        low: 0x901,
        high: 0x903,
    },
    XmlChSRange {
        low: 0x93c,
        high: 0x93c,
    },
    XmlChSRange {
        low: 0x93e,
        high: 0x94d,
    },
    XmlChSRange {
        low: 0x951,
        high: 0x954,
    },
    XmlChSRange {
        low: 0x962,
        high: 0x963,
    },
    XmlChSRange {
        low: 0x981,
        high: 0x983,
    },
    XmlChSRange {
        low: 0x9bc,
        high: 0x9bc,
    },
    XmlChSRange {
        low: 0x9be,
        high: 0x9c4,
    },
    XmlChSRange {
        low: 0x9c7,
        high: 0x9c8,
    },
    XmlChSRange {
        low: 0x9cb,
        high: 0x9cd,
    },
    XmlChSRange {
        low: 0x9d7,
        high: 0x9d7,
    },
    XmlChSRange {
        low: 0x9e2,
        high: 0x9e3,
    },
    XmlChSRange {
        low: 0xa01,
        high: 0xa03,
    },
    XmlChSRange {
        low: 0xa3c,
        high: 0xa3c,
    },
    XmlChSRange {
        low: 0xa3e,
        high: 0xa42,
    },
    XmlChSRange {
        low: 0xa47,
        high: 0xa48,
    },
    XmlChSRange {
        low: 0xa4b,
        high: 0xa4d,
    },
    XmlChSRange {
        low: 0xa70,
        high: 0xa71,
    },
    XmlChSRange {
        low: 0xa81,
        high: 0xa83,
    },
    XmlChSRange {
        low: 0xabc,
        high: 0xabc,
    },
    XmlChSRange {
        low: 0xabe,
        high: 0xac5,
    },
    XmlChSRange {
        low: 0xac7,
        high: 0xac9,
    },
    XmlChSRange {
        low: 0xacb,
        high: 0xacd,
    },
    XmlChSRange {
        low: 0xae2,
        high: 0xae3,
    },
    XmlChSRange {
        low: 0xb01,
        high: 0xb03,
    },
    XmlChSRange {
        low: 0xb3c,
        high: 0xb3c,
    },
    XmlChSRange {
        low: 0xb3e,
        high: 0xb43,
    },
    XmlChSRange {
        low: 0xb47,
        high: 0xb48,
    },
    XmlChSRange {
        low: 0xb4b,
        high: 0xb4d,
    },
    XmlChSRange {
        low: 0xb56,
        high: 0xb57,
    },
    XmlChSRange {
        low: 0xb82,
        high: 0xb82,
    },
    XmlChSRange {
        low: 0xbbe,
        high: 0xbc2,
    },
    XmlChSRange {
        low: 0xbc6,
        high: 0xbc8,
    },
    XmlChSRange {
        low: 0xbca,
        high: 0xbcd,
    },
    XmlChSRange {
        low: 0xbd7,
        high: 0xbd7,
    },
    XmlChSRange {
        low: 0xc01,
        high: 0xc03,
    },
    XmlChSRange {
        low: 0xc3e,
        high: 0xc44,
    },
    XmlChSRange {
        low: 0xc46,
        high: 0xc48,
    },
    XmlChSRange {
        low: 0xc4a,
        high: 0xc4d,
    },
    XmlChSRange {
        low: 0xc55,
        high: 0xc56,
    },
    XmlChSRange {
        low: 0xc82,
        high: 0xc83,
    },
    XmlChSRange {
        low: 0xcbc,
        high: 0xcbc,
    },
    XmlChSRange {
        low: 0xcbe,
        high: 0xcc4,
    },
    XmlChSRange {
        low: 0xcc6,
        high: 0xcc8,
    },
    XmlChSRange {
        low: 0xcca,
        high: 0xccd,
    },
    XmlChSRange {
        low: 0xcd5,
        high: 0xcd6,
    },
    XmlChSRange {
        low: 0xd02,
        high: 0xd03,
    },
    XmlChSRange {
        low: 0xd3e,
        high: 0xd43,
    },
    XmlChSRange {
        low: 0xd46,
        high: 0xd48,
    },
    XmlChSRange {
        low: 0xd4a,
        high: 0xd4d,
    },
    XmlChSRange {
        low: 0xd57,
        high: 0xd57,
    },
    XmlChSRange {
        low: 0xd82,
        high: 0xd83,
    },
    XmlChSRange {
        low: 0xdca,
        high: 0xdca,
    },
    XmlChSRange {
        low: 0xdcf,
        high: 0xdd4,
    },
    XmlChSRange {
        low: 0xdd6,
        high: 0xdd6,
    },
    XmlChSRange {
        low: 0xdd8,
        high: 0xddf,
    },
    XmlChSRange {
        low: 0xdf2,
        high: 0xdf3,
    },
    XmlChSRange {
        low: 0xe31,
        high: 0xe31,
    },
    XmlChSRange {
        low: 0xe34,
        high: 0xe3a,
    },
    XmlChSRange {
        low: 0xe47,
        high: 0xe4e,
    },
    XmlChSRange {
        low: 0xeb1,
        high: 0xeb1,
    },
    XmlChSRange {
        low: 0xeb4,
        high: 0xeb9,
    },
    XmlChSRange {
        low: 0xebb,
        high: 0xebc,
    },
    XmlChSRange {
        low: 0xec8,
        high: 0xecd,
    },
    XmlChSRange {
        low: 0xf18,
        high: 0xf19,
    },
    XmlChSRange {
        low: 0xf35,
        high: 0xf35,
    },
    XmlChSRange {
        low: 0xf37,
        high: 0xf37,
    },
    XmlChSRange {
        low: 0xf39,
        high: 0xf39,
    },
    XmlChSRange {
        low: 0xf3e,
        high: 0xf3f,
    },
    XmlChSRange {
        low: 0xf71,
        high: 0xf84,
    },
    XmlChSRange {
        low: 0xf86,
        high: 0xf87,
    },
    XmlChSRange {
        low: 0xf90,
        high: 0xf97,
    },
    XmlChSRange {
        low: 0xf99,
        high: 0xfbc,
    },
    XmlChSRange {
        low: 0xfc6,
        high: 0xfc6,
    },
    XmlChSRange {
        low: 0x102c,
        high: 0x1032,
    },
    XmlChSRange {
        low: 0x1036,
        high: 0x1039,
    },
    XmlChSRange {
        low: 0x1056,
        high: 0x1059,
    },
    XmlChSRange {
        low: 0x1712,
        high: 0x1714,
    },
    XmlChSRange {
        low: 0x1732,
        high: 0x1734,
    },
    XmlChSRange {
        low: 0x1752,
        high: 0x1753,
    },
    XmlChSRange {
        low: 0x1772,
        high: 0x1773,
    },
    XmlChSRange {
        low: 0x17b6,
        high: 0x17d3,
    },
    XmlChSRange {
        low: 0x17dd,
        high: 0x17dd,
    },
    XmlChSRange {
        low: 0x180b,
        high: 0x180d,
    },
    XmlChSRange {
        low: 0x18a9,
        high: 0x18a9,
    },
    XmlChSRange {
        low: 0x1920,
        high: 0x192b,
    },
    XmlChSRange {
        low: 0x1930,
        high: 0x193b,
    },
    XmlChSRange {
        low: 0x20d0,
        high: 0x20ea,
    },
    XmlChSRange {
        low: 0x302a,
        high: 0x302f,
    },
    XmlChSRange {
        low: 0x3099,
        high: 0x309a,
    },
    XmlChSRange {
        low: 0xfb1e,
        high: 0xfb1e,
    },
    XmlChSRange {
        low: 0xfe00,
        high: 0xfe0f,
    },
    XmlChSRange {
        low: 0xfe20,
        high: 0xfe23,
    },
];

const XML_ML: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x1d165,
        high: 0x1d169,
    },
    XmlChLRange {
        low: 0x1d16d,
        high: 0x1d172,
    },
    XmlChLRange {
        low: 0x1d17b,
        high: 0x1d182,
    },
    XmlChLRange {
        low: 0x1d185,
        high: 0x1d18b,
    },
    XmlChLRange {
        low: 0x1d1aa,
        high: 0x1d1ad,
    },
    XmlChLRange {
        low: 0xe0100,
        high: 0xe01ef,
    },
];

const XML_MG: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 113,
    nb_long_range: 6,
    short_range: XML_MS,
    long_range: XML_ML,
};

const XML_MC_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x903,
        high: 0x903,
    },
    XmlChSRange {
        low: 0x93e,
        high: 0x940,
    },
    XmlChSRange {
        low: 0x949,
        high: 0x94c,
    },
    XmlChSRange {
        low: 0x982,
        high: 0x983,
    },
    XmlChSRange {
        low: 0x9be,
        high: 0x9c0,
    },
    XmlChSRange {
        low: 0x9c7,
        high: 0x9c8,
    },
    XmlChSRange {
        low: 0x9cb,
        high: 0x9cc,
    },
    XmlChSRange {
        low: 0x9d7,
        high: 0x9d7,
    },
    XmlChSRange {
        low: 0xa03,
        high: 0xa03,
    },
    XmlChSRange {
        low: 0xa3e,
        high: 0xa40,
    },
    XmlChSRange {
        low: 0xa83,
        high: 0xa83,
    },
    XmlChSRange {
        low: 0xabe,
        high: 0xac0,
    },
    XmlChSRange {
        low: 0xac9,
        high: 0xac9,
    },
    XmlChSRange {
        low: 0xacb,
        high: 0xacc,
    },
    XmlChSRange {
        low: 0xb02,
        high: 0xb03,
    },
    XmlChSRange {
        low: 0xb3e,
        high: 0xb3e,
    },
    XmlChSRange {
        low: 0xb40,
        high: 0xb40,
    },
    XmlChSRange {
        low: 0xb47,
        high: 0xb48,
    },
    XmlChSRange {
        low: 0xb4b,
        high: 0xb4c,
    },
    XmlChSRange {
        low: 0xb57,
        high: 0xb57,
    },
    XmlChSRange {
        low: 0xbbe,
        high: 0xbbf,
    },
    XmlChSRange {
        low: 0xbc1,
        high: 0xbc2,
    },
    XmlChSRange {
        low: 0xbc6,
        high: 0xbc8,
    },
    XmlChSRange {
        low: 0xbca,
        high: 0xbcc,
    },
    XmlChSRange {
        low: 0xbd7,
        high: 0xbd7,
    },
    XmlChSRange {
        low: 0xc01,
        high: 0xc03,
    },
    XmlChSRange {
        low: 0xc41,
        high: 0xc44,
    },
    XmlChSRange {
        low: 0xc82,
        high: 0xc83,
    },
    XmlChSRange {
        low: 0xcbe,
        high: 0xcbe,
    },
    XmlChSRange {
        low: 0xcc0,
        high: 0xcc4,
    },
    XmlChSRange {
        low: 0xcc7,
        high: 0xcc8,
    },
    XmlChSRange {
        low: 0xcca,
        high: 0xccb,
    },
    XmlChSRange {
        low: 0xcd5,
        high: 0xcd6,
    },
    XmlChSRange {
        low: 0xd02,
        high: 0xd03,
    },
    XmlChSRange {
        low: 0xd3e,
        high: 0xd40,
    },
    XmlChSRange {
        low: 0xd46,
        high: 0xd48,
    },
    XmlChSRange {
        low: 0xd4a,
        high: 0xd4c,
    },
    XmlChSRange {
        low: 0xd57,
        high: 0xd57,
    },
    XmlChSRange {
        low: 0xd82,
        high: 0xd83,
    },
    XmlChSRange {
        low: 0xdcf,
        high: 0xdd1,
    },
    XmlChSRange {
        low: 0xdd8,
        high: 0xddf,
    },
    XmlChSRange {
        low: 0xdf2,
        high: 0xdf3,
    },
    XmlChSRange {
        low: 0xf3e,
        high: 0xf3f,
    },
    XmlChSRange {
        low: 0xf7f,
        high: 0xf7f,
    },
    XmlChSRange {
        low: 0x102c,
        high: 0x102c,
    },
    XmlChSRange {
        low: 0x1031,
        high: 0x1031,
    },
    XmlChSRange {
        low: 0x1038,
        high: 0x1038,
    },
    XmlChSRange {
        low: 0x1056,
        high: 0x1057,
    },
    XmlChSRange {
        low: 0x17b6,
        high: 0x17b6,
    },
    XmlChSRange {
        low: 0x17be,
        high: 0x17c5,
    },
    XmlChSRange {
        low: 0x17c7,
        high: 0x17c8,
    },
    XmlChSRange {
        low: 0x1923,
        high: 0x1926,
    },
    XmlChSRange {
        low: 0x1929,
        high: 0x192b,
    },
    XmlChSRange {
        low: 0x1930,
        high: 0x1931,
    },
    XmlChSRange {
        low: 0x1933,
        high: 0x1938,
    },
];

const XML_MC_L: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x1d165,
        high: 0x1d166,
    },
    XmlChLRange {
        low: 0x1d16d,
        high: 0x1d172,
    },
];

const XML_MC_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 55,
    nb_long_range: 2,
    short_range: XML_MC_S,
    long_range: XML_MC_L,
};

const XML_MN_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x300,
        high: 0x357,
    },
    XmlChSRange {
        low: 0x35d,
        high: 0x36f,
    },
    XmlChSRange {
        low: 0x483,
        high: 0x486,
    },
    XmlChSRange {
        low: 0x591,
        high: 0x5a1,
    },
    XmlChSRange {
        low: 0x5a3,
        high: 0x5b9,
    },
    XmlChSRange {
        low: 0x5bb,
        high: 0x5bd,
    },
    XmlChSRange {
        low: 0x5bf,
        high: 0x5bf,
    },
    XmlChSRange {
        low: 0x5c1,
        high: 0x5c2,
    },
    XmlChSRange {
        low: 0x5c4,
        high: 0x5c4,
    },
    XmlChSRange {
        low: 0x610,
        high: 0x615,
    },
    XmlChSRange {
        low: 0x64b,
        high: 0x658,
    },
    XmlChSRange {
        low: 0x670,
        high: 0x670,
    },
    XmlChSRange {
        low: 0x6d6,
        high: 0x6dc,
    },
    XmlChSRange {
        low: 0x6df,
        high: 0x6e4,
    },
    XmlChSRange {
        low: 0x6e7,
        high: 0x6e8,
    },
    XmlChSRange {
        low: 0x6ea,
        high: 0x6ed,
    },
    XmlChSRange {
        low: 0x711,
        high: 0x711,
    },
    XmlChSRange {
        low: 0x730,
        high: 0x74a,
    },
    XmlChSRange {
        low: 0x7a6,
        high: 0x7b0,
    },
    XmlChSRange {
        low: 0x901,
        high: 0x902,
    },
    XmlChSRange {
        low: 0x93c,
        high: 0x93c,
    },
    XmlChSRange {
        low: 0x941,
        high: 0x948,
    },
    XmlChSRange {
        low: 0x94d,
        high: 0x94d,
    },
    XmlChSRange {
        low: 0x951,
        high: 0x954,
    },
    XmlChSRange {
        low: 0x962,
        high: 0x963,
    },
    XmlChSRange {
        low: 0x981,
        high: 0x981,
    },
    XmlChSRange {
        low: 0x9bc,
        high: 0x9bc,
    },
    XmlChSRange {
        low: 0x9c1,
        high: 0x9c4,
    },
    XmlChSRange {
        low: 0x9cd,
        high: 0x9cd,
    },
    XmlChSRange {
        low: 0x9e2,
        high: 0x9e3,
    },
    XmlChSRange {
        low: 0xa01,
        high: 0xa02,
    },
    XmlChSRange {
        low: 0xa3c,
        high: 0xa3c,
    },
    XmlChSRange {
        low: 0xa41,
        high: 0xa42,
    },
    XmlChSRange {
        low: 0xa47,
        high: 0xa48,
    },
    XmlChSRange {
        low: 0xa4b,
        high: 0xa4d,
    },
    XmlChSRange {
        low: 0xa70,
        high: 0xa71,
    },
    XmlChSRange {
        low: 0xa81,
        high: 0xa82,
    },
    XmlChSRange {
        low: 0xabc,
        high: 0xabc,
    },
    XmlChSRange {
        low: 0xac1,
        high: 0xac5,
    },
    XmlChSRange {
        low: 0xac7,
        high: 0xac8,
    },
    XmlChSRange {
        low: 0xacd,
        high: 0xacd,
    },
    XmlChSRange {
        low: 0xae2,
        high: 0xae3,
    },
    XmlChSRange {
        low: 0xb01,
        high: 0xb01,
    },
    XmlChSRange {
        low: 0xb3c,
        high: 0xb3c,
    },
    XmlChSRange {
        low: 0xb3f,
        high: 0xb3f,
    },
    XmlChSRange {
        low: 0xb41,
        high: 0xb43,
    },
    XmlChSRange {
        low: 0xb4d,
        high: 0xb4d,
    },
    XmlChSRange {
        low: 0xb56,
        high: 0xb56,
    },
    XmlChSRange {
        low: 0xb82,
        high: 0xb82,
    },
    XmlChSRange {
        low: 0xbc0,
        high: 0xbc0,
    },
    XmlChSRange {
        low: 0xbcd,
        high: 0xbcd,
    },
    XmlChSRange {
        low: 0xc3e,
        high: 0xc40,
    },
    XmlChSRange {
        low: 0xc46,
        high: 0xc48,
    },
    XmlChSRange {
        low: 0xc4a,
        high: 0xc4d,
    },
    XmlChSRange {
        low: 0xc55,
        high: 0xc56,
    },
    XmlChSRange {
        low: 0xcbc,
        high: 0xcbc,
    },
    XmlChSRange {
        low: 0xcbf,
        high: 0xcbf,
    },
    XmlChSRange {
        low: 0xcc6,
        high: 0xcc6,
    },
    XmlChSRange {
        low: 0xccc,
        high: 0xccd,
    },
    XmlChSRange {
        low: 0xd41,
        high: 0xd43,
    },
    XmlChSRange {
        low: 0xd4d,
        high: 0xd4d,
    },
    XmlChSRange {
        low: 0xdca,
        high: 0xdca,
    },
    XmlChSRange {
        low: 0xdd2,
        high: 0xdd4,
    },
    XmlChSRange {
        low: 0xdd6,
        high: 0xdd6,
    },
    XmlChSRange {
        low: 0xe31,
        high: 0xe31,
    },
    XmlChSRange {
        low: 0xe34,
        high: 0xe3a,
    },
    XmlChSRange {
        low: 0xe47,
        high: 0xe4e,
    },
    XmlChSRange {
        low: 0xeb1,
        high: 0xeb1,
    },
    XmlChSRange {
        low: 0xeb4,
        high: 0xeb9,
    },
    XmlChSRange {
        low: 0xebb,
        high: 0xebc,
    },
    XmlChSRange {
        low: 0xec8,
        high: 0xecd,
    },
    XmlChSRange {
        low: 0xf18,
        high: 0xf19,
    },
    XmlChSRange {
        low: 0xf35,
        high: 0xf35,
    },
    XmlChSRange {
        low: 0xf37,
        high: 0xf37,
    },
    XmlChSRange {
        low: 0xf39,
        high: 0xf39,
    },
    XmlChSRange {
        low: 0xf71,
        high: 0xf7e,
    },
    XmlChSRange {
        low: 0xf80,
        high: 0xf84,
    },
    XmlChSRange {
        low: 0xf86,
        high: 0xf87,
    },
    XmlChSRange {
        low: 0xf90,
        high: 0xf97,
    },
    XmlChSRange {
        low: 0xf99,
        high: 0xfbc,
    },
    XmlChSRange {
        low: 0xfc6,
        high: 0xfc6,
    },
    XmlChSRange {
        low: 0x102d,
        high: 0x1030,
    },
    XmlChSRange {
        low: 0x1032,
        high: 0x1032,
    },
    XmlChSRange {
        low: 0x1036,
        high: 0x1037,
    },
    XmlChSRange {
        low: 0x1039,
        high: 0x1039,
    },
    XmlChSRange {
        low: 0x1058,
        high: 0x1059,
    },
    XmlChSRange {
        low: 0x1712,
        high: 0x1714,
    },
    XmlChSRange {
        low: 0x1732,
        high: 0x1734,
    },
    XmlChSRange {
        low: 0x1752,
        high: 0x1753,
    },
    XmlChSRange {
        low: 0x1772,
        high: 0x1773,
    },
    XmlChSRange {
        low: 0x17b7,
        high: 0x17bd,
    },
    XmlChSRange {
        low: 0x17c6,
        high: 0x17c6,
    },
    XmlChSRange {
        low: 0x17c9,
        high: 0x17d3,
    },
    XmlChSRange {
        low: 0x17dd,
        high: 0x17dd,
    },
    XmlChSRange {
        low: 0x180b,
        high: 0x180d,
    },
    XmlChSRange {
        low: 0x18a9,
        high: 0x18a9,
    },
    XmlChSRange {
        low: 0x1920,
        high: 0x1922,
    },
    XmlChSRange {
        low: 0x1927,
        high: 0x1928,
    },
    XmlChSRange {
        low: 0x1932,
        high: 0x1932,
    },
    XmlChSRange {
        low: 0x1939,
        high: 0x193b,
    },
    XmlChSRange {
        low: 0x20d0,
        high: 0x20dc,
    },
    XmlChSRange {
        low: 0x20e1,
        high: 0x20e1,
    },
    XmlChSRange {
        low: 0x20e5,
        high: 0x20ea,
    },
    XmlChSRange {
        low: 0x302a,
        high: 0x302f,
    },
    XmlChSRange {
        low: 0x3099,
        high: 0x309a,
    },
    XmlChSRange {
        low: 0xfb1e,
        high: 0xfb1e,
    },
    XmlChSRange {
        low: 0xfe00,
        high: 0xfe0f,
    },
    XmlChSRange {
        low: 0xfe20,
        high: 0xfe23,
    },
];

const XML_MN_L: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x1d167,
        high: 0x1d169,
    },
    XmlChLRange {
        low: 0x1d17b,
        high: 0x1d182,
    },
    XmlChLRange {
        low: 0x1d185,
        high: 0x1d18b,
    },
    XmlChLRange {
        low: 0x1d1aa,
        high: 0x1d1ad,
    },
    XmlChLRange {
        low: 0xe0100,
        high: 0xe01ef,
    },
];

const XML_MN_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 108,
    nb_long_range: 5,
    short_range: XML_MN_S,
    long_range: XML_MN_L,
};

const XML_NS: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x30,
        high: 0x39,
    },
    XmlChSRange {
        low: 0xb2,
        high: 0xb3,
    },
    XmlChSRange {
        low: 0xb9,
        high: 0xb9,
    },
    XmlChSRange {
        low: 0xbc,
        high: 0xbe,
    },
    XmlChSRange {
        low: 0x660,
        high: 0x669,
    },
    XmlChSRange {
        low: 0x6f0,
        high: 0x6f9,
    },
    XmlChSRange {
        low: 0x966,
        high: 0x96f,
    },
    XmlChSRange {
        low: 0x9e6,
        high: 0x9ef,
    },
    XmlChSRange {
        low: 0x9f4,
        high: 0x9f9,
    },
    XmlChSRange {
        low: 0xa66,
        high: 0xa6f,
    },
    XmlChSRange {
        low: 0xae6,
        high: 0xaef,
    },
    XmlChSRange {
        low: 0xb66,
        high: 0xb6f,
    },
    XmlChSRange {
        low: 0xbe7,
        high: 0xbf2,
    },
    XmlChSRange {
        low: 0xc66,
        high: 0xc6f,
    },
    XmlChSRange {
        low: 0xce6,
        high: 0xcef,
    },
    XmlChSRange {
        low: 0xd66,
        high: 0xd6f,
    },
    XmlChSRange {
        low: 0xe50,
        high: 0xe59,
    },
    XmlChSRange {
        low: 0xed0,
        high: 0xed9,
    },
    XmlChSRange {
        low: 0xf20,
        high: 0xf33,
    },
    XmlChSRange {
        low: 0x1040,
        high: 0x1049,
    },
    XmlChSRange {
        low: 0x1369,
        high: 0x137c,
    },
    XmlChSRange {
        low: 0x16ee,
        high: 0x16f0,
    },
    XmlChSRange {
        low: 0x17e0,
        high: 0x17e9,
    },
    XmlChSRange {
        low: 0x17f0,
        high: 0x17f9,
    },
    XmlChSRange {
        low: 0x1810,
        high: 0x1819,
    },
    XmlChSRange {
        low: 0x1946,
        high: 0x194f,
    },
    XmlChSRange {
        low: 0x2070,
        high: 0x2070,
    },
    XmlChSRange {
        low: 0x2074,
        high: 0x2079,
    },
    XmlChSRange {
        low: 0x2080,
        high: 0x2089,
    },
    XmlChSRange {
        low: 0x2153,
        high: 0x2183,
    },
    XmlChSRange {
        low: 0x2460,
        high: 0x249b,
    },
    XmlChSRange {
        low: 0x24ea,
        high: 0x24ff,
    },
    XmlChSRange {
        low: 0x2776,
        high: 0x2793,
    },
    XmlChSRange {
        low: 0x3007,
        high: 0x3007,
    },
    XmlChSRange {
        low: 0x3021,
        high: 0x3029,
    },
    XmlChSRange {
        low: 0x3038,
        high: 0x303a,
    },
    XmlChSRange {
        low: 0x3192,
        high: 0x3195,
    },
    XmlChSRange {
        low: 0x3220,
        high: 0x3229,
    },
    XmlChSRange {
        low: 0x3251,
        high: 0x325f,
    },
    XmlChSRange {
        low: 0x3280,
        high: 0x3289,
    },
    XmlChSRange {
        low: 0x32b1,
        high: 0x32bf,
    },
    XmlChSRange {
        low: 0xff10,
        high: 0xff19,
    },
];

const XML_NL: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x10107,
        high: 0x10133,
    },
    XmlChLRange {
        low: 0x10320,
        high: 0x10323,
    },
    XmlChLRange {
        low: 0x1034a,
        high: 0x1034a,
    },
    XmlChLRange {
        low: 0x104a0,
        high: 0x104a9,
    },
    XmlChLRange {
        low: 0x1d7ce,
        high: 0x1d7ff,
    },
];

const XML_NG: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 42,
    nb_long_range: 5,
    short_range: XML_NS,
    long_range: XML_NL,
};

const XML_ND_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x30,
        high: 0x39,
    },
    XmlChSRange {
        low: 0x660,
        high: 0x669,
    },
    XmlChSRange {
        low: 0x6f0,
        high: 0x6f9,
    },
    XmlChSRange {
        low: 0x966,
        high: 0x96f,
    },
    XmlChSRange {
        low: 0x9e6,
        high: 0x9ef,
    },
    XmlChSRange {
        low: 0xa66,
        high: 0xa6f,
    },
    XmlChSRange {
        low: 0xae6,
        high: 0xaef,
    },
    XmlChSRange {
        low: 0xb66,
        high: 0xb6f,
    },
    XmlChSRange {
        low: 0xbe7,
        high: 0xbef,
    },
    XmlChSRange {
        low: 0xc66,
        high: 0xc6f,
    },
    XmlChSRange {
        low: 0xce6,
        high: 0xcef,
    },
    XmlChSRange {
        low: 0xd66,
        high: 0xd6f,
    },
    XmlChSRange {
        low: 0xe50,
        high: 0xe59,
    },
    XmlChSRange {
        low: 0xed0,
        high: 0xed9,
    },
    XmlChSRange {
        low: 0xf20,
        high: 0xf29,
    },
    XmlChSRange {
        low: 0x1040,
        high: 0x1049,
    },
    XmlChSRange {
        low: 0x1369,
        high: 0x1371,
    },
    XmlChSRange {
        low: 0x17e0,
        high: 0x17e9,
    },
    XmlChSRange {
        low: 0x1810,
        high: 0x1819,
    },
    XmlChSRange {
        low: 0x1946,
        high: 0x194f,
    },
    XmlChSRange {
        low: 0xff10,
        high: 0xff19,
    },
];

const XML_ND_L: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x104a0,
        high: 0x104a9,
    },
    XmlChLRange {
        low: 0x1d7ce,
        high: 0x1d7ff,
    },
];

const XML_ND_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 21,
    nb_long_range: 2,
    short_range: XML_ND_S,
    long_range: XML_ND_L,
};

const XML_NO_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0xb2,
        high: 0xb3,
    },
    XmlChSRange {
        low: 0xb9,
        high: 0xb9,
    },
    XmlChSRange {
        low: 0xbc,
        high: 0xbe,
    },
    XmlChSRange {
        low: 0x9f4,
        high: 0x9f9,
    },
    XmlChSRange {
        low: 0xbf0,
        high: 0xbf2,
    },
    XmlChSRange {
        low: 0xf2a,
        high: 0xf33,
    },
    XmlChSRange {
        low: 0x1372,
        high: 0x137c,
    },
    XmlChSRange {
        low: 0x17f0,
        high: 0x17f9,
    },
    XmlChSRange {
        low: 0x2070,
        high: 0x2070,
    },
    XmlChSRange {
        low: 0x2074,
        high: 0x2079,
    },
    XmlChSRange {
        low: 0x2080,
        high: 0x2089,
    },
    XmlChSRange {
        low: 0x2153,
        high: 0x215f,
    },
    XmlChSRange {
        low: 0x2460,
        high: 0x249b,
    },
    XmlChSRange {
        low: 0x24ea,
        high: 0x24ff,
    },
    XmlChSRange {
        low: 0x2776,
        high: 0x2793,
    },
    XmlChSRange {
        low: 0x3192,
        high: 0x3195,
    },
    XmlChSRange {
        low: 0x3220,
        high: 0x3229,
    },
    XmlChSRange {
        low: 0x3251,
        high: 0x325f,
    },
    XmlChSRange {
        low: 0x3280,
        high: 0x3289,
    },
    XmlChSRange {
        low: 0x32b1,
        high: 0x32bf,
    },
];

const XML_NO_L: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x10107,
        high: 0x10133,
    },
    XmlChLRange {
        low: 0x10320,
        high: 0x10323,
    },
];

const XML_NO_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 20,
    nb_long_range: 2,
    short_range: XML_NO_S,
    long_range: XML_NO_L,
};

const XML_PS: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x21,
        high: 0x23,
    },
    XmlChSRange {
        low: 0x25,
        high: 0x2a,
    },
    XmlChSRange {
        low: 0x2c,
        high: 0x2f,
    },
    XmlChSRange {
        low: 0x3a,
        high: 0x3b,
    },
    XmlChSRange {
        low: 0x3f,
        high: 0x40,
    },
    XmlChSRange {
        low: 0x5b,
        high: 0x5d,
    },
    XmlChSRange {
        low: 0x5f,
        high: 0x5f,
    },
    XmlChSRange {
        low: 0x7b,
        high: 0x7b,
    },
    XmlChSRange {
        low: 0x7d,
        high: 0x7d,
    },
    XmlChSRange {
        low: 0xa1,
        high: 0xa1,
    },
    XmlChSRange {
        low: 0xab,
        high: 0xab,
    },
    XmlChSRange {
        low: 0xb7,
        high: 0xb7,
    },
    XmlChSRange {
        low: 0xbb,
        high: 0xbb,
    },
    XmlChSRange {
        low: 0xbf,
        high: 0xbf,
    },
    XmlChSRange {
        low: 0x37e,
        high: 0x37e,
    },
    XmlChSRange {
        low: 0x387,
        high: 0x387,
    },
    XmlChSRange {
        low: 0x55a,
        high: 0x55f,
    },
    XmlChSRange {
        low: 0x589,
        high: 0x58a,
    },
    XmlChSRange {
        low: 0x5be,
        high: 0x5be,
    },
    XmlChSRange {
        low: 0x5c0,
        high: 0x5c0,
    },
    XmlChSRange {
        low: 0x5c3,
        high: 0x5c3,
    },
    XmlChSRange {
        low: 0x5f3,
        high: 0x5f4,
    },
    XmlChSRange {
        low: 0x60c,
        high: 0x60d,
    },
    XmlChSRange {
        low: 0x61b,
        high: 0x61b,
    },
    XmlChSRange {
        low: 0x61f,
        high: 0x61f,
    },
    XmlChSRange {
        low: 0x66a,
        high: 0x66d,
    },
    XmlChSRange {
        low: 0x6d4,
        high: 0x6d4,
    },
    XmlChSRange {
        low: 0x700,
        high: 0x70d,
    },
    XmlChSRange {
        low: 0x964,
        high: 0x965,
    },
    XmlChSRange {
        low: 0x970,
        high: 0x970,
    },
    XmlChSRange {
        low: 0xdf4,
        high: 0xdf4,
    },
    XmlChSRange {
        low: 0xe4f,
        high: 0xe4f,
    },
    XmlChSRange {
        low: 0xe5a,
        high: 0xe5b,
    },
    XmlChSRange {
        low: 0xf04,
        high: 0xf12,
    },
    XmlChSRange {
        low: 0xf3a,
        high: 0xf3d,
    },
    XmlChSRange {
        low: 0xf85,
        high: 0xf85,
    },
    XmlChSRange {
        low: 0x104a,
        high: 0x104f,
    },
    XmlChSRange {
        low: 0x10fb,
        high: 0x10fb,
    },
    XmlChSRange {
        low: 0x1361,
        high: 0x1368,
    },
    XmlChSRange {
        low: 0x166d,
        high: 0x166e,
    },
    XmlChSRange {
        low: 0x169b,
        high: 0x169c,
    },
    XmlChSRange {
        low: 0x16eb,
        high: 0x16ed,
    },
    XmlChSRange {
        low: 0x1735,
        high: 0x1736,
    },
    XmlChSRange {
        low: 0x17d4,
        high: 0x17d6,
    },
    XmlChSRange {
        low: 0x17d8,
        high: 0x17da,
    },
    XmlChSRange {
        low: 0x1800,
        high: 0x180a,
    },
    XmlChSRange {
        low: 0x1944,
        high: 0x1945,
    },
    XmlChSRange {
        low: 0x2010,
        high: 0x2027,
    },
    XmlChSRange {
        low: 0x2030,
        high: 0x2043,
    },
    XmlChSRange {
        low: 0x2045,
        high: 0x2051,
    },
    XmlChSRange {
        low: 0x2053,
        high: 0x2054,
    },
    XmlChSRange {
        low: 0x2057,
        high: 0x2057,
    },
    XmlChSRange {
        low: 0x207d,
        high: 0x207e,
    },
    XmlChSRange {
        low: 0x208d,
        high: 0x208e,
    },
    XmlChSRange {
        low: 0x2329,
        high: 0x232a,
    },
    XmlChSRange {
        low: 0x23b4,
        high: 0x23b6,
    },
    XmlChSRange {
        low: 0x2768,
        high: 0x2775,
    },
    XmlChSRange {
        low: 0x27e6,
        high: 0x27eb,
    },
    XmlChSRange {
        low: 0x2983,
        high: 0x2998,
    },
    XmlChSRange {
        low: 0x29d8,
        high: 0x29db,
    },
    XmlChSRange {
        low: 0x29fc,
        high: 0x29fd,
    },
    XmlChSRange {
        low: 0x3001,
        high: 0x3003,
    },
    XmlChSRange {
        low: 0x3008,
        high: 0x3011,
    },
    XmlChSRange {
        low: 0x3014,
        high: 0x301f,
    },
    XmlChSRange {
        low: 0x3030,
        high: 0x3030,
    },
    XmlChSRange {
        low: 0x303d,
        high: 0x303d,
    },
    XmlChSRange {
        low: 0x30a0,
        high: 0x30a0,
    },
    XmlChSRange {
        low: 0x30fb,
        high: 0x30fb,
    },
    XmlChSRange {
        low: 0xfd3e,
        high: 0xfd3f,
    },
    XmlChSRange {
        low: 0xfe30,
        high: 0xfe52,
    },
    XmlChSRange {
        low: 0xfe54,
        high: 0xfe61,
    },
    XmlChSRange {
        low: 0xfe63,
        high: 0xfe63,
    },
    XmlChSRange {
        low: 0xfe68,
        high: 0xfe68,
    },
    XmlChSRange {
        low: 0xfe6a,
        high: 0xfe6b,
    },
    XmlChSRange {
        low: 0xff01,
        high: 0xff03,
    },
    XmlChSRange {
        low: 0xff05,
        high: 0xff0a,
    },
    XmlChSRange {
        low: 0xff0c,
        high: 0xff0f,
    },
    XmlChSRange {
        low: 0xff1a,
        high: 0xff1b,
    },
    XmlChSRange {
        low: 0xff1f,
        high: 0xff20,
    },
    XmlChSRange {
        low: 0xff3b,
        high: 0xff3d,
    },
    XmlChSRange {
        low: 0xff3f,
        high: 0xff3f,
    },
    XmlChSRange {
        low: 0xff5b,
        high: 0xff5b,
    },
    XmlChSRange {
        low: 0xff5d,
        high: 0xff5d,
    },
    XmlChSRange {
        low: 0xff5f,
        high: 0xff65,
    },
];

const XML_PL: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x10100,
        high: 0x10101,
    },
    XmlChLRange {
        low: 0x1039f,
        high: 0x1039f,
    },
];

const XML_PG: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 84,
    nb_long_range: 2,
    short_range: XML_PS,
    long_range: XML_PL,
};

const XML_PD_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x2d,
        high: 0x2d,
    },
    XmlChSRange {
        low: 0x58a,
        high: 0x58a,
    },
    XmlChSRange {
        low: 0x1806,
        high: 0x1806,
    },
    XmlChSRange {
        low: 0x2010,
        high: 0x2015,
    },
    XmlChSRange {
        low: 0x301c,
        high: 0x301c,
    },
    XmlChSRange {
        low: 0x3030,
        high: 0x3030,
    },
    XmlChSRange {
        low: 0x30a0,
        high: 0x30a0,
    },
    XmlChSRange {
        low: 0xfe31,
        high: 0xfe32,
    },
    XmlChSRange {
        low: 0xfe58,
        high: 0xfe58,
    },
    XmlChSRange {
        low: 0xfe63,
        high: 0xfe63,
    },
    XmlChSRange {
        low: 0xff0d,
        high: 0xff0d,
    },
];

const XML_PD_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 11,
    nb_long_range: 0,
    short_range: XML_PD_S,
    long_range: &[],
};

const XML_PE_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x29,
        high: 0x29,
    },
    XmlChSRange {
        low: 0x5d,
        high: 0x5d,
    },
    XmlChSRange {
        low: 0x7d,
        high: 0x7d,
    },
    XmlChSRange {
        low: 0xf3b,
        high: 0xf3b,
    },
    XmlChSRange {
        low: 0xf3d,
        high: 0xf3d,
    },
    XmlChSRange {
        low: 0x169c,
        high: 0x169c,
    },
    XmlChSRange {
        low: 0x2046,
        high: 0x2046,
    },
    XmlChSRange {
        low: 0x207e,
        high: 0x207e,
    },
    XmlChSRange {
        low: 0x208e,
        high: 0x208e,
    },
    XmlChSRange {
        low: 0x232a,
        high: 0x232a,
    },
    XmlChSRange {
        low: 0x23b5,
        high: 0x23b5,
    },
    XmlChSRange {
        low: 0x2769,
        high: 0x2769,
    },
    XmlChSRange {
        low: 0x276b,
        high: 0x276b,
    },
    XmlChSRange {
        low: 0x276d,
        high: 0x276d,
    },
    XmlChSRange {
        low: 0x276f,
        high: 0x276f,
    },
    XmlChSRange {
        low: 0x2771,
        high: 0x2771,
    },
    XmlChSRange {
        low: 0x2773,
        high: 0x2773,
    },
    XmlChSRange {
        low: 0x2775,
        high: 0x2775,
    },
    XmlChSRange {
        low: 0x27e7,
        high: 0x27e7,
    },
    XmlChSRange {
        low: 0x27e9,
        high: 0x27e9,
    },
    XmlChSRange {
        low: 0x27eb,
        high: 0x27eb,
    },
    XmlChSRange {
        low: 0x2984,
        high: 0x2984,
    },
    XmlChSRange {
        low: 0x2986,
        high: 0x2986,
    },
    XmlChSRange {
        low: 0x2988,
        high: 0x2988,
    },
    XmlChSRange {
        low: 0x298a,
        high: 0x298a,
    },
    XmlChSRange {
        low: 0x298c,
        high: 0x298c,
    },
    XmlChSRange {
        low: 0x298e,
        high: 0x298e,
    },
    XmlChSRange {
        low: 0x2990,
        high: 0x2990,
    },
    XmlChSRange {
        low: 0x2992,
        high: 0x2992,
    },
    XmlChSRange {
        low: 0x2994,
        high: 0x2994,
    },
    XmlChSRange {
        low: 0x2996,
        high: 0x2996,
    },
    XmlChSRange {
        low: 0x2998,
        high: 0x2998,
    },
    XmlChSRange {
        low: 0x29d9,
        high: 0x29d9,
    },
    XmlChSRange {
        low: 0x29db,
        high: 0x29db,
    },
    XmlChSRange {
        low: 0x29fd,
        high: 0x29fd,
    },
    XmlChSRange {
        low: 0x3009,
        high: 0x3009,
    },
    XmlChSRange {
        low: 0x300b,
        high: 0x300b,
    },
    XmlChSRange {
        low: 0x300d,
        high: 0x300d,
    },
    XmlChSRange {
        low: 0x300f,
        high: 0x300f,
    },
    XmlChSRange {
        low: 0x3011,
        high: 0x3011,
    },
    XmlChSRange {
        low: 0x3015,
        high: 0x3015,
    },
    XmlChSRange {
        low: 0x3017,
        high: 0x3017,
    },
    XmlChSRange {
        low: 0x3019,
        high: 0x3019,
    },
    XmlChSRange {
        low: 0x301b,
        high: 0x301b,
    },
    XmlChSRange {
        low: 0x301e,
        high: 0x301f,
    },
    XmlChSRange {
        low: 0xfd3f,
        high: 0xfd3f,
    },
    XmlChSRange {
        low: 0xfe36,
        high: 0xfe36,
    },
    XmlChSRange {
        low: 0xfe38,
        high: 0xfe38,
    },
    XmlChSRange {
        low: 0xfe3a,
        high: 0xfe3a,
    },
    XmlChSRange {
        low: 0xfe3c,
        high: 0xfe3c,
    },
    XmlChSRange {
        low: 0xfe3e,
        high: 0xfe3e,
    },
    XmlChSRange {
        low: 0xfe40,
        high: 0xfe40,
    },
    XmlChSRange {
        low: 0xfe42,
        high: 0xfe42,
    },
    XmlChSRange {
        low: 0xfe44,
        high: 0xfe44,
    },
    XmlChSRange {
        low: 0xfe48,
        high: 0xfe48,
    },
    XmlChSRange {
        low: 0xfe5a,
        high: 0xfe5a,
    },
    XmlChSRange {
        low: 0xfe5c,
        high: 0xfe5c,
    },
    XmlChSRange {
        low: 0xfe5e,
        high: 0xfe5e,
    },
    XmlChSRange {
        low: 0xff09,
        high: 0xff09,
    },
    XmlChSRange {
        low: 0xff3d,
        high: 0xff3d,
    },
    XmlChSRange {
        low: 0xff5d,
        high: 0xff5d,
    },
    XmlChSRange {
        low: 0xff60,
        high: 0xff60,
    },
    XmlChSRange {
        low: 0xff63,
        high: 0xff63,
    },
];

const XML_PE_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 63,
    nb_long_range: 0,
    short_range: XML_PE_S,
    long_range: &[],
};

const XML_PO_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x21,
        high: 0x23,
    },
    XmlChSRange {
        low: 0x25,
        high: 0x27,
    },
    XmlChSRange {
        low: 0x2a,
        high: 0x2a,
    },
    XmlChSRange {
        low: 0x2c,
        high: 0x2c,
    },
    XmlChSRange {
        low: 0x2e,
        high: 0x2f,
    },
    XmlChSRange {
        low: 0x3a,
        high: 0x3b,
    },
    XmlChSRange {
        low: 0x3f,
        high: 0x40,
    },
    XmlChSRange {
        low: 0x5c,
        high: 0x5c,
    },
    XmlChSRange {
        low: 0xa1,
        high: 0xa1,
    },
    XmlChSRange {
        low: 0xb7,
        high: 0xb7,
    },
    XmlChSRange {
        low: 0xbf,
        high: 0xbf,
    },
    XmlChSRange {
        low: 0x37e,
        high: 0x37e,
    },
    XmlChSRange {
        low: 0x387,
        high: 0x387,
    },
    XmlChSRange {
        low: 0x55a,
        high: 0x55f,
    },
    XmlChSRange {
        low: 0x589,
        high: 0x589,
    },
    XmlChSRange {
        low: 0x5be,
        high: 0x5be,
    },
    XmlChSRange {
        low: 0x5c0,
        high: 0x5c0,
    },
    XmlChSRange {
        low: 0x5c3,
        high: 0x5c3,
    },
    XmlChSRange {
        low: 0x5f3,
        high: 0x5f4,
    },
    XmlChSRange {
        low: 0x60c,
        high: 0x60d,
    },
    XmlChSRange {
        low: 0x61b,
        high: 0x61b,
    },
    XmlChSRange {
        low: 0x61f,
        high: 0x61f,
    },
    XmlChSRange {
        low: 0x66a,
        high: 0x66d,
    },
    XmlChSRange {
        low: 0x6d4,
        high: 0x6d4,
    },
    XmlChSRange {
        low: 0x700,
        high: 0x70d,
    },
    XmlChSRange {
        low: 0x964,
        high: 0x965,
    },
    XmlChSRange {
        low: 0x970,
        high: 0x970,
    },
    XmlChSRange {
        low: 0xdf4,
        high: 0xdf4,
    },
    XmlChSRange {
        low: 0xe4f,
        high: 0xe4f,
    },
    XmlChSRange {
        low: 0xe5a,
        high: 0xe5b,
    },
    XmlChSRange {
        low: 0xf04,
        high: 0xf12,
    },
    XmlChSRange {
        low: 0xf85,
        high: 0xf85,
    },
    XmlChSRange {
        low: 0x104a,
        high: 0x104f,
    },
    XmlChSRange {
        low: 0x10fb,
        high: 0x10fb,
    },
    XmlChSRange {
        low: 0x1361,
        high: 0x1368,
    },
    XmlChSRange {
        low: 0x166d,
        high: 0x166e,
    },
    XmlChSRange {
        low: 0x16eb,
        high: 0x16ed,
    },
    XmlChSRange {
        low: 0x1735,
        high: 0x1736,
    },
    XmlChSRange {
        low: 0x17d4,
        high: 0x17d6,
    },
    XmlChSRange {
        low: 0x17d8,
        high: 0x17da,
    },
    XmlChSRange {
        low: 0x1800,
        high: 0x1805,
    },
    XmlChSRange {
        low: 0x1807,
        high: 0x180a,
    },
    XmlChSRange {
        low: 0x1944,
        high: 0x1945,
    },
    XmlChSRange {
        low: 0x2016,
        high: 0x2017,
    },
    XmlChSRange {
        low: 0x2020,
        high: 0x2027,
    },
    XmlChSRange {
        low: 0x2030,
        high: 0x2038,
    },
    XmlChSRange {
        low: 0x203b,
        high: 0x203e,
    },
    XmlChSRange {
        low: 0x2041,
        high: 0x2043,
    },
    XmlChSRange {
        low: 0x2047,
        high: 0x2051,
    },
    XmlChSRange {
        low: 0x2053,
        high: 0x2053,
    },
    XmlChSRange {
        low: 0x2057,
        high: 0x2057,
    },
    XmlChSRange {
        low: 0x23b6,
        high: 0x23b6,
    },
    XmlChSRange {
        low: 0x3001,
        high: 0x3003,
    },
    XmlChSRange {
        low: 0x303d,
        high: 0x303d,
    },
    XmlChSRange {
        low: 0xfe30,
        high: 0xfe30,
    },
    XmlChSRange {
        low: 0xfe45,
        high: 0xfe46,
    },
    XmlChSRange {
        low: 0xfe49,
        high: 0xfe4c,
    },
    XmlChSRange {
        low: 0xfe50,
        high: 0xfe52,
    },
    XmlChSRange {
        low: 0xfe54,
        high: 0xfe57,
    },
    XmlChSRange {
        low: 0xfe5f,
        high: 0xfe61,
    },
    XmlChSRange {
        low: 0xfe68,
        high: 0xfe68,
    },
    XmlChSRange {
        low: 0xfe6a,
        high: 0xfe6b,
    },
    XmlChSRange {
        low: 0xff01,
        high: 0xff03,
    },
    XmlChSRange {
        low: 0xff05,
        high: 0xff07,
    },
    XmlChSRange {
        low: 0xff0a,
        high: 0xff0a,
    },
    XmlChSRange {
        low: 0xff0c,
        high: 0xff0c,
    },
    XmlChSRange {
        low: 0xff0e,
        high: 0xff0f,
    },
    XmlChSRange {
        low: 0xff1a,
        high: 0xff1b,
    },
    XmlChSRange {
        low: 0xff1f,
        high: 0xff20,
    },
    XmlChSRange {
        low: 0xff3c,
        high: 0xff3c,
    },
    XmlChSRange {
        low: 0xff61,
        high: 0xff61,
    },
    XmlChSRange {
        low: 0xff64,
        high: 0xff64,
    },
];

const XML_PO_L: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x10100,
        high: 0x10101,
    },
    XmlChLRange {
        low: 0x1039f,
        high: 0x1039f,
    },
];

const XML_PO_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 72,
    nb_long_range: 2,
    short_range: XML_PO_S,
    long_range: XML_PO_L,
};

const XML_PS_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x28,
        high: 0x28,
    },
    XmlChSRange {
        low: 0x5b,
        high: 0x5b,
    },
    XmlChSRange {
        low: 0x7b,
        high: 0x7b,
    },
    XmlChSRange {
        low: 0xf3a,
        high: 0xf3a,
    },
    XmlChSRange {
        low: 0xf3c,
        high: 0xf3c,
    },
    XmlChSRange {
        low: 0x169b,
        high: 0x169b,
    },
    XmlChSRange {
        low: 0x201a,
        high: 0x201a,
    },
    XmlChSRange {
        low: 0x201e,
        high: 0x201e,
    },
    XmlChSRange {
        low: 0x2045,
        high: 0x2045,
    },
    XmlChSRange {
        low: 0x207d,
        high: 0x207d,
    },
    XmlChSRange {
        low: 0x208d,
        high: 0x208d,
    },
    XmlChSRange {
        low: 0x2329,
        high: 0x2329,
    },
    XmlChSRange {
        low: 0x23b4,
        high: 0x23b4,
    },
    XmlChSRange {
        low: 0x2768,
        high: 0x2768,
    },
    XmlChSRange {
        low: 0x276a,
        high: 0x276a,
    },
    XmlChSRange {
        low: 0x276c,
        high: 0x276c,
    },
    XmlChSRange {
        low: 0x276e,
        high: 0x276e,
    },
    XmlChSRange {
        low: 0x2770,
        high: 0x2770,
    },
    XmlChSRange {
        low: 0x2772,
        high: 0x2772,
    },
    XmlChSRange {
        low: 0x2774,
        high: 0x2774,
    },
    XmlChSRange {
        low: 0x27e6,
        high: 0x27e6,
    },
    XmlChSRange {
        low: 0x27e8,
        high: 0x27e8,
    },
    XmlChSRange {
        low: 0x27ea,
        high: 0x27ea,
    },
    XmlChSRange {
        low: 0x2983,
        high: 0x2983,
    },
    XmlChSRange {
        low: 0x2985,
        high: 0x2985,
    },
    XmlChSRange {
        low: 0x2987,
        high: 0x2987,
    },
    XmlChSRange {
        low: 0x2989,
        high: 0x2989,
    },
    XmlChSRange {
        low: 0x298b,
        high: 0x298b,
    },
    XmlChSRange {
        low: 0x298d,
        high: 0x298d,
    },
    XmlChSRange {
        low: 0x298f,
        high: 0x298f,
    },
    XmlChSRange {
        low: 0x2991,
        high: 0x2991,
    },
    XmlChSRange {
        low: 0x2993,
        high: 0x2993,
    },
    XmlChSRange {
        low: 0x2995,
        high: 0x2995,
    },
    XmlChSRange {
        low: 0x2997,
        high: 0x2997,
    },
    XmlChSRange {
        low: 0x29d8,
        high: 0x29d8,
    },
    XmlChSRange {
        low: 0x29da,
        high: 0x29da,
    },
    XmlChSRange {
        low: 0x29fc,
        high: 0x29fc,
    },
    XmlChSRange {
        low: 0x3008,
        high: 0x3008,
    },
    XmlChSRange {
        low: 0x300a,
        high: 0x300a,
    },
    XmlChSRange {
        low: 0x300c,
        high: 0x300c,
    },
    XmlChSRange {
        low: 0x300e,
        high: 0x300e,
    },
    XmlChSRange {
        low: 0x3010,
        high: 0x3010,
    },
    XmlChSRange {
        low: 0x3014,
        high: 0x3014,
    },
    XmlChSRange {
        low: 0x3016,
        high: 0x3016,
    },
    XmlChSRange {
        low: 0x3018,
        high: 0x3018,
    },
    XmlChSRange {
        low: 0x301a,
        high: 0x301a,
    },
    XmlChSRange {
        low: 0x301d,
        high: 0x301d,
    },
    XmlChSRange {
        low: 0xfd3e,
        high: 0xfd3e,
    },
    XmlChSRange {
        low: 0xfe35,
        high: 0xfe35,
    },
    XmlChSRange {
        low: 0xfe37,
        high: 0xfe37,
    },
    XmlChSRange {
        low: 0xfe39,
        high: 0xfe39,
    },
    XmlChSRange {
        low: 0xfe3b,
        high: 0xfe3b,
    },
    XmlChSRange {
        low: 0xfe3d,
        high: 0xfe3d,
    },
    XmlChSRange {
        low: 0xfe3f,
        high: 0xfe3f,
    },
    XmlChSRange {
        low: 0xfe41,
        high: 0xfe41,
    },
    XmlChSRange {
        low: 0xfe43,
        high: 0xfe43,
    },
    XmlChSRange {
        low: 0xfe47,
        high: 0xfe47,
    },
    XmlChSRange {
        low: 0xfe59,
        high: 0xfe59,
    },
    XmlChSRange {
        low: 0xfe5b,
        high: 0xfe5b,
    },
    XmlChSRange {
        low: 0xfe5d,
        high: 0xfe5d,
    },
    XmlChSRange {
        low: 0xff08,
        high: 0xff08,
    },
    XmlChSRange {
        low: 0xff3b,
        high: 0xff3b,
    },
    XmlChSRange {
        low: 0xff5b,
        high: 0xff5b,
    },
    XmlChSRange {
        low: 0xff5f,
        high: 0xff5f,
    },
    XmlChSRange {
        low: 0xff62,
        high: 0xff62,
    },
];

const XML_PS_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 65,
    nb_long_range: 0,
    short_range: XML_PS_S,
    long_range: &[],
};

const XML_SS: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x24,
        high: 0x24,
    },
    XmlChSRange {
        low: 0x2b,
        high: 0x2b,
    },
    XmlChSRange {
        low: 0x3c,
        high: 0x3e,
    },
    XmlChSRange {
        low: 0x5e,
        high: 0x5e,
    },
    XmlChSRange {
        low: 0x60,
        high: 0x60,
    },
    XmlChSRange {
        low: 0x7c,
        high: 0x7c,
    },
    XmlChSRange {
        low: 0x7e,
        high: 0x7e,
    },
    XmlChSRange {
        low: 0xa2,
        high: 0xa9,
    },
    XmlChSRange {
        low: 0xac,
        high: 0xac,
    },
    XmlChSRange {
        low: 0xae,
        high: 0xb1,
    },
    XmlChSRange {
        low: 0xb4,
        high: 0xb4,
    },
    XmlChSRange {
        low: 0xb6,
        high: 0xb6,
    },
    XmlChSRange {
        low: 0xb8,
        high: 0xb8,
    },
    XmlChSRange {
        low: 0xd7,
        high: 0xd7,
    },
    XmlChSRange {
        low: 0xf7,
        high: 0xf7,
    },
    XmlChSRange {
        low: 0x2c2,
        high: 0x2c5,
    },
    XmlChSRange {
        low: 0x2d2,
        high: 0x2df,
    },
    XmlChSRange {
        low: 0x2e5,
        high: 0x2ed,
    },
    XmlChSRange {
        low: 0x2ef,
        high: 0x2ff,
    },
    XmlChSRange {
        low: 0x374,
        high: 0x375,
    },
    XmlChSRange {
        low: 0x384,
        high: 0x385,
    },
    XmlChSRange {
        low: 0x3f6,
        high: 0x3f6,
    },
    XmlChSRange {
        low: 0x482,
        high: 0x482,
    },
    XmlChSRange {
        low: 0x60e,
        high: 0x60f,
    },
    XmlChSRange {
        low: 0x6e9,
        high: 0x6e9,
    },
    XmlChSRange {
        low: 0x6fd,
        high: 0x6fe,
    },
    XmlChSRange {
        low: 0x9f2,
        high: 0x9f3,
    },
    XmlChSRange {
        low: 0x9fa,
        high: 0x9fa,
    },
    XmlChSRange {
        low: 0xaf1,
        high: 0xaf1,
    },
    XmlChSRange {
        low: 0xb70,
        high: 0xb70,
    },
    XmlChSRange {
        low: 0xbf3,
        high: 0xbfa,
    },
    XmlChSRange {
        low: 0xe3f,
        high: 0xe3f,
    },
    XmlChSRange {
        low: 0xf01,
        high: 0xf03,
    },
    XmlChSRange {
        low: 0xf13,
        high: 0xf17,
    },
    XmlChSRange {
        low: 0xf1a,
        high: 0xf1f,
    },
    XmlChSRange {
        low: 0xf34,
        high: 0xf34,
    },
    XmlChSRange {
        low: 0xf36,
        high: 0xf36,
    },
    XmlChSRange {
        low: 0xf38,
        high: 0xf38,
    },
    XmlChSRange {
        low: 0xfbe,
        high: 0xfc5,
    },
    XmlChSRange {
        low: 0xfc7,
        high: 0xfcc,
    },
    XmlChSRange {
        low: 0xfcf,
        high: 0xfcf,
    },
    XmlChSRange {
        low: 0x17db,
        high: 0x17db,
    },
    XmlChSRange {
        low: 0x1940,
        high: 0x1940,
    },
    XmlChSRange {
        low: 0x19e0,
        high: 0x19ff,
    },
    XmlChSRange {
        low: 0x1fbd,
        high: 0x1fbd,
    },
    XmlChSRange {
        low: 0x1fbf,
        high: 0x1fc1,
    },
    XmlChSRange {
        low: 0x1fcd,
        high: 0x1fcf,
    },
    XmlChSRange {
        low: 0x1fdd,
        high: 0x1fdf,
    },
    XmlChSRange {
        low: 0x1fed,
        high: 0x1fef,
    },
    XmlChSRange {
        low: 0x1ffd,
        high: 0x1ffe,
    },
    XmlChSRange {
        low: 0x2044,
        high: 0x2044,
    },
    XmlChSRange {
        low: 0x2052,
        high: 0x2052,
    },
    XmlChSRange {
        low: 0x207a,
        high: 0x207c,
    },
    XmlChSRange {
        low: 0x208a,
        high: 0x208c,
    },
    XmlChSRange {
        low: 0x20a0,
        high: 0x20b1,
    },
    XmlChSRange {
        low: 0x2100,
        high: 0x2101,
    },
    XmlChSRange {
        low: 0x2103,
        high: 0x2106,
    },
    XmlChSRange {
        low: 0x2108,
        high: 0x2109,
    },
    XmlChSRange {
        low: 0x2114,
        high: 0x2114,
    },
    XmlChSRange {
        low: 0x2116,
        high: 0x2118,
    },
    XmlChSRange {
        low: 0x211e,
        high: 0x2123,
    },
    XmlChSRange {
        low: 0x2125,
        high: 0x2125,
    },
    XmlChSRange {
        low: 0x2127,
        high: 0x2127,
    },
    XmlChSRange {
        low: 0x2129,
        high: 0x2129,
    },
    XmlChSRange {
        low: 0x212e,
        high: 0x212e,
    },
    XmlChSRange {
        low: 0x2132,
        high: 0x2132,
    },
    XmlChSRange {
        low: 0x213a,
        high: 0x213b,
    },
    XmlChSRange {
        low: 0x2140,
        high: 0x2144,
    },
    XmlChSRange {
        low: 0x214a,
        high: 0x214b,
    },
    XmlChSRange {
        low: 0x2190,
        high: 0x2328,
    },
    XmlChSRange {
        low: 0x232b,
        high: 0x23b3,
    },
    XmlChSRange {
        low: 0x23b7,
        high: 0x23d0,
    },
    XmlChSRange {
        low: 0x2400,
        high: 0x2426,
    },
    XmlChSRange {
        low: 0x2440,
        high: 0x244a,
    },
    XmlChSRange {
        low: 0x249c,
        high: 0x24e9,
    },
    XmlChSRange {
        low: 0x2500,
        high: 0x2617,
    },
    XmlChSRange {
        low: 0x2619,
        high: 0x267d,
    },
    XmlChSRange {
        low: 0x2680,
        high: 0x2691,
    },
    XmlChSRange {
        low: 0x26a0,
        high: 0x26a1,
    },
    XmlChSRange {
        low: 0x2701,
        high: 0x2704,
    },
    XmlChSRange {
        low: 0x2706,
        high: 0x2709,
    },
    XmlChSRange {
        low: 0x270c,
        high: 0x2727,
    },
    XmlChSRange {
        low: 0x2729,
        high: 0x274b,
    },
    XmlChSRange {
        low: 0x274d,
        high: 0x274d,
    },
    XmlChSRange {
        low: 0x274f,
        high: 0x2752,
    },
    XmlChSRange {
        low: 0x2756,
        high: 0x2756,
    },
    XmlChSRange {
        low: 0x2758,
        high: 0x275e,
    },
    XmlChSRange {
        low: 0x2761,
        high: 0x2767,
    },
    XmlChSRange {
        low: 0x2794,
        high: 0x2794,
    },
    XmlChSRange {
        low: 0x2798,
        high: 0x27af,
    },
    XmlChSRange {
        low: 0x27b1,
        high: 0x27be,
    },
    XmlChSRange {
        low: 0x27d0,
        high: 0x27e5,
    },
    XmlChSRange {
        low: 0x27f0,
        high: 0x2982,
    },
    XmlChSRange {
        low: 0x2999,
        high: 0x29d7,
    },
    XmlChSRange {
        low: 0x29dc,
        high: 0x29fb,
    },
    XmlChSRange {
        low: 0x29fe,
        high: 0x2b0d,
    },
    XmlChSRange {
        low: 0x2e80,
        high: 0x2e99,
    },
    XmlChSRange {
        low: 0x2e9b,
        high: 0x2ef3,
    },
    XmlChSRange {
        low: 0x2f00,
        high: 0x2fd5,
    },
    XmlChSRange {
        low: 0x2ff0,
        high: 0x2ffb,
    },
    XmlChSRange {
        low: 0x3004,
        high: 0x3004,
    },
    XmlChSRange {
        low: 0x3012,
        high: 0x3013,
    },
    XmlChSRange {
        low: 0x3020,
        high: 0x3020,
    },
    XmlChSRange {
        low: 0x3036,
        high: 0x3037,
    },
    XmlChSRange {
        low: 0x303e,
        high: 0x303f,
    },
    XmlChSRange {
        low: 0x309b,
        high: 0x309c,
    },
    XmlChSRange {
        low: 0x3190,
        high: 0x3191,
    },
    XmlChSRange {
        low: 0x3196,
        high: 0x319f,
    },
    XmlChSRange {
        low: 0x3200,
        high: 0x321e,
    },
    XmlChSRange {
        low: 0x322a,
        high: 0x3243,
    },
    XmlChSRange {
        low: 0x3250,
        high: 0x3250,
    },
    XmlChSRange {
        low: 0x3260,
        high: 0x327d,
    },
    XmlChSRange {
        low: 0x327f,
        high: 0x327f,
    },
    XmlChSRange {
        low: 0x328a,
        high: 0x32b0,
    },
    XmlChSRange {
        low: 0x32c0,
        high: 0x32fe,
    },
    XmlChSRange {
        low: 0x3300,
        high: 0x33ff,
    },
    XmlChSRange {
        low: 0x4dc0,
        high: 0x4dff,
    },
    XmlChSRange {
        low: 0xa490,
        high: 0xa4c6,
    },
    XmlChSRange {
        low: 0xfb29,
        high: 0xfb29,
    },
    XmlChSRange {
        low: 0xfdfc,
        high: 0xfdfd,
    },
    XmlChSRange {
        low: 0xfe62,
        high: 0xfe62,
    },
    XmlChSRange {
        low: 0xfe64,
        high: 0xfe66,
    },
    XmlChSRange {
        low: 0xfe69,
        high: 0xfe69,
    },
    XmlChSRange {
        low: 0xff04,
        high: 0xff04,
    },
    XmlChSRange {
        low: 0xff0b,
        high: 0xff0b,
    },
    XmlChSRange {
        low: 0xff1c,
        high: 0xff1e,
    },
    XmlChSRange {
        low: 0xff3e,
        high: 0xff3e,
    },
    XmlChSRange {
        low: 0xff40,
        high: 0xff40,
    },
    XmlChSRange {
        low: 0xff5c,
        high: 0xff5c,
    },
    XmlChSRange {
        low: 0xff5e,
        high: 0xff5e,
    },
    XmlChSRange {
        low: 0xffe0,
        high: 0xffe6,
    },
    XmlChSRange {
        low: 0xffe8,
        high: 0xffee,
    },
    XmlChSRange {
        low: 0xfffc,
        high: 0xfffd,
    },
];

const XML_SL: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x10102,
        high: 0x10102,
    },
    XmlChLRange {
        low: 0x10137,
        high: 0x1013f,
    },
    XmlChLRange {
        low: 0x1d000,
        high: 0x1d0f5,
    },
    XmlChLRange {
        low: 0x1d100,
        high: 0x1d126,
    },
    XmlChLRange {
        low: 0x1d12a,
        high: 0x1d164,
    },
    XmlChLRange {
        low: 0x1d16a,
        high: 0x1d16c,
    },
    XmlChLRange {
        low: 0x1d183,
        high: 0x1d184,
    },
    XmlChLRange {
        low: 0x1d18c,
        high: 0x1d1a9,
    },
    XmlChLRange {
        low: 0x1d1ae,
        high: 0x1d1dd,
    },
    XmlChLRange {
        low: 0x1d300,
        high: 0x1d356,
    },
    XmlChLRange {
        low: 0x1d6c1,
        high: 0x1d6c1,
    },
    XmlChLRange {
        low: 0x1d6db,
        high: 0x1d6db,
    },
    XmlChLRange {
        low: 0x1d6fb,
        high: 0x1d6fb,
    },
    XmlChLRange {
        low: 0x1d715,
        high: 0x1d715,
    },
    XmlChLRange {
        low: 0x1d735,
        high: 0x1d735,
    },
    XmlChLRange {
        low: 0x1d74f,
        high: 0x1d74f,
    },
    XmlChLRange {
        low: 0x1d76f,
        high: 0x1d76f,
    },
    XmlChLRange {
        low: 0x1d789,
        high: 0x1d789,
    },
    XmlChLRange {
        low: 0x1d7a9,
        high: 0x1d7a9,
    },
    XmlChLRange {
        low: 0x1d7c3,
        high: 0x1d7c3,
    },
];

const XML_SG: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 133,
    nb_long_range: 20,
    short_range: XML_SS,
    long_range: XML_SL,
};

const XML_SC_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x24,
        high: 0x24,
    },
    XmlChSRange {
        low: 0xa2,
        high: 0xa5,
    },
    XmlChSRange {
        low: 0x9f2,
        high: 0x9f3,
    },
    XmlChSRange {
        low: 0xaf1,
        high: 0xaf1,
    },
    XmlChSRange {
        low: 0xbf9,
        high: 0xbf9,
    },
    XmlChSRange {
        low: 0xe3f,
        high: 0xe3f,
    },
    XmlChSRange {
        low: 0x17db,
        high: 0x17db,
    },
    XmlChSRange {
        low: 0x20a0,
        high: 0x20b1,
    },
    XmlChSRange {
        low: 0xfdfc,
        high: 0xfdfc,
    },
    XmlChSRange {
        low: 0xfe69,
        high: 0xfe69,
    },
    XmlChSRange {
        low: 0xff04,
        high: 0xff04,
    },
    XmlChSRange {
        low: 0xffe0,
        high: 0xffe1,
    },
    XmlChSRange {
        low: 0xffe5,
        high: 0xffe6,
    },
];

const XML_SC_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 13,
    nb_long_range: 0,
    short_range: XML_SC_S,
    long_range: &[],
};

const XML_SK_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x5e,
        high: 0x5e,
    },
    XmlChSRange {
        low: 0x60,
        high: 0x60,
    },
    XmlChSRange {
        low: 0xa8,
        high: 0xa8,
    },
    XmlChSRange {
        low: 0xaf,
        high: 0xaf,
    },
    XmlChSRange {
        low: 0xb4,
        high: 0xb4,
    },
    XmlChSRange {
        low: 0xb8,
        high: 0xb8,
    },
    XmlChSRange {
        low: 0x2c2,
        high: 0x2c5,
    },
    XmlChSRange {
        low: 0x2d2,
        high: 0x2df,
    },
    XmlChSRange {
        low: 0x2e5,
        high: 0x2ed,
    },
    XmlChSRange {
        low: 0x2ef,
        high: 0x2ff,
    },
    XmlChSRange {
        low: 0x374,
        high: 0x375,
    },
    XmlChSRange {
        low: 0x384,
        high: 0x385,
    },
    XmlChSRange {
        low: 0x1fbd,
        high: 0x1fbd,
    },
    XmlChSRange {
        low: 0x1fbf,
        high: 0x1fc1,
    },
    XmlChSRange {
        low: 0x1fcd,
        high: 0x1fcf,
    },
    XmlChSRange {
        low: 0x1fdd,
        high: 0x1fdf,
    },
    XmlChSRange {
        low: 0x1fed,
        high: 0x1fef,
    },
    XmlChSRange {
        low: 0x1ffd,
        high: 0x1ffe,
    },
    XmlChSRange {
        low: 0x309b,
        high: 0x309c,
    },
    XmlChSRange {
        low: 0xff3e,
        high: 0xff3e,
    },
    XmlChSRange {
        low: 0xff40,
        high: 0xff40,
    },
    XmlChSRange {
        low: 0xffe3,
        high: 0xffe3,
    },
];

const XML_SK_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 22,
    nb_long_range: 0,
    short_range: XML_SK_S,
    long_range: &[],
};

const XML_SM_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x2b,
        high: 0x2b,
    },
    XmlChSRange {
        low: 0x3c,
        high: 0x3e,
    },
    XmlChSRange {
        low: 0x7c,
        high: 0x7c,
    },
    XmlChSRange {
        low: 0x7e,
        high: 0x7e,
    },
    XmlChSRange {
        low: 0xac,
        high: 0xac,
    },
    XmlChSRange {
        low: 0xb1,
        high: 0xb1,
    },
    XmlChSRange {
        low: 0xd7,
        high: 0xd7,
    },
    XmlChSRange {
        low: 0xf7,
        high: 0xf7,
    },
    XmlChSRange {
        low: 0x3f6,
        high: 0x3f6,
    },
    XmlChSRange {
        low: 0x2044,
        high: 0x2044,
    },
    XmlChSRange {
        low: 0x2052,
        high: 0x2052,
    },
    XmlChSRange {
        low: 0x207a,
        high: 0x207c,
    },
    XmlChSRange {
        low: 0x208a,
        high: 0x208c,
    },
    XmlChSRange {
        low: 0x2140,
        high: 0x2144,
    },
    XmlChSRange {
        low: 0x214b,
        high: 0x214b,
    },
    XmlChSRange {
        low: 0x2190,
        high: 0x2194,
    },
    XmlChSRange {
        low: 0x219a,
        high: 0x219b,
    },
    XmlChSRange {
        low: 0x21a0,
        high: 0x21a0,
    },
    XmlChSRange {
        low: 0x21a3,
        high: 0x21a3,
    },
    XmlChSRange {
        low: 0x21a6,
        high: 0x21a6,
    },
    XmlChSRange {
        low: 0x21ae,
        high: 0x21ae,
    },
    XmlChSRange {
        low: 0x21ce,
        high: 0x21cf,
    },
    XmlChSRange {
        low: 0x21d2,
        high: 0x21d2,
    },
    XmlChSRange {
        low: 0x21d4,
        high: 0x21d4,
    },
    XmlChSRange {
        low: 0x21f4,
        high: 0x22ff,
    },
    XmlChSRange {
        low: 0x2308,
        high: 0x230b,
    },
    XmlChSRange {
        low: 0x2320,
        high: 0x2321,
    },
    XmlChSRange {
        low: 0x237c,
        high: 0x237c,
    },
    XmlChSRange {
        low: 0x239b,
        high: 0x23b3,
    },
    XmlChSRange {
        low: 0x25b7,
        high: 0x25b7,
    },
    XmlChSRange {
        low: 0x25c1,
        high: 0x25c1,
    },
    XmlChSRange {
        low: 0x25f8,
        high: 0x25ff,
    },
    XmlChSRange {
        low: 0x266f,
        high: 0x266f,
    },
    XmlChSRange {
        low: 0x27d0,
        high: 0x27e5,
    },
    XmlChSRange {
        low: 0x27f0,
        high: 0x27ff,
    },
    XmlChSRange {
        low: 0x2900,
        high: 0x2982,
    },
    XmlChSRange {
        low: 0x2999,
        high: 0x29d7,
    },
    XmlChSRange {
        low: 0x29dc,
        high: 0x29fb,
    },
    XmlChSRange {
        low: 0x29fe,
        high: 0x2aff,
    },
    XmlChSRange {
        low: 0xfb29,
        high: 0xfb29,
    },
    XmlChSRange {
        low: 0xfe62,
        high: 0xfe62,
    },
    XmlChSRange {
        low: 0xfe64,
        high: 0xfe66,
    },
    XmlChSRange {
        low: 0xff0b,
        high: 0xff0b,
    },
    XmlChSRange {
        low: 0xff1c,
        high: 0xff1e,
    },
    XmlChSRange {
        low: 0xff5c,
        high: 0xff5c,
    },
    XmlChSRange {
        low: 0xff5e,
        high: 0xff5e,
    },
    XmlChSRange {
        low: 0xffe2,
        high: 0xffe2,
    },
    XmlChSRange {
        low: 0xffe9,
        high: 0xffec,
    },
];

const XML_SM_L: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x1d6c1,
        high: 0x1d6c1,
    },
    XmlChLRange {
        low: 0x1d6db,
        high: 0x1d6db,
    },
    XmlChLRange {
        low: 0x1d6fb,
        high: 0x1d6fb,
    },
    XmlChLRange {
        low: 0x1d715,
        high: 0x1d715,
    },
    XmlChLRange {
        low: 0x1d735,
        high: 0x1d735,
    },
    XmlChLRange {
        low: 0x1d74f,
        high: 0x1d74f,
    },
    XmlChLRange {
        low: 0x1d76f,
        high: 0x1d76f,
    },
    XmlChLRange {
        low: 0x1d789,
        high: 0x1d789,
    },
    XmlChLRange {
        low: 0x1d7a9,
        high: 0x1d7a9,
    },
    XmlChLRange {
        low: 0x1d7c3,
        high: 0x1d7c3,
    },
];

const XML_SM_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 48,
    nb_long_range: 10,
    short_range: XML_SM_S,
    long_range: XML_SM_L,
};

const XML_SO_S: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0xa6,
        high: 0xa7,
    },
    XmlChSRange {
        low: 0xa9,
        high: 0xa9,
    },
    XmlChSRange {
        low: 0xae,
        high: 0xae,
    },
    XmlChSRange {
        low: 0xb0,
        high: 0xb0,
    },
    XmlChSRange {
        low: 0xb6,
        high: 0xb6,
    },
    XmlChSRange {
        low: 0x482,
        high: 0x482,
    },
    XmlChSRange {
        low: 0x60e,
        high: 0x60f,
    },
    XmlChSRange {
        low: 0x6e9,
        high: 0x6e9,
    },
    XmlChSRange {
        low: 0x6fd,
        high: 0x6fe,
    },
    XmlChSRange {
        low: 0x9fa,
        high: 0x9fa,
    },
    XmlChSRange {
        low: 0xb70,
        high: 0xb70,
    },
    XmlChSRange {
        low: 0xbf3,
        high: 0xbf8,
    },
    XmlChSRange {
        low: 0xbfa,
        high: 0xbfa,
    },
    XmlChSRange {
        low: 0xf01,
        high: 0xf03,
    },
    XmlChSRange {
        low: 0xf13,
        high: 0xf17,
    },
    XmlChSRange {
        low: 0xf1a,
        high: 0xf1f,
    },
    XmlChSRange {
        low: 0xf34,
        high: 0xf34,
    },
    XmlChSRange {
        low: 0xf36,
        high: 0xf36,
    },
    XmlChSRange {
        low: 0xf38,
        high: 0xf38,
    },
    XmlChSRange {
        low: 0xfbe,
        high: 0xfc5,
    },
    XmlChSRange {
        low: 0xfc7,
        high: 0xfcc,
    },
    XmlChSRange {
        low: 0xfcf,
        high: 0xfcf,
    },
    XmlChSRange {
        low: 0x1940,
        high: 0x1940,
    },
    XmlChSRange {
        low: 0x19e0,
        high: 0x19ff,
    },
    XmlChSRange {
        low: 0x2100,
        high: 0x2101,
    },
    XmlChSRange {
        low: 0x2103,
        high: 0x2106,
    },
    XmlChSRange {
        low: 0x2108,
        high: 0x2109,
    },
    XmlChSRange {
        low: 0x2114,
        high: 0x2114,
    },
    XmlChSRange {
        low: 0x2116,
        high: 0x2118,
    },
    XmlChSRange {
        low: 0x211e,
        high: 0x2123,
    },
    XmlChSRange {
        low: 0x2125,
        high: 0x2125,
    },
    XmlChSRange {
        low: 0x2127,
        high: 0x2127,
    },
    XmlChSRange {
        low: 0x2129,
        high: 0x2129,
    },
    XmlChSRange {
        low: 0x212e,
        high: 0x212e,
    },
    XmlChSRange {
        low: 0x2132,
        high: 0x2132,
    },
    XmlChSRange {
        low: 0x213a,
        high: 0x213b,
    },
    XmlChSRange {
        low: 0x214a,
        high: 0x214a,
    },
    XmlChSRange {
        low: 0x2195,
        high: 0x2199,
    },
    XmlChSRange {
        low: 0x219c,
        high: 0x219f,
    },
    XmlChSRange {
        low: 0x21a1,
        high: 0x21a2,
    },
    XmlChSRange {
        low: 0x21a4,
        high: 0x21a5,
    },
    XmlChSRange {
        low: 0x21a7,
        high: 0x21ad,
    },
    XmlChSRange {
        low: 0x21af,
        high: 0x21cd,
    },
    XmlChSRange {
        low: 0x21d0,
        high: 0x21d1,
    },
    XmlChSRange {
        low: 0x21d3,
        high: 0x21d3,
    },
    XmlChSRange {
        low: 0x21d5,
        high: 0x21f3,
    },
    XmlChSRange {
        low: 0x2300,
        high: 0x2307,
    },
    XmlChSRange {
        low: 0x230c,
        high: 0x231f,
    },
    XmlChSRange {
        low: 0x2322,
        high: 0x2328,
    },
    XmlChSRange {
        low: 0x232b,
        high: 0x237b,
    },
    XmlChSRange {
        low: 0x237d,
        high: 0x239a,
    },
    XmlChSRange {
        low: 0x23b7,
        high: 0x23d0,
    },
    XmlChSRange {
        low: 0x2400,
        high: 0x2426,
    },
    XmlChSRange {
        low: 0x2440,
        high: 0x244a,
    },
    XmlChSRange {
        low: 0x249c,
        high: 0x24e9,
    },
    XmlChSRange {
        low: 0x2500,
        high: 0x25b6,
    },
    XmlChSRange {
        low: 0x25b8,
        high: 0x25c0,
    },
    XmlChSRange {
        low: 0x25c2,
        high: 0x25f7,
    },
    XmlChSRange {
        low: 0x2600,
        high: 0x2617,
    },
    XmlChSRange {
        low: 0x2619,
        high: 0x266e,
    },
    XmlChSRange {
        low: 0x2670,
        high: 0x267d,
    },
    XmlChSRange {
        low: 0x2680,
        high: 0x2691,
    },
    XmlChSRange {
        low: 0x26a0,
        high: 0x26a1,
    },
    XmlChSRange {
        low: 0x2701,
        high: 0x2704,
    },
    XmlChSRange {
        low: 0x2706,
        high: 0x2709,
    },
    XmlChSRange {
        low: 0x270c,
        high: 0x2727,
    },
    XmlChSRange {
        low: 0x2729,
        high: 0x274b,
    },
    XmlChSRange {
        low: 0x274d,
        high: 0x274d,
    },
    XmlChSRange {
        low: 0x274f,
        high: 0x2752,
    },
    XmlChSRange {
        low: 0x2756,
        high: 0x2756,
    },
    XmlChSRange {
        low: 0x2758,
        high: 0x275e,
    },
    XmlChSRange {
        low: 0x2761,
        high: 0x2767,
    },
    XmlChSRange {
        low: 0x2794,
        high: 0x2794,
    },
    XmlChSRange {
        low: 0x2798,
        high: 0x27af,
    },
    XmlChSRange {
        low: 0x27b1,
        high: 0x27be,
    },
    XmlChSRange {
        low: 0x2800,
        high: 0x28ff,
    },
    XmlChSRange {
        low: 0x2b00,
        high: 0x2b0d,
    },
    XmlChSRange {
        low: 0x2e80,
        high: 0x2e99,
    },
    XmlChSRange {
        low: 0x2e9b,
        high: 0x2ef3,
    },
    XmlChSRange {
        low: 0x2f00,
        high: 0x2fd5,
    },
    XmlChSRange {
        low: 0x2ff0,
        high: 0x2ffb,
    },
    XmlChSRange {
        low: 0x3004,
        high: 0x3004,
    },
    XmlChSRange {
        low: 0x3012,
        high: 0x3013,
    },
    XmlChSRange {
        low: 0x3020,
        high: 0x3020,
    },
    XmlChSRange {
        low: 0x3036,
        high: 0x3037,
    },
    XmlChSRange {
        low: 0x303e,
        high: 0x303f,
    },
    XmlChSRange {
        low: 0x3190,
        high: 0x3191,
    },
    XmlChSRange {
        low: 0x3196,
        high: 0x319f,
    },
    XmlChSRange {
        low: 0x3200,
        high: 0x321e,
    },
    XmlChSRange {
        low: 0x322a,
        high: 0x3243,
    },
    XmlChSRange {
        low: 0x3250,
        high: 0x3250,
    },
    XmlChSRange {
        low: 0x3260,
        high: 0x327d,
    },
    XmlChSRange {
        low: 0x327f,
        high: 0x327f,
    },
    XmlChSRange {
        low: 0x328a,
        high: 0x32b0,
    },
    XmlChSRange {
        low: 0x32c0,
        high: 0x32fe,
    },
    XmlChSRange {
        low: 0x3300,
        high: 0x33ff,
    },
    XmlChSRange {
        low: 0x4dc0,
        high: 0x4dff,
    },
    XmlChSRange {
        low: 0xa490,
        high: 0xa4c6,
    },
    XmlChSRange {
        low: 0xfdfd,
        high: 0xfdfd,
    },
    XmlChSRange {
        low: 0xffe4,
        high: 0xffe4,
    },
    XmlChSRange {
        low: 0xffe8,
        high: 0xffe8,
    },
    XmlChSRange {
        low: 0xffed,
        high: 0xffee,
    },
    XmlChSRange {
        low: 0xfffc,
        high: 0xfffd,
    },
];

const XML_SO_L: &[XmlChLRange] = &[
    XmlChLRange {
        low: 0x10102,
        high: 0x10102,
    },
    XmlChLRange {
        low: 0x10137,
        high: 0x1013f,
    },
    XmlChLRange {
        low: 0x1d000,
        high: 0x1d0f5,
    },
    XmlChLRange {
        low: 0x1d100,
        high: 0x1d126,
    },
    XmlChLRange {
        low: 0x1d12a,
        high: 0x1d164,
    },
    XmlChLRange {
        low: 0x1d16a,
        high: 0x1d16c,
    },
    XmlChLRange {
        low: 0x1d183,
        high: 0x1d184,
    },
    XmlChLRange {
        low: 0x1d18c,
        high: 0x1d1a9,
    },
    XmlChLRange {
        low: 0x1d1ae,
        high: 0x1d1dd,
    },
    XmlChLRange {
        low: 0x1d300,
        high: 0x1d356,
    },
];

const XML_SO_G: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 103,
    nb_long_range: 10,
    short_range: XML_SO_S,
    long_range: XML_SO_L,
};

const XML_ZS: &[XmlChSRange] = &[
    XmlChSRange {
        low: 0x20,
        high: 0x20,
    },
    XmlChSRange {
        low: 0xa0,
        high: 0xa0,
    },
    XmlChSRange {
        low: 0x1680,
        high: 0x1680,
    },
    XmlChSRange {
        low: 0x180e,
        high: 0x180e,
    },
    XmlChSRange {
        low: 0x2000,
        high: 0x200a,
    },
    XmlChSRange {
        low: 0x2028,
        high: 0x2029,
    },
    XmlChSRange {
        low: 0x202f,
        high: 0x202f,
    },
    XmlChSRange {
        low: 0x205f,
        high: 0x205f,
    },
    XmlChSRange {
        low: 0x3000,
        high: 0x3000,
    },
];

const XML_ZG: XmlChRangeGroup = XmlChRangeGroup {
    nb_short_range: 9,
    nb_long_range: 0,
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

/**
 * xml_ucs_isAegeanNumbers:
 * @code: UCS code point
 *
 * Check whether the character is part of AegeanNumbers UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_aegean_numbers(code: c_int) -> c_int {
    (0x10100..=0x1013F).contains(&code) as i32
}

/**
 * xml_ucs_isAlphabeticPresentationForms:
 * @code: UCS code point
 *
 * Check whether the character is part of AlphabeticPresentationForms UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_alphabetic_presentation_forms(code: c_int) -> c_int {
    (0xFB00..=0xFB4F).contains(&code) as i32
}

/**
 * xml_ucs_isArabic:
 * @code: UCS code point
 *
 * Check whether the character is part of Arabic UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_arabic(code: c_int) -> c_int {
    (0x0600..=0x06FF).contains(&code) as i32
}

/**
 * xml_ucs_isArabicPresentationFormsA:
 * @code: UCS code point
 *
 * Check whether the character is part of ArabicPresentationForms-A UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_arabic_presentation_forms_a(code: c_int) -> c_int {
    (0xFB50..=0xFDFF).contains(&code) as i32
}

/**
 * xml_ucs_isArabicPresentationFormsB:
 * @code: UCS code point
 *
 * Check whether the character is part of ArabicPresentationForms-B UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_arabic_presentation_forms_b(code: c_int) -> c_int {
    (0xFE70..=0xFEFF).contains(&code) as i32
}

/**
 * xml_ucs_isArmenian:
 * @code: UCS code point
 *
 * Check whether the character is part of Armenian UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_armenian(code: c_int) -> c_int {
    (0x0530..=0x058F).contains(&code) as i32
}

/**
 * xml_ucs_isArrows:
 * @code: UCS code point
 *
 * Check whether the character is part of Arrows UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_arrows(code: c_int) -> c_int {
    (0x2190..=0x21FF).contains(&code) as i32
}

/**
 * xml_ucs_isBasicLatin:
 * @code: UCS code point
 *
 * Check whether the character is part of BasicLatin UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_basic_latin(code: c_int) -> c_int {
    (0x0000..=0x007F).contains(&code) as i32
}

/**
 * xml_ucs_isBengali:
 * @code: UCS code point
 *
 * Check whether the character is part of Bengali UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_bengali(code: c_int) -> c_int {
    (0x0980..=0x09FF).contains(&code) as i32
}

/**
 * xml_ucs_isBlockElements:
 * @code: UCS code point
 *
 * Check whether the character is part of BlockElements UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_block_elements(code: c_int) -> c_int {
    (0x2580..=0x259F).contains(&code) as i32
}

/**
 * xml_ucs_isBopomofo:
 * @code: UCS code point
 *
 * Check whether the character is part of Bopomofo UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_bopomofo(code: c_int) -> c_int {
    (0x3100..=0x312F).contains(&code) as i32
}

/**
 * xml_ucs_isBopomofoExtended:
 * @code: UCS code point
 *
 * Check whether the character is part of BopomofoExtended UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_bopomofo_extended(code: c_int) -> c_int {
    (0x31A0..=0x31BF).contains(&code) as i32
}

/**
 * xml_ucs_isBoxDrawing:
 * @code: UCS code point
 *
 * Check whether the character is part of BoxDrawing UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_box_drawing(code: c_int) -> c_int {
    (0x2500..=0x257F).contains(&code) as i32
}

/**
 * xml_ucs_isBraillePatterns:
 * @code: UCS code point
 *
 * Check whether the character is part of BraillePatterns UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_braille_patterns(code: c_int) -> c_int {
    (0x2800..=0x28FF).contains(&code) as i32
}

/**
 * xml_ucs_isBuhid:
 * @code: UCS code point
 *
 * Check whether the character is part of Buhid UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_buhid(code: c_int) -> c_int {
    (0x1740..=0x175F).contains(&code) as i32
}

/**
 * xml_ucs_isByzantineMusicalSymbols:
 * @code: UCS code point
 *
 * Check whether the character is part of ByzantineMusicalSymbols UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_byzantine_musical_symbols(code: c_int) -> c_int {
    (0x1D000..=0x1D0FF).contains(&code) as i32
}

/**
 * xml_ucs_isCJKCompatibility:
 * @code: UCS code point
 *
 * Check whether the character is part of CJKCompatibility UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cjk_compatibility(code: c_int) -> c_int {
    (0x3300..=0x33FF).contains(&code) as i32
}

/**
 * xml_ucs_isCJKCompatibilityForms:
 * @code: UCS code point
 *
 * Check whether the character is part of CJKCompatibilityForms UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cjk_compatibility_forms(code: c_int) -> c_int {
    (0xFE30..=0xFE4F).contains(&code) as i32
}

/**
 * xml_ucs_isCJKCompatibilityIdeographs:
 * @code: UCS code point
 *
 * Check whether the character is part of CJKCompatibilityIdeographs UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cjk_compatibility_ideographs(code: c_int) -> c_int {
    (0xF900..=0xFAFF).contains(&code) as i32
}

/**
 * xml_ucs_isCJKCompatibilityIdeographsSupplement:
 * @code: UCS code point
 *
 * Check whether the character is part of CJKCompatibilityIdeographsSupplement UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cjk_compatibility_ideographs_supplement(code: c_int) -> c_int {
    (0x2F800..=0x2FA1F).contains(&code) as i32
}

/**
 * xml_ucs_isCJKRadicalsSupplement:
 * @code: UCS code point
 *
 * Check whether the character is part of CJKRadicalsSupplement UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cjk_radicals_supplement(code: c_int) -> c_int {
    (0x2E80..=0x2EFF).contains(&code) as i32
}

/**
 * xml_ucs_isCJKSymbolsandPunctuation:
 * @code: UCS code point
 *
 * Check whether the character is part of CJKSymbolsandPunctuation UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cjk_symbolsand_punctuation(code: c_int) -> c_int {
    (0x3000..=0x303F).contains(&code) as i32
}

/**
 * xml_ucs_isCJKUnifiedIdeographs:
 * @code: UCS code point
 *
 * Check whether the character is part of CJKUnifiedIdeographs UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cjk_unified_ideographs(code: c_int) -> c_int {
    (0x4E00..=0x9FFF).contains(&code) as i32
}

/**
 * xml_ucs_isCJKUnifiedIdeographsExtensionA:
 * @code: UCS code point
 *
 * Check whether the character is part of CJKUnifiedIdeographsExtensionA UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cjk_unified_ideographs_extension_a(code: c_int) -> c_int {
    (0x3400..=0x4DBF).contains(&code) as i32
}

/**
 * xml_ucs_isCJKUnifiedIdeographsExtensionB:
 * @code: UCS code point
 *
 * Check whether the character is part of CJKUnifiedIdeographsExtensionB UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cjk_unified_ideographs_extension_b(code: c_int) -> c_int {
    (0x20000..=0x2A6DF).contains(&code) as i32
}

/**
 * xml_ucs_isCherokee:
 * @code: UCS code point
 *
 * Check whether the character is part of Cherokee UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cherokee(code: c_int) -> c_int {
    (0x13A0..=0x13FF).contains(&code) as i32
}

/**
 * xml_ucs_isCombiningDiacriticalMarks:
 * @code: UCS code point
 *
 * Check whether the character is part of CombiningDiacriticalMarks UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_combining_diacritical_marks(code: c_int) -> c_int {
    (0x0300..=0x036F).contains(&code) as i32
}

/**
 * xml_ucs_isCombiningDiacriticalMarksforSymbols:
 * @code: UCS code point
 *
 * Check whether the character is part of CombiningDiacriticalMarksforSymbols UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_combining_diacritical_marksfor_symbols(code: c_int) -> c_int {
    (0x20D0..=0x20FF).contains(&code) as i32
}

/**
 * xml_ucs_isCombiningHalfMarks:
 * @code: UCS code point
 *
 * Check whether the character is part of CombiningHalfMarks UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_combining_half_marks(code: c_int) -> c_int {
    (0xFE20..=0xFE2F).contains(&code) as i32
}

/**
 * xml_ucs_isCombiningMarksforSymbols:
 * @code: UCS code point
 *
 * Check whether the character is part of CombiningMarksforSymbols UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_combining_marksfor_symbols(code: c_int) -> c_int {
    (0x20D0..=0x20FF).contains(&code) as i32
}

/**
 * xml_ucs_isControlPictures:
 * @code: UCS code point
 *
 * Check whether the character is part of ControlPictures UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_control_pictures(code: c_int) -> c_int {
    (0x2400..=0x243F).contains(&code) as i32
}

/**
 * xml_ucs_isCurrencySymbols:
 * @code: UCS code point
 *
 * Check whether the character is part of CurrencySymbols UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_currency_symbols(code: c_int) -> c_int {
    (0x20A0..=0x20CF).contains(&code) as i32
}

/**
 * xml_ucs_isCypriotSyllabary:
 * @code: UCS code point
 *
 * Check whether the character is part of CypriotSyllabary UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cypriot_syllabary(code: c_int) -> c_int {
    (0x10800..=0x1083F).contains(&code) as i32
}

/**
 * xml_ucs_isCyrillic:
 * @code: UCS code point
 *
 * Check whether the character is part of Cyrillic UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cyrillic(code: c_int) -> c_int {
    (0x0400..=0x04FF).contains(&code) as i32
}

/**
 * xml_ucs_isCyrillicSupplement:
 * @code: UCS code point
 *
 * Check whether the character is part of CyrillicSupplement UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cyrillic_supplement(code: c_int) -> c_int {
    (0x0500..=0x052F).contains(&code) as i32
}

/**
 * xml_ucs_isDeseret:
 * @code: UCS code point
 *
 * Check whether the character is part of Deseret UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_deseret(code: c_int) -> c_int {
    (0x10400..=0x1044F).contains(&code) as i32
}

/**
 * xml_ucs_isDevanagari:
 * @code: UCS code point
 *
 * Check whether the character is part of Devanagari UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_devanagari(code: c_int) -> c_int {
    (0x0900..=0x097F).contains(&code) as i32
}

/**
 * xml_ucs_isDingbats:
 * @code: UCS code point
 *
 * Check whether the character is part of Dingbats UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_dingbats(code: c_int) -> c_int {
    (0x2700..=0x27BF).contains(&code) as i32
}

/**
 * xml_ucs_isEnclosedAlphanumerics:
 * @code: UCS code point
 *
 * Check whether the character is part of EnclosedAlphanumerics UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_enclosed_alphanumerics(code: c_int) -> c_int {
    (0x2460..=0x24FF).contains(&code) as i32
}

/**
 * xml_ucs_isEnclosedCJKLettersandMonths:
 * @code: UCS code point
 *
 * Check whether the character is part of EnclosedCJKLettersandMonths UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_enclosed_cjk_lettersand_months(code: c_int) -> c_int {
    (0x3200..=0x32FF).contains(&code) as i32
}

/**
 * xml_ucs_isEthiopic:
 * @code: UCS code point
 *
 * Check whether the character is part of Ethiopic UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_ethiopic(code: c_int) -> c_int {
    (0x1200..=0x137F).contains(&code) as i32
}

/**
 * xml_ucs_isGeneralPunctuation:
 * @code: UCS code point
 *
 * Check whether the character is part of GeneralPunctuation UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_general_punctuation(code: c_int) -> c_int {
    (0x2000..=0x206F).contains(&code) as i32
}

/**
 * xml_ucs_isGeometricShapes:
 * @code: UCS code point
 *
 * Check whether the character is part of GeometricShapes UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_geometric_shapes(code: c_int) -> c_int {
    (0x25A0..=0x25FF).contains(&code) as i32
}

/**
 * xml_ucs_isGeorgian:
 * @code: UCS code point
 *
 * Check whether the character is part of Georgian UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_georgian(code: c_int) -> c_int {
    (0x10A0..=0x10FF).contains(&code) as i32
}

/**
 * xml_ucs_isGothic:
 * @code: UCS code point
 *
 * Check whether the character is part of Gothic UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_gothic(code: c_int) -> c_int {
    (0x10330..=0x1034F).contains(&code) as i32
}

/**
 * xml_ucs_isGreek:
 * @code: UCS code point
 *
 * Check whether the character is part of Greek UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_greek(code: c_int) -> c_int {
    (0x0370..=0x03FF).contains(&code) as i32
}

/**
 * xml_ucs_isGreekExtended:
 * @code: UCS code point
 *
 * Check whether the character is part of GreekExtended UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_greek_extended(code: c_int) -> c_int {
    (0x1F00..=0x1FFF).contains(&code) as i32
}

/**
 * xml_ucs_isGreekandCoptic:
 * @code: UCS code point
 *
 * Check whether the character is part of GreekandCoptic UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_greekand_coptic(code: c_int) -> c_int {
    (0x0370..=0x03FF).contains(&code) as i32
}

/**
 * xml_ucs_isGujarati:
 * @code: UCS code point
 *
 * Check whether the character is part of Gujarati UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_gujarati(code: c_int) -> c_int {
    (0x0A80..=0x0AFF).contains(&code) as i32
}

/**
 * xml_ucs_isGurmukhi:
 * @code: UCS code point
 *
 * Check whether the character is part of Gurmukhi UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_gurmukhi(code: c_int) -> c_int {
    (0x0A00..=0x0A7F).contains(&code) as i32
}

/**
 * xml_ucs_isHalfwidthandFullwidthForms:
 * @code: UCS code point
 *
 * Check whether the character is part of HalfwidthandFullwidthForms UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_halfwidthand_fullwidth_forms(code: c_int) -> c_int {
    (0xFF00..=0xFFEF).contains(&code) as i32
}

/**
 * xml_ucs_isHangulCompatibilityJamo:
 * @code: UCS code point
 *
 * Check whether the character is part of HangulCompatibilityJamo UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_hangul_compatibility_jamo(code: c_int) -> c_int {
    (0x3130..=0x318F).contains(&code) as i32
}

/**
 * xml_ucs_isHangulJamo:
 * @code: UCS code point
 *
 * Check whether the character is part of HangulJamo UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_hangul_jamo(code: c_int) -> c_int {
    (0x1100..=0x11FF).contains(&code) as i32
}

/**
 * xml_ucs_isHangulSyllables:
 * @code: UCS code point
 *
 * Check whether the character is part of HangulSyllables UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_hangul_syllables(code: c_int) -> c_int {
    (0xAC00..=0xD7AF).contains(&code) as i32
}

/**
 * xml_ucs_isHanunoo:
 * @code: UCS code point
 *
 * Check whether the character is part of Hanunoo UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_hanunoo(code: c_int) -> c_int {
    (0x1720..=0x173F).contains(&code) as i32
}

/**
 * xml_ucs_isHebrew:
 * @code: UCS code point
 *
 * Check whether the character is part of Hebrew UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_hebrew(code: c_int) -> c_int {
    (0x0590..=0x05FF).contains(&code) as i32
}

/**
 * xml_ucs_isHighPrivateUseSurrogates:
 * @code: UCS code point
 *
 * Check whether the character is part of HighPrivateUseSurrogates UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_high_private_use_surrogates(code: c_int) -> c_int {
    (0xDB80..=0xDBFF).contains(&code) as i32
}

/**
 * xml_ucs_isHighSurrogates:
 * @code: UCS code point
 *
 * Check whether the character is part of HighSurrogates UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_high_surrogates(code: c_int) -> c_int {
    (0xD800..=0xDB7F).contains(&code) as i32
}

/**
 * xml_ucs_isHiragana:
 * @code: UCS code point
 *
 * Check whether the character is part of Hiragana UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_hiragana(code: c_int) -> c_int {
    (0x3040..=0x309F).contains(&code) as i32
}

/**
 * xml_ucs_isIPAExtensions:
 * @code: UCS code point
 *
 * Check whether the character is part of IPAExtensions UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_ipa_extensions(code: c_int) -> c_int {
    (0x0250..=0x02AF).contains(&code) as i32
}

/**
 * xml_ucs_isIdeographicDescriptionCharacters:
 * @code: UCS code point
 *
 * Check whether the character is part of IdeographicDescriptionCharacters UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_ideographic_description_characters(code: c_int) -> c_int {
    (0x2FF0..=0x2FFF).contains(&code) as i32
}

/**
 * xml_ucs_isKanbun:
 * @code: UCS code point
 *
 * Check whether the character is part of Kanbun UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_kanbun(code: c_int) -> c_int {
    (0x3190..=0x319F).contains(&code) as i32
}

/**
 * xml_ucs_isKangxiRadicals:
 * @code: UCS code point
 *
 * Check whether the character is part of KangxiRadicals UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_kangxi_radicals(code: c_int) -> c_int {
    (0x2F00..=0x2FDF).contains(&code) as i32
}

/**
 * xml_ucs_isKannada:
 * @code: UCS code point
 *
 * Check whether the character is part of Kannada UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_kannada(code: c_int) -> c_int {
    (0x0C80..=0x0CFF).contains(&code) as i32
}

/**
 * xml_ucs_isKatakana:
 * @code: UCS code point
 *
 * Check whether the character is part of Katakana UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_katakana(code: c_int) -> c_int {
    (0x30A0..=0x30FF).contains(&code) as i32
}

/**
 * xml_ucs_isKatakanaPhoneticExtensions:
 * @code: UCS code point
 *
 * Check whether the character is part of KatakanaPhoneticExtensions UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_katakana_phonetic_extensions(code: c_int) -> c_int {
    (0x31F0..=0x31FF).contains(&code) as i32
}

/**
 * xml_ucs_isKhmer:
 * @code: UCS code point
 *
 * Check whether the character is part of Khmer UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_khmer(code: c_int) -> c_int {
    (0x1780..=0x17FF).contains(&code) as i32
}

/**
 * xml_ucs_isKhmerSymbols:
 * @code: UCS code point
 *
 * Check whether the character is part of KhmerSymbols UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_khmer_symbols(code: c_int) -> c_int {
    (0x19E0..=0x19FF).contains(&code) as i32
}

/**
 * xml_ucs_isLao:
 * @code: UCS code point
 *
 * Check whether the character is part of Lao UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_lao(code: c_int) -> c_int {
    (0x0E80..=0x0EFF).contains(&code) as i32
}

/**
 * xml_ucs_isLatin1Supplement:
 * @code: UCS code point
 *
 * Check whether the character is part of Latin-1Supplement UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_latin1_supplement(code: c_int) -> c_int {
    (0x0080..=0x00FF).contains(&code) as i32
}

/**
 * xml_ucs_isLatinExtendedA:
 * @code: UCS code point
 *
 * Check whether the character is part of LatinExtended-A UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_latin_extended_a(code: c_int) -> c_int {
    (0x0100..=0x017F).contains(&code) as i32
}

/**
 * xml_ucs_isLatinExtendedB:
 * @code: UCS code point
 *
 * Check whether the character is part of LatinExtended-B UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_latin_extended_b(code: c_int) -> c_int {
    (0x0180..=0x024F).contains(&code) as i32
}

/**
 * xml_ucs_isLatinExtendedAdditional:
 * @code: UCS code point
 *
 * Check whether the character is part of LatinExtendedAdditional UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_latin_extended_additional(code: c_int) -> c_int {
    (0x1E00..=0x1EFF).contains(&code) as i32
}

/**
 * xml_ucs_isLetterlikeSymbols:
 * @code: UCS code point
 *
 * Check whether the character is part of LetterlikeSymbols UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_letterlike_symbols(code: c_int) -> c_int {
    (0x2100..=0x214F).contains(&code) as i32
}

/**
 * xml_ucs_isLimbu:
 * @code: UCS code point
 *
 * Check whether the character is part of Limbu UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_limbu(code: c_int) -> c_int {
    (0x1900..=0x194F).contains(&code) as i32
}

/**
 * xml_ucs_isLinearBIdeograms:
 * @code: UCS code point
 *
 * Check whether the character is part of LinearBIdeograms UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_linear_bideograms(code: c_int) -> c_int {
    (0x10080..=0x100FF).contains(&code) as i32
}

/**
 * xml_ucs_isLinearBSyllabary:
 * @code: UCS code point
 *
 * Check whether the character is part of LinearBSyllabary UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_linear_bsyllabary(code: c_int) -> c_int {
    (0x10000..=0x1007F).contains(&code) as i32
}

/**
 * xml_ucs_isLowSurrogates:
 * @code: UCS code point
 *
 * Check whether the character is part of LowSurrogates UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_low_surrogates(code: c_int) -> c_int {
    (0xDC00..=0xDFFF).contains(&code) as i32
}

/**
 * xml_ucs_isMalayalam:
 * @code: UCS code point
 *
 * Check whether the character is part of Malayalam UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_malayalam(code: c_int) -> c_int {
    (0x0D00..=0x0D7F).contains(&code) as i32
}

/**
 * xml_ucs_isMathematicalAlphanumericSymbols:
 * @code: UCS code point
 *
 * Check whether the character is part of MathematicalAlphanumericSymbols UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_mathematical_alphanumeric_symbols(code: c_int) -> c_int {
    (0x1D400..=0x1D7FF).contains(&code) as i32
}

/**
 * xml_ucs_isMathematicalOperators:
 * @code: UCS code point
 *
 * Check whether the character is part of MathematicalOperators UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_mathematical_operators(code: c_int) -> c_int {
    (0x2200..=0x22FF).contains(&code) as i32
}

/**
 * xml_ucs_isMiscellaneousMathematicalSymbolsA:
 * @code: UCS code point
 *
 * Check whether the character is part of MiscellaneousMathematicalSymbols-A UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_miscellaneous_mathematical_symbols_a(code: c_int) -> c_int {
    (0x27C0..=0x27EF).contains(&code) as i32
}

/**
 * xml_ucs_isMiscellaneousMathematicalSymbolsB:
 * @code: UCS code point
 *
 * Check whether the character is part of MiscellaneousMathematicalSymbols-B UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_miscellaneous_mathematical_symbols_b(code: c_int) -> c_int {
    (0x2980..=0x29FF).contains(&code) as i32
}

/**
 * xml_ucs_isMiscellaneousSymbols:
 * @code: UCS code point
 *
 * Check whether the character is part of MiscellaneousSymbols UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_miscellaneous_symbols(code: c_int) -> c_int {
    (0x2600..=0x26FF).contains(&code) as i32
}

/**
 * xml_ucs_isMiscellaneousSymbolsandArrows:
 * @code: UCS code point
 *
 * Check whether the character is part of MiscellaneousSymbolsandArrows UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_miscellaneous_symbolsand_arrows(code: c_int) -> c_int {
    (0x2B00..=0x2BFF).contains(&code) as i32
}

/**
 * xml_ucs_isMiscellaneousTechnical:
 * @code: UCS code point
 *
 * Check whether the character is part of MiscellaneousTechnical UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_miscellaneous_technical(code: c_int) -> c_int {
    (0x2300..=0x23FF).contains(&code) as i32
}

/**
 * xml_ucs_isMongolian:
 * @code: UCS code point
 *
 * Check whether the character is part of Mongolian UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_mongolian(code: c_int) -> c_int {
    (0x1800..=0x18AF).contains(&code) as i32
}

/**
 * xml_ucs_isMusicalSymbols:
 * @code: UCS code point
 *
 * Check whether the character is part of MusicalSymbols UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_musical_symbols(code: c_int) -> c_int {
    (0x1D100..=0x1D1FF).contains(&code) as i32
}

/**
 * xml_ucs_isMyanmar:
 * @code: UCS code point
 *
 * Check whether the character is part of Myanmar UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_myanmar(code: c_int) -> c_int {
    (0x1000..=0x109F).contains(&code) as i32
}

/**
 * xml_ucs_isNumberForms:
 * @code: UCS code point
 *
 * Check whether the character is part of NumberForms UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_number_forms(code: c_int) -> c_int {
    (0x2150..=0x218F).contains(&code) as i32
}

/**
 * xml_ucs_isOgham:
 * @code: UCS code point
 *
 * Check whether the character is part of Ogham UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_ogham(code: c_int) -> c_int {
    (0x1680..=0x169F).contains(&code) as i32
}

/**
 * xml_ucs_isOldItalic:
 * @code: UCS code point
 *
 * Check whether the character is part of OldItalic UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_old_italic(code: c_int) -> c_int {
    (0x10300..=0x1032F).contains(&code) as i32
}

/**
 * xml_ucs_isOpticalCharacterRecognition:
 * @code: UCS code point
 *
 * Check whether the character is part of OpticalCharacterRecognition UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_optical_character_recognition(code: c_int) -> c_int {
    (0x2440..=0x245F).contains(&code) as i32
}

/**
 * xml_ucs_isOriya:
 * @code: UCS code point
 *
 * Check whether the character is part of Oriya UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_oriya(code: c_int) -> c_int {
    (0x0B00..=0x0B7F).contains(&code) as i32
}

/**
 * xml_ucs_isOsmanya:
 * @code: UCS code point
 *
 * Check whether the character is part of Osmanya UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_osmanya(code: c_int) -> c_int {
    (0x10480..=0x104AF).contains(&code) as i32
}

/**
 * xml_ucs_isPhoneticExtensions:
 * @code: UCS code point
 *
 * Check whether the character is part of PhoneticExtensions UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_phonetic_extensions(code: c_int) -> c_int {
    (0x1D00..=0x1D7F).contains(&code) as i32
}

/**
 * xml_ucs_isPrivateUse:
 * @code: UCS code point
 *
 * Check whether the character is part of PrivateUse UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_private_use(code: c_int) -> c_int {
    ((0xE000..=0xF8FF).contains(&code)
        || (0xF0000..=0xFFFFF).contains(&code)
        || (0x100000..=0x10FFFF).contains(&code)) as i32
}

/**
 * xml_ucs_isPrivateUseArea:
 * @code: UCS code point
 *
 * Check whether the character is part of PrivateUseArea UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_private_use_area(code: c_int) -> c_int {
    (0xE000..=0xF8FF).contains(&code) as i32
}

/**
 * xml_ucs_isRunic:
 * @code: UCS code point
 *
 * Check whether the character is part of Runic UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_runic(code: c_int) -> c_int {
    (0x16A0..=0x16FF).contains(&code) as i32
}

/**
 * xml_ucs_isShavian:
 * @code: UCS code point
 *
 * Check whether the character is part of Shavian UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_shavian(code: c_int) -> c_int {
    (0x10450..=0x1047F).contains(&code) as i32
}

/**
 * xml_ucs_isSinhala:
 * @code: UCS code point
 *
 * Check whether the character is part of Sinhala UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_sinhala(code: c_int) -> c_int {
    (0x0D80..=0x0DFF).contains(&code) as i32
}

/**
 * xml_ucs_isSmallFormVariants:
 * @code: UCS code point
 *
 * Check whether the character is part of SmallFormVariants UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_small_form_variants(code: c_int) -> c_int {
    (0xFE50..=0xFE6F).contains(&code) as i32
}

/**
 * xml_ucs_isSpacingModifierLetters:
 * @code: UCS code point
 *
 * Check whether the character is part of SpacingModifierLetters UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_spacing_modifier_letters(code: c_int) -> c_int {
    (0x02B0..=0x02FF).contains(&code) as i32
}

/**
 * xml_ucs_isSpecials:
 * @code: UCS code point
 *
 * Check whether the character is part of Specials UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_specials(code: c_int) -> c_int {
    (0xFFF0..=0xFFFF).contains(&code) as i32
}

/**
 * xml_ucs_isSuperscriptsandSubscripts:
 * @code: UCS code point
 *
 * Check whether the character is part of SuperscriptsandSubscripts UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_superscriptsand_subscripts(code: c_int) -> c_int {
    (0x2070..=0x209F).contains(&code) as i32
}

/**
 * xml_ucs_isSupplementalArrowsA:
 * @code: UCS code point
 *
 * Check whether the character is part of SupplementalArrows-A UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_supplemental_arrows_a(code: c_int) -> c_int {
    (0x27F0..=0x27FF).contains(&code) as i32
}

/**
 * xml_ucs_isSupplementalArrowsB:
 * @code: UCS code point
 *
 * Check whether the character is part of SupplementalArrows-B UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_supplemental_arrows_b(code: c_int) -> c_int {
    (0x2900..=0x297F).contains(&code) as i32
}

/**
 * xml_ucs_isSupplementalMathematicalOperators:
 * @code: UCS code point
 *
 * Check whether the character is part of SupplementalMathematicalOperators UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_supplemental_mathematical_operators(code: c_int) -> c_int {
    (0x2A00..=0x2AFF).contains(&code) as i32
}

/**
 * xml_ucs_isSupplementaryPrivateUseAreaA:
 * @code: UCS code point
 *
 * Check whether the character is part of SupplementaryPrivateUseArea-A UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_supplementary_private_use_area_a(code: c_int) -> c_int {
    (0xF0000..=0xFFFFF).contains(&code) as i32
}

/**
 * xml_ucs_isSupplementaryPrivateUseAreaB:
 * @code: UCS code point
 *
 * Check whether the character is part of SupplementaryPrivateUseArea-B UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_supplementary_private_use_area_b(code: c_int) -> c_int {
    (0x100000..=0x10FFFF).contains(&code) as i32
}

/**
 * xml_ucs_isSyriac:
 * @code: UCS code point
 *
 * Check whether the character is part of Syriac UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_syriac(code: c_int) -> c_int {
    (0x0700..=0x074F).contains(&code) as i32
}

/**
 * xml_ucs_isTagalog:
 * @code: UCS code point
 *
 * Check whether the character is part of Tagalog UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_tagalog(code: c_int) -> c_int {
    (0x1700..=0x171F).contains(&code) as i32
}

/**
 * xml_ucs_isTagbanwa:
 * @code: UCS code point
 *
 * Check whether the character is part of Tagbanwa UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_tagbanwa(code: c_int) -> c_int {
    (0x1760..=0x177F).contains(&code) as i32
}

/**
 * xml_ucs_isTags:
 * @code: UCS code point
 *
 * Check whether the character is part of Tags UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_tags(code: c_int) -> c_int {
    (0xE0000..=0xE007F).contains(&code) as i32
}

/**
 * xml_ucs_isTaiLe:
 * @code: UCS code point
 *
 * Check whether the character is part of TaiLe UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_tai_le(code: c_int) -> c_int {
    (0x1950..=0x197F).contains(&code) as i32
}

/**
 * xml_ucs_isTaiXuanJingSymbols:
 * @code: UCS code point
 *
 * Check whether the character is part of TaiXuanJingSymbols UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_tai_xuan_jing_symbols(code: c_int) -> c_int {
    (0x1D300..=0x1D35F).contains(&code) as i32
}

/**
 * xml_ucs_isTamil:
 * @code: UCS code point
 *
 * Check whether the character is part of Tamil UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_tamil(code: c_int) -> c_int {
    (0x0B80..=0x0BFF).contains(&code) as i32
}

/**
 * xml_ucs_isTelugu:
 * @code: UCS code point
 *
 * Check whether the character is part of Telugu UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_telugu(code: c_int) -> c_int {
    (0x0C00..=0x0C7F).contains(&code) as i32
}

/**
 * xml_ucs_isThaana:
 * @code: UCS code point
 *
 * Check whether the character is part of Thaana UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_thaana(code: c_int) -> c_int {
    (0x0780..=0x07BF).contains(&code) as i32
}

/**
 * xml_ucs_isThai:
 * @code: UCS code point
 *
 * Check whether the character is part of Thai UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_thai(code: c_int) -> c_int {
    (0x0E00..=0x0E7F).contains(&code) as i32
}

/**
 * xml_ucs_isTibetan:
 * @code: UCS code point
 *
 * Check whether the character is part of Tibetan UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_tibetan(code: c_int) -> c_int {
    (0x0F00..=0x0FFF).contains(&code) as i32
}

/**
 * xml_ucs_isUgaritic:
 * @code: UCS code point
 *
 * Check whether the character is part of Ugaritic UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_ugaritic(code: c_int) -> c_int {
    (0x10380..=0x1039F).contains(&code) as i32
}

/**
 * xml_ucs_isUnifiedCanadianAboriginalSyllabics:
 * @code: UCS code point
 *
 * Check whether the character is part of UnifiedCanadianAboriginalSyllabics UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_unified_canadian_aboriginal_syllabics(code: c_int) -> c_int {
    (0x1400..=0x167F).contains(&code) as i32
}

/**
 * xml_ucs_isVariationSelectors:
 * @code: UCS code point
 *
 * Check whether the character is part of VariationSelectors UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_variation_selectors(code: c_int) -> c_int {
    (0xFE00..=0xFE0F).contains(&code) as i32
}

/**
 * xml_ucs_isVariationSelectorsSupplement:
 * @code: UCS code point
 *
 * Check whether the character is part of VariationSelectorsSupplement UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_variation_selectors_supplement(code: c_int) -> c_int {
    (0xE0100..=0xE01EF).contains(&code) as i32
}

/**
 * xml_ucs_isYiRadicals:
 * @code: UCS code point
 *
 * Check whether the character is part of YiRadicals UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_yi_radicals(code: c_int) -> c_int {
    (0xA490..=0xA4CF).contains(&code) as i32
}

/**
 * xml_ucs_isYiSyllables:
 * @code: UCS code point
 *
 * Check whether the character is part of YiSyllables UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_yi_syllables(code: c_int) -> c_int {
    (0xA000..=0xA48F).contains(&code) as i32
}

/**
 * xml_ucs_isYijingHexagramSymbols:
 * @code: UCS code point
 *
 * Check whether the character is part of YijingHexagramSymbols UCS Block
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_yijing_hexagram_symbols(code: c_int) -> c_int {
    (0x4DC0..=0x4DFF).contains(&code) as i32
}

/**
 * xml_ucs_isBlock:
 * @code: UCS code point
 * @block: UCS block name
 *
 * Check whether the character is part of the UCS Block
 *
 * Returns 1 if true, 0 if false and -1 on unknown block
 */
pub unsafe extern "C" fn xml_ucs_is_block(code: c_int, block: *const c_char) -> c_int {
    if let Some(func) = xml_unicode_lookup(&XML_UNICODE_BLOCK_TBL, block) {
        func(code)
    } else {
        -1
    }
}

/**
 * xml_ucs_isCatC:
 * @code: UCS code point
 *
 * Check whether the character is part of C UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_c(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_CG) as i32
}

/**
 * xml_ucs_isCatCc:
 * @code: UCS code point
 *
 * Check whether the character is part of Cc UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_cc(code: c_int) -> c_int {
    ((0x0..=0x1f).contains(&code) || (0x7f..=0x9f).contains(&code)) as i32
}

/**
 * xml_ucs_isCatCf:
 * @code: UCS code point
 *
 * Check whether the character is part of Cf UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_cf(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_CF_G) as i32
}

/**
 * xml_ucs_isCatCo:
 * @code: UCS code point
 *
 * Check whether the character is part of Co UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_co(code: c_int) -> c_int {
    ((code == 0xe000)
        || (code == 0xf8ff)
        || (code == 0xf0000)
        || (code == 0xffffd)
        || (code == 0x100000)
        || (code == 0x10fffd)) as i32
}

/**
 * xml_ucs_isCatCs:
 * @code: UCS code point
 *
 * Check whether the character is part of Cs UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_cs(code: c_int) -> c_int {
    ((code == 0xd800)
        || (0xdb7f..=0xdb80).contains(&code)
        || (0xdbff..=0xdc00).contains(&code)
        || (code == 0xdfff)) as i32
}

/**
 * xml_ucs_isCatL:
 * @code: UCS code point
 *
 * Check whether the character is part of L UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_l(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_LG) as i32
}

/**
 * xml_ucs_isCatLl:
 * @code: UCS code point
 *
 * Check whether the character is part of Ll UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_ll(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_LL_G) as i32
}

/**
 * xml_ucs_isCatLm:
 * @code: UCS code point
 *
 * Check whether the character is part of Lm UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_lm(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_LM_G) as i32
}

/**
 * xml_ucs_isCatLo:
 * @code: UCS code point
 *
 * Check whether the character is part of Lo UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_lo(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_LO_G) as i32
}

/**
 * xml_ucs_isCatLt:
 * @code: UCS code point
 *
 * Check whether the character is part of Lt UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_lt(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_LT_G) as i32
}

/**
 * xml_ucs_isCatLu:
 * @code: UCS code point
 *
 * Check whether the character is part of Lu UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_lu(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_LU_G) as i32
}

/**
 * xml_ucs_isCatM:
 * @code: UCS code point
 *
 * Check whether the character is part of M UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_m(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_MG) as i32
}

/**
 * xml_ucs_isCatMc:
 * @code: UCS code point
 *
 * Check whether the character is part of Mc UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_mc(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_MC_G) as i32
}

/**
 * xml_ucs_isCatMe:
 * @code: UCS code point
 *
 * Check whether the character is part of Me UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_me(code: c_int) -> c_int {
    ((0x488..=0x489).contains(&code)
        || (code == 0x6de)
        || (0x20dd..=0x20e0).contains(&code)
        || (0x20e2..=0x20e4).contains(&code)) as i32
}

/**
 * xml_ucs_isCatMn:
 * @code: UCS code point
 *
 * Check whether the character is part of Mn UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_mn(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_MN_G) as i32
}

/**
 * xml_ucs_isCatN:
 * @code: UCS code point
 *
 * Check whether the character is part of N UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_n(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_NG) as i32
}

/**
 * xml_ucs_isCatNd:
 * @code: UCS code point
 *
 * Check whether the character is part of Nd UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_nd(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_ND_G) as i32
}

/**
 * xml_ucs_isCatNl:
 * @code: UCS code point
 *
 * Check whether the character is part of Nl UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_nl(code: c_int) -> c_int {
    ((0x16ee..=0x16f0).contains(&code)
        || (0x2160..=0x2183).contains(&code)
        || (code == 0x3007)
        || (0x3021..=0x3029).contains(&code)
        || (0x3038..=0x303a).contains(&code)
        || (code == 0x1034a)) as i32
}

/**
 * xml_ucs_isCatNo:
 * @code: UCS code point
 *
 * Check whether the character is part of No UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_no(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_NO_G) as i32
}

/**
 * xml_ucs_isCatP:
 * @code: UCS code point
 *
 * Check whether the character is part of P UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_p(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_PG) as i32
}

/**
 * xml_ucs_isCatPc:
 * @code: UCS code point
 *
 * Check whether the character is part of Pc UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_pc(code: c_int) -> c_int {
    ((code == 0x5f)
        || (0x203f..=0x2040).contains(&code)
        || (code == 0x2054)
        || (code == 0x30fb)
        || (0xfe33..=0xfe34).contains(&code)
        || (0xfe4d..=0xfe4f).contains(&code)
        || (code == 0xff3f)
        || (code == 0xff65)) as i32
}

/**
 * xml_ucs_isCatPd:
 * @code: UCS code point
 *
 * Check whether the character is part of Pd UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_pd(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_PD_G) as i32
}

/**
 * xml_ucs_isCatPe:
 * @code: UCS code point
 *
 * Check whether the character is part of Pe UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_pe(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_PE_G) as i32
}

/**
 * xml_ucs_isCatPf:
 * @code: UCS code point
 *
 * Check whether the character is part of Pf UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_pf(code: c_int) -> c_int {
    ((code == 0xbb) || (code == 0x2019) || (code == 0x201d) || (code == 0x203a)) as i32
}

/**
 * xml_ucs_isCatPi:
 * @code: UCS code point
 *
 * Check whether the character is part of Pi UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_pi(code: c_int) -> c_int {
    ((code == 0xab)
        || (code == 0x2018)
        || (0x201b..=0x201c).contains(&code)
        || (code == 0x201f)
        || (code == 0x2039)) as i32
}

/**
 * xml_ucs_isCatPo:
 * @code: UCS code point
 *
 * Check whether the character is part of Po UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_po(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_PO_G) as i32
}

/**
 * xml_ucs_isCatPs:
 * @code: UCS code point
 *
 * Check whether the character is part of Ps UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_ps(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_PS_G) as i32
}

/**
 * xml_ucs_isCatS:
 * @code: UCS code point
 *
 * Check whether the character is part of S UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_s(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_SG) as i32
}

/**
 * xml_ucs_isCatSc:
 * @code: UCS code point
 *
 * Check whether the character is part of Sc UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_sc(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_SC_G) as i32
}

/**
 * xml_ucs_isCatSk:
 * @code: UCS code point
 *
 * Check whether the character is part of Sk UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_sk(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_SK_G) as i32
}

/**
 * xml_ucs_isCatSm:
 * @code: UCS code point
 *
 * Check whether the character is part of Sm UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_sm(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_SM_G) as i32
}

/**
 * xml_ucs_isCatSo:
 * @code: UCS code point
 *
 * Check whether the character is part of So UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_so(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_SO_G) as i32
}

/**
 * xml_ucs_isCatZ:
 * @code: UCS code point
 *
 * Check whether the character is part of Z UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_z(code: c_int) -> c_int {
    xml_char_in_range(code as c_uint, &XML_ZG) as i32
}

/**
 * xml_ucs_isCatZl:
 * @code: UCS code point
 *
 * Check whether the character is part of Zl UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_zl(code: c_int) -> c_int {
    (code == 0x2028) as i32
}

/**
 * xml_ucs_isCatZp:
 * @code: UCS code point
 *
 * Check whether the character is part of Zp UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_zp(code: c_int) -> c_int {
    (code == 0x2029) as i32
}

/**
 * xml_ucs_isCatZs:
 * @code: UCS code point
 *
 * Check whether the character is part of Zs UCS Category
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_ucs_is_cat_zs(code: c_int) -> c_int {
    ((code == 0x20)
        || (code == 0xa0)
        || (code == 0x1680)
        || (code == 0x180e)
        || (0x2000..=0x200a).contains(&code)
        || (code == 0x202f)
        || (code == 0x205f)
        || (code == 0x3000)) as i32
}

/**
 * xml_ucs_isCat:
 * @code: UCS code point
 * @cat: UCS Category name
 *
 * Check whether the character is part of the UCS Category
 *
 * Returns 1 if true, 0 if false and -1 on unknown category
 */
pub unsafe extern "C" fn xml_ucs_is_cat(code: c_int, cat: *const c_char) -> c_int {
    if let Some(func) = xml_unicode_lookup(&XML_UNICODE_CAT_TBL, cat) {
        func(code)
    } else {
        -1
    }
}

/**
 * xmlUnicodeLookup:
 * @tptr: pointer to the name table
 * @name: name to be found
 *
 * binary table lookup for user-supplied name
 *
 * Returns pointer to range function if found, otherwise NULL
 */
unsafe extern "C" fn xml_unicode_lookup(
    tptr: *const XmlUnicodeNameTable,
    tname: *const c_char,
) -> Option<XmlIntFunc> {
    let mut low: c_int;
    let mut high: c_int;
    let mut mid: c_int;
    let mut cmp: c_int;

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
    use crate::{
        libxml::{xmlerror::xml_reset_last_error, xmlmemory::xml_mem_blocks},
        test_util::*,
    };

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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                    xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                    xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
