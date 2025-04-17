use std::{fmt::Debug, str::FromStr};

use crate::libxml::schemas_internals::XmlSchemaValType;

use super::{XmlSchemaVal, XmlSchemaValPrimitives, is_wsp_blank_ch, primitives::XmlSchemaValDate};

#[doc(alias = "VALID_YEAR")]
fn validate_year(year: i64) -> bool {
    year != 0
}

#[doc(alias = "VALID_MONTH")]
fn validate_month(month: u8) -> bool {
    (1..=12).contains(&month)
}

// VALID_DAY should only be used when month is unknown
#[doc(alias = "VALID_DAY")]
fn validate_day(day: u8) -> bool {
    (1..=31).contains(&day)
}

#[doc(alias = "VALID_HOUR")]
fn validate_hour(hour: u8) -> bool {
    (0..24).contains(&hour)
}

#[doc(alias = "VALID_MIN")]
fn validate_minute(min: u8) -> bool {
    (0..60).contains(&min)
}

#[doc(alias = "VALID_SEC")]
fn validate_second(sec: f64) -> bool {
    (0.0..60.0).contains(&sec)
}

#[doc(alias = "VALID_END_OF_DAY")]
fn validate_end_of_day(dt: &XmlSchemaValDate) -> bool {
    dt.hour == 24 && dt.min == 0 && dt.sec == 0.0
}

#[doc(alias = "IS_TZO_CHAR")]
fn is_tzo_char(c: char) -> bool {
    matches!(c, 'Z' | '+' | '-')
}
fn starts_with_tzo_char(s: &str) -> bool {
    s.chars().next().is_none_or(is_tzo_char)
}

#[doc(alias = "VALID_TZO")]
fn validate_tzo(tzo: i16) -> bool {
    (-840..=840).contains(&tzo)
}

#[doc(alias = "VALID_TIME")]
fn validate_time(dt: &XmlSchemaValDate) -> bool {
    ((validate_hour(dt.hour) && validate_minute(dt.min) && validate_second(dt.sec))
        || validate_end_of_day(dt))
        && validate_tzo(dt.tzo)
}

#[doc(alias = "IS_LEAP")]
fn is_leap(year: i64) -> bool {
    (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
}

const DAYS_IN_MONTH: [u8; 12] = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
const DAYS_IN_MONTH_LEAP: [u8; 12] = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

#[doc(alias = "VALID_MDAY")]
fn validate_mday(dt: &XmlSchemaValDate) -> bool {
    if is_leap(dt.year) {
        dt.day <= DAYS_IN_MONTH_LEAP[dt.mon as usize - 1]
    } else {
        dt.day <= DAYS_IN_MONTH[dt.mon as usize - 1]
    }
}

#[doc(alias = "VALID_DATE")]
fn validate_date(dt: &XmlSchemaValDate) -> bool {
    validate_year(dt.year) && validate_month(dt.mon) && validate_mday(dt)
}

#[doc(alias = "VALID_DATETIME")]
fn validate_datetime(dt: &XmlSchemaValDate) -> bool {
    validate_date(dt) && validate_time(dt)
}

/// Parse the first two digits of a string `s`.
///
/// The value must be exactly two digits.  
/// That is, even if the value is less than 10, it must be two digits with leading zeros added.
///
/// If successfully parsed, return parsed value and advance `s` two digits.  
/// Otherwise, return `None`.
#[doc(alias = "PARSE_2_DIGITS")]
fn parse_2digits<F>(s: &mut &str) -> Option<F>
where
    F: FromStr,
    F::Err: Debug,
{
    let cur = *s;
    let rem = cur
        .strip_prefix(|c: char| c.is_ascii_digit())
        .and_then(|cur| cur.strip_prefix(|c: char| c.is_ascii_digit()))?;
    *s = rem;
    Some(cur[..2].parse().unwrap())
}

/// Parse the floating point number.
///
/// This functions assumes `s` starts with `xx` or `xx.xxxx` ('x' is an ASCII digit).  
/// In other words, this function assumes that a string that can be expressed
/// by regular expression "\d{2}(\.\d+)?" is at the beginning.  
/// Parsing of input that does not follow this format will fail.
///
/// If successfully parsed, return parsed value and advance `s` the length of value.  
/// Otherwise, return `None`.
#[doc(alias = "PARSE_FLOAT")]
fn parse_float(s: &mut &str) -> Option<f64> {
    let mut cur = *s;
    let mut val = parse_2digits::<u8>(&mut cur)? as f64;
    if let Some(rem) = cur.strip_prefix('.') {
        let pos = rem
            .bytes()
            .position(|c| !c.is_ascii_digit())
            .unwrap_or(rem.len());
        // if a '.' is found, at least one digit needs to follow.
        if pos == 0 {
            return None;
        }
        let (dig, rem) = rem.split_at(pos);
        cur = rem;
        // Maybe not the best or most accurate parsing method,
        // but follow the original method.
        let mut mult = 1.;
        for b in dig.bytes() {
            mult /= 10.;
            val += (b - b'0') as f64 * mult;
        }
    }
    *s = cur;
    Some(val)
}

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

/// Parses a xs:gYear without time zone and fills in the appropriate
/// field of the @dt structure. @str is updated to point just after the
/// xs:gYear. It is supposed that @(*dt).year is big enough to contain the year.
///
/// Returns 0 or the error code
#[doc(alias = "_xmlSchemaParseGYear")]
fn parse_gyear(s: &mut &str, dt: &mut XmlSchemaValDate) -> Option<()> {
    let mut cur = *s;
    if !cur.starts_with(|c: char| c.is_ascii_digit() || c == '-' || c == '+') {
        return None;
    }

    let mut isneg = false;
    if let Some(rem) = cur.strip_prefix('-') {
        isneg = true;
        cur = rem;
    } else if let Some(rem) = cur.strip_prefix('+') {
        cur = rem;
    }

    let first_char = cur;
    let pos = cur
        .bytes()
        .position(|b| !b.is_ascii_digit())
        .unwrap_or(cur.len());
    // year must be at least 4 digits (CCYY); over 4
    // digits cannot have a leading zero.
    if pos < 4 || (pos > 4 && first_char.starts_with('0')) {
        return None;
    }
    let (dig, cur) = cur.split_at(pos);
    dt.year = dig.parse().ok()?;

    if isneg {
        dt.year = -dt.year;
    }

    if !validate_year(dt.year) {
        return None;
    }

    *s = cur;
    Some(())
}

/// Parses a xs:gMonth without time zone and fills in the appropriate
/// field of the @dt structure. @str is updated to point just after the xs:gMonth.
///
/// Returns 0 or the error code
#[doc(alias = "_xmlSchemaParseGMonth")]
fn parse_gmonth(s: &mut &str, dt: &mut XmlSchemaValDate) -> Option<()> {
    let mut cur = *s;
    dt.mon = parse_2digits::<u8>(&mut cur).filter(|mon| validate_month(*mon))?;
    *s = cur;
    Some(())
}

/// Parses a xs:gDay without time zone and fills in the appropriate
/// field of the @dt structure. @str is updated to point just after the xs:gDay.
///
/// Returns 0 or the error code
#[doc(alias = "_xmlSchemaParseGDay")]
fn parse_gday(s: &mut &str, dt: &mut XmlSchemaValDate) -> Option<()> {
    let mut cur = *s;
    dt.day = parse_2digits::<u8>(&mut cur).filter(|day| validate_day(*day))?;
    *s = cur;
    Some(())
}

/// Parses a xs:time without time zone and fills in the appropriate
/// fields of the @dt structure. @str is updated to point just after the xs:time.
/// In case of error, values of @dt fields are undefined.
///
/// Returns 0 or the error code
#[doc(alias = "_xmlSchemaParseTime")]
fn parse_time(s: &mut &str, dt: &mut XmlSchemaValDate) -> Option<()> {
    let mut cur = *s;
    let value = parse_2digits::<u8>(&mut cur).filter(|hour| {
        // Allow end-of-day hour
        // not `(0..=23)`, `(0..=24)` is correct.
        (0..=24).contains(hour)
    })?;

    let mut cur = cur.strip_prefix(':')?;

    // the ':' insures this string is xs:time
    dt.hour = value;

    dt.min = parse_2digits::<u8>(&mut cur).filter(|min| validate_minute(*min))?;

    let mut cur = cur.strip_prefix(':')?;

    let val = parse_2digits::<u8>(&mut cur)?;
    dt.sec = val as f64;
    if let Some(rem) = cur.strip_prefix('.') {
        let pos = rem
            .bytes()
            .position(|c| !c.is_ascii_digit())
            .unwrap_or(rem.len());
        if pos == 0 {
            return None;
        }
        let (dig, rem) = rem.split_at(pos);
        cur = rem;
        let mut mult = 1.;
        for b in dig.bytes() {
            mult /= 10.;
            dt.sec += (b - b'0') as f64 * mult;
        }
    }

    if !validate_time(dt) {
        return None;
    }

    *s = cur;
    Some(())
}

/// Parses a time zone without time zone and fills in the appropriate
/// field of the @dt structure. @str is updated to point just after the time zone.
///
/// Returns 0 or the error code
#[doc(alias = "_xmlSchemaParseTimeZone")]
fn parse_time_zone(s: &mut &str, dt: &mut XmlSchemaValDate) -> Option<()> {
    let mut cur = *s;

    match cur.as_bytes().first() {
        None => {
            dt.tz_flag = 0;
            dt.tzo = 0;
        }
        Some(&b'Z') => {
            dt.tz_flag = 1;
            dt.tzo = 0;
            *s = &cur[1..];
        }
        Some(&sgn @ (b'+' | b'-')) => {
            let isneg = sgn == b'-';
            cur = &cur[1..];

            let tmp = parse_2digits::<u8>(&mut cur).filter(|hour| validate_hour(*hour))?;
            let mut cur = cur.strip_prefix(':')?;
            dt.tzo = tmp as i16 * 60;

            let tmp = parse_2digits::<u8>(&mut cur).filter(|min| validate_minute(*min))?;
            dt.tzo += tmp as i16;
            if isneg {
                dt.tzo = -dt.tzo;
            }

            if !validate_tzo(dt.tzo) {
                return None;
            }

            dt.tz_flag = 1;
            *s = cur;
        }
        _ => {
            return None;
        }
    }
    Some(())
}

/// Check that @dateTime conforms to the lexical space of one of the date types.
/// if true a value is computed and returned in @val.
///
/// Returns 0 if this validates, a positive error code number otherwise
/// and -1 in case of internal or API error.
#[doc(alias = "xmlSchemaValidateDates")]
fn validate_dates(typ: XmlSchemaValType, date_time: &str, collapse: bool) -> Option<XmlSchemaVal> {
    let mut cur = date_time;

    if collapse {
        cur = cur.trim_matches(is_wsp_blank_ch);
    }

    if !cur.starts_with(|c: char| c == '-' || c.is_ascii_digit()) {
        return None;
    }

    let mut dt = XmlSchemaValDate::default();
    let make_val =
        |remain: &str, dt: XmlSchemaValDate, expect: XmlSchemaValType| -> Option<XmlSchemaVal> {
            if !remain.is_empty() || (typ != XmlSchemaValType::XmlSchemasUnknown && typ != expect) {
                return None;
            }
            Some(XmlSchemaVal {
                typ: expect,
                next: None,
                value: XmlSchemaValPrimitives::Date(dt),
            })
        };

    if let Some(rem) = cur.strip_prefix("--") {
        // It's an incomplete date (xs:gMonthDay, xs:gMonth or xs:gDay)
        cur = rem;

        // is it an xs:gDay?
        if let Some(rem) = cur.strip_prefix('-') {
            if typ == XmlSchemaValType::XmlSchemasGMonth {
                return None;
            }
            cur = rem;
            parse_gday(&mut cur, &mut dt)?;

            if starts_with_tzo_char(cur) && parse_time_zone(&mut cur, &mut dt).is_some() {
                return make_val(cur, dt, XmlSchemaValType::XmlSchemasGDay);
            };

            return None;
        }

        // it should be an xs:gMonthDay or xs:gMonth
        parse_gmonth(&mut cur, &mut dt)?;

        // a '-' c_char could indicate this type is xs:gMonthDay or
        // a negative time zone offset. Check for xs:gMonthDay first.
        // Also the first three c_char's of a negative tzo (-MM:SS) can
        // appear to be a valid day; so even if the day portion
        // of the xs:gMonthDay verifies, we must insure it was not
        // a tzo.
        if let Some(rem) = cur.strip_prefix('-') {
            let rewnd = cur;
            cur = rem;

            if parse_gday(&mut cur, &mut dt).is_some() && (cur.is_empty() || !cur.starts_with(':'))
            {
                // we can use the VALID_MDAY macro to validate the month
                // and day because the leap year test will flag year zero
                // as a leap year (even though zero is an invalid year).
                // FUTURE TODO: Zero will become valid in XML Schema 1.1
                // probably.
                if validate_mday(&dt) {
                    if starts_with_tzo_char(cur) {
                        parse_time_zone(&mut cur, &mut dt)?;
                        return make_val(cur, dt, XmlSchemaValType::XmlSchemasGMonthDay);
                    };

                    return None;
                }
            }

            // not xs:gMonthDay so rewind and check if just xs:gMonth
            // with an optional time zone.
            cur = rewnd;
        }

        if starts_with_tzo_char(cur) && parse_time_zone(&mut cur, &mut dt).is_some() {
            return make_val(cur, dt, XmlSchemaValType::XmlSchemasGMonth);
        };

        return None;
    }

    // It's a right-truncated date or an xs:time.
    // Try to parse an xs:time then fallback on right-truncated dates.
    if cur.starts_with(|c: char| c.is_ascii_digit()) && parse_time(&mut cur, &mut dt).is_some() {
        // it's an xs:time
        if starts_with_tzo_char(cur) && parse_time_zone(&mut cur, &mut dt).is_some() {
            return make_val(cur, dt, XmlSchemaValType::XmlSchemasTime);
        };
    }

    // fallback on date parsing
    cur = date_time;

    parse_gyear(&mut cur, &mut dt)?;

    // is it an xs:gYear?
    if starts_with_tzo_char(cur) && parse_time_zone(&mut cur, &mut dt).is_some() {
        return make_val(cur, dt, XmlSchemaValType::XmlSchemasGYear);
    };

    cur = cur.strip_prefix('-')?;

    parse_gmonth(&mut cur, &mut dt)?;

    // is it an xs:gYearMonth?
    if starts_with_tzo_char(cur) && parse_time_zone(&mut cur, &mut dt).is_some() {
        return make_val(cur, dt, XmlSchemaValType::XmlSchemasGYearMonth);
    };

    cur = cur.strip_prefix('-')?;

    parse_gday(&mut cur, &mut dt).filter(|_| validate_date(&dt))?;

    // is it an xs:date?
    if starts_with_tzo_char(cur) && parse_time_zone(&mut cur, &mut dt).is_some() {
        return make_val(cur, dt, XmlSchemaValType::XmlSchemasDate);
    };

    cur = cur.strip_prefix('T')?;

    // it should be an xs:dateTime
    parse_time(&mut cur, &mut dt)?;

    parse_time_zone(&mut cur, &mut dt).filter(|_| validate_datetime(&dt))?;

    make_val(cur, dt, XmlSchemaValType::XmlSchemasDatetime)
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

    #[test]
    fn parse_time_test() {
        let mut dt = XmlSchemaValDate::default();

        let mut s = "00:00:00";
        assert!(parse_time(&mut s, &mut dt).is_some());
        assert!(dt.hour == 0 && dt.min == 0 && dt.sec == 0.0);

        let mut s = "19:30:45";
        assert!(parse_time(&mut s, &mut dt).is_some());
        assert!(dt.hour == 19 && dt.min == 30 && dt.sec == 45.0);

        let mut s = "21:15:30.500";
        assert!(parse_time(&mut s, &mut dt).is_some());
        assert!(dt.hour == 21 && dt.min == 15 && dt.sec == 30.5);

        let mut s = "24:00:00.000";
        assert!(parse_time(&mut s, &mut dt).is_some());
        assert!(dt.hour == 24 && dt.min == 0 && dt.sec == 0.0);

        // https://tc39.es/proposal-uniform-interchange-date-parsing/cases.html

        // Positive leap second -> it is not supported ??
        // let mut s = "23:59:60Z";
        // assert!(parse_time(&mut s, &mut dt).is_some());

        // Too few fractional second digits
        let mut s = "14:00:00.9Z";
        assert!(parse_time(&mut s, &mut dt).is_some());
        assert!(dt.hour == 14 && dt.min == 0 && dt.sec == 0.9);

        // Too many fractional second digits
        let mut s = "14:00:00.4999Z";
        assert!(parse_time(&mut s, &mut dt).is_some());
        assert!(dt.hour == 14 && dt.min == 0 && dt.sec == 0.4999);

        // Comma as decimal sign -> it is not supported ??
        // let mut s = "14:00:00,999Z";
        // assert!(parse_time(&mut s, &mut dt).is_some());
        // assert!(dt.hour == 14 && dt.min == 0 && dt.sec == 0.999);

        // Hours-only offset -> it is not supported ??
        // let mut s = "10:00-04";
        // assert!(parse_time(&mut s, &mut dt).is_some());

        // Fractional minuts -> what does it mean ??
        // let mut s = "14:00.9Z";
        // assert!(parse_time(&mut s, &mut dt).is_some());
        // assert!(dt.hour == 14 && dt.min == 0 && dt.sec == )
    }

    #[test]
    fn parse_gyear_test() {
        let mut dt = XmlSchemaValDate::default();

        let mut s = "2025";
        assert!(parse_gyear(&mut s, &mut dt).is_some());
        assert_eq!(dt.year, 2025);

        let mut s = "0205";
        assert!(parse_gyear(&mut s, &mut dt).is_some());
        assert_eq!(dt.year, 205);

        let mut s = "0025";
        assert!(parse_gyear(&mut s, &mut dt).is_some());
        assert_eq!(dt.year, 25);

        let mut s = "0002";
        assert!(parse_gyear(&mut s, &mut dt).is_some());
        assert_eq!(dt.year, 2);

        let mut s = "-2025";
        assert!(parse_gyear(&mut s, &mut dt).is_some());
        assert_eq!(dt.year, -2025);

        let mut s = "20250205";
        assert!(parse_gyear(&mut s, &mut dt).is_some());
        assert_eq!(dt.year, 20250205);

        let mut s = "02025";
        assert!(parse_gyear(&mut s, &mut dt).is_none());

        let mut s = "205";
        assert!(parse_gyear(&mut s, &mut dt).is_none());
    }

    #[test]
    fn validate_dates_test() {
        use XmlSchemaValPrimitives::*;
        use XmlSchemaValType::*;
        let s = "2019-03-26T14:00:00.999Z";
        let val = validate_dates(XmlSchemasUnknown, s, false);
        assert!(val.is_some());
        let val = val.unwrap();
        assert!(matches!(val.value, Date(_)));
        assert_eq!(val.typ, XmlSchemasDatetime);
        let Date(date) = val.value else {
            unreachable!()
        };
        assert!(
            date.year == 2019
                && date.mon == 3
                && date.day == 26
                && date.hour == 14
                && date.min == 0
                && date.sec == 0.999
                && date.tzo == 0
        );
    }
}
