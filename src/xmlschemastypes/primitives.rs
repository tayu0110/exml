use std::rc::Rc;

/// Implement [3.2.3 decimal](https://www.w3.org/TR/xmlschema-2/#decimal)
/// of [XML Schema Part 2: Datatypes](https://www.w3.org/TR/xmlschema-2)
#[derive(Debug, Clone, Default)]
pub(super) struct XmlSchemaValDecimal {
    lo: u64,
    mi: u64,
    hi: u64,
    extra: u32,
    sign: u8,
    frac: u8,
    total: u8,
}

/// Implement [3.2.6 duration](https://www.w3.org/TR/xmlschema-2/#duration)
/// of [XML Schema Part 2: Datatypes](https://www.w3.org/TR/xmlschema-2)
#[derive(Debug, Clone, Default)]
pub(super) struct XmlSchemaValDuration {
    mon: i64,
    day: i64,
    sec: i64,
}

/// Implement [3.2.9 date](https://www.w3.org/TR/xmlschema-2/#date)
/// of [XML Schema Part 2: Datatypes](https://www.w3.org/TR/xmlschema-2)
#[derive(Debug, Clone, Default)]
pub(super) struct XmlSchemaValDate {
    year: i64,
    mon: u8,
    day: u8,
    pub(super) hour: u8,
    pub(super) min: u8,
    pub(super) sec: f64,
    // is tzo explicitly set?
    pub(super) tz_flag: u8,
    // -1440 <= tzo <= 1440;
    // currently only -840 to +840 are needed
    pub(super) tzo: i16,
}

/// Implement [3.2.15 hexBinary](https://www.w3.org/TR/xmlschema-2/#hexBinary)
/// of [XML Schema Part 2: Datatypes](https://www.w3.org/TR/xmlschema-2)
#[derive(Debug, Clone, Default)]
pub(super) struct XmlSchemaValHex {
    s: Rc<str>,
    total: u32,
}

/// Implement [3.2.16 base64Binary](https://www.w3.org/TR/xmlschema-2/#base64Binary)
/// of [XML Schema Part 2: Datatypes](https://www.w3.org/TR/xmlschema-2)
#[derive(Debug, Clone, Default)]
pub(super) struct XmlSchemaValBase64 {
    s: Rc<str>,
    total: u32,
}

/// Implement [3.2.18 QName](https://www.w3.org/TR/xmlschema-2/#QName)
/// of [XML Schema Part 2: Datatypes](https://www.w3.org/TR/xmlschema-2)
#[derive(Debug, Clone, Default)]
pub(super) struct XmlSchemaValQName {
    name: Rc<str>,
    uri: Rc<str>,
}

/// Implement [3.2 Primitive datatypes](https://www.w3.org/TR/xmlschema-2/#built-in-primitive-datatypes)
/// of [XML Schema Part 2: Datatypes](https://www.w3.org/TR/xmlschema-2)
#[derive(Debug, Clone)]
pub(super) enum XmlSchemaValPrimitives {
    String(Rc<str>),
    Boolean(bool),
    Decimal(XmlSchemaValDecimal),
    Float(f32),
    Double(f64),
    Duration(XmlSchemaValDuration),
    Date(XmlSchemaValDate),
    Hex(XmlSchemaValHex),
    Base64(XmlSchemaValBase64),
    QName(XmlSchemaValQName),
}
