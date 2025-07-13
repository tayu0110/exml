use std::{rc::Rc, sync::Arc};

use crate::dom::{DOMException, dom_error::DOMErrorHandler};

#[derive(Clone)]
pub enum DOMConfigurationParameter {
    CanonicalForm(bool),
    CDATASections(bool),
    CheckCharacterNormalization(bool),
    Comments(bool),
    DatatypeNormalization(bool),
    ElementContentWhitespace(bool),
    Entities(bool),
    ErrorHandler(Arc<dyn DOMErrorHandler>),
    Infoset(bool),
    Namespaces(bool),
    NamespaceDeclarations(bool),
    NormalizeCharacters(bool),
    SchemaLocation(Arc<str>),
    SchemaType(Arc<str>),
    SplitCDATASections(bool),
    Validate(bool),
    ValidateIfSchema(bool),
    WellFormed(bool),
    CharsetOverridesXMLEncoding(bool),
    DisallowDoctype(bool),
    IgnoreUnknownCharacterDenormalizations(bool),
    ResourceResolver, // TODO
    SupportedMediaTypesOnly(bool),
    DiscardDefaultContent(bool),
    FormatPrettyPrint(bool),
    XMLDeclaration(bool),
}

impl From<DOMConfigurationParameter> for &'static str {
    fn from(value: DOMConfigurationParameter) -> Self {
        match value {
            DOMConfigurationParameter::CanonicalForm(_) => "canonical-form",
            DOMConfigurationParameter::CDATASections(_) => "cdata-sections",
            DOMConfigurationParameter::CheckCharacterNormalization(_) => {
                "check-character-normalization"
            }
            DOMConfigurationParameter::Comments(_) => "comments",
            DOMConfigurationParameter::DatatypeNormalization(_) => "datatype-normalization",
            DOMConfigurationParameter::ElementContentWhitespace(_) => "element-content-whitespace",
            DOMConfigurationParameter::Entities(_) => "entities",
            DOMConfigurationParameter::ErrorHandler(_) => "error-handler",
            DOMConfigurationParameter::Infoset(_) => "infoset",
            DOMConfigurationParameter::Namespaces(_) => "namespaces",
            DOMConfigurationParameter::NamespaceDeclarations(_) => "namespace-declarations",
            DOMConfigurationParameter::NormalizeCharacters(_) => "normalize-characters",
            DOMConfigurationParameter::SchemaLocation(_) => "schema-location",
            DOMConfigurationParameter::SchemaType(_) => "schema-type",
            DOMConfigurationParameter::SplitCDATASections(_) => "split-cdata-sections",
            DOMConfigurationParameter::Validate(_) => "validate",
            DOMConfigurationParameter::ValidateIfSchema(_) => "validate-if-schema",
            DOMConfigurationParameter::WellFormed(_) => "well-formed",
            DOMConfigurationParameter::CharsetOverridesXMLEncoding(_) => {
                "charset-overrides-xml-encoding"
            }
            DOMConfigurationParameter::DisallowDoctype(_) => "disallow-doctype",
            DOMConfigurationParameter::IgnoreUnknownCharacterDenormalizations(_) => {
                "ignore-unknown-character-denormalizations"
            }
            DOMConfigurationParameter::ResourceResolver => "resource-resolver",
            DOMConfigurationParameter::SupportedMediaTypesOnly(_) => "supported-media-types-only",
            DOMConfigurationParameter::DiscardDefaultContent(_) => "discard-default-content",
            DOMConfigurationParameter::FormatPrettyPrint(_) => "format-pretty-print",
            DOMConfigurationParameter::XMLDeclaration(_) => "xml-declaration",
        }
    }
}

impl<S: AsRef<str>> TryFrom<(S, bool)> for DOMConfigurationParameter {
    type Error = DOMException;

    fn try_from((r#type, bool): (S, bool)) -> Result<Self, Self::Error> {
        match r#type.as_ref() {
            "canonical-form" => Ok(Self::CanonicalForm(bool)),
            "cdata-sections" => Ok(Self::CDATASections(bool)),
            "check-character-normalization" => Ok(Self::CheckCharacterNormalization(bool)),
            "comments" => Ok(Self::Comments(bool)),
            "datatype-normalization" => Ok(Self::DatatypeNormalization(bool)),
            "element-content-whitespace" => Ok(Self::ElementContentWhitespace(bool)),
            "entities" => Ok(Self::Entities(bool)),
            "infoset" => Ok(Self::Infoset(bool)),
            "namespaces" => Ok(Self::Namespaces(bool)),
            "namespace-declarations" => Ok(Self::NamespaceDeclarations(bool)),
            "normalize-characters" => Ok(Self::NormalizeCharacters(bool)),
            "split-cdata-sections" => Ok(Self::SplitCDATASections(bool)),
            "validate" => Ok(Self::Validate(bool)),
            "validate-if-schema" => Ok(Self::ValidateIfSchema(bool)),
            "well-formed" => Ok(Self::WellFormed(bool)),
            "charset-overrides-xml-encoding" => Ok(Self::CharsetOverridesXMLEncoding(bool)),
            "disallow-doctype" => Ok(Self::DisallowDoctype(bool)),
            "ignore-unknown-character-denormalizations" => {
                Ok(Self::IgnoreUnknownCharacterDenormalizations(bool))
            }
            "supported-media-types-only" => Ok(Self::SupportedMediaTypesOnly(bool)),
            "discard-default-content" => Ok(Self::DiscardDefaultContent(bool)),
            "format-pretty-print" => Ok(Self::FormatPrettyPrint(bool)),
            "xml-declaration" => Ok(Self::XMLDeclaration(bool)),
            r#type
                if DOMConfiguration::PARAMETER_NAMES
                    .binary_search(&r#type)
                    .is_ok() =>
            {
                Err(DOMException::TypeMismatchErr)
            }
            _ => Err(DOMException::NotSupportedErr),
        }
    }
}

impl<S: AsRef<str>> TryFrom<(S, Arc<str>)> for DOMConfigurationParameter {
    type Error = DOMException;

    fn try_from((r#type, value): (S, Arc<str>)) -> Result<Self, Self::Error> {
        match r#type.as_ref() {
            "schema-location" => Ok(Self::SchemaLocation(value)),
            "schema-type" => Ok(Self::SchemaType(value)),
            r#type
                if DOMConfiguration::PARAMETER_NAMES
                    .binary_search(&r#type)
                    .is_ok() =>
            {
                Err(DOMException::TypeMismatchErr)
            }
            _ => Err(DOMException::NotFoundErr),
        }
    }
}

impl<S: AsRef<str>> TryFrom<(S, &str)> for DOMConfigurationParameter {
    type Error = DOMException;

    fn try_from((r#type, value): (S, &str)) -> Result<Self, Self::Error> {
        match r#type.as_ref() {
            "schema-location" => Ok(Self::SchemaLocation(value.into())),
            "schema-type" => Ok(Self::SchemaType(value.into())),
            r#type
                if DOMConfiguration::PARAMETER_NAMES
                    .binary_search(&r#type)
                    .is_ok() =>
            {
                Err(DOMException::TypeMismatchErr)
            }
            _ => Err(DOMException::NotFoundErr),
        }
    }
}

impl<S: AsRef<str>> TryFrom<(S, String)> for DOMConfigurationParameter {
    type Error = DOMException;

    fn try_from((r#type, value): (S, String)) -> Result<Self, Self::Error> {
        (r#type, value.as_str()).try_into()
    }
}

impl<S: AsRef<str>> TryFrom<(S, Rc<str>)> for DOMConfigurationParameter {
    type Error = DOMException;

    fn try_from((r#type, value): (S, Rc<str>)) -> Result<Self, Self::Error> {
        (r#type, value.as_ref()).try_into()
    }
}

#[derive(Clone)]
pub struct DOMConfiguration {
    /// The state of parameters described in the specification.\
    /// Some flags do not take boolean values but are reserved for future use.
    ///
    /// 0 ~ 17  : DOM Level 3 Core.\
    /// 18 ~ 25 : DOM Level 3 Load and Save.
    ///
    /// | offset | parameter                                    | default |
    /// | -----: | :------------------------------------------- | :-----: |
    /// |    0   | "canonical-form"                             |  false  |
    /// |    1   | "cdata-sections"                             |  true   |
    /// |    2   | "check-character-normalization"              |  false  |
    /// |    3   | "comments"                                   |  true   |
    /// |    4   | "datatype-normalization"                     |  false  |
    /// |    5   | "element-content-whitespace"                 |  true   |
    /// |    6   | "entities"                                   |  true   |
    /// |    7   | "error-handler"                              |    -    |
    /// |    8   | "infoset"                                    |  true   |
    /// |    9   | "namespaces"                                 |  true   |
    /// |   10   | "namespace-declarations"                     |  true   |
    /// |   11   | "normalize-characters"                       |  false  |
    /// |   12   | "schema-location"                            |    -    |
    /// |   13   | "schema-type"                                |    -    |
    /// |   14   | "split-cdata-sections"                       |  true   |
    /// |   15   | "validate"                                   |  false  |
    /// |   16   | "validate-if-schema"                         |  false  |
    /// |   17   | "well-formed"                                |  true   |
    /// |   18   | "charset-overrides-xml-encoding"             |  true   |
    /// |   19   | "disallow-doctype"                           |  false  |
    /// |   20   | "ignore-unknown-character-denormalizations"  |  true   |
    /// |   21   | "resource-resolver"                          |    -    |
    /// |   22   | "supported-media-types-only"                 |  false  |
    /// |   23   | "discard-default-content"                    |  true   |
    /// |   24   | "format-pretty-print"                        |  false  |
    /// |   25   | "xml-declaration"                            |  true   |
    flags: u64,

    error_handler: Option<Arc<dyn DOMErrorHandler>>,
    schema_location: Option<Arc<str>>,
    schema_type: Option<Arc<str>>,
}

impl DOMConfiguration {
    // Keep lexical order for binary search.
    const PARAMETER_NAMES: &[&str] = &[
        "canonical-form",
        "cdata-sections",
        "charset-overrides-xml-encoding",
        "check-character-normalization",
        "comments",
        "datatype-normalization",
        "disallow-doctype",
        "discard-default-content",
        "element-content-whitespace",
        "entities",
        "error-handler",
        "format-pretty-print",
        "ignore-unknown-character-denormalizations",
        "infoset",
        "namespace-declarations",
        "namespaces",
        "normalize-characters",
        "resource-resolver",
        "schema-location",
        "schema-type",
        "split-cdata-sections",
        "supported-media-types-only",
        "validate",
        "validate-if-schema",
        "well-formed",
        "xml-declaration",
    ];
    // Align the order with PARAMETER_NAMES.
    const FLAG_POSITION: &[usize] = &[
        0,  // "canonical-form"
        1,  // "cdata-sections"
        18, // "charset-overrides-xml-encoding"
        2,  // "check-character-normalization"
        3,  // "comments"
        4,  // "datatype-normalization"
        19, // "disallow-doctype"
        23, // "discard-default-content"
        5,  // "element-content-whitespace"
        6,  // "entities"
        7,  // "error-handler"
        24, // "format-pretty-print"
        20, // "ignore-unknown-character-denormalizations"
        8,  // "infoset"
        10, // "namespace-declarations"
        9,  // "namespaces"
        11, // "normalize-characters"
        21, // "resource-resolver"
        12, // "schema-location"
        13, // "schema-type"
        14, // "split-cdata-sections"
        22, // "supported-media-types-only"
        15, // "validate"
        16, // "validate-if-schema"
        17, // "well-formed"
        25, // "xml-declaration"
    ];

    /// Implementation of [`setParameter`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-DOMConfiguration-property) method.
    ///
    /// # Specification
    /// ```text
    /// Set the value of a parameter.
    ///
    /// Parameters
    ///     name of type DOMString
    ///         The name of the parameter to set.
    ///     value of type DOMUserData
    ///         The new value or null if the user wishes to unset the parameter.
    ///         While the type of the value parameter is defined as DOMUserData,
    ///         the object type must match the type defined by the definition of
    ///         the parameter. For example, if the parameter is "error-handler",
    ///         the value must be of type DOMErrorHandler.
    ///
    /// Exceptions
    ///     DOMException
    ///     NOT_FOUND_ERR:     Raised when the parameter name is not recognized.
    ///     NOT_SUPPORTED_ERR: Raised when the parameter name is recognized but the
    ///                        requested value cannot be set.
    ///     TYPE_MISMATCH_ERR: Raised if the value type for this parameter name is
    ///                        incompatible with the expected value type.
    ///
    /// No Return Value
    /// ```
    pub fn set_parameter(
        &mut self,
        parameter: impl TryInto<DOMConfigurationParameter, Error = DOMException>,
    ) -> Result<(), DOMException> {
        let param: DOMConfigurationParameter = parameter.try_into()?;
        match param.clone() {
            DOMConfigurationParameter::ErrorHandler(error_handler) => {
                self.error_handler = Some(error_handler);
            }
            DOMConfigurationParameter::SchemaLocation(schema_location) => {
                self.schema_location = Some(schema_location);
            }
            DOMConfigurationParameter::SchemaType(schema_type) => {
                self.schema_type = Some(schema_type);
            }
            DOMConfigurationParameter::ResourceResolver => {
                todo!()
            }
            DOMConfigurationParameter::CanonicalForm(bool)
            | DOMConfigurationParameter::CDATASections(bool)
            | DOMConfigurationParameter::CheckCharacterNormalization(bool)
            | DOMConfigurationParameter::Comments(bool)
            | DOMConfigurationParameter::DatatypeNormalization(bool)
            | DOMConfigurationParameter::ElementContentWhitespace(bool)
            | DOMConfigurationParameter::Entities(bool)
            | DOMConfigurationParameter::Infoset(bool)
            | DOMConfigurationParameter::Namespaces(bool)
            | DOMConfigurationParameter::NamespaceDeclarations(bool)
            | DOMConfigurationParameter::NormalizeCharacters(bool)
            | DOMConfigurationParameter::SplitCDATASections(bool)
            | DOMConfigurationParameter::Validate(bool)
            | DOMConfigurationParameter::ValidateIfSchema(bool)
            | DOMConfigurationParameter::WellFormed(bool)
            | DOMConfigurationParameter::CharsetOverridesXMLEncoding(bool)
            | DOMConfigurationParameter::DisallowDoctype(bool)
            | DOMConfigurationParameter::IgnoreUnknownCharacterDenormalizations(bool)
            | DOMConfigurationParameter::SupportedMediaTypesOnly(bool)
            | DOMConfigurationParameter::DiscardDefaultContent(bool)
            | DOMConfigurationParameter::FormatPrettyPrint(bool)
            | DOMConfigurationParameter::XMLDeclaration(bool) => {
                let name: &'static str = param.into();
                let index = Self::PARAMETER_NAMES.binary_search(&name).unwrap();
                if bool {
                    self.flags |= 1 << Self::FLAG_POSITION[index];
                } else {
                    self.flags &= !(1 << Self::FLAG_POSITION[index]);
                }
            }
        }

        Ok(())
    }

    /// Implementation of [`getParameter`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-DOMConfiguration-getParameter) method.
    ///
    /// # Specification
    /// ```text
    /// Return the value of a parameter if known.
    ///
    /// Parameters
    ///     name of type DOMString
    ///         The name of the parameter.
    ///
    /// Return Value
    ///     DOMUserData The current object associated with the specified parameter
    ///                 or null if no object has been associated or if the parameter
    ///                 is not supported.
    ///
    /// Exceptions
    ///     DOMException
    ///     NOT_FOUND_ERR: Raised when the parameter name is not recognized.
    /// ```
    pub fn get_parameter(
        &self,
        name: &str,
    ) -> Result<Option<DOMConfigurationParameter>, DOMException> {
        let index = Self::PARAMETER_NAMES
            .binary_search(&name)
            .map_err(|_| DOMException::NotFoundErr)?;

        if name == "error-handler" {
            Ok(self
                .error_handler
                .clone()
                .map(DOMConfigurationParameter::ErrorHandler))
        } else if name == "schema-location" {
            Ok(self
                .schema_location
                .clone()
                .map(DOMConfigurationParameter::SchemaLocation))
        } else if name == "schema-type" {
            Ok(self
                .schema_location
                .clone()
                .map(DOMConfigurationParameter::SchemaType))
        } else {
            let bool = (self.flags >> Self::FLAG_POSITION[index]) & 1 != 0;
            Ok(Some((name, bool).try_into().unwrap()))
        }
    }

    /// Implementation of [`canSetParameter`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-DOMConfiguration-canSetParameter) method.
    ///
    /// # Note
    /// This method in this crate only checks if `parameter` can be converted
    /// to a `DOMConfigurationParameter`.\
    /// Therefore, there is basically no need to use this method.
    ///
    /// # Specification
    /// ```text
    /// Check if setting a parameter to a specific value is supported.
    ///
    /// Parameters
    ///     name of type DOMString
    ///         The name of the parameter to check.
    ///     value of type DOMUserData
    ///         An object. if null, the returned value is true.
    ///
    /// Return Value
    ///     boolean true if the parameter could be successfully set to the specified value,
    ///             or false if the parameter is not recognized or the requested value is
    ///             not supported. This does not change the current value of the parameter
    ///             itself.
    ///
    /// No Exceptions
    /// ```
    pub fn can_set_parameter(&self, parameter: impl TryInto<DOMConfigurationParameter>) -> bool {
        parameter.try_into().is_ok()
    }

    /// Implementation of [`parameterNames`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-DOMConfiguration-parameterNames) attribute.
    ///
    /// # Specification
    /// ```text
    /// parameterNames of type DOMStringList, readonly
    ///     The list of the parameters supported by this DOMConfiguration object
    ///     and for which at least one value can be set by the application. Note
    ///     that this list can also contain parameter names defined outside this
    ///     specification.
    /// ```
    pub fn parameter_names(&self) -> &[&'static str] {
        Self::PARAMETER_NAMES
    }
}

impl Default for DOMConfiguration {
    fn default() -> Self {
        Self {
            flags: 0b10_1001_0110_0100_0111_0110_1010,
            error_handler: None,
            schema_location: None,
            schema_type: None,
        }
    }
}
