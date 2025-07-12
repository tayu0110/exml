use std::borrow::Cow;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DOMErrorSeverity {
    SeverityWarning = 1,
    SeverityError = 2,
    SeverityFatalError = 3,
}

/// Implementation of [`Interface DOMError`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ERROR-Interfaces-DOMError).
///
/// # Specification
/// ```text
/// Interface DOMError (introduced in DOM Level 3)
///     DOMError is an interface that describes an error.
/// ```
pub struct DOMError {
    severity: DOMErrorSeverity,
    message: Cow<'static, str>,
}

/// Implementation of [`Interface DOMErrorHandler`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ERROR-Interfaces-DOMErrorHandler).
///
/// # Specification
/// ```text
/// Interface DOMErrorHandler (introduced in DOM Level 3)
///     DOMErrorHandler is a callback interface that the DOM implementation can call
///     when reporting errors that happens while processing XML data, or when doing
///     some other processing (e.g. validating a document). A DOMErrorHandler object
///     can be attached to a Document using the "error-handler" on the DOMConfiguration
///     interface. If more than one error needs to be reported during an operation, the
///     sequence and numbers of the errors passed to the error handler are implementation
///     dependent.
///     
///     The application that is using the DOM implementation is expected to implement
///     this interface.
/// ```
pub trait DOMErrorHandler {
    /// Implementation of [`handleError`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-ERRORS-DOMErrorHandler-handleError) method.
    ///
    /// # Specification
    /// ```text
    /// This method is called on the error handler when an error occurs.
    /// If an exception is thrown from this method, it is considered to be equivalent
    /// of returning true.
    ///
    /// Parameters
    ///     error of type DOMError
    ///         The error object that describes the error. This object may be reused by
    ///         the DOM implementation across multiple calls to the handleError method.
    ///
    /// Return Value
    ///     boolean If the handleError method returns false, the DOM implementation should
    ///             stop the current processing when possible. If the method returns true,
    ///             the processing may continue depending on DOMError.severity.
    ///
    /// No Exceptions
    /// ```
    fn handle_error(&self, error: DOMError) -> bool;
}
