use std::sync::{Arc, LazyLock, Mutex};

use crate::dom::{DOMException, document::DocumentRef, document_type::DocumentTypeRef};

/// Implementation of `DOMImplementationRegistry` that is described
/// at [`1.3.7 Bootstrapping`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Bootstrap)
/// in the DOM Level 3 Core specification.
pub struct DOMImplementationRegistry {
    sources: Mutex<Vec<Arc<dyn DOMImplementationSource>>>,
}

impl DOMImplementationRegistry {
    pub fn add_source(&mut self, source: Arc<dyn DOMImplementationSource>) {
        let mut sources = self.sources.lock().unwrap();
        if let Some(pos) = sources.iter().position(|s| Arc::ptr_eq(s, &source)) {
            sources.remove(pos);
        }
        sources.push(source);
    }

    /// # Specification
    /// [1.3.7 Bootstrapping](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Bootstrap)
    /// ```text
    /// the DOMImplementationRegistry provides a getDOMImplementation method accepting
    /// a features string, which is passed to every known DOMImplementationSource until
    /// a suitable DOMImplementation is found and returned.
    /// ```
    pub fn get_dom_implementation(&self, features: &str) -> Option<Arc<dyn DOMImplementation>> {
        let sources = self.sources.lock().unwrap();
        // Search from the end to give preference to user-added sources.
        sources
            .iter()
            .rev()
            .find_map(|source| source.get_dom_implementation(features))
    }

    /// # Specification
    /// [1.3.7 Bootstrapping](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Bootstrap)
    /// ```text
    /// The DOMImplementationRegistry also provides a getDOMImplementationList method
    /// accepting a features string, which is passed to every known DOMImplementationSource,
    /// and returns a list of suitable DOMImplementations.
    /// ```
    pub fn get_dom_implementation_list(&self, features: &str) -> Arc<dyn DOMImplementationList> {
        let sources = self.sources.lock().unwrap();
        let mut res = vec![];
        // Search from the end to give preference to user-added sources.
        for source in sources.iter().rev() {
            let list = source.get_dom_implementation_list(features);
            for i in 0..list.length() {
                let implementation = list.item(i).unwrap();
                if !res.iter().any(|p| Arc::ptr_eq(&implementation, p)) {
                    res.push(implementation);
                }
            }
        }
        Arc::new(res) as _
    }
}

pub static DOM_IMPLEMENTATION_REGISTRY: LazyLock<DOMImplementationRegistry> =
    LazyLock::new(|| DOMImplementationRegistry {
        sources: Mutex::new(vec![(*DEFAULT_DOM_IMPLEMENTATION_SOURCE).clone()]),
    });

/// Implementation of [`DOMImpelmentationSource`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-DOMImplementationSource) interface.
pub trait DOMImplementationSource: Send + Sync + 'static {
    /// Implementation of [`getDOMImplementation`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-getDOMImpl) method.
    ///
    /// # Specification
    /// ```text
    /// A method to request the first DOM implementation that supports the specified features.
    ///
    /// Parameters
    ///     features of type DOMString
    ///         A string that specifies which features and versions are required. This is a
    ///         space separated list in which each feature is specified by its name optionally
    ///         followed by a space and a version number.
    ///         This method returns the first item of the list returned by
    ///         getDOMImplementationList.
    ///         As an example, the string "XML 3.0 Traversal +Events 2.0" will request
    ///         a DOM implementation that supports the module "XML" for its 3.0 version,
    ///         a module that support of the "Traversal" module for any version, and the module
    ///         "Events" for its 2.0 version. The module "Events" must be accessible using
    ///         the method Node.getFeature() and DOMImplementation.getFeature().
    ///
    /// Return Value
    ///     DOMImplementation The first DOM implementation that support the desired features,
    ///                       or null if this source has none.
    ///
    /// No Exceptions
    /// ```
    fn get_dom_implementation(&self, features: &str) -> Option<Arc<dyn DOMImplementation>>;
    /// Implementation of [`getDOMImplementationList`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-getDOMImpls) method.
    ///
    /// # Specification
    /// ```text
    /// A method to request a list of DOM implementations that support the specified features
    /// and versions, as specified in DOM Features.
    ///
    /// Parameters
    ///     features of type DOMString
    ///         A string that specifies which features and versions are required.
    ///         This is a space separated list in which each feature is specified by its name
    ///         optionally followed by a space and a version number. This is something like:
    ///         "XML 3.0 Traversal +Events 2.0"
    ///
    /// Return Value
    ///     DOMImplementationList A list of DOM implementations that support the desired
    ///                           features.
    ///
    /// No Exceptions
    /// ```
    fn get_dom_implementation_list(&self, features: &str) -> Arc<dyn DOMImplementationList>;
}

/// Implementation of [`DOMImpelmentationSource`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-DOMImplementationSource) interface.
pub struct DefaultDOMImplementationSource;

impl DOMImplementationSource for DefaultDOMImplementationSource {
    fn get_dom_implementation(&self, features: &str) -> Option<Arc<dyn DOMImplementation>> {
        let implementation = (*DEFAULT_DOM_IMPLEMENTATION).clone();
        let mut features = features.split_ascii_whitespace().peekable();
        while let Some(feature) = features.next() {
            if feature.is_empty() {
                continue;
            }

            while features.next_if(|f| f.is_empty()).is_some() {}
            let ok = if let Some(version) = features.next_if(|version| {
                version.split_once('.').is_some_and(|(major, minor)| {
                    major.chars().all(char::is_numeric) && minor.chars().all(char::is_numeric)
                })
            }) {
                implementation.has_feature(feature, Some(version))
            } else {
                implementation.has_feature(feature, None)
            };
            if !ok {
                return None;
            }
        }
        Some(implementation)
    }

    fn get_dom_implementation_list(&self, features: &str) -> Arc<dyn DOMImplementationList> {
        let mut res = vec![];
        if let Some(implementation) = self.get_dom_implementation(features) {
            res.push(implementation);
        }
        Arc::new(res) as _
    }
}

pub static DEFAULT_DOM_IMPLEMENTATION_SOURCE: LazyLock<Arc<dyn DOMImplementationSource>> =
    LazyLock::new(|| Arc::new(DefaultDOMImplementationSource) as _);

/// Implementation for [`DOMImplementation`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-102161490) interface.
pub trait DOMImplementation: Send + Sync + 'static {
    /// Implementation of [`hasFeature`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-5CED94D7) method.
    ///
    /// # Specification
    /// ```text
    /// Test if the DOM implementation implements a specific feature and version,
    /// as specified in DOM Features.
    ///
    /// Parameters
    ///     feature of type DOMString
    ///         The name of the feature to test.
    ///     version of type DOMString
    ///         This is the version number of the feature to test.
    ///
    /// Return Value
    ///     boolean true if the feature is implemented in the specified version,
    ///             false otherwise.
    /// ```
    fn has_feature(&self, feature: &str, version: Option<&str>) -> bool;
    /// Implementation of [`createDocumentType`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Level-2-Core-DOM-createDocType) method.
    ///
    /// # Specification
    /// ```text
    /// Creates an empty DocumentType node. Entity declarations and notations are not made available.
    /// Entity reference expansions and default attribute additions do not occur..
    ///
    /// Parameters
    ///     qualifiedName of type DOMString
    ///         The qualified name of the document type to be created.
    ///     publicId of type DOMString
    ///         The external subset public identifier.
    ///     systemId of type DOMString
    ///         The external subset system identifier.
    ///
    /// Return Value
    ///     DocumentType A new DocumentType node with Node.ownerDocument set to null.
    ///
    /// Exceptions
    ///     DOMException
    ///     INVALID_CHARACTER_ERR: Raised if the specified qualified name is not an XML name
    ///                            according to [XML 1.0].
    ///     NAMESPACE_ERR:         Raised if the qualifiedName is malformed.
    ///     NOT_SUPPORTED_ERR:     May be raised if the implementation does not support
    ///                            the feature "XML" and the language exposed through
    ///                            the Document does not support XML Namespaces
    ///                            (such as [HTML 4.01]).
    /// ```
    fn create_document_type(
        &self,
        qualified_name: &str,
        public_id: Option<&str>,
        system_id: Option<&str>,
    ) -> Result<DocumentTypeRef, DOMException>;
    /// Implementation of [`createDocument`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Level-2-Core-DOM-createDocument) method.
    ///
    /// # Specification
    /// ```text
    /// Creates an empty DocumentType node. Entity declarations and notations are not made
    /// available. Entity reference expansions and default attribute additions do not occur.
    ///
    /// Parameters
    ///     qualifiedName of type DOMString
    ///         The qualified name of the document type to be created.
    ///     publicId of type DOMString
    ///         The external subset public identifier.
    ///     systemId of type DOMString
    ///         The external subset system identifier.
    ///
    /// Return Value
    ///     DocumentType A new DocumentType node with Node.ownerDocument set to null.
    ///
    /// Exceptions
    ///     DOMException
    ///     INVALID_CHARACTER_ERR: Raised if the specified qualified name is not an XML name
    ///                            according to [XML 1.0].
    ///     NAMESPACE_ERR:         Raised if the qualifiedName is malformed.
    ///     NOT_SUPPORTED_ERR:     May be raised if the implementation does not support the
    ///                            feature "XML" and the language exposed through the Document
    ///                            does not support XML Namespaces (such as [HTML 4.01]).
    /// ```
    fn create_document(
        &self,
        namespace_uri: Option<&str>,
        qualified_name: Option<&str>,
        doctype: Option<DocumentTypeRef>,
    ) -> Result<DocumentRef, DOMException>;
    /// Implementation of [`getFeature`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-DOMImplementation3-getFeature) method.
    ///
    /// # Specification
    /// ```text
    /// This method returns a specialized object which implements the specialized APIs of the specified feature and version, as specified in DOM Features. The specialized object may also be obtained by using binding-specific casting methods but is not necessarily expected to, as discussed in Mixed DOM Implementations. This method also allow the implementation to provide specialized objects which do not support the DOMImplementation interface.
    ///
    /// Parameters
    ///     feature of type DOMString
    ///         The name of the feature requested. Note that any plus sign "+" prepended to the name of the feature will be ignored since it is not significant in the context of this method.
    ///     version of type DOMString
    ///         This is the version number of the feature to test.
    ///
    /// Return Value
    ///     DOMObject Returns an object which implements the specialized APIs of the
    ///               specified feature and version, if any, or null if there is no object
    ///               which implements interfaces associated with that feature. If the
    ///               DOMObject returned by this method implements the DOMImplementation
    ///               interface, it must delegate to the primary core DOMImplementation
    ///               and not return results inconsistent with the primary core
    ///               DOMImplementation such as hasFeature, getFeature, etc.
    ///
    /// No Exceptions
    /// ```
    fn get_feature(
        &self,
        feature: &str,
        version: Option<&str>,
    ) -> Option<Arc<dyn DOMImplementation>>;
}

/// Implementation for [`DOMImplementation`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-102161490) interface.
pub struct DefaultDOMImplementation;

impl DOMImplementation for DefaultDOMImplementation {
    fn has_feature(&self, feature: &str, version: Option<&str>) -> bool {
        (feature.eq_ignore_ascii_case("Core")
            && version.is_none_or(|version| matches!(version, "1.0" | "2.0" | "3.0")))
            || (feature.eq_ignore_ascii_case("XML")
                && version.is_none_or(|version| matches!(version, "1.0" | "2.0" | "3.0")))
            || (feature.eq_ignore_ascii_case("XMLVersion")
                && version.is_none_or(|version| matches!(version, "1.0")))
    }

    fn create_document_type(
        &self,
        qualified_name: &str,
        public_id: Option<&str>,
        system_id: Option<&str>,
    ) -> Result<DocumentTypeRef, DOMException> {
        DocumentTypeRef::new(qualified_name, public_id, system_id)
    }

    fn create_document(
        &self,
        namespace_uri: Option<&str>,
        qualified_name: Option<&str>,
        doctype: Option<DocumentTypeRef>,
    ) -> Result<DocumentRef, DOMException> {
        DocumentRef::new(namespace_uri, qualified_name, doctype)
    }

    fn get_feature(
        &self,
        feature: &str,
        version: Option<&str>,
    ) -> Option<Arc<dyn DOMImplementation>> {
        self.has_feature(feature, version)
            .then(|| Arc::new(Self) as Arc<dyn DOMImplementation>)
    }
}

pub static DEFAULT_DOM_IMPLEMENTATION: LazyLock<Arc<dyn DOMImplementation>> =
    LazyLock::new(|| Arc::new(DefaultDOMImplementation) as Arc<dyn DOMImplementation>);

/// Implementation of [`DOMImplementationList`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-DOMImplementationList) interface.
pub trait DOMImplementationList {
    /// Implementation of [`item`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-DOMImplementationList-item) method.
    ///
    /// # Specification
    /// ```text
    /// Returns the indexth item in the collection. If index is greater than or equal to
    /// the number of DOMImplementations in the list, this returns null.
    ///
    /// Parameters
    ///     index of type unsigned long
    ///         Index into the collection.
    ///
    /// Return Value
    ///     DOMImplementation The DOMImplementation at the indexth position in the
    ///                       DOMImplementationList, or null if that is not a valid index.
    ///
    /// No Exceptions
    /// ```
    fn item(&self, index: usize) -> Option<Arc<dyn DOMImplementation>>;
    /// Implementation of [`length`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-DOMImplementationList-length) attribute.
    ///
    /// # Specification
    /// ```text
    /// length of type unsigned long, readonly
    ///     The number of DOMImplementations in the list. The range of valid child node
    ///     indices is 0 to length-1 inclusive.
    /// ```
    fn length(&self) -> usize;
}

impl DOMImplementationList for Vec<Arc<dyn DOMImplementation>> {
    fn item(&self, index: usize) -> Option<Arc<dyn DOMImplementation>> {
        self.get(index).cloned()
    }

    fn length(&self) -> usize {
        self.len()
    }
}
