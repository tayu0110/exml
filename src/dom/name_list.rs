use std::rc::Rc;

/// Implementation of [NameList](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-NameList)
/// interface on [1.4 Fundamental Interfaces: Core Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-BBACDC08)
///
/// The specification does not specify what orders the sets.  
/// In this implementation, it is simply the order of insertion into the set.
pub struct NameList {
    /// (name, namespaceURI)
    names: Vec<(Rc<str>, Option<Rc<str>>)>,
}

impl NameList {
    /// Implementation of `getName` method.
    pub fn get_name(&self, index: usize) -> Option<&str> {
        self.names.get(index).map(|n| n.0.as_ref())
    }

    /// Implementation of `getNamespaceURI` method.
    pub fn get_namespace_uri(&self, index: usize) -> Option<&str> {
        self.names.get(index).and_then(|n| n.1.as_deref())
    }

    /// Implementation of `length` attribute.
    pub fn len(&self) -> usize {
        self.names.len()
    }

    /// Check if this list is empty.  
    /// In other words, check `self.len() == 0` is satisfied.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Implementation of `contains` method.
    pub fn contains(&self, name: &str) -> bool {
        self.names.iter().any(|n| n.0.as_ref() == name)
    }

    /// Implementation of `containsNS` method.
    pub fn contains_ns(&self, ns_uri: Option<&str>, name: &str) -> bool {
        self.names
            .iter()
            .any(|n| n.0.as_ref() == name && n.1.as_deref() == ns_uri)
    }
}
