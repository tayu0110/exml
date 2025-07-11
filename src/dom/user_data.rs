use std::{any::Any, sync::Arc};

use crate::dom::node::NodeRef;

/// Implementation of [`DOMUserData`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-DOMUserData).
///
/// # Specification
/// ```text
/// A DOMUserData represents a reference to application data.
///
/// For Java, DOMUserData is bound to the Object type.
/// For ECMAScript, DOMUserData is bound to any type.
/// ```
pub type DOMUserData = Arc<dyn Any>;

/// Operation type constants for [`UserDataHandler`].
///
/// # Specification
/// ```text
/// NODE_ADOPTED
///     The node is adopted, using Document.adoptNode().
/// NODE_CLONED
///     The node is cloned, using Node.cloneNode().
/// NODE_DELETED
///     The node is deleted.
///     Note: This may not be supported or may not be reliable in certain
///           environments, such as Java, where the implementation has no
///           real control over when objects are actually deleted.
/// NODE_IMPORTED
///     The node is imported, using Document.importNode().
/// NODE_RENAMED
///     The node is renamed, using Document.renameNode().
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OperationType {
    NodeCloned = 1,
    NodeImported = 2,
    NodeDeleted = 3,
    NodeRenamed = 4,
    NodeAdopted = 5,
}

/// Implementation of [`UserDataHandler`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-UserDataHandler) interface.
///
/// # Specification
/// ```text
/// Interface UserDataHandler (introduced in DOM Level 3)
///     When associating an object to a key on a node using Node.setUserData() the
///     application can provide a handler that gets called when the node the object
///     is associated to is being cloned, imported, or renamed. This can be used by
///     the application to implement various behaviors regarding the data it associates
///     to the DOM nodes. This interface defines that handler.
/// ```
pub trait UserDataHandler {
    /// Implementation of [`handle`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-handleUserDataEvent) method.
    ///
    /// # Note
    /// The implementation of this crate does not guarantee that change operations
    /// on `src` within this method can be performed successfully.\
    /// It might cause panic, or some side effect might cause the tree structure to go wrong.
    ///
    /// # Specification
    /// ```text
    /// This method is called whenever the node for which this handler is registered
    /// is imported or cloned.
    /// DOM applications must not raise exceptions in a UserDataHandler. The effect
    /// of throwing exceptions from the handler is DOM implementation dependent.
    ///
    /// Parameters
    ///     operation of type unsigned short
    ///         Specifies the type of operation that is being performed on the node.
    ///     key of type DOMString
    ///         Specifies the key for which this handler is being called.
    ///     data of type DOMUserData
    ///         Specifies the data for which this handler is being called.
    ///     src of type Node
    ///         Specifies the node being cloned, adopted, imported, or renamed.
    ///         This is null when the node is being deleted.
    ///     dst of type Node
    ///         Specifies the node newly created if any, or null.
    ///
    /// No Return Value
    /// No Exceptions
    /// ```
    fn handle(
        &self,
        operation: OperationType,
        key: &str,
        data: DOMUserData,
        src: NodeRef,
        dst: Option<NodeRef>,
    );
}
