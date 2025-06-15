use std::{
    cell::RefCell,
    collections::HashMap,
    mem::replace,
    rc::{Rc, Weak},
};

use crate::{
    dom::{
        DOMException,
        attlistdecl::AttlistDeclRef,
        document::{DocumentRef, DocumentWeakRef},
    },
    tree::validate_name,
};

/// `contentspec` of a Element Type Declaration.
#[repr(C)]
#[derive(Debug, Clone)]
pub enum ContentSpec {
    Empty,
    Any,
    Mixed(Rc<ElementContent>),
    Children(Rc<ElementContent>),
}

/// Possible definitions of element content types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ElementContentType {
    PCDATA,
    Element,
    Seq,
    Or,
}

/// Possible definitions of element content occurrences.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ElementContentOccur {
    Once,
    Opt,
    Mult,
    Plus,
}

#[derive(Debug)]
pub struct ElementContent {
    r#type: ElementContentType,
    occur: ElementContentOccur,
    name: Option<Rc<str>>,
    parent: RefCell<Weak<ElementContent>>,
    first_child: RefCell<Option<Rc<ElementContent>>>,
    second_child: RefCell<Option<Rc<ElementContent>>>,
}

impl ElementContent {
    pub fn new_pcdata(occur: ElementContentOccur) -> Rc<ElementContent> {
        Rc::new(ElementContent {
            r#type: ElementContentType::PCDATA,
            occur,
            name: None,
            parent: RefCell::new(Weak::new()),
            first_child: RefCell::new(None),
            second_child: RefCell::new(None),
        })
    }

    pub fn new_element(occur: ElementContentOccur, name: impl Into<Rc<str>>) -> Rc<ElementContent> {
        Rc::new(ElementContent {
            r#type: ElementContentType::Element,
            occur,
            name: Some(name.into()),
            parent: RefCell::new(Weak::new()),
            first_child: RefCell::new(None),
            second_child: RefCell::new(None),
        })
    }

    pub fn new_seq(occur: ElementContentOccur) -> Rc<ElementContent> {
        Rc::new(ElementContent {
            r#type: ElementContentType::Seq,
            occur,
            name: None,
            parent: RefCell::new(Weak::new()),
            first_child: RefCell::new(None),
            second_child: RefCell::new(None),
        })
    }

    pub fn new_or(occur: ElementContentOccur) -> Rc<ElementContent> {
        Rc::new(ElementContent {
            r#type: ElementContentType::Or,
            occur,
            name: None,
            parent: RefCell::new(Weak::new()),
            first_child: RefCell::new(None),
            second_child: RefCell::new(None),
        })
    }

    pub fn first_child(&self) -> Option<Rc<Self>> {
        self.first_child.borrow().clone()
    }

    pub fn second_child(&self) -> Option<Rc<Self>> {
        self.second_child.borrow().clone()
    }

    /// Connect `child` as a new first child of this node.  
    /// The parent of `child` is changed to this node.
    ///
    /// Return the old first child if it exists.  
    /// The parent of the returned node is cleared.
    pub fn set_first_child(self: &Rc<Self>, child: Rc<Self>) -> Option<Rc<Self>> {
        *child.parent.borrow_mut() = Rc::downgrade(self);
        replace(&mut *self.first_child.borrow_mut(), Some(child))
            .inspect(|res| *res.parent.borrow_mut() = Weak::new())
    }

    /// Connect `child` as a new second child of this node.  
    /// The parent of `child` is changed to this node.
    ///
    /// Return the old second child if it exists.  
    /// The parent of the returned node is cleared.
    pub fn set_second_child(self: &Rc<Self>, child: Rc<Self>) -> Option<Rc<Self>> {
        *child.parent.borrow_mut() = Rc::downgrade(self);
        replace(&mut *self.second_child.borrow_mut(), Some(child))
            .inspect(|res| *res.parent.borrow_mut() = Weak::new())
    }
}

/// Represents an Element Type Declaration.  
/// This is not an interface included in the DOM specification.
pub struct ElementDecl {
    name: Rc<str>,
    owner_document: Option<DocumentWeakRef>,
    attrdecl: HashMap<String, AttlistDeclRef>,
    contentspec: ContentSpec,
}

#[derive(Clone)]
pub struct ElementDeclRef(Rc<RefCell<ElementDecl>>);

impl ElementDeclRef {
    pub(crate) fn new(doc: Option<DocumentRef>, name: Rc<str>, contentspec: ContentSpec) -> Self {
        ElementDeclRef(Rc::new(RefCell::new(ElementDecl {
            name,
            owner_document: doc.map(|doc| doc.downgrade()),
            attrdecl: HashMap::new(),
            contentspec,
        })))
    }

    pub fn name(&self) -> Rc<str> {
        self.0.borrow().name.clone()
    }

    pub fn owner_document(&self) -> Option<DocumentRef> {
        self.0
            .borrow()
            .owner_document
            .clone()
            .and_then(|doc| doc.upgrade())
    }

    pub fn set_owner_document(&mut self, doc: DocumentRef) -> Option<DocumentRef> {
        for decl in self.0.borrow_mut().attrdecl.values_mut() {
            decl.set_owner_document(doc.clone());
        }
        replace(
            &mut self.0.borrow_mut().owner_document,
            Some(doc.downgrade()),
        )
        .and_then(|doc| doc.upgrade())
    }

    pub fn clone_node(&self, deep: bool) -> ElementDeclRef {
        let mut new = Self(Rc::new(RefCell::new(ElementDecl {
            name: self.name().clone(),
            owner_document: self.0.borrow().owner_document.clone(),
            attrdecl: HashMap::new(),
            contentspec: self.0.borrow().contentspec.clone(),
        })));

        if deep {
            for decl in self.0.borrow().attrdecl.values() {
                new.add_attlist_decl(decl.clone_node(true)).unwrap();
            }

            fn deep_clone_element_content(
                content: Option<Rc<ElementContent>>,
                parent: Weak<ElementContent>,
            ) -> Option<Rc<ElementContent>> {
                let content = content?;
                let new = Rc::new(ElementContent {
                    r#type: content.r#type,
                    occur: content.occur,
                    name: content.name.clone(),
                    parent: RefCell::new(parent),
                    first_child: RefCell::new(None),
                    second_child: RefCell::new(None),
                });

                let first = deep_clone_element_content(
                    content.first_child.borrow().clone(),
                    Rc::downgrade(&new),
                );
                let second = deep_clone_element_content(
                    content.second_child.borrow().clone(),
                    Rc::downgrade(&new),
                );

                *new.first_child.borrow_mut() = first;
                *new.second_child.borrow_mut() = second;
                Some(new)
            }

            match &self.0.borrow().contentspec {
                ContentSpec::Mixed(cont) => {
                    new.0.borrow_mut().contentspec = ContentSpec::Mixed(
                        deep_clone_element_content(Some(cont.clone()), Weak::new()).unwrap(),
                    )
                }
                ContentSpec::Children(cont) => {
                    new.0.borrow_mut().contentspec = ContentSpec::Children(
                        deep_clone_element_content(Some(cont.clone()), Weak::new()).unwrap(),
                    )
                }
                _ => {}
            }
        }

        new
    }

    pub fn is_same_node(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    pub(crate) fn add_attlist_decl(&mut self, decl: AttlistDeclRef) -> Result<(), DOMException> {
        let name = decl.name();
        if validate_name::<false>(&name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }

        if self.0.borrow().attrdecl.contains_key(name.as_ref()) {
            // Is this correct ???
            return Err(DOMException::NoDataAllowedErr);
        }

        self.0.borrow_mut().attrdecl.insert(name.to_string(), decl);
        Ok(())
    }
}
