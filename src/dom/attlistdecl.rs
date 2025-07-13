use std::{
    cell::RefCell,
    mem::replace,
    rc::{Rc, Weak},
};

use crate::dom::document::{Document, DocumentRef};

/// Attribute Types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttType {
    // [55] StringType      ::= 'CDATA'
    CDATA,
    // [56] TokenizedType   ::= 'ID'
    //                      | 'IDREF'
    //                      | 'IDREFS'
    //                      | 'ENTITY'
    //                      | 'ENTITIES'
    //                      | 'NMTOKEN'
    //                      | 'NMTOKENS'
    ID,
    IDREF,
    IDREFS,
    Entity,
    Entities,
    Nmtoken,
    Nmtokens,
    // [57] EnumeratedType  ::= NotationType | Enumeration
    // [58] NotationType    ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
    Notation(Vec<Rc<str>>),
    // [59] Enumeration     ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
    Enumeration(Vec<Rc<str>>),
}

/// Attribute Defaults.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefaultDecl {
    None(Rc<str>),
    // [60] DefaultDecl ::= '#REQUIRED' | '#IMPLIED'| (('#FIXED' S)? AttValue)
    Required,
    Implied,
    Fixed(Rc<str>),
}

/// Represents an Attribute-list Declaration.  
/// This is not an interface included in the DOM specification.
pub struct AttlistDecl {
    name: Rc<str>,
    owner_document: Weak<RefCell<Document>>,

    att_type: AttType,
    default_decl: DefaultDecl,
    elem_name: Rc<str>,
}

#[derive(Clone)]
pub struct AttlistDeclRef(Rc<RefCell<AttlistDecl>>);

impl AttlistDeclRef {
    pub(crate) fn new(
        doc: Option<DocumentRef>,
        name: Rc<str>,
        att_type: AttType,
        default_decl: DefaultDecl,
        elem_name: Rc<str>,
    ) -> Self {
        AttlistDeclRef(Rc::new(RefCell::new(AttlistDecl {
            name,
            owner_document: doc.map(|doc| Rc::downgrade(&doc.0)).unwrap_or_default(),
            att_type,
            default_decl,
            elem_name,
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
            .upgrade()
            .map(From::from)
    }

    pub fn element_name(&self) -> Rc<str> {
        self.0.borrow().elem_name.clone()
    }

    pub fn att_type(&self) -> AttType {
        self.0.borrow().att_type.clone()
    }

    pub fn default_decl(&self) -> DefaultDecl {
        self.0.borrow().default_decl.clone()
    }

    pub fn is_id(&self) -> bool {
        matches!(self.att_type(), AttType::ID)
    }

    pub fn set_owner_document(&mut self, doc: DocumentRef) -> Option<DocumentRef> {
        replace(
            &mut self.0.borrow_mut().owner_document,
            Rc::downgrade(&doc.0),
        )
        .upgrade()
        .map(From::from)
    }

    pub fn clone_node(&self, _deep: bool) -> AttlistDeclRef {
        Self(Rc::new(RefCell::new(AttlistDecl {
            name: self.name(),
            owner_document: self.0.borrow().owner_document.clone(),
            att_type: self.0.borrow().att_type.clone(),
            default_decl: self.0.borrow().default_decl.clone(),
            elem_name: self.0.borrow().elem_name.clone(),
        })))
    }

    pub fn is_same_node(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}
