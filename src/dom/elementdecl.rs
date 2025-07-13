use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt::Write,
    mem::replace,
    rc::{Rc, Weak},
};

use crate::{
    dom::{
        DOMException,
        attlistdecl::AttlistDeclRef,
        document::{Document, DocumentRef},
    },
    parser::split_qname2,
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

pub struct ElementContent {
    r#type: ElementContentType,
    occur: Cell<ElementContentOccur>,
    name: Option<Rc<str>>,
    prefix: Option<Rc<str>>,
    parent: RefCell<Weak<ElementContent>>,
    first_child: RefCell<Option<Rc<ElementContent>>>,
    second_child: RefCell<Option<Rc<ElementContent>>>,
}

impl ElementContent {
    pub fn new_pcdata(occur: ElementContentOccur) -> Rc<ElementContent> {
        Rc::new(ElementContent {
            r#type: ElementContentType::PCDATA,
            occur: Cell::new(occur),
            name: None,
            prefix: None,
            parent: RefCell::new(Weak::new()),
            first_child: RefCell::new(None),
            second_child: RefCell::new(None),
        })
    }

    pub fn new_element(occur: ElementContentOccur, name: impl Into<Rc<str>>) -> Rc<ElementContent> {
        let mut res = ElementContent {
            r#type: ElementContentType::Element,
            occur: Cell::new(occur),
            name: Some(name.into()),
            prefix: None,
            parent: RefCell::new(Weak::new()),
            first_child: RefCell::new(None),
            second_child: RefCell::new(None),
        };

        if let Some((prefix, _)) = split_qname2(res.name.as_deref().unwrap()) {
            res.prefix = Some(prefix.into());
        }

        Rc::new(res)
    }

    pub fn new_seq(occur: ElementContentOccur) -> Rc<ElementContent> {
        Rc::new(ElementContent {
            r#type: ElementContentType::Seq,
            occur: Cell::new(occur),
            name: None,
            prefix: None,
            parent: RefCell::new(Weak::new()),
            first_child: RefCell::new(None),
            second_child: RefCell::new(None),
        })
    }

    pub fn new_or(occur: ElementContentOccur) -> Rc<ElementContent> {
        Rc::new(ElementContent {
            r#type: ElementContentType::Or,
            occur: Cell::new(occur),
            name: None,
            prefix: None,
            parent: RefCell::new(Weak::new()),
            first_child: RefCell::new(None),
            second_child: RefCell::new(None),
        })
    }

    pub fn r#type(&self) -> ElementContentType {
        self.r#type
    }

    pub fn occur(&self) -> ElementContentOccur {
        self.occur.get()
    }

    pub fn name(&self) -> Option<Rc<str>> {
        self.name.clone()
    }

    pub fn prefix(&self) -> Option<Rc<str>> {
        self.prefix.clone()
    }

    pub fn first_child(&self) -> Option<Rc<Self>> {
        self.first_child.borrow().clone()
    }

    pub fn second_child(&self) -> Option<Rc<Self>> {
        self.second_child.borrow().clone()
    }

    pub fn set_occur(self: &mut Rc<Self>, occur: ElementContentOccur) -> ElementContentOccur {
        self.occur.replace(occur)
    }

    /// Connect `child` as a new first child of this node.  
    /// The parent of `child` is changed to this node.
    ///
    /// Return the old first child if it exists.  
    /// The parent of the returned node is cleared.
    pub fn set_first_child(self: &mut Rc<Self>, child: Option<Rc<Self>>) -> Option<Rc<Self>> {
        if let Some(child) = child.as_deref() {
            *child.parent.borrow_mut() = Rc::downgrade(self);
        }
        replace(&mut *self.first_child.borrow_mut(), child)
            .inspect(|res| *res.parent.borrow_mut() = Weak::new())
    }

    /// Connect `child` as a new second child of this node.  
    /// The parent of `child` is changed to this node.
    ///
    /// Return the old second child if it exists.  
    /// The parent of the returned node is cleared.
    pub fn set_second_child(self: &mut Rc<Self>, child: Option<Rc<Self>>) -> Option<Rc<Self>> {
        if let Some(child) = child.as_deref() {
            *child.parent.borrow_mut() = Rc::downgrade(self);
        }
        replace(&mut *self.second_child.borrow_mut(), child)
            .inspect(|res| *res.parent.borrow_mut() = Weak::new())
    }

    pub(crate) fn write_to<'a>(
        &self,
        to: &mut (impl Write + 'a),
        englob: bool,
        size: usize,
        cur: &mut usize,
        last: &mut char,
    ) -> std::fmt::Result {
        if size - *cur < 50 {
            if size - *cur > 4 && *last != '.' {
                write!(to, " ...")?;
                *cur += 4;
                *last = '.';
            }
            return Ok(());
        }
        if englob {
            write!(to, "(")?;
            *cur += 1;
            *last = '(';
        }
        match self.r#type {
            ElementContentType::PCDATA => {
                write!(to, "#PCDATA")?;
                *last = 'A';
                *cur += 7;
            }
            ElementContentType::Element => {
                let name = self.name.as_deref().unwrap();
                if size - *cur < name.len() + 10 {
                    write!(to, " ...")?;
                    *cur += 4;
                    *last = '.';
                    return Ok(());
                }
                write!(to, "{name}")?;
                *cur += name.len();
                if !name.is_empty() {
                    *last = name.chars().next_back().unwrap();
                }
            }
            ElementContentType::Seq => {
                let c1 = self.first_child().unwrap();
                c1.write_to(
                    to,
                    matches!(
                        c1.r#type(),
                        ElementContentType::Or | ElementContentType::Seq
                    ),
                    size,
                    cur,
                    last,
                )?;
                if size - *cur < 50 {
                    if size - *cur > 4 && *last != '.' {
                        write!(to, " ...")?;
                        *cur += 4;
                        *last = '.';
                    }
                    return Ok(());
                }
                write!(to, " , ")?;
                *cur += 3;
                *last = ' ';
                let c2 = self.second_child().unwrap();
                let typ = c2.r#type();
                let ocur = c2.occur();
                c2.write_to(
                    to,
                    (matches!(typ, ElementContentType::Or)
                        || !matches!(ocur, ElementContentOccur::Once))
                        && !matches!(typ, ElementContentType::Element),
                    size,
                    cur,
                    last,
                )?;
            }
            ElementContentType::Or => {
                let c1 = self.first_child().unwrap();
                c1.write_to(
                    to,
                    matches!(
                        c1.r#type(),
                        ElementContentType::Or | ElementContentType::Seq
                    ),
                    size,
                    cur,
                    last,
                )?;
                if size - *cur < 50 {
                    if size - *cur > 4 && *last != '.' {
                        write!(to, " ...")?;
                        *cur += 4;
                        *last = '.';
                    }
                    return Ok(());
                }
                let c2 = self.second_child().unwrap();
                let typ = c2.r#type();
                let ocur = c2.occur();
                write!(to, " | ")?;
                *cur += 3;
                *last = ' ';
                c2.write_to(
                    to,
                    (matches!(typ, ElementContentType::Seq)
                        || !matches!(ocur, ElementContentOccur::Once))
                        && !matches!(typ, ElementContentType::Element),
                    size,
                    cur,
                    last,
                )?;
            }
        }
        if size - *cur <= 2 {
            return Ok(());
        }
        if englob {
            write!(to, ")")?;
            *cur += 1;
            *last = ')';
        }
        match self.occur() {
            ElementContentOccur::Once => {}
            ElementContentOccur::Opt => {
                write!(to, "?")?;
                *cur += 1;
                *last = '?';
            }
            ElementContentOccur::Mult => {
                write!(to, "*")?;
                *cur += 1;
                *last = '*';
            }
            ElementContentOccur::Plus => {
                write!(to, "+")?;
                *cur += 1;
                *last = '+';
            }
        }
        Ok(())
    }
}

impl std::fmt::Debug for ElementContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write_to(f, true, 5000, &mut 0, &mut '\0')
    }
}

impl std::fmt::Display for ElementContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write_to(f, true, usize::MAX, &mut 0, &mut '\0')
    }
}

pub(crate) fn deep_clone_element_content(
    content: Option<Rc<ElementContent>>,
    parent: Weak<ElementContent>,
) -> Option<Rc<ElementContent>> {
    let content = content?;
    let new = Rc::new(ElementContent {
        r#type: content.r#type,
        occur: content.occur.clone(),
        name: content.name.clone(),
        prefix: content.prefix.clone(),
        parent: RefCell::new(parent),
        first_child: RefCell::new(None),
        second_child: RefCell::new(None),
    });

    let first =
        deep_clone_element_content(content.first_child.borrow().clone(), Rc::downgrade(&new));
    let second =
        deep_clone_element_content(content.second_child.borrow().clone(), Rc::downgrade(&new));

    *new.first_child.borrow_mut() = first;
    *new.second_child.borrow_mut() = second;
    Some(new)
}

/// Represents an Element Type Declaration.  
/// This is not an interface included in the DOM specification.
pub struct ElementDecl {
    name: Rc<str>,
    owner_document: Weak<RefCell<Document>>,
    attrdecl: HashMap<String, AttlistDeclRef>,
    contentspec: ContentSpec,
}

#[derive(Clone)]
pub struct ElementDeclRef(Rc<RefCell<ElementDecl>>);

impl ElementDeclRef {
    pub(crate) fn new(doc: Option<DocumentRef>, name: Rc<str>, contentspec: ContentSpec) -> Self {
        ElementDeclRef(Rc::new(RefCell::new(ElementDecl {
            name,
            owner_document: doc.map(|doc| Rc::downgrade(&doc.0)).unwrap_or_default(),
            attrdecl: HashMap::new(),
            contentspec,
        })))
    }

    pub fn name(&self) -> Rc<str> {
        self.0.borrow().name.clone()
    }

    pub fn owner_document(&self) -> Option<DocumentRef> {
        self.0.borrow().owner_document.upgrade().map(From::from)
    }

    pub fn set_owner_document(&mut self, doc: DocumentRef) -> Option<DocumentRef> {
        for decl in self.0.borrow_mut().attrdecl.values_mut() {
            decl.set_owner_document(doc.clone());
        }
        replace(
            &mut self.0.borrow_mut().owner_document,
            Rc::downgrade(&doc.0),
        )
        .upgrade()
        .map(From::from)
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

    pub(crate) fn get_attribute_decl(&self, name: &str) -> Option<AttlistDeclRef> {
        self.0.borrow().attrdecl.get(name).cloned()
    }

    pub(crate) fn get_attribute_decls(&self) -> Vec<AttlistDeclRef> {
        self.0.borrow().attrdecl.values().cloned().collect()
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
