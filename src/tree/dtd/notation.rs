/// A DTD Notation definition.
pub type XmlNotationPtr = *mut XmlNotation;
#[repr(C)]
#[derive(Clone, Default)]
pub struct XmlNotation {
    pub(crate) name: Option<String>,      /* Notation name */
    pub(crate) public_id: Option<String>, /* Public identifier, if any */
    pub(crate) system_id: Option<String>, /* System identifier, if any */
}
