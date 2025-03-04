use super::error::xml_schema_psimple_err;

#[doc(alias = "xmlSchemaItemListPtr")]
pub type XmlSchemaItemListPtr<T> = *mut XmlSchemaItemList<T>;
#[doc(alias = "xmlSchemaItemList")]
#[repr(C)]
pub struct XmlSchemaItemList<T> {
    pub(crate) items: Vec<T>, /* used for dynamic addition of schemata */
}

impl<T> XmlSchemaItemList<T> {
    #[doc(alias = "xmlSchemaItemListAddSize", alias = "xmlSchemaItemListAdd")]
    pub(crate) fn push(&mut self, item: T) -> i32 {
        self.items.push(item);
        0
    }

    #[doc(alias = "xmlSchemaItemListInsert")]
    pub(crate) fn insert(&mut self, item: T, idx: usize) -> i32 {
        // Just append if the index is greater/equal than the item count.
        let idx = idx.min(self.items.len());
        self.items.insert(idx, item);
        0
    }

    #[doc(alias = "xmlSchemaItemListRemove")]
    pub(crate) fn remove(&mut self, idx: usize) -> i32 {
        if idx >= self.items.len() {
            unsafe {
                xml_schema_psimple_err("Internal error: xmlSchemaItemListRemove, index error.\n");
            }
            return -1;
        }
        self.items.remove(idx);
        0
    }

    #[doc(alias = "xmlSchemaItemListClear")]
    pub(crate) fn clear(&mut self) {
        self.items.clear();
        self.items.shrink_to_fit();
    }
}

impl<T> Default for XmlSchemaItemList<T> {
    fn default() -> Self {
        Self { items: vec![] }
    }
}

pub(crate) fn xml_schema_item_list_create<T>() -> XmlSchemaItemListPtr<T> {
    Box::leak(Box::new(XmlSchemaItemList::<T>::default()))
}

/// Deallocate a annotation structure
#[doc(alias = "xmlSchemaItemListFree")]
pub(crate) unsafe fn xml_schema_item_list_free<T>(list: XmlSchemaItemListPtr<T>) {
    unsafe {
        if list.is_null() {
            return;
        }
        let _ = Box::from_raw(list);
    }
}
