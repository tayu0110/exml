pub mod libxml_api;

use std::{
    cmp::Ordering,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    rc::Rc,
};

pub use libxml_api::*;

pub struct XmlLink<T> {
    prev: Option<XmlLinkRef<T>>,
    next: Option<XmlLinkRef<T>>,
    data: T,
}

impl<T> XmlLink<T> {
    pub fn get(&self) -> &T {
        &self.data
    }
}

#[derive(Debug)]
pub(crate) struct XmlLinkRef<T>(NonNull<XmlLink<T>>);

impl<T> XmlLinkRef<T> {
    fn new(data: T) -> Option<Self> {
        let boxed = Box::new(XmlLink {
            prev: None,
            next: None,
            data,
        });
        let leaked = Box::leak(boxed);
        NonNull::new(leaked).map(Self)
    }

    fn from_raw(ptr: *mut XmlLink<T>) -> Option<Self> {
        NonNull::new(ptr).map(Self)
    }

    fn into_inner(self) -> XmlLink<T> {
        unsafe { *Box::from_raw(self.0.as_ptr()) }
    }
}

impl<T> Clone for XmlLinkRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for XmlLinkRef<T> {}

impl<T> Deref for XmlLinkRef<T> {
    type Target = XmlLink<T>;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl<T> DerefMut for XmlLinkRef<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

type Deallocator<T> = Rc<dyn Fn(T)>;
type Comparator<T> = Rc<dyn Fn(&T, &T) -> Ordering>;

pub struct XmlList<T> {
    head: Option<XmlLinkRef<T>>,
    tail: Option<XmlLinkRef<T>>,
    deallocator: Deallocator<T>,
    comparator: Comparator<T>,
}

impl<T> XmlList<T> {
    /// Clear all elements.
    ///
    /// # Examples
    /// ```rust
    /// use exml::list::XmlList;
    ///
    /// let mut list = XmlList::new(None, None);
    /// list.insert_lower_bound(1u32);
    /// list.insert_lower_bound(2u32);
    ///
    /// assert!(!list.is_empty());
    ///
    /// list.clear();
    ///
    /// assert!(list.is_empty());
    /// ```
    pub fn clear(&mut self) {
        let mut link = self.head.take();
        while let Some(now) = link {
            link = now.next;
            let now = now.into_inner();
            (self.deallocator)(now.data);
        }
        self.tail = None;
    }
}

impl<T: Ord + 'static> XmlList<T> {
    /// Create new `XmlList`.
    ///
    /// If `deallocator` is `None`, `drop` is used by default.  
    /// If `comparator` is `None`, `T::cmp` is used by default.
    pub fn new(deallocator: Option<Deallocator<T>>, comparator: Option<Comparator<T>>) -> Self {
        let mut default = Self::default();
        if let Some(deallocator) = deallocator {
            default.deallocator = deallocator;
        }
        if let Some(comparator) = comparator {
            default.comparator = comparator;
        }
        default
    }

    /// Return a `link` satisfies `!self.comparator(&link.data, &data).is_lt()`.  
    ///
    /// If all elements satisfies `self.comparator(&link.data, &data).is_lt()`, return `None`.
    ///
    /// # Constraint
    /// - `self` must be sorted.
    fn lower_search(&self, data: &T) -> Option<XmlLinkRef<T>> {
        let mut link = self.head;
        while let Some(now) = link.filter(|link| (self.comparator)(&link.data, data).is_lt()) {
            link = now.next;
        }
        link
    }

    /// Return the first `link` satisfies `self.comparator(&link.data, &data).is_eq()`.
    ///
    /// If such element is not found, return `None`.
    ///
    /// # Constraint
    /// - `self` must be sorted.
    fn link_search(&self, data: &T) -> Option<XmlLinkRef<T>> {
        let link = self.lower_search(data)?;
        (self.comparator)(&link.data, data).is_eq().then_some(link)
    }

    /// Return the first data that `self.comparator` determines to be equal to `data`.
    ///
    /// If such element is not found, return `None`.
    ///
    /// # Constraint
    /// - `self` must be sorted.
    ///
    /// # Examples
    /// ```rust
    /// use std::rc::Rc;
    ///
    /// use exml::list::XmlList;
    ///
    /// // ignore second element when comparing
    /// let comparator = |l: &(u32, u32), r: &(u32, u32)| l.0.cmp(&r.0);
    /// let mut list = XmlList::new(None, Some(Rc::new(comparator)));
    /// list.push_last((0u32, 0u32));
    /// list.push_last((1, 0));
    /// list.push_last((1, 2));
    /// list.push_last((3, 0));
    ///
    /// assert_eq!(list.search(&(0, 0)), Some(&(0, 0)));
    /// // `(1, 0)` is equal to `(1, 0)` and `(1, 2)` bacause second element is ignored.
    /// // Of these, the more forward `(1, 0)` is chosen.
    /// assert_eq!(list.search(&(1, 0)), Some(&(1, 0)));
    /// assert_eq!(list.search(&(2, 0)), None);
    /// assert_eq!(list.search(&(3, 0)), Some(&(3, 0)));
    /// assert_eq!(list.search(&(4, 0)), None);
    /// ```
    pub fn search(&self, data: &T) -> Option<&T> {
        self.link_search(data).map(|link| unsafe {
            // we cannot use `&link.data` because of lifetime constraint.
            &link.0.as_ref().data
        })
    }

    /// Return a `link` satisfies `!self.comparator(&link.data, &data).is_gt()`.  
    ///
    /// If all elements satisfies `self.comparator(&link.data, &data).is_gt()`, return `None`.
    ///
    /// # Constraint
    /// - `self` must be sorted.
    fn higher_search(&self, data: &T) -> Option<XmlLinkRef<T>> {
        let mut link = self.tail;
        while let Some(now) = link.filter(|link| (self.comparator)(&link.data, data).is_gt()) {
            link = now.prev;
        }
        link
    }

    /// Return the last `link` satisfies `self.comparator(&link.data, &data).is_eq()`.
    ///
    /// If such element is not found, return `None`.
    ///
    /// # Constraint
    /// - `self` must be sorted.
    fn link_reverse_search(&self, data: &T) -> Option<XmlLinkRef<T>> {
        let link = self.higher_search(data)?;
        (self.comparator)(&link.data, data).is_eq().then_some(link)
    }

    /// Return the last data that `self.comparator` determines to be equal to `data`.
    ///
    /// If such element is not found, return `None`.
    ///
    /// # Constraint
    /// - `self` must be sorted.
    ///
    /// # Examples
    /// ```rust
    /// use std::rc::Rc;
    ///
    /// use exml::list::XmlList;
    ///
    /// // ignore second element when comparing
    /// let comparator = |l: &(u32, u32), r: &(u32, u32)| l.0.cmp(&r.0);
    /// let mut list = XmlList::new(None, Some(Rc::new(comparator)));
    /// list.push_last((0u32, 0u32));
    /// list.push_last((1, 0));
    /// list.push_last((1, 2));
    /// list.push_last((3, 0));
    ///
    /// assert_eq!(list.reverse_search(&(0, 0)), Some(&(0, 0)));
    /// // `(1, 0)` is equal to `(1, 0)` and `(1, 2)` bacause second element is ignored.
    /// // Of these, the more backward `(1, 2)` is chosen.
    /// assert_eq!(list.reverse_search(&(1, 0)), Some(&(1, 2)));
    /// assert_eq!(list.reverse_search(&(2, 0)), None);
    /// assert_eq!(list.reverse_search(&(3, 0)), Some(&(3, 0)));
    /// assert_eq!(list.reverse_search(&(4, 0)), None);
    /// ```
    pub fn reverse_search(&self, data: &T) -> Option<&T> {
        self.link_reverse_search(data).map(|link| unsafe {
            // we cannot use `&link.data` because of lifetime constraint.
            &link.0.as_ref().data
        })
    }

    /// Insert `data` in a position where the list can remain sorted.
    ///
    /// If there are multiple such positions, insert at the beginning of them.
    ///
    /// # Constraint
    /// - `self` must be sorted.
    ///
    /// # Examples
    /// ```rust
    /// use std::rc::Rc;
    ///
    /// use exml::list::XmlList;
    ///
    /// // ignore second element when comparing
    /// let comparator = |l: &(u32, u32), r: &(u32, u32)| l.0.cmp(&r.0);
    /// let mut list = XmlList::new(None, Some(Rc::new(comparator)));
    /// list.insert_lower_bound((0u32, 0u32));
    /// list.insert_lower_bound((1, 0));
    /// list.insert_lower_bound((1, 2));
    /// list.insert_lower_bound((3, 0));
    ///
    /// // `(1, 0)` is equal to `(1, 2)` because second element is ignored.
    /// // `insert_lower_bound` inserted `(1, 2)` before `(1, 0)`.
    /// assert_eq!(list.search(&(1, 0)), Some(&(1, 2)));
    /// ```
    pub fn insert_lower_bound(&mut self, data: T) {
        let bound = self.lower_search(&data);
        let mut new = XmlLinkRef::new(data).expect("Failed to generate new XmlLinkRef");
        if let Some(mut bound) = bound {
            // (bound.prev ->) new -> bound
            new.prev = bound.prev;
            new.next = Some(bound);
            if let Some(mut prev) = bound.prev {
                prev.next = Some(new);
            } else {
                // If `bound.prev` is `None`, `bound` is the head of the list
                self.head = Some(new);
            }
            bound.prev = Some(new);
        } else {
            // If `bound` is `None`, `new` is the tail of the list
            if let Some(mut tail) = self.tail {
                // old-tail -> new
                tail.next = Some(new);
                new.prev = Some(tail);
            } else {
                // If `self.tail` is `None`, the list is empty.
                self.head = Some(new);
            }
            self.tail = Some(new);
        }
    }

    /// Insert `data` in a position where the list can remain sorted.
    ///
    /// If there are multiple such positions, insert at the end of them.
    ///
    /// # Constraint
    /// - `self` must be sorted.
    ///
    /// # Examples
    /// ```rust
    /// use std::rc::Rc;
    ///
    /// use exml::list::XmlList;
    ///
    /// // ignore second element when comparing
    /// let comparator = |l: &(u32, u32), r: &(u32, u32)| l.0.cmp(&r.0);
    /// let mut list = XmlList::new(None, Some(Rc::new(comparator)));
    /// list.insert_upper_bound((0u32, 0u32));
    /// list.insert_upper_bound((1, 0));
    /// list.insert_upper_bound((1, 2));
    /// list.insert_upper_bound((3, 0));
    ///
    /// // `(1, 0)` is equal to `(1, 2)` because second element is ignored.
    /// // `insert_lower_bound` inserted `(1, 2)` after `(1, 0)`.
    /// assert_eq!(list.search(&(1, 0)), Some(&(1, 0)));
    /// ```
    pub fn insert_upper_bound(&mut self, data: T) {
        let bound = self.higher_search(&data);
        let mut new = XmlLinkRef::new(data).expect("Failed to generate new XmlLinkRef");
        if let Some(mut bound) = bound {
            // bound -> new (-> bound.next)
            new.prev = Some(bound);
            new.next = bound.next;
            if let Some(mut next) = bound.next {
                next.prev = Some(new);
            } else {
                // If `bound.next` is `None`, `bound` is the tail of the list
                self.tail = Some(new);
            }
            bound.next = Some(new);
        } else {
            // If `bound` is `None`, `new` is the head of the list
            if let Some(mut head) = self.head {
                // new -> old-head
                head.prev = Some(new);
                new.next = Some(head);
            } else {
                // If `self.head` is `None`, the list is empty
                self.tail = Some(new);
            }
            self.head = Some(new);
        }
    }

    fn remove_link(&mut self, link: XmlLinkRef<T>) -> T {
        if let Some(mut prev) = link.prev {
            prev.next = link.next;
        } else {
            self.head = link.next;
        }
        if let Some(mut next) = link.next {
            next.prev = link.prev;
        } else {
            self.tail = link.prev;
        }
        let XmlLink { data, .. } = link.into_inner();
        data
    }

    /// Remove the first element determined to be equal with `data` by `self.comparator` of the list
    /// and return its data.
    ///
    /// # Constraint
    /// - `self` must be sorted.
    ///
    /// # Examples
    /// ```rust
    /// use std::rc::Rc;
    ///
    /// use exml::list::XmlList;
    ///
    /// // ignore second element when comparing
    /// let comparator = |l: &(u32, u32), r: &(u32, u32)| l.0.cmp(&r.0);
    /// let mut list = XmlList::new(None, Some(Rc::new(comparator)));
    /// list.push_last((0u32, 0u32));
    /// list.push_last((1, 0));
    /// list.push_last((1, 2));
    /// list.push_last((3, 0));
    ///
    /// assert_eq!(list.search(&(1, 0)), Some(&(1, 0)));
    ///
    /// assert_eq!(list.remove_first(&(1, 0)), Some((1, 0)));
    ///
    /// assert_eq!(list.search(&(1, 0)), Some(&(1, 2)));
    /// ```
    pub fn remove_first(&mut self, data: &T) -> Option<T> {
        let link = self.link_search(data)?;
        Some(self.remove_link(link))
    }

    /// Remove the last element determined to be equal with `data` by `self.comparator` of the list
    /// and return its data.
    ///
    /// # Constraint
    /// - `self` must be sorted.
    ///
    /// # Examples
    /// ```rust
    /// use std::rc::Rc;
    ///
    /// use exml::list::XmlList;
    ///
    /// // ignore second element when comparing
    /// let comparator = |l: &(u32, u32), r: &(u32, u32)| l.0.cmp(&r.0);
    /// let mut list = XmlList::new(None, Some(Rc::new(comparator)));
    /// list.push_last((0u32, 0u32));
    /// list.push_last((1, 0));
    /// list.push_last((1, 2));
    /// list.push_last((3, 0));
    ///
    /// assert_eq!(list.reverse_search(&(1, 0)), Some(&(1, 2)));
    ///
    /// assert_eq!(list.remove_last(&(1, 0)), Some((1, 2)));
    ///
    /// assert_eq!(list.reverse_search(&(1, 0)), Some(&(1, 0)));
    /// ```
    pub fn remove_last(&mut self, data: &T) -> Option<T> {
        let link = self.link_reverse_search(data)?;
        Some(self.remove_link(link))
    }

    /// Remove all elements determined to be equal with `data` by `self.comparator` of the list
    /// and return its data.
    ///
    /// # Constraint
    /// - `self` must be sorted.
    ///
    /// # Examples
    /// ```rust
    /// use std::rc::Rc;
    ///
    /// use exml::list::XmlList;
    ///
    /// // ignore second element when comparing
    /// let comparator = |l: &(u32, u32), r: &(u32, u32)| l.0.cmp(&r.0);
    /// let mut list = XmlList::new(None, Some(Rc::new(comparator)));
    /// list.push_last((0u32, 0u32));
    /// list.push_last((1, 0));
    /// list.push_last((1, 2));
    /// list.push_last((3, 0));
    ///
    /// assert_eq!(list.search(&(1, 0)), Some(&(1, 0)));
    ///
    /// assert_eq!(list.remove_all(&(1, 0)).collect::<Vec<_>>(), vec![(1, 0), (1, 2)]);
    ///
    /// assert_eq!(list.search(&(1, 0)), None);
    /// ```
    pub fn remove_all<'a>(&'a mut self, data: &'a T) -> impl Iterator<Item = T> + 'a {
        let mut link = self.link_search(data);
        std::iter::from_fn(move || {
            let now = link?;
            (self.comparator)(&now.data, data).is_eq().then(|| {
                link = now.next;
                self.remove_link(now)
            })
        })
    }

    /// Check if the list is empty.
    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    /// Count how many elements the list has.
    ///
    /// # Note
    /// Since `XmlList` does not has a counter, this method enumerates elements on each call.  
    /// Therefore, the complexity of this method is `O(N)`, `N` is the number of elements of the list.
    pub fn len(&self) -> usize {
        let mut link = self.head;
        let mut count = 0;
        while let Some(now) = link {
            count += 1;
            link = now.next;
        }
        count
    }

    /// In original libxml2, this is the public API.  
    /// However, XmlLinkRef should only be published in the crate
    /// because publishing XmlLinkRef may break the constraints of XmlList.
    pub(crate) fn first(&self) -> Option<&XmlLinkRef<T>> {
        self.head.as_ref()
    }

    /// In original libxml2, this is the public API.  
    /// However, XmlLinkRef should only be published in the crate
    /// because publishing XmlLinkRef may break the constraints of XmlList.
    pub(crate) fn last(&self) -> Option<&XmlLinkRef<T>> {
        self.tail.as_ref()
    }

    /// Remove the head of the list and return its data.
    ///
    /// # Examples
    /// ```rust
    /// use std::rc::Rc;
    ///
    /// use exml::list::XmlList;
    ///
    /// let mut list = XmlList::new(None, None);
    /// list.push_last(0u32);
    /// list.push_last(1);
    /// list.push_last(1);
    ///
    /// assert_eq!(list.pop_first(), Some(0));
    /// assert_eq!(list.pop_first(), Some(1));
    /// assert_eq!(list.pop_first(), Some(1));
    /// assert_eq!(list.pop_first(), None);
    /// ```
    pub fn pop_first(&mut self) -> Option<T> {
        let head = self.head?;
        Some(self.remove_link(head))
    }

    /// Remove the tail of the list and return its data.
    ///
    /// # Examples
    /// ```rust
    /// use std::rc::Rc;
    ///
    /// use exml::list::XmlList;
    ///
    /// let mut list = XmlList::new(None, None);
    /// list.push_last(0u32);
    /// list.push_last(1);
    /// list.push_last(1);
    ///
    /// assert_eq!(list.pop_last(), Some(1));
    /// assert_eq!(list.pop_last(), Some(1));
    /// assert_eq!(list.pop_last(), Some(0));
    /// assert_eq!(list.pop_last(), None);
    /// ```
    pub fn pop_last(&mut self) -> Option<T> {
        let tail = self.tail?;
        Some(self.remove_link(tail))
    }

    /// Insert `data` at the head of the list.
    ///
    /// This method may makes the order of `self` inconsistent.  
    /// Therefore, the user should guarantee the order of `self` yourself after using this method.
    ///
    /// # Examples
    /// ```rust
    /// use std::rc::Rc;
    ///
    /// use exml::list::XmlList;
    ///
    /// let mut list = XmlList::new(None, None);
    /// list.push_first(0u32);
    /// list.push_first(2);
    /// list.push_first(1);
    ///
    /// // Unless explicitly sorted, the order in which they are pushed is preserved.
    /// assert_eq!(list.pop_first(), Some(1));
    /// assert_eq!(list.pop_first(), Some(2));
    /// assert_eq!(list.pop_first(), Some(0));
    /// assert_eq!(list.pop_first(), None);
    /// ```
    pub fn push_first(&mut self, data: T) {
        let mut new = XmlLinkRef::new(data).expect("Failed to generate new XmlLinkRef");
        if let Some(mut head) = self.head {
            new.next = Some(head);
            head.prev = Some(new);
        } else {
            self.tail = Some(new);
        }
        self.head = Some(new);
    }

    /// Insert `data` at the tail of the list.
    ///
    /// This method may makes the order of `self` inconsistent.  
    /// Therefore, the user should guarantee the order of `self` yourself after using this method.
    ///
    /// # Examples
    /// ```rust
    /// use std::rc::Rc;
    ///
    /// use exml::list::XmlList;
    ///
    /// let mut list = XmlList::new(None, None);
    /// list.push_last(0u32);
    /// list.push_last(2);
    /// list.push_last(1);
    ///
    /// // Unless explicitly sorted, the order in which they are pushed is preserved.
    /// assert_eq!(list.pop_first(), Some(0));
    /// assert_eq!(list.pop_first(), Some(2));
    /// assert_eq!(list.pop_first(), Some(1));
    /// assert_eq!(list.pop_first(), None);
    /// ```
    pub fn push_last(&mut self, data: T) {
        let mut new = XmlLinkRef::new(data).expect("Failed to generate new XmlLinkRef");
        if let Some(mut tail) = self.tail {
            new.prev = Some(tail);
            tail.next = Some(new);
        } else {
            self.head = Some(new);
        }
        self.tail = Some(new);
    }

    /// Reverse the order of `self`.
    ///
    /// # Examples
    /// ```rust
    /// use std::rc::Rc;
    ///
    /// use exml::list::XmlList;
    ///
    /// let mut list = XmlList::new(None, None);
    /// list.push_last(0u32);
    /// list.push_last(2);
    /// list.push_last(1);
    ///
    /// list.reverse();
    ///
    /// assert_eq!(list.pop_first(), Some(1));
    /// assert_eq!(list.pop_first(), Some(2));
    /// assert_eq!(list.pop_first(), Some(0));
    /// assert_eq!(list.pop_first(), None);
    /// ```
    pub fn reverse(&mut self) {
        let Some(mut link) = self.head else {
            return;
        };
        self.tail = Some(link);

        let mut prev = None;
        while let Some(next) = link.next {
            // before: prev -> link -> next
            // after : next -> link -> prev
            link.next = prev;
            link.prev = Some(next);
            prev = Some(link);
            link = next;
        }
        link.next = prev;
        link.prev = None;
        self.head = Some(link);
    }

    /// Split the list at `at`-th element and return the tail-side of split lists.
    fn split_off(&mut self, at: usize) -> XmlList<T> {
        let mut count = 0;
        let mut link = self.head;
        let mut prev = None::<XmlLinkRef<T>>;
        while let Some(mut now) = link {
            if count == at {
                let new = XmlList {
                    head: Some(now),
                    tail: self.tail,
                    deallocator: Rc::clone(&self.deallocator),
                    comparator: Rc::clone(&self.comparator),
                };
                now.prev = None;
                if let Some(mut prev) = prev {
                    prev.next = None;
                }
                self.tail = prev;
                return new;
            }

            prev = Some(now);
            link = now.next;
            count += 1;
        }

        assert!(count == at);
        XmlList {
            head: None,
            tail: None,
            deallocator: Rc::clone(&self.deallocator),
            comparator: Rc::clone(&self.comparator),
        }
    }

    /// Sort the elements of the list.
    ///
    /// In original libxml2, it seems that insertion sort is used.  
    /// This method uses merge sort.
    ///
    /// # Examples
    /// ```rust
    /// use std::rc::Rc;
    ///
    /// use exml::list::XmlList;
    ///
    /// let mut list = XmlList::new(None, None);
    /// list.push_last(0u32);
    /// list.push_last(2);
    /// list.push_last(1);
    ///
    /// list.sort();
    ///
    /// assert_eq!(list.pop_first(), Some(0));
    /// assert_eq!(list.pop_first(), Some(1));
    /// assert_eq!(list.pop_first(), Some(2));
    /// assert_eq!(list.pop_first(), None);
    /// ```
    pub fn sort(&mut self) {
        let len = self.len();
        if len <= 1 {
            return;
        }

        let mut back = self.split_off(len / 2);
        self.sort();
        back.sort();

        self.merge_sorted_lists(back);
    }

    /// # Constraint
    /// - Both `self` and `other` must be sorted.
    /// - Both `self` and `other` must have same `deallocator` and `comparator`.
    /// - `self` and `other` can be empty.
    fn merge_sorted_lists(&mut self, other: Self) {
        if self.is_empty() {
            self.head = other.head;
            self.tail = other.tail;
            return;
        }

        if other.is_empty() {
            return;
        }

        let (mut shead, mut ohead) = (self.head, other.head);
        let mut prev = None::<XmlLinkRef<T>>;
        while let (Some(slink), Some(olink)) = (shead, ohead) {
            let mut link = if (self.comparator)(&slink.data, &olink.data).is_le() {
                shead = slink.next;
                slink
            } else {
                ohead = olink.next;
                olink
            };
            if let Some(mut p) = prev {
                p.next = Some(link);
                link.prev = Some(p);
            } else {
                self.head = Some(link);
            }
            prev = Some(link);
        }

        // Both `self` and `other` are checked that they are not empty.
        // Therefore, at least either `shead` or `ohead` is not `None` and `prev` cannot be `None`.
        // By the above, it is not a problem to do `unwrap` at this point.
        let mut prev = prev.unwrap();
        if let Some(mut shead) = shead {
            prev.next = Some(shead);
            shead.prev = Some(prev);
        } else if let Some(mut ohead) = ohead {
            prev.next = Some(ohead);
            ohead.prev = Some(prev);
            self.tail = other.tail;
        }
    }

    /// Walk from the head and process each data with `walk` until `walk` returns `false`.
    ///
    /// # Examples
    /// ```rust
    /// use std::rc::Rc;
    ///
    /// use exml::list::XmlList;
    ///
    /// let mut list = XmlList::new(None, None);
    /// list.push_last(0u32);
    /// list.push_last(1);
    /// list.push_last(2);
    /// list.push_last(3);
    /// list.push_last(4);
    ///
    /// let mut buf = vec![];
    /// list.walk(|&value| {
    ///     buf.push(value * 2);
    ///     value < 2
    /// });
    ///
    /// assert_eq!(buf, vec![0, 2, 4]);
    /// ```
    pub fn walk(&self, mut walk: impl FnMut(&T) -> bool) {
        let mut link = self.head;
        while let Some(now) = link {
            if !walk(&now.data) {
                break;
            }
            link = now.next;
        }
    }

    /// Walk from the tail and process each data with `walk` until `walk` returns `false`.
    ///
    /// # Examples
    /// ```rust
    /// use std::rc::Rc;
    ///
    /// use exml::list::XmlList;
    ///
    /// let mut list = XmlList::new(None, None);
    /// list.push_last(0u32);
    /// list.push_last(1);
    /// list.push_last(2);
    /// list.push_last(3);
    /// list.push_last(4);
    ///
    /// let mut buf = vec![];
    /// list.reverse_walk(|&value| {
    ///     buf.push(value * 2);
    ///     value > 2
    /// });
    ///
    /// assert_eq!(buf, vec![8, 6, 4]);
    /// ```
    pub fn reverse_walk(&self, mut walk: impl FnMut(&T) -> bool) {
        let mut link = self.tail;
        while let Some(now) = link {
            if !walk(&now.data) {
                break;
            }
            link = now.prev;
        }
    }

    /// Append list `other` to `self`.  
    /// `self` will sorted after processing this method.
    ///
    /// If the comparator of `other` is different from the one of `self`,
    /// the one of `self` is used in preference.
    ///
    /// # Examples
    /// ```rust
    /// use std::rc::Rc;
    ///
    /// use exml::list::XmlList;
    ///
    /// let mut list = XmlList::new(None, None);
    /// list.push_last(0u32);
    /// list.push_last(2);
    /// list.push_last(4);
    ///
    /// let mut list2 = XmlList::new(None, None);
    /// list.push_last(1u32);
    /// list.push_last(3);
    /// list.push_last(4);
    /// list.push_last(5);
    ///
    /// list.extend(list2);
    ///
    /// assert_eq!(list.into_iter().collect::<Vec<_>>(), vec![0, 1, 2, 3, 4, 4, 5]);
    /// ```
    pub fn extend(&mut self, mut other: Self) {
        self.sort();
        other.comparator = Rc::clone(&self.comparator);
        other.sort();
        self.merge_sorted_lists(other);
    }
}

impl<T: Ord + Clone + 'static> Clone for XmlList<T> {
    fn clone(&self) -> Self {
        let mut new = XmlList {
            deallocator: Rc::clone(&self.deallocator),
            comparator: Rc::clone(&self.comparator),
            ..Default::default()
        };

        if self.is_empty() {
            return new;
        }

        let mut head = XmlLinkRef::new(self.head.unwrap().data.clone())
            .expect("Failed to generate new XmlLinkRef");
        new.head = Some(head);
        let mut shead = self.head.unwrap();
        while let Some(snext) = shead.next {
            let next =
                XmlLinkRef::new(snext.data.clone()).expect("Failed to generate new XmlLinkRef");
            head = next;
            shead = snext;
        }
        new.tail = Some(head);
        new
    }
}

impl<T: Ord + 'static> Default for XmlList<T> {
    fn default() -> Self {
        Self {
            head: None,
            tail: None,
            deallocator: Rc::new(drop),
            comparator: Rc::new(T::cmp),
        }
    }
}

impl<T: Ord + 'static> IntoIterator for XmlList<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter { list: self }
    }
}

impl<T> Drop for XmlList<T> {
    fn drop(&mut self) {
        self.clear();
    }
}

pub struct IntoIter<T> {
    list: XmlList<T>,
}

impl<T: Ord + 'static> Iterator for IntoIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        self.list.pop_first()
    }
}

impl<T: Ord + 'static> DoubleEndedIterator for IntoIter<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.list.pop_last()
    }
}

pub struct XmlListRef<T>(NonNull<XmlList<T>>);

impl<T> XmlListRef<T> {
    pub fn from_list(list: XmlList<T>) -> Option<Self> {
        let boxed = Box::new(list);
        let leaked = Box::leak(boxed);
        NonNull::new(leaked).map(Self)
    }

    fn from_raw(ptr: *mut XmlList<T>) -> Option<Self> {
        NonNull::new(ptr).map(Self)
    }

    pub fn free(self) {
        unsafe {
            Box::from_raw(self.0.as_ptr());
        }
    }
}

impl<T> Clone for XmlListRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for XmlListRef<T> {}

impl<T> Deref for XmlListRef<T> {
    type Target = XmlList<T>;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl<T> DerefMut for XmlListRef<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}
