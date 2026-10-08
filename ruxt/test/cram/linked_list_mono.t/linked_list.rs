// Adapted from the Rust standard library's `alloc::collections::linked_list`,
// licensed under MIT OR Apache-2.0, at rust-lang/rust commit
// 8fa1c96cfd489e4c27654c144ae871ce2c4db6c6 (nightly-2026-08-18):
// https://github.com/rust-lang/rust/blob/8fa1c96cfd489e4c27654c144ae871ce2c4db6c6/library/alloc/src/collections/linked_list.rs
//
// Modifications: monomorphised to `u32` elements and the global allocator,
// which removes `new_in` and the specialisation of `Extend` for `LinkedList`;
// documentation, attributes and `assert_covariance` removed; `cycle` added,
// which is not part of the standard library.

#![feature(exact_size_is_empty)]
#![feature(extend_one)]
#![feature(hasher_prefixfree_extras)]

use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::iter::{FromIterator, FusedIterator};
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::{fmt, mem};

pub struct LinkedList {
    head: Option<NonNull<Node>>,
    tail: Option<NonNull<Node>>,
    len: usize,
    marker: PhantomData<Box<Node>>,
}

struct Node {
    next: Option<NonNull<Node>>,
    prev: Option<NonNull<Node>>,
    element: u32,
}

pub struct Iter<'a> {
    head: Option<NonNull<Node>>,
    tail: Option<NonNull<Node>>,
    len: usize,
    marker: PhantomData<&'a Node>,
}

impl fmt::Debug for Iter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Iter")
            .field(&*mem::ManuallyDrop::new(LinkedList {
                head: self.head,
                tail: self.tail,
                len: self.len,
                marker: PhantomData,
            }))
            .field(&self.len)
            .finish()
    }
}

// FIXME(#26925) Remove in favor of `#[derive(Clone)]`
impl Clone for Iter<'_> {
    fn clone(&self) -> Self {
        Iter { ..*self }
    }
}

pub struct IterMut<'a> {
    head: Option<NonNull<Node>>,
    tail: Option<NonNull<Node>>,
    len: usize,
    marker: PhantomData<&'a mut Node>,
}

impl fmt::Debug for IterMut<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("IterMut")
            .field(&*mem::ManuallyDrop::new(LinkedList {
                head: self.head,
                tail: self.tail,
                len: self.len,
                marker: PhantomData,
            }))
            .field(&self.len)
            .finish()
    }
}

#[derive(Clone)]
pub struct IntoIter {
    list: LinkedList,
}

impl fmt::Debug for IntoIter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("IntoIter").field(&self.list).finish()
    }
}

impl Node {
    fn new(element: u32) -> Self {
        Node { next: None, prev: None, element }
    }

    fn into_element(self: Box<Self>) -> u32 {
        self.element
    }
}

// private methods
impl LinkedList {
    unsafe fn push_front_node(&mut self, node: NonNull<Node>) {
        // This method takes care not to create mutable references to whole nodes,
        // to maintain validity of aliasing pointers into `element`.
        unsafe {
            (*node.as_ptr()).next = self.head;
            (*node.as_ptr()).prev = None;
            let node = Some(node);

            match self.head {
                None => self.tail = node,
                // Not creating new mutable (unique!) references overlapping `element`.
                Some(head) => (*head.as_ptr()).prev = node,
            }

            self.head = node;
            self.len += 1;
        }
    }

    fn pop_front_node(&mut self) -> Option<Box<Node>> {
        // This method takes care not to create mutable references to whole nodes,
        // to maintain validity of aliasing pointers into `element`.
        self.head.map(|node| unsafe {
            let node = Box::from_raw(node.as_ptr());
            self.head = node.next;

            match self.head {
                None => self.tail = None,
                // Not creating new mutable (unique!) references overlapping `element`.
                Some(head) => (*head.as_ptr()).prev = None,
            }

            self.len -= 1;
            node
        })
    }

    unsafe fn push_back_node(&mut self, node: NonNull<Node>) {
        // This method takes care not to create mutable references to whole nodes,
        // to maintain validity of aliasing pointers into `element`.
        unsafe {
            (*node.as_ptr()).next = None;
            (*node.as_ptr()).prev = self.tail;
            let node = Some(node);

            match self.tail {
                None => self.head = node,
                // Not creating new mutable (unique!) references overlapping `element`.
                Some(tail) => (*tail.as_ptr()).next = node,
            }

            self.tail = node;
            self.len += 1;
        }
    }

    fn pop_back_node(&mut self) -> Option<Box<Node>> {
        // This method takes care not to create mutable references to whole nodes,
        // to maintain validity of aliasing pointers into `element`.
        self.tail.map(|node| unsafe {
            let node = Box::from_raw(node.as_ptr());
            self.tail = node.prev;

            match self.tail {
                None => self.head = None,
                // Not creating new mutable (unique!) references overlapping `element`.
                Some(tail) => (*tail.as_ptr()).next = None,
            }

            self.len -= 1;
            node
        })
    }

    unsafe fn unlink_node(&mut self, mut node: NonNull<Node>) {
        let node = unsafe { node.as_mut() }; // this one is ours now, we can create an &mut.

        // Not creating new mutable (unique!) references overlapping `element`.
        match node.prev {
            Some(prev) => unsafe { (*prev.as_ptr()).next = node.next },
            // this node is the head node
            None => self.head = node.next,
        };

        match node.next {
            Some(next) => unsafe { (*next.as_ptr()).prev = node.prev },
            // this node is the tail node
            None => self.tail = node.prev,
        };

        self.len -= 1;
    }

    unsafe fn splice_nodes(
        &mut self,
        existing_prev: Option<NonNull<Node>>,
        existing_next: Option<NonNull<Node>>,
        mut splice_start: NonNull<Node>,
        mut splice_end: NonNull<Node>,
        splice_length: usize,
    ) {
        // This method takes care not to create multiple mutable references to whole nodes at the same time,
        // to maintain validity of aliasing pointers into `element`.
        if let Some(mut existing_prev) = existing_prev {
            unsafe {
                existing_prev.as_mut().next = Some(splice_start);
            }
        } else {
            self.head = Some(splice_start);
        }
        if let Some(mut existing_next) = existing_next {
            unsafe {
                existing_next.as_mut().prev = Some(splice_end);
            }
        } else {
            self.tail = Some(splice_end);
        }
        unsafe {
            splice_start.as_mut().prev = existing_prev;
            splice_end.as_mut().next = existing_next;
        }

        self.len += splice_length;
    }

    fn detach_all_nodes(mut self) -> Option<(NonNull<Node>, NonNull<Node>, usize)> {
        let head = self.head.take();
        let tail = self.tail.take();
        let len = mem::replace(&mut self.len, 0);
        if let Some(head) = head {
            // SAFETY: In a LinkedList, either both the head and tail are None because
            // the list is empty, or both head and tail are Some because the list is populated.
            // Since we have verified the head is Some, we are sure the tail is Some too.
            let tail = unsafe { tail.unwrap_unchecked() };
            Some((head, tail, len))
        } else {
            None
        }
    }

    unsafe fn split_off_before_node(&mut self, split_node: Option<NonNull<Node>>, at: usize) -> Self {
        // The split node is the new head node of the second part
        if let Some(mut split_node) = split_node {
            let first_part_head;
            let first_part_tail;
            unsafe {
                first_part_tail = split_node.as_mut().prev.take();
            }
            if let Some(mut tail) = first_part_tail {
                unsafe {
                    tail.as_mut().next = None;
                }
                first_part_head = self.head;
            } else {
                first_part_head = None;
            }

            let first_part = LinkedList {
                head: first_part_head,
                tail: first_part_tail,
                len: at,
                marker: PhantomData,
            };

            // Fix the head ptr of the second part
            self.head = Some(split_node);
            self.len = self.len - at;

            first_part
        } else {
            mem::replace(self, LinkedList::new())
        }
    }

    unsafe fn split_off_after_node(&mut self, split_node: Option<NonNull<Node>>, at: usize) -> Self {
        // The split node is the new tail node of the first part and owns
        // the head of the second part.
        if let Some(mut split_node) = split_node {
            let second_part_head;
            let second_part_tail;
            unsafe {
                second_part_head = split_node.as_mut().next.take();
            }
            if let Some(mut head) = second_part_head {
                unsafe {
                    head.as_mut().prev = None;
                }
                second_part_tail = self.tail;
            } else {
                second_part_tail = None;
            }

            let second_part = LinkedList {
                head: second_part_head,
                tail: second_part_tail,
                len: self.len - at,
                marker: PhantomData,
            };

            // Fix the tail ptr of the first part
            self.tail = Some(split_node);
            self.len = at;

            second_part
        } else {
            mem::replace(self, LinkedList::new())
        }
    }
}

impl Default for LinkedList {
    fn default() -> Self {
        Self::new()
    }
}

impl LinkedList {
    pub const fn new() -> Self {
        LinkedList { head: None, tail: None, len: 0, marker: PhantomData }
    }

    pub fn append(&mut self, other: &mut Self) {
        match self.tail {
            None => mem::swap(self, other),
            Some(mut tail) => {
                // `as_mut` is okay here because we have exclusive access to the entirety
                // of both lists.
                if let Some(mut other_head) = other.head.take() {
                    unsafe {
                        tail.as_mut().next = Some(other_head);
                        other_head.as_mut().prev = Some(tail);
                    }

                    self.tail = other.tail.take();
                    self.len += mem::replace(&mut other.len, 0);
                }
            }
        }
    }
}

impl LinkedList {
    pub fn iter(&self) -> Iter<'_> {
        Iter { head: self.head, tail: self.tail, len: self.len, marker: PhantomData }
    }

    pub fn iter_mut(&mut self) -> IterMut<'_> {
        IterMut { head: self.head, tail: self.tail, len: self.len, marker: PhantomData }
    }

    pub fn cursor_front(&self) -> Cursor<'_> {
        Cursor { index: 0, current: self.head, list: self }
    }

    pub fn cursor_front_mut(&mut self) -> CursorMut<'_> {
        CursorMut { index: 0, current: self.head, list: self }
    }

    pub fn cursor_back(&self) -> Cursor<'_> {
        Cursor { index: self.len.saturating_sub(1), current: self.tail, list: self }
    }

    pub fn cursor_back_mut(&mut self) -> CursorMut<'_> {
        CursorMut { index: self.len.saturating_sub(1), current: self.tail, list: self }
    }

    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn clear(&mut self) {
        drop(LinkedList {
            head: self.head.take(),
            tail: self.tail.take(),
            len: mem::take(&mut self.len),
            marker: PhantomData,
        });
    }

    pub fn contains(&self, x: &u32) -> bool {
        self.iter().any(|e| e == x)
    }

    pub fn front(&self) -> Option<&u32> {
        unsafe { self.head.as_ref().map(|node| &node.as_ref().element) }
    }

    pub fn front_mut(&mut self) -> Option<&mut u32> {
        unsafe { self.head.as_mut().map(|node| &mut node.as_mut().element) }
    }

    pub fn back(&self) -> Option<&u32> {
        unsafe { self.tail.as_ref().map(|node| &node.as_ref().element) }
    }

    pub fn back_mut(&mut self) -> Option<&mut u32> {
        unsafe { self.tail.as_mut().map(|node| &mut node.as_mut().element) }
    }

    pub fn push_front(&mut self, elt: u32) {
        let _ = self.push_front_mut(elt);
    }

    pub fn push_front_mut(&mut self, elt: u32) -> &mut u32 {
        let mut node = NonNull::from(Box::leak(Box::new(Node::new(elt))));
        // SAFETY: node is a unique pointer to a node
        unsafe {
            self.push_front_node(node);
            &mut node.as_mut().element
        }
    }

    pub fn pop_front(&mut self) -> Option<u32> {
        self.pop_front_node().map(Node::into_element)
    }

    pub fn push_back(&mut self, elt: u32) {
        let _ = self.push_back_mut(elt);
    }

    pub fn push_back_mut(&mut self, elt: u32) -> &mut u32 {
        let mut node = NonNull::from(Box::leak(Box::new(Node::new(elt))));
        // SAFETY: node is a unique pointer to a node
        unsafe {
            self.push_back_node(node);
            &mut node.as_mut().element
        }
    }

    pub fn pop_back(&mut self) -> Option<u32> {
        self.pop_back_node().map(Node::into_element)
    }

    pub fn split_off(&mut self, at: usize) -> LinkedList {
        let len = self.len();
        assert!(at <= len, "Cannot split off at a nonexistent index");
        if at == 0 {
            return mem::replace(self, Self::new());
        } else if at == len {
            return Self::new();
        }

        // Below, we iterate towards the `i-1`th node, either from the start or the end,
        // depending on which would be faster.
        let split_node = if at - 1 <= len - 1 - (at - 1) {
            let mut iter = self.iter_mut();
            // instead of skipping using .skip() (which creates a new struct),
            // we skip manually so we can access the head field without
            // depending on implementation details of Skip
            for _ in 0..at - 1 {
                iter.next();
            }
            iter.head
        } else {
            // better off starting from the end
            let mut iter = self.iter_mut();
            for _ in 0..len - 1 - (at - 1) {
                iter.next_back();
            }
            iter.tail
        };
        unsafe { self.split_off_after_node(split_node, at) }
    }

    pub fn remove(&mut self, at: usize) -> u32 {
        let len = self.len();
        assert!(at < len, "Cannot remove at an index outside of the list bounds");

        // Below, we iterate towards the node at the given index, either from
        // the start or the end, depending on which would be faster.
        let offset_from_end = len - at - 1;
        if at <= offset_from_end {
            let mut cursor = self.cursor_front_mut();
            for _ in 0..at {
                cursor.move_next();
            }
            cursor.remove_current().unwrap()
        } else {
            let mut cursor = self.cursor_back_mut();
            for _ in 0..offset_from_end {
                cursor.move_prev();
            }
            cursor.remove_current().unwrap()
        }
    }

    pub fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut u32) -> bool,
    {
        let mut cursor = self.cursor_front_mut();
        while let Some(node) = cursor.current() {
            if !f(node) {
                cursor.remove_current().unwrap();
            } else {
                cursor.move_next();
            }
        }
    }

    pub fn extract_if<F>(&mut self, filter: F) -> ExtractIf<'_, F>
    where
        F: FnMut(&mut u32) -> bool,
    {
        // avoid borrow issues.
        let it = self.head;
        let old_len = self.len;

        ExtractIf { list: self, it, pred: filter, idx: 0, old_len }
    }

    // Not part of the standard library: creates a cycle in the list, which is
    // incompatible with how `drop` is implemented.
    pub fn cycle(&mut self) {
        self.tail.map(|node| unsafe {
            (*node.as_ptr()).next = self.head;
        });
    }
}

impl Drop for LinkedList {
    fn drop(&mut self) {
        struct DropGuard<'a>(&'a mut LinkedList);

        impl<'a> Drop for DropGuard<'a> {
            fn drop(&mut self) {
                // Continue the same loop we do below. This only runs when a destructor has
                // panicked. If another one panics this will abort.
                while self.0.pop_front_node().is_some() {}
            }
        }

        // Wrap self so that if a destructor panics, we can try to keep looping
        let guard = DropGuard(self);
        while guard.0.pop_front_node().is_some() {}
        mem::forget(guard);
    }
}

impl<'a> Iterator for Iter<'a> {
    type Item = &'a u32;

    fn next(&mut self) -> Option<&'a u32> {
        if self.len == 0 {
            None
        } else {
            self.head.map(|node| unsafe {
                // Need an unbound lifetime to get 'a
                let node = &*node.as_ptr();
                self.len -= 1;
                self.head = node.next;
                &node.element
            })
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }

    fn last(mut self) -> Option<&'a u32> {
        self.next_back()
    }
}

impl<'a> DoubleEndedIterator for Iter<'a> {
    fn next_back(&mut self) -> Option<&'a u32> {
        if self.len == 0 {
            None
        } else {
            self.tail.map(|node| unsafe {
                // Need an unbound lifetime to get 'a
                let node = &*node.as_ptr();
                self.len -= 1;
                self.tail = node.prev;
                &node.element
            })
        }
    }
}

impl ExactSizeIterator for Iter<'_> {}

impl FusedIterator for Iter<'_> {}

impl Default for Iter<'_> {
    fn default() -> Self {
        Iter { head: None, tail: None, len: 0, marker: Default::default() }
    }
}

impl<'a> Iterator for IterMut<'a> {
    type Item = &'a mut u32;

    fn next(&mut self) -> Option<&'a mut u32> {
        if self.len == 0 {
            None
        } else {
            self.head.map(|node| unsafe {
                // Need an unbound lifetime to get 'a
                let node = &mut *node.as_ptr();
                self.len -= 1;
                self.head = node.next;
                &mut node.element
            })
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }

    fn last(mut self) -> Option<&'a mut u32> {
        self.next_back()
    }
}

impl<'a> DoubleEndedIterator for IterMut<'a> {
    fn next_back(&mut self) -> Option<&'a mut u32> {
        if self.len == 0 {
            None
        } else {
            self.tail.map(|node| unsafe {
                // Need an unbound lifetime to get 'a
                let node = &mut *node.as_ptr();
                self.len -= 1;
                self.tail = node.prev;
                &mut node.element
            })
        }
    }
}

impl ExactSizeIterator for IterMut<'_> {}

impl FusedIterator for IterMut<'_> {}

impl Default for IterMut<'_> {
    fn default() -> Self {
        IterMut { head: None, tail: None, len: 0, marker: Default::default() }
    }
}

pub struct Cursor<'a> {
    index: usize,
    current: Option<NonNull<Node>>,
    list: &'a LinkedList,
}

impl Clone for Cursor<'_> {
    fn clone(&self) -> Self {
        let Cursor { index, current, list } = *self;
        Cursor { index, current, list }
    }
}

impl fmt::Debug for Cursor<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Cursor").field(&self.list).field(&self.index()).finish()
    }
}

pub struct CursorMut<'a> {
    index: usize,
    current: Option<NonNull<Node>>,
    list: &'a mut LinkedList,
}

impl fmt::Debug for CursorMut<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("CursorMut").field(&self.list).field(&self.index()).finish()
    }
}

impl<'a> Cursor<'a> {
    pub fn index(&self) -> Option<usize> {
        let _ = self.current?;
        Some(self.index)
    }

    pub fn move_next(&mut self) {
        match self.current.take() {
            // We had no current element; the cursor was sitting at the start position
            // Next element should be the head of the list
            None => {
                self.current = self.list.head;
                self.index = 0;
            }
            // We had a previous element, so let's go to its next
            Some(current) => unsafe {
                self.current = current.as_ref().next;
                self.index += 1;
            },
        }
    }

    pub fn move_prev(&mut self) {
        match self.current.take() {
            // No current. We're at the start of the list. Yield None and jump to the end.
            None => {
                self.current = self.list.tail;
                self.index = self.list.len().saturating_sub(1);
            }
            // Have a prev. Yield it and go to the previous element.
            Some(current) => unsafe {
                self.current = current.as_ref().prev;
                self.index = self.index.checked_sub(1).unwrap_or_else(|| self.list.len());
            },
        }
    }

    pub fn current(&self) -> Option<&'a u32> {
        unsafe { self.current.map(|current| &(*current.as_ptr()).element) }
    }

    pub fn peek_next(&self) -> Option<&'a u32> {
        unsafe {
            let next = match self.current {
                None => self.list.head,
                Some(current) => current.as_ref().next,
            };
            next.map(|next| &(*next.as_ptr()).element)
        }
    }

    pub fn peek_prev(&self) -> Option<&'a u32> {
        unsafe {
            let prev = match self.current {
                None => self.list.tail,
                Some(current) => current.as_ref().prev,
            };
            prev.map(|prev| &(*prev.as_ptr()).element)
        }
    }

    pub fn front(&self) -> Option<&'a u32> {
        self.list.front()
    }

    pub fn back(&self) -> Option<&'a u32> {
        self.list.back()
    }

    pub fn as_list(&self) -> &'a LinkedList {
        self.list
    }
}

impl<'a> CursorMut<'a> {
    pub fn index(&self) -> Option<usize> {
        let _ = self.current?;
        Some(self.index)
    }

    pub fn move_next(&mut self) {
        match self.current.take() {
            // We had no current element; the cursor was sitting at the start position
            // Next element should be the head of the list
            None => {
                self.current = self.list.head;
                self.index = 0;
            }
            // We had a previous element, so let's go to its next
            Some(current) => unsafe {
                self.current = current.as_ref().next;
                self.index += 1;
            },
        }
    }

    pub fn move_prev(&mut self) {
        match self.current.take() {
            // No current. We're at the start of the list. Yield None and jump to the end.
            None => {
                self.current = self.list.tail;
                self.index = self.list.len().saturating_sub(1);
            }
            // Have a prev. Yield it and go to the previous element.
            Some(current) => unsafe {
                self.current = current.as_ref().prev;
                self.index = self.index.checked_sub(1).unwrap_or_else(|| self.list.len());
            },
        }
    }

    pub fn current(&mut self) -> Option<&mut u32> {
        unsafe { self.current.map(|current| &mut (*current.as_ptr()).element) }
    }

    pub fn peek_next(&mut self) -> Option<&mut u32> {
        unsafe {
            let next = match self.current {
                None => self.list.head,
                Some(current) => current.as_ref().next,
            };
            next.map(|next| &mut (*next.as_ptr()).element)
        }
    }

    pub fn peek_prev(&mut self) -> Option<&mut u32> {
        unsafe {
            let prev = match self.current {
                None => self.list.tail,
                Some(current) => current.as_ref().prev,
            };
            prev.map(|prev| &mut (*prev.as_ptr()).element)
        }
    }

    pub fn as_cursor(&self) -> Cursor<'_> {
        Cursor { list: self.list, current: self.current, index: self.index }
    }

    pub fn as_list(&self) -> &LinkedList {
        self.list
    }
}

// Now the list editing operations

impl<'a> CursorMut<'a> {
    pub fn splice_after(&mut self, list: LinkedList) {
        unsafe {
            let Some((splice_head, splice_tail, splice_len)) = list.detach_all_nodes() else {
                return;
            };
            let node_next = match self.current {
                None => self.list.head,
                Some(node) => node.as_ref().next,
            };
            self.list.splice_nodes(self.current, node_next, splice_head, splice_tail, splice_len);
            if self.current.is_none() {
                // The "ghost" non-element's index has changed.
                self.index = self.list.len;
            }
        }
    }

    pub fn splice_before(&mut self, list: LinkedList) {
        unsafe {
            let (splice_head, splice_tail, splice_len) = match list.detach_all_nodes() {
                Some(parts) => parts,
                _ => return,
            };
            let node_prev = match self.current {
                None => self.list.tail,
                Some(node) => node.as_ref().prev,
            };
            self.list.splice_nodes(node_prev, self.current, splice_head, splice_tail, splice_len);
            self.index += splice_len;
        }
    }
}

impl<'a> CursorMut<'a> {
    pub fn insert_after(&mut self, item: u32) {
        unsafe {
            let spliced_node = NonNull::from(Box::leak(Box::new(Node::new(item))));
            let node_next = match self.current {
                None => self.list.head,
                Some(node) => node.as_ref().next,
            };
            self.list.splice_nodes(self.current, node_next, spliced_node, spliced_node, 1);
            if self.current.is_none() {
                // The "ghost" non-element's index has changed.
                self.index = self.list.len;
            }
        }
    }

    pub fn insert_before(&mut self, item: u32) {
        unsafe {
            let spliced_node = NonNull::from(Box::leak(Box::new(Node::new(item))));
            let node_prev = match self.current {
                None => self.list.tail,
                Some(node) => node.as_ref().prev,
            };
            self.list.splice_nodes(node_prev, self.current, spliced_node, spliced_node, 1);
            self.index += 1;
        }
    }

    pub fn remove_current(&mut self) -> Option<u32> {
        let unlinked_node = self.current?;
        unsafe {
            self.current = unlinked_node.as_ref().next;
            self.list.unlink_node(unlinked_node);
            let unlinked_node = Box::from_raw(unlinked_node.as_ptr());
            Some(unlinked_node.element)
        }
    }

    pub fn remove_current_as_list(&mut self) -> Option<LinkedList> {
        let mut unlinked_node = self.current?;
        unsafe {
            self.current = unlinked_node.as_ref().next;
            self.list.unlink_node(unlinked_node);

            unlinked_node.as_mut().prev = None;
            unlinked_node.as_mut().next = None;
            Some(LinkedList {
                head: Some(unlinked_node),
                tail: Some(unlinked_node),
                len: 1,
                marker: PhantomData,
            })
        }
    }

    pub fn split_after(&mut self) -> LinkedList {
        let split_off_idx = if self.index == self.list.len { 0 } else { self.index + 1 };
        if self.index == self.list.len {
            // The "ghost" non-element's index has changed to 0.
            self.index = 0;
        }
        unsafe { self.list.split_off_after_node(self.current, split_off_idx) }
    }

    pub fn split_before(&mut self) -> LinkedList {
        let split_off_idx = self.index;
        self.index = 0;
        unsafe { self.list.split_off_before_node(self.current, split_off_idx) }
    }

    // `push_front` continues to point to "ghost" when it adds a node to mimic
    // the behavior of `insert_before` on an empty list.
    pub fn push_front(&mut self, elt: u32) {
        // Safety: We know that `push_front` does not change the position in
        // memory of other nodes. This ensures that `self.current` remains
        // valid.
        self.list.push_front(elt);
        self.index += 1;
    }

    pub fn push_back(&mut self, elt: u32) {
        // Safety: We know that `push_back` does not change the position in
        // memory of other nodes. This ensures that `self.current` remains
        // valid.
        self.list.push_back(elt);
        if self.current().is_none() {
            // The index of "ghost" is the length of the list, so we just need
            // to increment self.index to reflect the new length of the list.
            self.index += 1;
        }
    }

    pub fn pop_front(&mut self) -> Option<u32> {
        // We can't check if current is empty, we must check the list directly.
        // It is possible for `self.current == None` and the list to be
        // non-empty.
        if self.list.is_empty() {
            None
        } else {
            // We can't point to the node that we pop. Copying the behavior of
            // `remove_current`, we move on to the next node in the sequence.
            // If the list is of length 1 then we end pointing to the "ghost"
            // node at index 0, which is expected.
            if self.list.head == self.current {
                self.move_next();
            }
            // An element was removed before (or at) our current position, so
            // the index must be decremented. `saturating_sub` handles the
            // ghost node case where index could be 0.
            self.index = self.index.saturating_sub(1);
            self.list.pop_front()
        }
    }

    pub fn pop_back(&mut self) -> Option<u32> {
        if self.list.is_empty() {
            None
        } else {
            if self.list.tail == self.current {
                // The index now reflects the length of the list. It was the
                // length of the list minus 1, but now the list is 1 smaller. No
                // change is needed for `index`.
                self.current = None;
            } else if self.current.is_none() {
                self.index = self.list.len - 1;
            }
            self.list.pop_back()
        }
    }

    pub fn front(&self) -> Option<&u32> {
        self.list.front()
    }

    pub fn front_mut(&mut self) -> Option<&mut u32> {
        self.list.front_mut()
    }

    pub fn back(&self) -> Option<&u32> {
        self.list.back()
    }

    pub fn back_mut(&mut self) -> Option<&mut u32> {
        self.list.back_mut()
    }
}

pub struct ExtractIf<'a, F: 'a> {
    list: &'a mut LinkedList,
    it: Option<NonNull<Node>>,
    pred: F,
    idx: usize,
    old_len: usize,
}

impl<F> Iterator for ExtractIf<'_, F>
where
    F: FnMut(&mut u32) -> bool,
{
    type Item = u32;

    fn next(&mut self) -> Option<u32> {
        while let Some(mut node) = self.it {
            unsafe {
                self.it = node.as_ref().next;
                self.idx += 1;

                if (self.pred)(&mut node.as_mut().element) {
                    // `unlink_node` is okay with aliasing `element` references.
                    self.list.unlink_node(node);
                    return Some(Box::from_raw(node.as_ptr()).element);
                }
            }
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.old_len - self.idx))
    }
}

impl<F> fmt::Debug for ExtractIf<'_, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let peek = self.it.map(|node| unsafe { &node.as_ref().element });
        f.debug_struct("ExtractIf").field("peek", &peek).finish_non_exhaustive()
    }
}

impl Iterator for IntoIter {
    type Item = u32;

    fn next(&mut self) -> Option<u32> {
        self.list.pop_front()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.list.len, Some(self.list.len))
    }
}

impl DoubleEndedIterator for IntoIter {
    fn next_back(&mut self) -> Option<u32> {
        self.list.pop_back()
    }
}

impl ExactSizeIterator for IntoIter {}

impl FusedIterator for IntoIter {}

impl Default for IntoIter {
    fn default() -> Self {
        LinkedList::new().into_iter()
    }
}

impl FromIterator<u32> for LinkedList {
    fn from_iter<I: IntoIterator<Item = u32>>(iter: I) -> Self {
        let mut list = Self::new();
        list.extend(iter);
        list
    }
}

impl IntoIterator for LinkedList {
    type Item = u32;
    type IntoIter = IntoIter;

    fn into_iter(self) -> IntoIter {
        IntoIter { list: self }
    }
}

impl<'a> IntoIterator for &'a LinkedList {
    type Item = &'a u32;
    type IntoIter = Iter<'a>;

    fn into_iter(self) -> Iter<'a> {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a mut LinkedList {
    type Item = &'a mut u32;
    type IntoIter = IterMut<'a>;

    fn into_iter(self) -> IterMut<'a> {
        self.iter_mut()
    }
}

impl Extend<u32> for LinkedList {
    fn extend<I: IntoIterator<Item = u32>>(&mut self, iter: I) {
        iter.into_iter().for_each(move |elt| self.push_back(elt));
    }

    fn extend_one(&mut self, elem: u32) {
        self.push_back(elem);
    }
}

impl<'a> Extend<&'a u32> for LinkedList {
    fn extend<I: IntoIterator<Item = &'a u32>>(&mut self, iter: I) {
        self.extend(iter.into_iter().cloned());
    }

    fn extend_one(&mut self, &elem: &'a u32) {
        self.push_back(elem);
    }
}

impl PartialEq for LinkedList {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().eq(other)
    }

    fn ne(&self, other: &Self) -> bool {
        self.len() != other.len() || self.iter().ne(other)
    }
}

impl Eq for LinkedList {}

impl PartialOrd for LinkedList {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.iter().partial_cmp(other)
    }
}

impl Ord for LinkedList {
    fn cmp(&self, other: &Self) -> Ordering {
        self.iter().cmp(other)
    }
}

impl Clone for LinkedList {
    fn clone(&self) -> Self {
        let mut list = Self::new();
        list.extend(self.iter().cloned());
        list
    }

    fn clone_from(&mut self, source: &Self) {
        let mut source_iter = source.iter();
        for elem in self.iter_mut() {
            let Some(source_elem) = source_iter.next() else {
                break;
            };
            elem.clone_from(source_elem);
        }
        while self.len() > source.len() {
            self.pop_back();
        }
        if !source_iter.is_empty() {
            self.extend(source_iter.cloned());
        }
    }
}

impl fmt::Debug for LinkedList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self).finish()
    }
}

impl Hash for LinkedList {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_length_prefix(self.len());
        for elt in self {
            elt.hash(state);
        }
    }
}

impl<const N: usize> From<[u32; N]> for LinkedList {
    fn from(arr: [u32; N]) -> Self {
        Self::from_iter(arr)
    }
}

unsafe impl Send for LinkedList {}

unsafe impl Sync for LinkedList {}

unsafe impl Send for Iter<'_> {}

unsafe impl Sync for Iter<'_> {}

unsafe impl Send for IterMut<'_> {}

unsafe impl Sync for IterMut<'_> {}

unsafe impl Send for Cursor<'_> {}

unsafe impl Sync for Cursor<'_> {}

unsafe impl Send for CursorMut<'_> {}

unsafe impl Sync for CursorMut<'_> {}
