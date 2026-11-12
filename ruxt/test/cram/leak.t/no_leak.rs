// Nothing here drops these types, so Charon translates no drop glue for them:
// their drops must be run from their structure.
pub struct Boxed {
    b: Box<(i32, i32)>,
}
pub fn boxed(x: i32) -> Boxed {
    Boxed { b: Box::new((x, x)) }
}
pub fn set_boxed(w: &mut Boxed, x: i32) {
    (*w.b).0 = x;
}

pub fn plain(x: i32) -> Box<i32> {
    Box::new(x)
}
pub fn peek_plain(_w: &Box<i32>) {}

pub struct Nested {
    inner: Boxed,
    all: (Box<i32>, [Box<i32>; 2]),
    boxes: Box<Box<i32>>,
}
pub fn nested(x: i32) -> Nested {
    Nested {
        inner: boxed(x),
        all: (Box::new(x), [Box::new(x), Box::new(x)]),
        boxes: Box::new(Box::new(x)),
    }
}
pub fn peek_nested(_w: &Nested) {}

pub struct List {
    next: Option<Box<List>>,
}
pub fn nil() -> List {
    List { next: None }
}
pub fn cons(l: List) -> List {
    List {
        next: Some(Box::new(l)),
    }
}
pub fn peek_list(_l: &List) {}

pub struct Generic<T> {
    b: Box<T>,
}
pub fn generic<T>(x: T) -> Generic<T> {
    Generic { b: Box::new(x) }
}
pub fn peek_generic<T>(_w: &Generic<T>) {}

// A `Drop` impl is also run for a field
pub struct Raw<T> {
    p: *mut T,
}
impl<T> Drop for Raw<T> {
    fn drop(&mut self) {
        unsafe { drop(Box::from_raw(self.p)) }
    }
}
pub struct HasDrop {
    raw: Raw<i32>,
    b: Box<i32>,
}
pub fn has_drop(x: i32) -> HasDrop {
    HasDrop {
        raw: Raw {
            p: Box::into_raw(Box::new(x)),
        },
        b: Box::new(x),
    }
}
pub fn peek_has_drop(_w: &HasDrop) {}

// A return value that borrows from an argument is dropped before it
pub struct Target {
    v: i32,
}
pub fn target(v: i32) -> Target {
    Target { v }
}
pub struct Guard<'a> {
    t: &'a mut Target,
    b: Box<i32>,
}
impl<'a> Drop for Guard<'a> {
    fn drop(&mut self) {
        self.t.v = 0;
    }
}
pub fn guard(t: &mut Target) -> Guard<'_> {
    Guard {
        t,
        b: Box::new(1),
    }
}
