// Opaque values behind a pointer keep their layout and type variable from one
// call to the next, under renaming.
pub struct Boxed<T, U> {
    b: Box<(T, U)>,
}

pub fn new<T, U>(x: T, y: U) -> Boxed<T, U> {
    Boxed { b: Box::new((x, y)) }
}

pub fn swap<T, U>(w: Boxed<T, U>) -> Boxed<U, T> {
    let (x, y) = *w.b;
    Boxed { b: Box::new((y, x)) }
}

pub fn first<X, Y>(w: Boxed<Y, X>) -> Y {
    (*w.b).0
}

pub fn set_first<T, U>(w: &mut Boxed<T, U>, x: T) {
    (*w.b).0 = x;
}

pub fn get_first<T, U>(w: &mut Boxed<T, U>) -> Option<&mut T> {
    Some(&mut (*w.b).0)
}
