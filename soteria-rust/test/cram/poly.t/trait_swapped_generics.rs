use std::marker::PhantomData;
use std::mem::size_of;

struct Pair<A, B>(PhantomData<(A, B)>);

fn size<T>() -> usize {
    size_of::<T>()
}

trait Provided {
    fn first_size() -> usize;
}

impl<A, B> Provided for Pair<A, B> {
    fn first_size() -> usize {
        size::<A>()
    }
}

fn call_provided<T: Provided>() -> usize {
    T::first_size()
}

trait Defaulted<A, B> {
    fn first_size() -> usize {
        size::<A>()
    }
}

impl<A, B> Defaulted<A, B> for () {}

fn call_defaulted<A, B, T: Defaulted<A, B>>() -> usize {
    T::first_size()
}

#[soteria::test]
fn impl_method<X, Y>() {
    assert!(call_provided::<Pair<Y, X>>() == size_of::<Y>());
}

#[soteria::test]
fn default_method<X, Y>() {
    assert!(call_defaulted::<Y, X, ()>() == size_of::<Y>());
}
