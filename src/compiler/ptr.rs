use std::{cell::RefCell, fmt::Debug, ops::Deref, rc::Rc};
use std::rc::Weak;

#[derive(Debug)]
pub struct MutRc<T>(Rc<RefCell<T>>);

#[derive(Debug)]
pub struct MutWeak<T>(Weak<RefCell<T>>);

impl<T> MutRc<T> {
    pub fn new(val: T) -> MutRc<T> {
        MutRc(Rc::new(RefCell::new(val)))
    }

    pub fn from(rc: &Rc<RefCell<T>>) -> MutRc<T> {
        MutRc { 0: rc.clone() }
    }

    pub fn weak(&self) -> MutWeak<T> {
        MutWeak(Rc::downgrade(&self.0))
    }

    pub fn take_inner(this: Self) -> Result<T, MutRc<T>> {
        Rc::try_unwrap(this.0).map(|x| x.into_inner()).map_err(MutRc)
    }
}

impl<T> Deref for MutRc<T> {
    type Target = Rc<RefCell<T>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> Deref for MutWeak<T> {
    type Target = std::rc::Weak<RefCell<T>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> Clone for MutRc<T> {
    fn clone(&self) -> Self {
        MutRc(self.0.clone())
    }
}

impl<T> Clone for MutWeak<T> {
    fn clone(&self) -> Self {
        MutWeak(self.0.clone())
    }
}
