
use chumsky::span::SimpleSpan;
use internment::Intern;
use std::{
    fmt::{Debug, Display},
    hash::{self, Hash},
};

pub trait Variable:
    Clone + PartialEq + Debug + Eq + Hash + Copy + Sync + Send + 'static + Ident + Display
{
}
pub trait Syntax: Debug + Copy + 'static {
    type Expr: Clone + Copy + Debug + PartialEq + Eq + Hash + 'static;
    type Type: Clone + Copy + Debug + PartialEq + Eq + Hash + 'static;

    type Variable: Variable + Copy;
    type Name: Clone + Copy + Debug + PartialEq + Eq + Hash + 'static + Ident;
}

pub trait Ident {
    fn ident(&self);
}

pub trait HasSpan {
    fn span(&self);
}

impl<T> HasSpan for Spanned<Intern<T>> {
    fn span(&self) {}
}

impl Ident for Spanned<Intern<String>> {
    fn ident(&self) {}
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Spanned<T>(pub T, pub SimpleSpan<usize, u64>);

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: Eq> Eq for Spanned<T> {}

impl<T: PartialOrd> PartialOrd for Spanned<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.0.partial_cmp(&other.0) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        self.1.partial_cmp(&other.1)
    }
}

impl<T: hash::Hash> hash::Hash for Spanned<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T: Ord> Ord for Spanned<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.partial_cmp(&other.0).unwrap()
    }
}

impl<T> Spanned<T> {
    pub fn modify(self, value: impl Into<T>) -> Self {
        Self(value.into(), self.1)
    }

    pub fn convert<U>(self, value: impl Into<U>) -> Spanned<U> {
        Spanned(value.into(), self.1)
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned((f)(self.0), self.1)
    }
}

impl<T> From<(T, SimpleSpan<usize, u64>)> for Spanned<T> {
    fn from(value: (T, SimpleSpan<usize, u64>)) -> Self {
        Self(value.0, value.1)
    }
}
pub type NodeId = SimpleSpan<usize, u64>;
