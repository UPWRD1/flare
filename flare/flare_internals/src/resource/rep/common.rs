/// Common traits and types that are pervasive throughout the compiler.
/// Most of these are frontend related.
use crate::resource::rep::frontend::files::FileID;
use internment::Intern;
use std::{
    fmt::{Debug, Display},
    hash::{self, Hash},
    ops::Range,
};

pub trait Variable:
    Clone + Copy + PartialEq + Debug + Eq + Hash + Sync + Send + 'static + Display
{
}
pub trait Syntax: Debug + Copy + PartialEq + Eq + Hash + Send + Sync + 'static {
    type Expr: Clone + Copy + Debug + PartialEq + Eq + Hash + Send + Sync + 'static + Default;
    type Type: Clone + Copy + Debug + PartialEq + Eq + Hash + Send + Sync + 'static;
    type Pattern: Clone + Copy + Debug + PartialEq + Eq + Hash + Send + Sync + 'static;
    type Variable: Variable;
    type Name: Clone + Copy + Debug + PartialEq + Eq + Hash + Send + Sync + 'static + Display;
}

pub trait HasSpan {
    fn span(&self) -> FlareSpan;
}

#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FlareSpan(pub usize, pub usize, pub FileID);

impl Debug for FlareSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}[{}..{}]", self.2, self.0, self.1)
    }
}

impl FlareSpan {
    pub fn into_range(self) -> Range<usize> {
        self.0..self.1
    }
    pub fn union(self, rhs: Self) -> Self {
        if self.2 == rhs.2 {
            Self(self.0.max(rhs.0), self.1.max(rhs.1), self.2)
        } else {
            panic!("Incompatible ids")
        }
    }

    pub fn with<T>(self, val: T) -> Spanned<T> {
        Spanned(val, self)
    }
}

#[derive(Clone, Copy, Default)]
pub struct Spanned<T>(pub T, pub FlareSpan);

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.0)
        // f.debug_tuple("").field(&self.0).field(&self.1).finish()
    }
}

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

    pub fn map_inner<U, V>(self, f: impl FnOnce(T) -> U) -> Spanned<V>
    where
        U: Into<V>,
    {
        Spanned(((f)(self.0)).into(), self.1)
    }

    pub fn map<U, V>(self, f: impl FnOnce(Self) -> U) -> Spanned<V>
    where
        U: Into<V>,
    {
        let span = self.1.clone();
        Spanned(((f)(self)).into(), span)
    }

    pub fn default_with(v: impl Into<T>) -> Self {
        Self(v.into(), FlareSpan::default())
    }
}

impl<T> From<(T, FlareSpan)> for Spanned<T> {
    fn from(value: (T, FlareSpan)) -> Self {
        Self(value.0, value.1)
    }
}

impl<T> HasSpan for Spanned<T> {
    fn span(&self) -> FlareSpan {
        self.1
    }
}

impl Display for Spanned<Intern<String>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
