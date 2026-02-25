use crate::resource::{
    errors::CompResult,
    rep::frontend::{
        ast::{Expr, Variable},
        files::FileID,
    },
};
use chumsky::span::SimpleSpan;
use internment::Intern;
use std::{fmt, hash};

pub trait SpanWrapped {
    fn get_span(&self) -> SimpleSpan<usize, u64>;
}

pub trait Ident {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>>;
}

pub trait HasSpan {
    fn span(&self) -> SimpleSpan<usize, u64>;
}

impl<T> HasSpan for Spanned<Intern<T>> {
    fn span(&self) -> SimpleSpan<usize, u64> {
        self.1
    }
}

impl Ident for Spanned<Intern<String>> {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        Ok(*self)
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Spanned<T>(pub T, pub SimpleSpan<usize, FileID>);

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

// pub type Spanned<T> = (T, SimpleSpan<usize, FileID>);
impl<T> SpanWrapped for Spanned<T> {
    fn get_span(&self) -> SimpleSpan<usize, u64>
    where
        Self: SpanWrapped,
    {
        self.1
    }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
pub type NodeId = SimpleSpan<usize, u64>;
/// Trait for entities that have Names. Implementing this trait is preferred
/// over a custom name implementation. Currently the only major type that
/// implements its own name getter is `QualifierFragment`, since it doesn't
/// carry span information (since it is statically created within the compiler)
pub trait Named<V: Variable>: std::fmt::Debug {
    // #[clippy::deny()]
    /// Internal get_name that returns a name or `None`. Users should implement this function, but shouldn't call it.
    fn get_name(&self) -> Option<Spanned<Intern<Expr<V>>>>;

    fn name(&self) -> CompResult<Spanned<Intern<Expr<V>>>>
// where
        // Self: std::fmt::Debug,
    {
        let n = self.get_name();
        match n {
            Some(d) => Ok(d),
            None => todo!("Cannot get name, {self:?}"),
            // None => DynamicErr::new(format!("Cannot get name of {:?}", self))
            // .label("here", self.to_owned()),
        }
    }
}
