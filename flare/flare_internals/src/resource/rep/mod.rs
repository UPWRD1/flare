pub mod ast;
pub mod common;
pub mod entry;
pub mod files;
pub mod mir;
pub mod quantifier;
// pub mod concretetypes;

use std::fmt;

use chumsky::span::SimpleSpan;

use files::FileID;

use crate::resource::rep::common::SpanWrapped;

// use rkyv::{with::ArchiveWith, Archive, Deserialize, Serialize};
// use rkyv_with::ArchiveWith;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Spanned<T>(pub T, pub SimpleSpan<usize, FileID>);

impl<T: PartialOrd> PartialOrd for Spanned<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.0.partial_cmp(&other.0) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        self.1.partial_cmp(&other.1)
    }
}

impl<T: Ord> Ord for Spanned<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl<T> Spanned<T> {
    pub fn replace(self, value: impl Into<T>) -> Self {
        Self(value.into(), self.1)
    }

    pub fn inplace(&mut self, value: impl Into<T>) {
        self.0 = value.into();
    }

    pub fn update<U>(self, value: impl Into<U>) -> Spanned<U> {
        Spanned(value.into(), self.1)
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
