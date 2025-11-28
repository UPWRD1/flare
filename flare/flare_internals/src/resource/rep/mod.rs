pub mod ast;
pub mod common;
pub mod entry;
pub mod files;
pub mod mir;
pub mod quantifier;
// pub mod concretetypes;

use chumsky::span::SimpleSpan;

use files::FileID;

use crate::resource::rep::common::SpanWrapped;

// use rkyv::{with::ArchiveWith, Archive, Deserialize, Serialize};
// use rkyv_with::ArchiveWith;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Spanned<T>(pub T, pub SimpleSpan<usize, FileID>);

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

impl<T> std::fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
