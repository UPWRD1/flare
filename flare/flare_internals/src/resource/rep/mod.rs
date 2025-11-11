pub mod ast;
pub mod common;
pub mod entry;
pub mod files;
pub mod mir;
pub mod quantifier;
pub mod types;

use chumsky::span::SimpleSpan;

use files::FileID;

use crate::resource::rep::common::SpanWrapped;

// use rkyv::{with::ArchiveWith, Archive, Deserialize, Serialize};
// use rkyv_with::ArchiveWith;

pub type Spanned<T> = (T, SimpleSpan<usize, FileID>);
impl<T> SpanWrapped for Spanned<T> {
    fn get_span(&self) -> SimpleSpan<usize, u64>
    where
        Self: SpanWrapped,
    {
        self.1
    }
}
