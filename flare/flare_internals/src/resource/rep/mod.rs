pub mod ast;
pub mod entry;
pub mod files;
pub mod mir;
pub mod quantifier;
pub mod types;

use chumsky::span::SimpleSpan;

use files::FileID;

// use rkyv::{with::ArchiveWith, Archive, Deserialize, Serialize};
// use rkyv_with::ArchiveWith;

pub type Spanned<T> = (T, SimpleSpan<usize, FileID>);
