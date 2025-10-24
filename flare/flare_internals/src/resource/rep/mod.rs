pub mod ast;
pub mod types;
pub mod files;
pub mod quantifier;
pub mod entry;

use chumsky::span::SimpleSpan;

use files::FileID;

pub type Spanned<T> = (T, SimpleSpan<usize, FileID>);

