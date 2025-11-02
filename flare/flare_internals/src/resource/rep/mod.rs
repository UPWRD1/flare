pub mod ast;
pub mod entry;
pub mod files;
pub mod mir;
pub mod quantifier;
pub mod types;

use chumsky::span::SimpleSpan;

use files::FileID;

pub type Spanned<T: Copy> = (T, SimpleSpan<usize, FileID>);
