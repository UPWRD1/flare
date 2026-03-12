use std::{
    any::Any,
    error::Error,
    fmt::{self, Display},
    io::Cursor,
    ops::Deref,
};

mod templates {

    use std::fmt::Display;

    use crate::resource::errors::DynamicErr;
    use crate::*;
    use chumsky::span::SimpleSpan;
    pub fn not_defined(q: impl Display, s: &SimpleSpan<usize, u64>) -> CompilerErr {
        loop {}
    }

    // pub fn bad_ident(item: impl Display) -> CompilerErr {
    //     DynamicErr::new(format!("Cannot get an identifier from a {}", item)).into()
    // }
}

use rustc_hash::FxHashMap;
pub(crate) use templates::*;

use ariadne::{Color, Label, Report, ReportKind, sources};

use chumsky::span::{SimpleSpan, Span};
use thiserror::Error;

pub type CompResult<T> = Result<T, CompilerErr>;

#[derive(Debug, Error)]
pub struct CompilerErr(Box<dyn Error>);

// use crate::{FileCtx, FileID, resource::rep::frontend::files::FileSource};

impl Display for CompilerErr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
#[derive(Debug, Error, Clone)]
pub struct DynamicErr {}

impl std::fmt::Display for DynamicErr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> fmt::Result {
        write!(f, "Dynamic Error: {:?}", self)
    }
}
/// Opaque Error created from DynamicErr.
#[derive(Error, Debug)]
pub struct GeneralErr {}
impl std::fmt::Display for GeneralErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        loop {}
    }
}
