use std::{
    fmt::{self, Debug, Display},
    hash::Hash,
};

use crate::resource::rep::common::{HasSpan, Ident, Spanned, Variable};

use chumsky::span::SimpleSpan;
use internment::Intern;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
#[repr(transparent)]
pub struct Untyped(pub Spanned<Intern<String>>);

impl Variable for Untyped {}

impl HasSpan for Untyped {
    fn span(&self) {}
}

impl Ident for Untyped {
    fn ident(&self) {}
}

impl Display for Untyped {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.0)
    }
}
