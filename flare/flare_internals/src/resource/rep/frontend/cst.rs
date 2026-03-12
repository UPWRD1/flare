use internment::Intern;

use crate::resource::rep::{
    common::{Spanned, Syntax},
    frontend::ast::Untyped,
};

/// Type representing a Pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Pattern {
    Unit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MatchArm {
    pub pat: Spanned<Intern<Pattern>>,
    pub body: Spanned<Intern<CstExpr>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum CstExpr {
    Ident,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UntypedCst;

impl Syntax for UntypedCst {
    type Expr = Spanned<Intern<CstExpr>>;
    type Type = Spanned<Intern<super::ast::Type>>;
    type Variable = Untyped;
    type Name = Spanned<Intern<String>>;
}
