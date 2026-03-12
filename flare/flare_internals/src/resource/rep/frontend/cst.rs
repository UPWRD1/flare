use internment::Intern;

use crate::resource::rep::{
    common::{Ident, Spanned, Syntax, Variable},
    frontend::ast::Untyped,
};

/// Type representing a Pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Pattern<V: Variable> {
    Unit(V),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MatchArm<V: Variable> {
    pub pat: Spanned<Intern<Pattern<V>>>,
    pub body: Spanned<Intern<CstExpr<V>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum CstExpr<V>
where
    V: Variable,
{
    Ident(V),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UntypedCst;

impl Syntax for UntypedCst {
    type Expr = Spanned<Intern<CstExpr<Self::Variable>>>;
    type Type = Spanned<Intern<super::ast::Type>>;
    type Variable = Untyped;
    type Name = Spanned<Intern<String>>;
}
