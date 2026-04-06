use internment::Intern;

use crate::resource::rep::{common::Spanned, frontend::ast::Label};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub enum CstType {
    Generic(Spanned<String>),

    Particle(Spanned<String>),
    #[default]
    Unit,
    Num,
    Bool,
    String,
    Func(Spanned<Intern<Self>>, Spanned<Intern<Self>>),

    // Package(Spanned<Intern<String>>),
    // Item(ItemId),
    GenericFun(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    GenericApp(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    User(Spanned<Intern<String>>, &'static [Spanned<Intern<Self>>]),
    Prod(CstClosedRow),
    Sum(CstClosedRow),
    Label(Label, Spanned<Intern<Self>>),
    Hole,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CstClosedRow {
    pub fields: &'static [Label],
    pub values: &'static [Spanned<Intern<CstType>>],
}
