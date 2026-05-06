use internment::Intern;

use crate::{
    passes::frontend::typing::PrimitiveType,
    resource::rep::{
        common::Spanned,
        frontend::ast::{ItemId, Label},
    },
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CstType {
    Generic(Spanned<Intern<String>>),
    Primitive(PrimitiveType),
    Func(Spanned<Intern<Self>>, Spanned<Intern<Self>>),

    // Package(Spanned<Intern<String>>),
    Item(ItemId),
    GenericFun(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    ForAll(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    GenericApp(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    User(Spanned<Intern<String>>),
    Prod(CstClosedRow),
    Sum(CstClosedRow),
    Label(Label, Spanned<Intern<Self>>),
    Hole,
}
impl Default for CstType {
    fn default() -> Self {
        Self::Primitive(PrimitiveType::default())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CstClosedRow {
    pub fields: &'static [Label],
    pub values: &'static [Spanned<Intern<CstType>>],
}
