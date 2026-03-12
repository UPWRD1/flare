use crate::resource::rep::common::Syntax;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PackageEntry<S: Syntax> {
    pub name: S::Name,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct FunctionItem<S: Syntax> {
    pub name: S::Name,
    pub sig: S::Type,
    pub body: S::Expr,
    // extra_vars: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[repr(transparent)]
pub struct Item<S: Syntax> {
    pub kind: ItemKind<S>,
    // pub is_checked: Cell<bool>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ItemKind<S: Syntax> {
    Root,
    Function(FunctionItem<S>),
}
