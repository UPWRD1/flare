use crate::resource::rep::{common::Syntax, frontend::files::FileID};
use internment::Intern;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PackageEntry<S: Syntax> {
    pub name: S::Name,
    pub id: FileID,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct FunctionItem<S: Syntax> {
    pub name: S::Name,
    pub sig: S::Type,
    pub body: S::Expr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[repr(transparent)]
pub struct Item<S: Syntax> {
    pub kind: ItemKind<S>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ItemKind<S: Syntax> {
    Root,
    Filename(Intern<String>),
    Function(FunctionItem<S>),
    Type(
        S::Name,
        &'static [S::Type],
        S::Type,
        // SimpleSpan<usize, u64>,
    ),
    Extern {
        name: S::Name,
        args: &'static [S::Variable],
        sig: S::Type,
    },
    Dummy(&'static str),
}

impl<S: Syntax> Item<S> {
    pub fn new(kind: ItemKind<S>) -> Self {
        Self { kind }
    }

    pub fn ident(&self) -> String {
        match &self.kind {
            ItemKind::Root => String::from("ROOT"),
            ItemKind::Function(FunctionItem { name, .. }) => name.to_string(),
            ItemKind::Extern { name, .. } => name.to_string(),
            ItemKind::Type(name, _, _) => name.to_string(),
            _ => panic!(),
        }
    }
}

impl<S: Syntax> Default for Item<S> {
    fn default() -> Self {
        Self {
            kind: ItemKind::Dummy(""),
        }
    }
}
