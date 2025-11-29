use std::cell::Cell;

use chumsky::span::{SimpleSpan, Span};
use internment::Intern;
// use lasso::Spur;

use crate::{
    passes::midend::typing::Type,
    resource::{
        errors::{CompResult, CompilerErr, FatalErr},
        rep::{
            ast::{Untyped, Variable},
            common::{Ident, SpanWrapped},
            files::FileID,
        },
    },
};

use super::{
    ast::Expr,
    // concretetypes::{EnumVariant, Ty},
    Spanned,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PackageEntry {
    pub name: Spanned<Intern<String>>,
    pub id: FileID,
    // pub file: &'static Path,
    //    pub deps: Vec<Spanned<Expr>>,

    //contains: Vec<Index>, // Consider using pure index-based referencing instead of the Trie
    // pub src: &'static str,
}

// #[derive(Debug, PartialEq, Eq, Clone, Copy)]
// /// Wrapper type denoting a type as a Struct
// pub struct StructEntry {
//     pub ty: Spanned<Intern<Type>>,
// }

// #[derive(Debug, PartialEq, Eq, Clone, Copy)]
// /// Wrapper type denoting a type as an Enum
// pub struct EnumEntry {
//     pub ty: Spanned<Intern<Ty>>,
// }

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct FunctionItem<V: Variable> {
    pub name: V,
    // pub sig: Cell<Option<Spanned<Intern<Type>>>>,
    pub sig: Spanned<Intern<Type>>,
    pub body: Spanned<Intern<Expr<V>>>,
    // pub checked: Cell<bool>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Item {
    pub kind: ItemKind,
    pub is_checked: Cell<bool>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ItemKind {
    Root,
    Filename(Intern<String>),
    Package(PackageEntry),
    Function(FunctionItem<Untyped>),
    Type(Type, SimpleSpan<usize, u64>),
    Extern {
        name: Spanned<Intern<String>>,
        sig: Spanned<Intern<Type>>,
    },
    Dummy(&'static str),
}

impl SpanWrapped for Item {
    // TODO: Remove panicking code
    fn get_span(&self) -> chumsky::prelude::SimpleSpan<usize, u64>
    where
        Self: SpanWrapped,
    {
        match &self.kind {
            ItemKind::Root => panic!(), // SimpleSpan::new(0, 0..0)
            ItemKind::Filename(_intern) => panic!(),
            ItemKind::Package(package_entry) => package_entry.name.1,
            ItemKind::Function(function_item) => function_item.name.0 .1.union(
                function_item
                    .sig
                    // .get()
                    // .unwrap()
                    .1
                    .union(function_item.body.1),
            ),
            ItemKind::Type(_, s) => *s,
            ItemKind::Extern { name, sig } => name.1.union(sig.1),
            ItemKind::Dummy(_) => panic!(),
        }
    }
}

impl Ident for Item {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        match self.kind {
            ItemKind::Root => todo!(),
            // Item::Filename(s) => s,
            ItemKind::Package(PackageEntry { name, .. }) => Ok(name),
            // ItemKind::Struct(StructEntry { ty, .. }) => ty.ident(),
            // ItemKind::Enum(EnumEntry { ty, .. }) => ty.ident(),
            // Item::Variant((EnumVariant { name, .. }, _)) => &name.0,
            // ItemKind::Field((name, ..)) => Ok(name),
            ItemKind::Function(FunctionItem { name, .. }) => Ok(name.0),
            ItemKind::Extern { name, .. } => Ok(name),
            _ => panic!(),
        }
    }
}

impl Item {
    pub fn new(kind: ItemKind, is_checked: bool) -> Self {
        Self {
            kind,
            is_checked: is_checked.into(),
        }
    }

    #[must_use]
    /// Get the signature of functions
    pub fn get_sig(&self) -> Option<Spanned<Intern<Type>>> {
        match &self.kind {
            ItemKind::Function(FunctionItem { sig, .. }) => Some(*sig),
            ItemKind::Extern { sig, .. } => Some(*sig),
            _ => None,
        }
    }

    pub fn is_checked(&self) -> bool {
        self.is_checked.get()
    }

    /// Get the type of the `Item`.
    pub fn get_ty(&self) -> CompResult<Spanned<Intern<Type>>> {
        fn err(t: &Item) -> CompilerErr {
            FatalErr::new(format!("Could not get the type from {:?}", t))
        }
        match &self.kind {
            ItemKind::Function(FunctionItem { sig, .. }) => Ok(*sig),
            // ItemKind::Struct(StructEntry { ty, .. }) => Ok(*ty),
            // ItemKind::Enum(EnumEntry { ty, .. }) => Ok(*ty),
            // ItemKind::Variant(Spanned(v, s)) => Ok(Spanned(Intern::from(Ty::Variant(*v)), *s)),
            // ItemKind::Field((_, ty)) => Ok(*ty),
            ItemKind::Package(p) => Ok(Spanned(Intern::from(Type::Package(p.name)), p.name.1)),
            _ => Err(err(self)),
        }
    }
}

impl Default for Item {
    fn default() -> Self {
        Self {
            kind: ItemKind::Dummy(""),
            is_checked: false.into(),
        }
    }
}
