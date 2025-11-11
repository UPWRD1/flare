use std::cell::Cell;

use chumsky::span::Span;
use internment::Intern;
// use lasso::Spur;

use crate::resource::rep::{
    common::{Named, SpanWrapped},
    files::FileID,
};

use super::{
    ast::Expr,
    types::{EnumVariant, Ty},
    Spanned,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PackageEntry {
    pub name: Spanned<Intern<Expr>>,
    pub id: FileID,
    // pub file: &'static Path,
    //    pub deps: Vec<Spanned<Expr>>,

    //contains: Vec<Index>, // Consider using pure index-based referencing instead of the Trie
    // pub src: &'static str,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// Wrapper type denoting a type as a Struct
pub struct StructEntry {
    pub ty: Spanned<Intern<Ty>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// Wrapper type denoting a type as an Enum
pub struct EnumEntry {
    pub ty: Spanned<Intern<Ty>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionItem {
    pub name: Spanned<Intern<Expr>>,
    pub sig: Cell<Option<Spanned<Intern<Ty>>>>,
    pub body: Spanned<Intern<Expr>>,
    pub checked: Cell<bool>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Item {
    Root,
    Filename(Intern<String>),
    Package(PackageEntry),
    Struct(StructEntry),
    Enum(EnumEntry),
    Variant(Spanned<EnumVariant>),

    Field((Spanned<Intern<Expr>>, Spanned<Intern<Ty>>)),
    Function(FunctionItem),
    Extern {
        name: Spanned<Intern<Expr>>,
        sig: Spanned<Intern<Ty>>,
    },
    Dummy(&'static str),
}

impl SpanWrapped for Item {
    // TODO: Remove panicking code
    fn get_span(&self) -> chumsky::prelude::SimpleSpan<usize, u64>
    where
        Self: SpanWrapped,
    {
        match self {
            Item::Root => panic!(), // SimpleSpan::new(0, 0..0)
            Item::Filename(_intern) => panic!(),
            Item::Package(package_entry) => package_entry.name.1,
            Item::Struct(struct_entry) => struct_entry.ty.1,
            Item::Enum(enum_entry) => enum_entry.ty.1,
            Item::Variant(v) => v.1,
            Item::Field(f) => f.0 .1.union(f.1 .1),
            Item::Function(function_item) => function_item.name.1.union(
                function_item
                    .sig
                    .get()
                    .unwrap()
                    .1
                    .union(function_item.body.1),
            ),
            Item::Extern { name, sig } => name.1.union(sig.1),
            Item::Dummy(_) => panic!(),
        }
    }
}

impl Named for Item {
    fn get_name(&self) -> Option<Spanned<Intern<Expr>>> {
        match self {
            Item::Root => todo!(),
            // Item::Filename(s) => s,
            Item::Package(PackageEntry { name, .. }) => name.name().ok(),
            Item::Struct(StructEntry { ty, .. }) => ty.name().ok(),
            Item::Enum(EnumEntry { ty, .. }) => ty.name().ok(),
            // Item::Variant((EnumVariant { name, .. }, _)) => &name.0,
            Item::Field((name, ..)) => name.name().ok(),
            Item::Function(FunctionItem { name, .. }) => name.name().ok(),
            Item::Extern { name, .. } => name.name().ok(),
            _ => panic!(),
        }
    }
}

impl Item {
    #[must_use]
    /// Get the signature of functions
    pub fn get_sig(&self) -> Option<Spanned<Intern<Ty>>> {
        match self {
            Item::Function(FunctionItem { sig, .. }) => sig.get(),
            Item::Extern { sig, .. } => Some(*sig),
            _ => None,
        }
    }

    pub fn is_checked(&self) -> bool {
        match self {
            Item::Function(function_item) => function_item.checked.get(),
            _ => true,
        }
    }

    #[must_use]
    /// Get the type of the `Item`.
    pub fn get_ty<'t>(&'t self) -> Option<Spanned<Intern<Ty>>> {
        match self {
            Self::Function(FunctionItem { sig, .. }) => sig.get(),
            Self::Struct(StructEntry { ty, .. }) => Some(*ty),
            Self::Enum(EnumEntry { ty, .. }) => Some(*ty),
            Self::Variant((v, s)) => Some((Intern::from(Ty::Variant(*v)), *s)),
            Self::Field((_, ty)) => Some(*ty),
            Self::Package(p) => Some((Intern::from(Ty::Package(p.name)), p.name.1)),
            _ => None,
        }
    }
}

impl Default for Item {
    fn default() -> Self {
        Self::Dummy("")
    }
}
