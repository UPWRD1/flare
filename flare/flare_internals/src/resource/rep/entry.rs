use std::cell::Cell;

use chumsky::span::Span;
use internment::Intern;
// use lasso::Spur;

use crate::resource::{
    errors::{CompResult, CompilerErr, FatalErr},
    rep::{
        common::{Named, SpanWrapped},
        files::FileID,
    },
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
        match &self.kind {
            ItemKind::Root => panic!(), // SimpleSpan::new(0, 0..0)
            ItemKind::Filename(_intern) => panic!(),
            ItemKind::Package(package_entry) => package_entry.name.1,
            ItemKind::Struct(struct_entry) => struct_entry.ty.1,
            ItemKind::Enum(enum_entry) => enum_entry.ty.1,
            ItemKind::Variant(v) => v.1,
            ItemKind::Field(f) => f.0 .1.union(f.1 .1),
            ItemKind::Function(function_item) => function_item.name.1.union(
                function_item
                    .sig
                    .get()
                    .unwrap()
                    .1
                    .union(function_item.body.1),
            ),
            ItemKind::Extern { name, sig } => name.1.union(sig.1),
            ItemKind::Dummy(_) => panic!(),
        }
    }
}

impl Named for Item {
    fn get_name(&self) -> Option<Spanned<Intern<Expr>>> {
        match &self.kind {
            ItemKind::Root => todo!(),
            // Item::Filename(s) => s,
            ItemKind::Package(PackageEntry { name, .. }) => name.name().ok(),
            ItemKind::Struct(StructEntry { ty, .. }) => ty.name().ok(),
            ItemKind::Enum(EnumEntry { ty, .. }) => ty.name().ok(),
            // Item::Variant((EnumVariant { name, .. }, _)) => &name.0,
            ItemKind::Field((name, ..)) => name.name().ok(),
            ItemKind::Function(FunctionItem { name, .. }) => name.name().ok(),
            ItemKind::Extern { name, .. } => name.name().ok(),
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
    pub fn get_sig(&self) -> Option<Spanned<Intern<Ty>>> {
        match &self.kind {
            ItemKind::Function(FunctionItem { sig, .. }) => sig.get(),
            ItemKind::Extern { sig, .. } => Some(*sig),
            _ => None,
        }
    }

    pub fn is_checked(&self) -> bool {
        self.is_checked.get()
    }

    /// Get the type of the `Item`.
    pub fn get_ty(&self) -> CompResult<Spanned<Intern<Ty>>> {
        fn err(t: &Item) -> CompilerErr {
            FatalErr::new(format!("Could not get the type from {:?}", t))
        }
        match &self.kind {
            ItemKind::Function(FunctionItem { sig, .. }) => sig.get().ok_or(err(self)),
            ItemKind::Struct(StructEntry { ty, .. }) => Ok(*ty),
            ItemKind::Enum(EnumEntry { ty, .. }) => Ok(*ty),
            ItemKind::Variant(Spanned(v, s)) => Ok(Spanned(Intern::from(Ty::Variant(*v)), *s)),
            ItemKind::Field((_, ty)) => Ok(*ty),
            ItemKind::Package(p) => Ok(Spanned(Intern::from(Ty::Package(p.name)), p.name.1)),
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
