use chumsky::span::{SimpleSpan, Span};
use internment::Intern;
// use lasso::Spur;

use crate::{
    passes::frontend::typing::Type,
    resource::{
        errors::CompResult,
        rep::{
            common::{HasSpan, Ident, SpanWrapped, Spanned},
            frontend::{
                ast::{Syntax, Variable},
                cst::CstExpr,
                csttypes::CstType,
                files::FileID,
            },
        },
    },
};

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
    Filename(Intern<String>),
    Package(PackageEntry<S>),
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

impl<S: Syntax> SpanWrapped for Item<S> {
    // TODO: Remove panicking code
    fn get_span(&self) -> chumsky::prelude::SimpleSpan<usize, u64>
    where
        Self: SpanWrapped,
    {
        match &self.kind {
            ItemKind::Root => SimpleSpan::new(0, 0..0),
            ItemKind::Filename(_intern) => panic!(),
            ItemKind::Package(package_entry) => package_entry.name.ident().unwrap().1,
            ItemKind::Function(function_item) => {
                function_item.name.ident().unwrap().1.union(
                    function_item
                        .sig
                        // .get()
                        // .unwrap()
                        .span()
                        .union(function_item.body.span()),
                )
            }
            ItemKind::Type(_, _, t) => t.span(),
            ItemKind::Extern { name, args: _, sig } => name.ident().unwrap().1.union(sig.span()),

            ItemKind::Dummy(_) => panic!(),
        }
    }
}

impl<S: Syntax> Ident for Item<S> {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        match self.kind {
            ItemKind::Root => {
                panic!()
                // let s = SimpleSpan::new(0, 0..0);
                // Spanned(Intern::from_ref("ROOT"), s)
            }
            // Item::Filename(s) => s,
            ItemKind::Package(PackageEntry { name, .. }) => name,
            // ItemKind::Struct(StructEntry { ty, .. }) => ty.ident(),
            // ItemKind::Enum(EnumEntry { ty, .. }) => ty.ident(),
            // Item::Variant((EnumVariant { name, .. }, _)) => &name.0,
            // ItemKind::Field((name, ..)) => Ok(name),
            ItemKind::Function(FunctionItem { name, .. }) => name,
            ItemKind::Extern { name, .. } => name,
            ItemKind::Type(name, _, _) => name,
            _ => panic!(),
        }
        .ident()
    }
}

impl<S: Syntax> Item<S> {
    pub fn new(kind: ItemKind<S>) -> Self {
        Self { kind }
    }

    #[must_use]
    /// Get the signature of functions
    pub fn get_sig(&self) -> Option<S::Type> {
        match &self.kind {
            ItemKind::Function(FunctionItem { sig, .. }) => Some(*sig),
            ItemKind::Extern { sig, .. } => Some(*sig),
            _ => None,
        }
    }

    // Get the type of the `Item`.
    // pub fn get_ty(&self) -> CompResult<Spanned<Intern<Type>>> {
    //     fn err<V: Variable>(t: &Item<V>) -> CompilerErr {
    //         DynamicErr::new(format!("Could not get the type from {:?}", t)).into()
    //     }
    //     match &self.kind {
    //         // ItemKind::Function(FunctionItem { sig, .. }) => Ok(*sig),
    //         // ItemKind::Struct(StructEntry { ty, .. }) => Ok(*ty),
    //         // ItemKind::Enum(EnumEntry { ty, .. }) => Ok(*ty),
    //         // ItemKind::Variant(Spanned(v, s)) => Ok(Spanned(Intern::from(Ty::Variant(*v)), *s)),
    //         // ItemKind::Field((_, ty)) => Ok(*ty),
    //         // ItemKind::Package(p) => Ok(Spanned(Intern::from(Type::Package(p.name)), p.name.1)),
    //         ItemKind::Type(_, _, t) => Ok(*t),
    //         _ => Err(err(self)),
    //     }
    // }

    // pub fn get_type_universal(&self) -> CompResult<Spanned<Intern<Type>>> {
    //     fn err<V: Variable>(t: &Item<V>) -> CompilerErr {
    //         DynamicErr::new(format!("Could not get the type from {:?}", t)).into()
    //     }
    //     match &self.kind {
    //         ItemKind::Function(FunctionItem { sig, .. }) => Ok(*sig),
    //         ItemKind::Package(p) => Ok(Spanned(Intern::from(Type::Package(p.name)), p.name.1)),
    //         ItemKind::Type(_, _, t) => Ok(*t),
    //         ItemKind::Extern {
    //             name: _,
    //             args: _,
    //             sig,
    //         } => Ok(*sig),
    //         _ => Err(err(self)),
    //     }
    // }
}

impl<S: Syntax> Default for Item<S> {
    fn default() -> Self {
        Self {
            kind: ItemKind::Dummy(""),
        }
    }
}
