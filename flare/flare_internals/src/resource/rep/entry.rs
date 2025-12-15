use chumsky::span::{SimpleSpan, Span};
use internment::Intern;
// use lasso::Spur;

use crate::{
    passes::midend::typing::Type,
    resource::{
        errors::{CompResult, CompilerErr, DynamicErr},
        rep::{
            ast::{Label, Variable},
            common::{Ident, SpanWrapped},
            files::FileID,
        },
    },
};

use super::{
    // concretetypes::{EnumVariant, Ty},
    Spanned,
    ast::Expr,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
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
    pub sig: Spanned<Intern<Type>>,
    pub body: Spanned<Intern<Expr<V>>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[repr(transparent)]
pub struct Item<V: Variable> {
    pub kind: ItemKind<V>,
    // pub is_checked: Cell<bool>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ItemKind<V: Variable> {
    Root,
    Filename(Intern<String>),
    Package(PackageEntry),
    Function(FunctionItem<V>),
    Type(
        Spanned<Intern<String>>,
        Spanned<Intern<Type>>,
        // SimpleSpan<usize, u64>,
    ),
    Extern {
        name: Spanned<Intern<String>>,
        // args: &'static [V],
        sig: Spanned<Intern<Type>>,
    },
    Field {
        name: Label,
        value: Spanned<Intern<Type>>,
    },
    Dummy(&'static str),
}

impl<V: Variable> SpanWrapped for Item<V> {
    // TODO: Remove panicking code
    fn get_span(&self) -> chumsky::prelude::SimpleSpan<usize, u64>
    where
        Self: SpanWrapped,
    {
        match &self.kind {
            ItemKind::Root => SimpleSpan::new(0, 0..0),
            ItemKind::Filename(_intern) => panic!(),
            ItemKind::Package(package_entry) => package_entry.name.1,
            ItemKind::Function(function_item) => {
                function_item.name.ident().unwrap().1.union(
                    function_item
                        .sig
                        // .get()
                        // .unwrap()
                        .1
                        .union(function_item.body.1),
                )
            }
            ItemKind::Type(_, t) => t.1,
            ItemKind::Extern { name, sig } => name.1.union(sig.1),
            ItemKind::Field { name, value: _ } => name.0.1,

            ItemKind::Dummy(_) => panic!(),
        }
    }
}

impl<V: Variable> Ident for Item<V> {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        match self.kind {
            ItemKind::Root => {
                let s = SimpleSpan::new(0, 0..0);
                Ok(Spanned(Intern::from_ref("ROOT"), s))
            }
            // Item::Filename(s) => s,
            ItemKind::Package(PackageEntry { name, .. }) => Ok(name),
            // ItemKind::Struct(StructEntry { ty, .. }) => ty.ident(),
            // ItemKind::Enum(EnumEntry { ty, .. }) => ty.ident(),
            // Item::Variant((EnumVariant { name, .. }, _)) => &name.0,
            // ItemKind::Field((name, ..)) => Ok(name),
            ItemKind::Function(FunctionItem { name, .. }) => Ok(name.ident()?),
            ItemKind::Extern { name, .. } => Ok(name),
            ItemKind::Type(name, _) => Ok(name),
            ItemKind::Field { name, value: _ } => Ok(name.0),
            _ => panic!(),
        }
    }
}

impl<V: Variable> Item<V> {
    pub fn new(kind: ItemKind<V>) -> Self {
        Self { kind }
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

    /// Get the type of the `Item`.
    pub fn get_ty(&self) -> CompResult<Spanned<Intern<Type>>> {
        fn err<V: Variable>(t: &Item<V>) -> CompilerErr {
            DynamicErr::new(format!("Could not get the type from {:?}", t)).into()
        }
        match &self.kind {
            // ItemKind::Function(FunctionItem { sig, .. }) => Ok(*sig),
            // ItemKind::Struct(StructEntry { ty, .. }) => Ok(*ty),
            // ItemKind::Enum(EnumEntry { ty, .. }) => Ok(*ty),
            // ItemKind::Variant(Spanned(v, s)) => Ok(Spanned(Intern::from(Ty::Variant(*v)), *s)),
            // ItemKind::Field((_, ty)) => Ok(*ty),
            ItemKind::Package(p) => Ok(Spanned(Intern::from(Type::Package(p.name)), p.name.1)),
            ItemKind::Field { name: _, value } => Ok(*value),
            ItemKind::Type(_, t) => Ok(*t),
            _ => Err(err(self)),
        }
    }

    pub fn get_type_universal(&self) -> CompResult<Spanned<Intern<Type>>> {
        fn err<V: Variable>(t: &Item<V>) -> CompilerErr {
            DynamicErr::new(format!("Could not get the type from {:?}", t)).into()
        }
        match &self.kind {
            ItemKind::Function(FunctionItem { sig, .. }) => Ok(*sig),
            ItemKind::Package(p) => Ok(Spanned(Intern::from(Type::Package(p.name)), p.name.1)),
            ItemKind::Field { name: _, value } => Ok(*value),
            ItemKind::Type(_, t) => Ok(*t),
            ItemKind::Extern {
                name: _,
                // args: _,
                sig,
            } => Ok(*sig),
            _ => Err(err(self)),
        }
    }
}

impl<V: Variable> Default for Item<V> {
    fn default() -> Self {
        Self {
            kind: ItemKind::Dummy(""),
        }
    }
}
