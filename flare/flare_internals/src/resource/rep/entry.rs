use std::cell::OnceCell;

use internment::Intern;
// use lasso::Spur;
use serde::{Deserialize, Serialize};

use crate::resource::rep::files::FileID;

use super::{
    ast::Expr,
    types::{EnumVariant, Ty},
    Spanned,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub struct PackageEntry {
    pub name: Spanned<Expr>,
    pub id: FileID,
    // pub file: &'static Path,
    //    pub deps: Vec<Spanned<Expr>>,

    //contains: Vec<Index>, // Consider using pure index-based referencing instead of the Trie
    // pub src: &'static str,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub struct StructEntry {
    //parent: Quantifier,
    pub ty: Spanned<Ty>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub struct EnumEntry {
    //parent: Quantifier,
    //pub name: String,
    pub ty: Spanned<Ty>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
#[serde(from = "FunctionItemRepr")]
#[serde(into = "FunctionItemRepr")]
pub struct FunctionItem {
    pub name: Spanned<Expr>,
    pub sig: &'static OnceCell<Spanned<Ty>>,
    pub body: Spanned<Expr>,
}

#[derive(Serialize, Deserialize)]
struct FunctionItemRepr(Spanned<Expr>, Option<Spanned<Ty>>, Spanned<Expr>);

impl From<FunctionItemRepr> for FunctionItem {
    fn from(value: FunctionItemRepr) -> Self {
        let cell = OnceCell::new();
        if let Some(v) = value.1 {
            let _ = cell.set(v);
        } else {
            //do nothing
        };
        let cell = Box::leak(Box::new(cell));
        Self {
            name: value.0,
            sig: cell,
            body: value.2,
        }
    }
}

/// Converts a function item into a Serde-friendly representation
impl From<FunctionItem> for FunctionItemRepr {
    fn from(value: FunctionItem) -> Self {
        Self(value.name, value.sig.get().cloned(), value.body)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
// #[derive(Debug, PartialEq, Eq)]
// #[serde(bound(deserialize = "'static: 'de"))]
pub enum Item {
    Root,
    Filename(Intern<String>),
    Package(PackageEntry),
    Struct(StructEntry),
    Enum(EnumEntry),
    Variant(Spanned<EnumVariant>),

    Field((Spanned<Expr>, Spanned<Ty>)),
    Function(FunctionItem),
    // name: Spanned<Expr>,
    // #[serde(borrow = "'static")]
    // #[serde(from = "Option<Spanned<Ty>>")]
    // #[serde(deserialize_with = "OnceCell::from")]
    // #[serde(serialize_with = "OnceCell::get")]
    // sig: &'static OnceCell<Spanned<Ty>>,
    // body: Spanned<Expr>,
    Extern {
        name: Spanned<Expr>,
        sig: Spanned<Ty>,
    },
    #[serde(skip)]
    Dummy(&'static str),
}

impl Item {
    #[must_use]
    pub fn get_sig(&self) -> Option<&Spanned<Ty>> {
        match self {
            Item::Function(FunctionItem { sig, .. }) => sig.get(), //Some(sig.as_ref().unwrap()),
            Item::Extern { sig, .. } => Some(sig),
            _ => None,
        }
    }

    pub fn name(&self) -> &Intern<String> {
        match self {
            Item::Root => todo!(),
            // Item::Filename(s) => s,
            Item::Package(PackageEntry { name, .. }) => name.0.get_ident(name.1).unwrap(),
            Item::Struct(StructEntry { ty, .. }) => ty.0.get_user_name().as_ref().unwrap(),
            Item::Enum(EnumEntry { ty, .. }) => ty.0.get_user_name().as_ref().unwrap(),
            // Item::Variant((EnumVariant { name, .. }, _)) => &name.0,
            Item::Field((name, ..)) => name.0.get_ident(name.1).as_ref().unwrap(),
            Item::Function(FunctionItem { name, .. }) => name.0.get_ident(name.1).as_ref().unwrap(),
            Item::Extern { name, .. } => name.0.get_ident(name.1).as_ref().unwrap(),
            _ => panic!(),
        }
    }

    pub fn get_raw_name(&self) -> Option<Spanned<&Intern<String>>> {
        match self {
            Item::Root => todo!(),
            Item::Package(PackageEntry { name, .. }) => {
                Some((name.0.get_ident(name.1).ok()?, name.1))
            }
            Item::Struct(StructEntry { ty }) => Some((ty.0.get_user_name()?, ty.1)),
            Item::Enum(EnumEntry { ty }) => todo!(),
            Item::Variant(_) => todo!(),
            Item::Field(_) => todo!(),
            Item::Function(function_item) => todo!(),
            Item::Extern { name, sig } => todo!(),
            _ => panic!(),
        }
    }

    // #[must_use]
    // pub fn get_file(&self) -> Option<&Path> {
    //     match self {
    //         Item::Package(PackageEntry { file, .. }) => Some(file),
    //         _ => None,
    //     }
    // }

    #[must_use]
    pub fn get_ty(self) -> Option<Spanned<Ty>> {
        match self {
            Self::Function(FunctionItem { sig, .. }) => sig.get().copied(),
            Self::Struct(StructEntry { ty, .. }) => Some(ty),
            Self::Enum(EnumEntry { ty, .. }) => Some(ty),
            Self::Variant((v, s)) => Some((Ty::Variant(v), s)),
            Self::Field((_, ty)) => Some(ty),
            _ => None,
        }
    }

    pub fn get_ty_ref(&self) -> Option<&Spanned<Ty>> {
        match &self {
            Self::Function(FunctionItem { sig, .. }) => sig.get(),
            Self::Struct(StructEntry { ty, .. }) => Some(ty),
            Self::Enum(EnumEntry { ty, .. }) => Some(ty),
            Self::Variant((v, s)) => Some(Box::leak(Box::new((Ty::Variant(*v), *s)))),
            Self::Field((_, ty)) => Some(ty),
            _ => None,
        }
    }
}

impl Default for Item {
    fn default() -> Self {
        Self::Dummy("")
    }
}
