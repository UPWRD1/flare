use std::{cell::OnceCell, path::Path};

use super::{
    ast::Expr,
    types::{EnumVariant, Ty},
    Spanned,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PackageEntry {
    pub name: Spanned<Expr>,

    pub file: &'static Path,
    //    pub deps: Vec<Spanned<Expr>>,

    //contains: Vec<Index>, // Consider using pure index-based referencing instead of the Trie
    pub src: &'static str,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct StructEntry {
    //parent: Quantifier,
    pub ty: Spanned<Ty>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct EnumEntry {
    //parent: Quantifier,
    //pub name: String,
    pub ty: Spanned<Ty>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
// #[derive(Debug, PartialEq, Eq)]
pub enum Item {
    Root,
    Filename(&'static str),
    Package(PackageEntry),
    Struct(StructEntry),
    Enum(EnumEntry),
    Variant(Spanned<EnumVariant>),

    Field((Spanned<Expr>, Spanned<Ty>)),
    Let {
        name: Spanned<Expr>,
        sig: &'static OnceCell<Spanned<Ty>>,
        body: Spanned<Expr>,
    },
    Extern {
        name: Spanned<Expr>,
        sig: Spanned<Ty>,
    },
    Dummy(&'static str),
}

impl Item {
    #[must_use]
    pub fn get_sig(&self) -> Option<&Spanned<Ty>> {
        match self {
            Item::Let { sig, .. } => sig.get(), //Some(sig.as_ref().unwrap()),
            Item::Extern { sig, .. } => Some(sig),
            _ => None,
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Item::Root => todo!(),
            Item::Filename(s) => s,
            Item::Package(PackageEntry { name, .. }) => name.0.get_ident(name.1).unwrap(),
            Item::Struct(StructEntry { ty, .. }) => ty.0.get_user_name().unwrap(),
            Item::Enum(EnumEntry { ty, .. }) => ty.0.get_user_name().unwrap(),
            Item::Variant((EnumVariant { name, .. }, _)) => name.0.get_ident(name.1).unwrap(),
            Item::Field((name, ..)) => name.0.get_ident(name.1).unwrap(),
            Item::Let { name, .. } => name.0.get_ident(name.1).unwrap(),
            Item::Extern { name, .. } => name.0.get_ident(name.1).unwrap(),
            _ => panic!(),
        }
    }

    #[must_use]
    pub fn get_file(&self) -> Option<&Path> {
        match self {
            Item::Package(PackageEntry { file, .. }) => Some(file),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_ty(&self) -> Option<&Spanned<Ty>> {
        match self {
            Self::Let { sig, .. } => sig.get(),
            Self::Struct(StructEntry { ty, .. }) => Some(ty),
            Self::Enum(EnumEntry { ty, .. }) => Some(ty),
            Self::Variant(v) => Some(Box::leak(Box::new((Ty::Variant(v.0), v.1)))),
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

// impl fmt::Display for Item {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Item::Package(PackageEntry { name, .. }) => {
//                 write!(f, "{}", name.0.get_ident().unwrap())
//             }
//             Item::Struct(StructEntry { name, .. }) => write!(
//                 f,
//                 "{}",
//                 name.0.get_ident().unwrap(),

//             ),

//             Item::Enum(v) => {
//                 write!(f, "{v:?}")
//             }
//             Item::Let { name, sig, .. } => {
//                 if let Some(sig) = sig.get() {
//                     write!(f, "{}: {:?}", name.0.get_ident().unwrap(), sig)
//                 } else {
//                     write!(f, "{}: ?", name.0.get_ident().unwrap())
//                 }
//             }
//             Item::Variant(v) => write!(f, "{:?}", v),
//             Item::Field(t) => write!(f, "{:?}", t),

//             Item::Filename(n) => write!(f, "File {n}"),
//             Item::Root => write!(f, "Root"),
//             Item::Extern { name, sig, .. } => {
//                 write!(f, "extern {}: {:?}", name.0.get_ident().unwrap(), sig)
//             }
//         }
//     }
// }
