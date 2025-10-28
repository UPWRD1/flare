use std::{cell::OnceCell, path::PathBuf};

use super::{
    ast::Expr,
    types::{EnumVariant, Ty},
    Spanned,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PackageEntry {
    pub name: Spanned<Expr>,

    pub file: PathBuf,
    //    pub deps: Vec<Spanned<Expr>>,

    //contains: Vec<Index>, // Consider using pure index-based referencing instead of the Trie
    pub src: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructEntry {
    //parent: Quantifier,
    pub ty: Spanned<Ty>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EnumEntry {
    //parent: Quantifier,
    //pub name: String,
    pub ty: Spanned<Ty>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Item {
    Root,
    Filename(String),
    Package(PackageEntry),
    Struct(StructEntry),
    Enum(EnumEntry),
    Variant(Spanned<EnumVariant>),

    Field((Spanned<Expr>, Spanned<Ty>)),
    Let {
        name: Spanned<Expr>,
        sig: OnceCell<Spanned<Ty>>,
        body: Spanned<Expr>,
    },
    Extern {
        name: Spanned<Expr>,
        sig: Spanned<Ty>,
    },
    Dummy(String),
}

impl<'expr> Item {
    #[must_use]
    pub fn get_sig(&self) -> Option<&Spanned<Ty>> {
        match self {
            Item::Let { sig, .. } => sig.get(),
            Item::Extern { sig, .. } => Some(sig),
            _ => None,
        }
    }

    pub fn name(&self) -> String {
        match self {
            Item::Root => todo!(),
            Item::Filename(s) => s.clone(),
            Item::Package(PackageEntry { name, .. }) => name.0.get_ident().unwrap().clone(),
            Item::Struct(StructEntry { ty, .. }) => ty.0.get_user_name().unwrap().clone(),
            Item::Enum(EnumEntry { ty, .. }) => ty.0.get_user_name().unwrap().clone(),
            Item::Variant((EnumVariant { name, .. }, _)) => name.0.get_ident().unwrap().clone(),
            Item::Field((name, ..)) => name.0.get_ident().unwrap().clone(),
            Item::Let { name, .. } => name.0.get_ident().unwrap().clone(),
            Item::Extern { name, .. } => name.0.get_ident().unwrap().clone(),
            _ => panic!(),
        }
    }

    #[must_use]
    pub fn get_file(&self) -> Option<&PathBuf> {
        match self {
            Item::Package(PackageEntry { file, .. }) => Some(file),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_ty(&self) -> Option<Spanned<Ty>> {
        match self {
            Self::Let { sig, .. } => Some(sig.get().unwrap().clone()),
            Self::Struct(StructEntry { ty, .. }) => Some(ty.clone()),
            Self::Enum(EnumEntry { ty, .. }) => Some(ty.clone()),
            Self::Variant(v) => Some((Ty::Variant(v.0.clone()), v.1)),
            Self::Field((_, ty)) => Some(ty.clone()),
            _ => None,
        }
    }
}

impl Default for Item {
    fn default() -> Self {
        Self::Dummy("".to_string())
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
