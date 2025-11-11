use std::fmt;

use crate::resource::rep::common::{Ident, Named};

// use serde::{Deserialize, Serialize};
// use super::deserialize_slice
use super::{ast::Expr, Spanned};
use internment::Intern;
/// Represents a primitive type within `Ty`
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum PrimitiveType {
    Num,
    Str,
    Bool,
    Unit,
}

/// Represents a type in the parser and master environment.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Ty {
    Primitive(PrimitiveType),
    // #[serde(deserialize_with = "deserialize_slice")]
    User(Spanned<Intern<Expr>>, Intern<Vec<Spanned<Intern<Self>>>>),
    Tuple(Intern<Vec<Spanned<Intern<Self>>>>),
    Seq(Spanned<Intern<Self>>),
    Arrow(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Myself,
    Generic(Spanned<Intern<Expr>>),
    Variant(EnumVariant),
    Package(Spanned<Intern<Expr>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
// #[serde(into = "EnumVariantHelper")]
pub struct EnumVariant {
    pub parent_name: Option<Spanned<Intern<Expr>>>,
    // pub name: Spanned<Expr>,
    pub name: Spanned<Intern<Expr>>,
    pub types: Intern<Vec<Spanned<Intern<Ty>>>>,
}

impl Named for Spanned<Intern<Ty>> {
    fn get_name(&self) -> Option<Spanned<Intern<Expr>>> {
        match *self.0 {
            Ty::User(name, _) => name.name().ok(),
            Ty::Variant(v) => v.name.name().ok(),
            _ => None,
        }
    }
}

impl Ty {
    pub fn get_arrow(&self) -> (&Spanned<Intern<Self>>, &Spanned<Intern<Self>>) {
        if let Self::Arrow(l, r) = self {
            (l, r)
        } else {
            panic!()
        }
    }

    pub fn get_variant(&self) -> Option<EnumVariant> {
        match self {
            Self::Variant(v) => Some(*v),
            _ => None,
        }
    }

    pub fn monomorph_user(self, g: Intern<Vec<Spanned<Intern<Ty>>>>) -> Self {
        match self {
            Self::User(name, _) => Self::User(name, g),
            _ => unreachable!("Cannot monomorph non-generic type"),
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ty::Primitive(p) => match p {
                crate::resource::rep::types::PrimitiveType::Num => write!(f, "num"),
                crate::resource::rep::types::PrimitiveType::Bool => write!(f, "bool"),
                crate::resource::rep::types::PrimitiveType::Str => write!(f, "str"),
                crate::resource::rep::types::PrimitiveType::Unit => write!(f, "unit"),
            },

            Ty::Tuple(t) => {
                write!(f, "{{")?;
                for i in t.iter() {
                    write!(f, "{}, ", i.0)?;
                }
                write!(f, "}}")
            }
            Ty::Seq(t) => {
                write!(f, "Seq {{{}}}", t.0)
            }

            Ty::Arrow(l, r) => write!(f, "({} -> {})", l.0, r.0),
            Ty::Generic(n) => write!(f, "Generic({})", n.ident().unwrap()),
            Ty::User(n, args) => {
                write!(f, "{}[", n.ident().unwrap())?;
                for a in args.iter() {
                    write!(f, "{}, ", a.0)?;
                }
                write!(f, "]")
            }
            Ty::Variant(t) => {
                write!(f, "{t:?}")
            }
            Ty::Package(n) => {
                write!(f, "Module {n:?}")
            }
            Ty::Myself => {
                write!(f, "self")
            }
        }
    }
}
