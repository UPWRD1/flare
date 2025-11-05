use serde::{Deserialize, Serialize};

use super::{ast::Expr, Spanned};
use std::fmt;

/// Represents a primitive type within `Ty`
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Serialize)]
pub enum PrimitiveType {
    Num,
    Str,
    Bool,
    Unit,
}

/// Represents a type in the parser and master environment.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Serialize)]
pub enum Ty {
    Primitive(PrimitiveType),
    User(Spanned<Expr>, &'static [Spanned<Self>]),
    Tuple(&'static [Spanned<Self>]),
    Seq(&'static Spanned<Self>),
    Arrow(&'static Spanned<Self>, &'static Spanned<Self>),
    Myself,
    Generic(Spanned<Expr>),
    Variant(EnumVariant),
    Module(Spanned<Expr>),
}

// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pub enum EnumVariant {
//     Simple(Spanned<Expr>),
//     Associated(Spanned<Expr>, Vec<Spanned<Ty>>),
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Serialize)]
pub struct EnumVariant {
    pub parent_name: Option<&'static str>,
    pub name: Spanned<Expr>,
    pub types: &'static [Spanned<Ty>],
}

// impl EnumVariant {
//     pub fn get_name(&self) -> String {
//         match self {
//             Self::Simple(n) => n.0.get_ident().unwrap(),
//             Self::Associated(n, ..) => n.0.get_ident().unwrap(),
//         }
//     }
// }

impl Ty {
    pub fn get_arrow(&self) -> (&Spanned<Self>, &Spanned<Self>) {
        if let Self::Arrow(l, r) = self {
            (l, r)
        } else {
            panic!()
        }
    }

    pub fn get_user_name(&self) -> Option<&'static str> {
        match self {
            Self::User(name, _) => name.0.get_ident(name.1).ok(),
            Self::Variant(v) => v.name.0.get_ident(v.name.1).ok(),
            _ => None,
        }
    }

    pub fn get_raw_expr_name(&self) -> Option<Spanned<Expr>> {
        match self {
            Self::User(name, _) => Some(*name),
            Self::Variant(v) => Some(v.name),
            _ => None,
        }
    }

    pub fn get_raw_name(&self) -> &'static str {
        format!("{self}").leak()
    }

    pub fn get_variant(&self) -> Option<EnumVariant> {
        match self {
            Self::Variant(v) => Some(*v),
            _ => None,
        }
    }

    pub fn monomorph_user(self, g: &'static [Spanned<Ty>]) -> Self {
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
            Ty::Generic(n) => write!(f, "Generic({})", n.0.get_ident(n.1).unwrap_or("?")),
            Ty::User(n, args) => {
                write!(f, "{}[", n.0.get_ident(n.1).unwrap_or("?"))?;
                for a in args.iter() {
                    write!(f, "{}, ", a.0)?;
                }
                write!(f, "]")
            }
            Ty::Variant(t) => {
                write!(f, "{t:?}")
            }
            Ty::Module(n) => {
                write!(f, "Module {n:?}")
            }
            Ty::Myself => {
                write!(f, "self")
            }
        }
    }
}
