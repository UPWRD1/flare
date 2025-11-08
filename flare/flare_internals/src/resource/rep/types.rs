use std::fmt;

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
    User(Spanned<Intern<String>>, Intern<Vec<Spanned<Self>>>),
    Tuple(Intern<Vec<Spanned<Self>>>),
    Seq(Intern<Spanned<Self>>),
    Arrow(Intern<Spanned<Self>>, Intern<Spanned<Self>>),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
// #[serde(into = "EnumVariantHelper")]
pub struct EnumVariant {
    pub parent_name: Option<Spanned<Intern<String>>>,
    // pub name: Spanned<Expr>,
    pub name: Spanned<Intern<String>>,
    pub types: Intern<Vec<Spanned<Ty>>>,
}

impl Ty {
    pub fn get_arrow(&self) -> (&Spanned<Self>, &Spanned<Self>) {
        if let Self::Arrow(l, r) = self {
            (l, r)
        } else {
            panic!()
        }
    }

    pub fn get_user_name(&self) -> Option<&Intern<String>> {
        match self {
            Self::User(name, _) => Some(&name.0),
            Self::Variant(v) => Some(&v.name.0),
            _ => None,
        }
    }

    // pub fn get_raw_name(&self) -> &'src str {
    //     format!("{self}").leak()
    // }

    pub fn get_variant(&self) -> Option<EnumVariant> {
        match self {
            Self::Variant(v) => Some(*v),
            _ => None,
        }
    }

    pub fn monomorph_user(self, g: Intern<Vec<Spanned<Ty>>>) -> Self {
        match self {
            Self::User(name, _) => Self::User(name, g),
            _ => unreachable!("Cannot monomorph non-generic type"),
        }
    }
}
// #[derive(Deserialize, Serialize)]
// #[serde(rename = "Ty")]
// enum TyHelper {
//     Primitive(PrimitiveType),
//     User(Spanned<Intern<String>>, Vec<Spanned<Ty>>),
//     Tuple(Vec<Spanned<Ty>>),
//     Seq(Box<Spanned<Ty>>),
//     Arrow(Box<Spanned<Ty>>, Box<Spanned<Ty>>),
//     Myself,
//     Generic(Spanned<Expr>),
//     Variant(EnumVariant),
//     Module(Spanned<Expr>),
// }

// impl From<Ty> for TyHelper {
//     fn from(ty: Ty) -> Self {
//         match ty {
//             Ty::Primitive(p) => TyHelper::Primitive(p),
//             Ty::User(name, generics) => TyHelper::User(name, generics.to_vec()),
//             Ty::Tuple(types) => TyHelper::Tuple(types.to_vec()),
//             Ty::Seq(ty) => TyHelper::Seq(Box::new(*ty)),
//             Ty::Arrow(from, to) => TyHelper::Arrow(Box::new(*from), Box::new(*to)),
//             Ty::Myself => TyHelper::Myself,
//             Ty::Generic(expr) => TyHelper::Generic(expr),
//             Ty::Variant(variant) => TyHelper::Variant(variant),
//             Ty::Module(expr) => TyHelper::Module(expr),
//         }
//     }
// }

// impl From<TyHelper> for Ty {
//     fn from(helper: TyHelper) -> Self {
//         match helper {
//             TyHelper::Primitive(p) => Ty::Primitive(p),
//             TyHelper::User(name, generics) => Ty::User(name, Intern::from(generics.as_slice())),
//             TyHelper::Tuple(types) => Ty::Tuple(Intern::from(types.as_slice())),
//             TyHelper::Seq(ty) => Ty::Seq(Intern::from(ty)),
//             TyHelper::Arrow(from, to) => Ty::Arrow(Intern::from(from), Intern::from(to)),
//             TyHelper::Myself => Ty::Myself,
//             TyHelper::Generic(expr) => Ty::Generic(expr),
//             TyHelper::Variant(variant) => Ty::Variant(variant),
//             TyHelper::Module(expr) => Ty::Module(expr),
//         }
//     }
// }

// impl<'de> Deserialize<'de> for Ty {
//     fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
//     where
//         D: Deserializer<'de>,
//     {
//         TyHelper::deserialize(deserializer).map(Into::into)
//     }
// }

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
            Ty::Generic(n) => write!(
                f,
                "Generic({})",
                n.0.get_ident(n.1)
                    .unwrap_or(&Intern::<String>::from_ref("?"))
            ),
            Ty::User(n, args) => {
                write!(f, "{}[", n.0)?;
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
