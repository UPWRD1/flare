use std::fmt;

use serde::{Deserialize, Deserializer, Serialize};
// use super::deserialize_slice
use super::{ast::Expr, Spanned};
use internment::Intern;
/// Represents a primitive type within `Ty`
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Serialize, Deserialize)]
pub enum PrimitiveType {
    Num,
    Str,
    Bool,
    Unit,
}

/// Represents a type in the parser and master environment.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Serialize)]
#[serde(into = "TyHelper")]
pub enum Ty {
    Primitive(PrimitiveType),
    // #[serde(deserialize_with = "deserialize_slice")]
    User(Spanned<Intern<String>>, &'static [Spanned<Self>]),
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
#[serde(into = "EnumVariantHelper")]
pub struct EnumVariant {
    pub parent_name: Option<Intern<String>>,
    // pub name: Spanned<Expr>,
    pub name: Spanned<Intern<String>>,
    pub types: &'static [Spanned<Ty>],
}

#[derive(Deserialize, Serialize)]
#[serde(rename = "EnumVariant")]
struct EnumVariantHelper {
    pub parent_name: Option<String>,
    pub name: Spanned<String>,
    pub types: Vec<Spanned<Ty>>,
}

impl From<EnumVariant> for EnumVariantHelper {
    fn from(variant: EnumVariant) -> Self {
        EnumVariantHelper {
            parent_name: variant.parent_name.map(|x| x.to_string()),
            name: (variant.name.0.to_string(), variant.name.1),
            types: variant.types.to_vec(),
        }
    }
}

impl From<EnumVariantHelper> for EnumVariant {
    fn from(helper: EnumVariantHelper) -> Self {
        EnumVariant {
            parent_name: helper.parent_name.map(|x| x.into()),
            name: (helper.name.0.into(), helper.name.1),
            types: Box::leak(helper.types.into_boxed_slice()),
        }
    }
}

impl<'de> Deserialize<'de> for EnumVariant {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        EnumVariantHelper::deserialize(deserializer).map(Into::into)
    }
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

    pub fn monomorph_user(self, g: &'static [Spanned<Ty>]) -> Self {
        match self {
            Self::User(name, _) => Self::User(name, g),
            _ => unreachable!("Cannot monomorph non-generic type"),
        }
    }
}
#[derive(Deserialize, Serialize)]
#[serde(rename = "Ty")]
enum TyHelper {
    Primitive(PrimitiveType),
    User(Spanned<Intern<String>>, Vec<Spanned<Ty>>),
    Tuple(Vec<Spanned<Ty>>),
    Seq(Box<Spanned<Ty>>),
    Arrow(Box<Spanned<Ty>>, Box<Spanned<Ty>>),
    Myself,
    Generic(Spanned<Expr>),
    Variant(EnumVariant),
    Module(Spanned<Expr>),
}

impl From<Ty> for TyHelper {
    fn from(ty: Ty) -> Self {
        match ty {
            Ty::Primitive(p) => TyHelper::Primitive(p),
            Ty::User(name, generics) => TyHelper::User(name, generics.to_vec()),
            Ty::Tuple(types) => TyHelper::Tuple(types.to_vec()),
            Ty::Seq(ty) => TyHelper::Seq(Box::new(*ty)),
            Ty::Arrow(from, to) => TyHelper::Arrow(Box::new(*from), Box::new(*to)),
            Ty::Myself => TyHelper::Myself,
            Ty::Generic(expr) => TyHelper::Generic(expr),
            Ty::Variant(variant) => TyHelper::Variant(variant),
            Ty::Module(expr) => TyHelper::Module(expr),
        }
    }
}

impl From<TyHelper> for Ty {
    fn from(helper: TyHelper) -> Self {
        match helper {
            TyHelper::Primitive(p) => Ty::Primitive(p),
            TyHelper::User(name, generics) => {
                Ty::User(name, Box::leak(generics.into_boxed_slice()))
            }
            TyHelper::Tuple(types) => Ty::Tuple(Box::leak(types.into_boxed_slice())),
            TyHelper::Seq(ty) => Ty::Seq(Box::leak(ty)),
            TyHelper::Arrow(from, to) => Ty::Arrow(Box::leak(from), Box::leak(to)),
            TyHelper::Myself => Ty::Myself,
            TyHelper::Generic(expr) => Ty::Generic(expr),
            TyHelper::Variant(variant) => Ty::Variant(variant),
            TyHelper::Module(expr) => Ty::Module(expr),
        }
    }
}

impl<'de> Deserialize<'de> for Ty {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        TyHelper::deserialize(deserializer).map(Into::into)
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
