use super::{ast::Expr, Spanned};
use std::fmt;

/// Represents a primitive type within `Ty`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Num,
    Str,
    Bool,
    Unit,
}

/// Represents a type in the parser and master environment.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    Primitive(PrimitiveType),
    User(Spanned<Expr>, Vec<Spanned<Self>>),
    Tuple(Vec<Spanned<Self>>),
    Arrow(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Generic(Spanned<Expr>),
    Variant(EnumVariant),
    Module(Spanned<Expr>),
}

// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pub enum EnumVariant {
//     Simple(Spanned<Expr>),
//     Associated(Spanned<Expr>, Vec<Spanned<Ty>>),
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    pub name: Spanned<Expr>,
    pub types: Vec<Spanned<Ty>>,
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

    pub fn get_user_name(&self) -> Option<String> {
        match self {
            Self::User(name, _) => Some(name.0.get_ident()?),
            Self::Variant(v) => Some(v.name.0.get_ident()?),
            _ => None,
        }
    }

    pub fn get_raw_expr_name(&self) -> Option<Spanned<Expr>> {
        match self {
            Self::User(name, _) => Some(name.clone()),
            Self::Variant(v) => Some(v.name.clone()),
            _ => None,
        }
    }

    pub fn get_raw_name(&self) -> String {
        format!("{self}")
    }

    pub fn monomorph_user(self, g: &[Spanned<Ty>]) -> Self {
        match self {
            Self::User(name, _) => Self::User(name, g.to_vec()),
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
                for i in t {
                    write!(f, "{}, ", i.0)?;
                }
                write!(f, "}}")
            }

            Ty::Arrow(l, r) => write!(f, "({} -> {})", l.0, r.0),
            Ty::Generic(n) => write!(f, "Generic({})", n.0.get_ident().unwrap_or("?".to_string())),
            Ty::User(n, args) => {
                write!(f, "{}[", n.0.get_ident().unwrap_or("?".to_string()))?;
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
        }
    }
}
