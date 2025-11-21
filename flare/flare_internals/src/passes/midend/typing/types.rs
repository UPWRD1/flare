use std::{fmt, hash::Hash};

use ena::unify::{EqUnifyValue, UnifyKey};
use internment::Intern;

use crate::{passes::midend::typing::rows::Row, resource::{
    errors::CompResult,
    rep::{Spanned, ast::Label, common::Ident},
}};
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TyUniVar(pub u32);

impl fmt::Display for TyUniVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "tv{}", self.0)
    }
}

impl UnifyKey for TyUniVar {
    type Value = Option<Type>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn tag() -> &'static str {
        ""
    }
}

#[derive(Copy, Clone, Debug, Eq)]
pub enum Type {
    Unknown,
    Unifier(TyUniVar),
    User(Spanned<Intern<String>>, &'static [TyUniVar]),
    Variant(TyUniVar, Spanned<Intern<String>>),
    Unit,
    Num,
    Bool,
    String,
    Func(Intern<Self>, Intern<Self>),
    Tuple(&'static [TyUniVar]),
    Seq(&'static TyUniVar),
    Generic(Spanned<Intern<String>>),
    Package(Spanned<Intern<String>>),
    Prod(Row),
    Sum(Row),
    Label(Label, Intern<Self>),
}

impl EqUnifyValue for Type {}

impl Ident for Type {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        match self {
            Self::User(spanned, _) | Self::Variant(_, spanned) => Ok(*spanned),
            Self::Generic(spanned) => Ok(*spanned),
            Self::Package(spanned) => Ok(*spanned),
            _ => panic!(),
        }
    }
}

impl Type {
    #[must_use]
    pub const fn get_user_name(&self) -> Option<Spanned<Intern<String>>> {
        match self {
            Self::User(n, _) | Self::Generic(n) => Some(*n),
            _ => None,
        }
    }
    #[must_use]
    pub fn get_tuple_index(&self, idx: usize) -> Option<&TyUniVar> {
        match self {
            Self::Tuple(v) => v.get(idx),
            _ => None,
        }
    }

    pub fn occurs_check(&self, var: TyUniVar) -> Result<(), Self> {
        match self {
            Self::Num | Self::String | Self::Bool | Self::Unit => Ok(()),
            Self::Unifier(v) => {
                if *v == var {
                    Err(Self::Unifier(*v))
                } else {
                    Ok(())
                }
            }
            Self::Func(arg, ret) => {
                arg.occurs_check(var).map_err(|_| *self)?;
                ret.occurs_check(var).map_err(|_| *self)
            }

            _ => todo!(),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Self::Generic(_)) => true,

            (Self::Unifier(l0), Self::Unifier(r0)) => l0 == r0,
            (Self::User(l0, l1), Self::User(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Variant(l0, l1), Self::Variant(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Func(l0, l1), Self::Func(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Tuple(l), Self::Tuple(r)) => l == r,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Unifier(r) => r.hash(state),
            Self::User(n, v) => {
                n.hash(state);
                v.hash(state);
            }
            Self::Variant(p, n) => {
                p.hash(state);
                n.hash(state);
            }
            Self::Func(l, r) => {
                l.hash(state);
                r.hash(state);
            }
            Self::Tuple(l) => {
                l.hash(state);
            }
            _ => core::mem::discriminant(self).hash(state),
        }
    }
}
