use std::{collections::BTreeSet, fmt, hash::Hash};

use ena::unify::{EqUnifyValue, UnifyKey};
use internment::Intern;

use crate::{
    passes::midend::typing::rows::{Row, RowUniVar, RowVar},
    resource::{
        errors::CompResult,
        rep::{ast::Label, common::Ident, Spanned},
    },
};
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub Intern<String>);

#[derive(Copy, Clone, Debug, Eq)]
pub enum Type {
    Unknown,
    Unifier(TyUniVar),
    Var(TypeVar),
    // User(Spanned<Intern<String>>, &'static [TyUniVar]),
    // Variant(TyUniVar, Spanned<Intern<String>>),
    Unit,
    Num,
    Bool,
    String,
    Func(Intern<Self>, Intern<Self>),
    // Tuple(&'static [TyUniVar]),
    // Seq(&'static TyUniVar),
    // Generic(Spanned<Intern<String>>),
    Package(Spanned<Intern<String>>),

    User(Spanned<Intern<String>>),
    Prod(Row),
    Sum(Row),
    Label(Label, Intern<Self>),
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Type {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            // Same variants - compare fields
            (Self::Num, Self::Num) => std::cmp::Ordering::Equal,
            (Self::Unifier(a), Self::Unifier(b)) => a.cmp(b),
            (Self::Func(a1, a2), Self::Func(b1, b2)) => a1.cmp(b1).then_with(|| a2.cmp(b2)),
            (Self::Prod(a), Self::Prod(b)) => a.cmp(b),
            (Self::Sum(a), Self::Sum(b)) => a.cmp(b),
            (Self::Label(a1, a2), Self::Label(b1, b2)) => {
                a1.0 .0.cmp(&b1.0 .0).then_with(|| a2.cmp(b2))
            }

            // Different variants - compare discriminants
            _ => self.discriminant().cmp(&other.discriminant()),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unifier(l0), Self::Unifier(r0)) => l0 == r0,
            (Self::Func(l0, l1), Self::Func(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Package(l0), Self::Package(r0)) => l0 == r0,
            (Self::Prod(l0), Self::Prod(r0)) => l0 == r0,
            (Self::Sum(l0), Self::Sum(r0)) => l0 == r0,
            (Self::Label(l0, l1), Self::Label(r0, r1)) => l0 == r0 && l1 == r1,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl EqUnifyValue for Type {}

impl Ident for Type {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        match self {
            Self::Package(spanned) => Ok(*spanned),
            _ => panic!(),
        }
    }
}

impl Type {
    fn discriminant(&self) -> usize {
        match self {
            Self::Num => 0,
            Self::Unifier(_) => 1,
            Self::Func(_, _) => 2,
            Self::Prod(_) => 3,
            Self::Sum(_) => 4,
            Self::Label(_, _) => 5,
            _ => todo!(),
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

    pub fn mentions(
        &self,
        unbound_tys: &BTreeSet<TyUniVar>,
        unbound_rows: &BTreeSet<RowUniVar>,
    ) -> bool {
        match self {
            Self::Num => false,
            Self::Unifier(v) => unbound_tys.contains(v),
            Self::Func(arg, ret) => {
                arg.mentions(unbound_tys, unbound_rows) || ret.mentions(unbound_tys, unbound_rows)
            }
            Self::Label(_, ty) => ty.mentions(unbound_tys, unbound_rows),
            Self::Prod(row) | Self::Sum(row) => match row {
                Row::Unifier(var) => unbound_rows.contains(var),
                Row::Open(_) => false,
                Row::Closed(row) => row.mentions(unbound_tys, unbound_rows),
            },
            _ => todo!(),
        }
    }

    pub fn split_func(&self) -> (&Intern<Self>, &Intern<Self>) {
        if let Self::Func(l, r) = self {
            (l, r)
        } else {
            panic!()
        }
    }
    pub fn destructure_arrow(&self) -> (Vec<Intern<Self>>, Intern<Self>) {
        let (l, r) = self.split_func();
        fn worker(t: &Intern<Type>, v: &mut Vec<Intern<Type>>) -> Vec<Intern<Type>> {
            match **t {
                Type::Func(l, r) => {
                    v.push(r);
                    worker(&l, v);
                }
                _ => v.push(*t),
            }
            v.to_vec()
        }
        let mut v = vec![];
        (worker(l, &mut v), *r)
    }

    pub fn fun(arg: Self, ret: Self) -> Self {
        Self::Func(Intern::from(arg), Intern::from(ret))
    }
}

impl Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Unifier(r) => r.hash(state),
            Self::Func(l, r) => {
                l.hash(state);
                r.hash(state);
            }
            _ => core::mem::discriminant(self).hash(state),
        }
    }
}
