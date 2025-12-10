use std::{collections::BTreeSet, fmt, hash::Hash};

use chumsky::span::Span;
use ena::unify::{EqUnifyValue, UnifyKey};
use internment::Intern;

use crate::{
    passes::midend::typing::rows::{Row, RowUniVar},
    resource::{
        errors::CompResult,
        rep::{
            Spanned,
            ast::{ItemId, Label},
            common::Ident,
        },
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
        "TypeUniVar"
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub Intern<String>);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Unknown,
    Unifier(TyUniVar),
    Var(TypeVar),
    Particle(Spanned<Intern<String>>),

    Unit,
    Num,
    Bool,
    String,
    Func(Intern<Self>, Intern<Self>),

    Package(Spanned<Intern<String>>),
    Item(ItemId),

    User(Spanned<Intern<String>>),
    Prod(Row),
    Sum(Row),
    Label(Label, Intern<Self>),
}

// #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
// pub struct InternType(pub Intern<Type>);

impl EqUnifyValue for Type {}

impl Ident for Type {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        match self {
            Self::Package(spanned) => Ok(*spanned),
            Self::Label(l, _) => Ok(l.0),
            _ => panic!("{:?}", self),
        }
    }
}

impl Type {
    pub fn occurs_check(&self, var: TyUniVar) -> Result<(), Self> {
        match self {
            Self::Num | Self::String | Self::Bool | Self::Unit | Self::Particle(_) => Ok(()),
            Self::Unifier(v) => {
                if *v == var {
                    Err(Self::Unifier(*v))
                } else {
                    Ok(())
                }
            }
            Self::Var(_) => Ok(()),
            Self::Func(arg, ret) => {
                arg.occurs_check(var).map_err(|_| *self)?;
                ret.occurs_check(var).map_err(|_| *self)
            }

            Self::Label(_, ty) => ty.occurs_check(var).map_err(|_| *self),
            Self::Prod(row) | Self::Sum(row) => match row {
                Row::Open(_) => Ok(()),
                Row::Unifier(_) => Ok(()),
                Row::Closed(closed_row) => {
                    for ty in closed_row.values.iter() {
                        ty.occurs_check(var).map_err(|_| *self)?
                    }
                    Ok(())
                }
            },
            _ => todo!("{self:?}"),
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
            Self::Var(_) => false,
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
        fn worker(t: &Type, v: &mut Vec<Intern<Type>>) {
            match t {
                Type::Func(l, r) => {
                    v.push(*l);
                    worker(r, v);
                }
                _ => v.push((*t).into()),
            }
        }
        let mut v = vec![];
        worker(self, &mut v);
        let (ret, args) = v.split_last().unwrap();
        (args.to_vec(), *ret)
    }

    pub fn fun(arg: Self, ret: Self) -> Self {
        Self::Func(arg.into(), ret.into())
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unifier(u) => write!(f, "%var{}", u.0),
            Self::Var(v) => write!(f, "?{}", v.0),
            Self::Func(l, r) => write!(f, "{l} -> {r}"),
            _ => write!(f, "{self:#?}"),
        }
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
