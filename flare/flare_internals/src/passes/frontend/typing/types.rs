use std::{collections::BTreeSet, fmt, hash::Hash};

use chumsky::span::SimpleSpan;
use ena::unify::{EqUnifyValue, UnifyKey};
use internment::Intern;

use crate::{
    passes::frontend::typing::{
        TypeScheme,
        rows::{Row, RowUniVar},
    },
    resource::{
        errors::CompResult,
        rep::{
            common::{Ident, Spanned},
            frontend::ast::Label,
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
    type Value = Option<Spanned<Intern<Type>>>;

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
// pub struct TypeVar(pub Intern<String>);
pub struct TypeVar(pub usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub enum Type {
    // Infer,
    Subtable(Spanned<Intern<Self>>, SimpleSpan<usize, u64>),
    Unifier(TyUniVar),
    Generic(Spanned<Intern<String>>),

    // Template(Spanned<Intern<String>>),
    Var(TypeVar),
    Particle(Spanned<Intern<String>>),
    #[default]
    Unit,
    Num,
    Bool,
    String,
    Func(Spanned<Intern<Self>>, Spanned<Intern<Self>>),

    // Package(Spanned<Intern<String>>),
    // Item(ItemId),
    TypeFun(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    TypeApp(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    User(Spanned<Intern<String>>, &'static [Spanned<Intern<Self>>]),
    Prod(Spanned<Intern<Row>>),
    Sum(Spanned<Intern<Row>>),
    Label(Label, Spanned<Intern<Self>>),
    Hole,
}
// #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
// pub struct InternType(pub Intern<Type>);

impl EqUnifyValue for Spanned<Intern<Type>> {}

impl Ident for Type {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        match self {
            // Self::Package(spanned) => Ok(*spanned),
            Self::Label(l, _) => Ok(l.0),
            _ => unreachable!("{:?}", self),
        }
    }
}

impl Type {
    pub fn render(&self, scheme: &TypeScheme) -> String {
        match self {
            Self::Var(v) => format!(
                "?{}",
                scheme.types_to_name.get(v).copied().unwrap_or_else(
                    || Intern::from(v.0.to_string()) // panic!("Type variable {v:?} has no associated name : {scheme:?}")
                ) // .coopied()
                  // .uwrap_or_else(|| v.0.to_string().into())
            ),
            Self::Func(l, r) => format!("{} -> {}", l.0.render(scheme), r.0.render(scheme)),
            Self::Prod(r) => format!("{{{}}}", r.render(scheme)),
            Self::Sum(r) => format!("|{}|", r.render(scheme)),
            Self::TypeFun(l, r) => format!("[{}]{}", l.0.render(scheme), r.0.render(scheme)),
            Self::TypeApp(l, r) => format!("{}::[{}]", r.0.render(scheme), l.0.render(scheme)),
            _ => format!("{self}"),
        }
    }

    pub fn to_default_span(self) -> Spanned<Intern<Self>> {
        Spanned(
            Intern::from(self),
            SimpleSpan {
                start: 0,
                end: 0,
                context: 0,
            },
        )
    }

    pub fn occurs_check(&self, var: TyUniVar) -> Result<(), Self> {
        match self {
            Self::Unifier(v) => {
                if *v == var {
                    Err(Self::Unifier(*v))
                } else {
                    Ok(())
                }
            }
            Self::Num
            | Self::String
            | Self::Bool
            | Self::Unit
            | Self::Particle(_)
            | Self::Generic(_)
            | Self::Var(_) => Ok(()),
            Self::Func(arg, ret) => {
                arg.0.occurs_check(var).map_err(|_| *self)?;
                ret.0.occurs_check(var).map_err(|_| *self)
            }

            Self::Label(_, ty) => ty.0.occurs_check(var).map_err(|_| *self),
            Self::Prod(row) | Self::Sum(row) => match *row.0 {
                Row::Open(_) | Row::Unifier(_) => Ok(()),
                Row::Closed(closed_row) => {
                    for ty in closed_row.values {
                        ty.0.occurs_check(var).map_err(|_| *self)?;
                    }
                    Ok(())
                }
            },
            Self::TypeFun(l, r) => {
                l.0.occurs_check(var).map_err(|_| *self)?;
                r.0.occurs_check(var).map_err(|_| *self)
            }

            Self::TypeApp(l, r) => {
                l.0.occurs_check(var).map_err(|_| *self)?;
                r.0.occurs_check(var).map_err(|_| *self)
            }

            Self::User(..) | Self::Hole => unreachable!("Shouldn't happen"),
            Self::Subtable(_, _) => todo!(), // _ => todo!("{self:?}"),
        }
    }

    pub fn mentions(
        &self,
        unbound_tys: &BTreeSet<TyUniVar>,
        unbound_rows: &BTreeSet<RowUniVar>,
    ) -> bool {
        match self {
            Self::Unifier(v) => unbound_tys.contains(v),
            Self::Num | Self::Var(_) => false,
            Self::Func(arg, ret) => {
                arg.0.mentions(unbound_tys, unbound_rows)
                    || ret.0.mentions(unbound_tys, unbound_rows)
            }
            Self::Label(_, ty) => ty.0.mentions(unbound_tys, unbound_rows),
            Self::Prod(row) | Self::Sum(row) => match &*row.0 {
                Row::Unifier(var) => unbound_rows.contains(var),
                Row::Open(_) => false,
                Row::Closed(row) => row.mentions(unbound_tys, unbound_rows),
            },
            _ => todo!(),
        }
    }

    pub fn split_func(&self) -> (&Spanned<Intern<Self>>, &Spanned<Intern<Self>>) {
        if let Self::Func(l, r) = self {
            (l, r)
        } else {
            unreachable!("Called on non-func type {self:?}")
        }
    }

    pub fn destructure_arrow(
        t: Spanned<Intern<Self>>,
    ) -> (Vec<Spanned<Intern<Self>>>, Spanned<Intern<Self>>) {
        fn worker(t: &Spanned<Intern<Type>>, v: &mut Vec<Spanned<Intern<Type>>>) {
            match *t.0 {
                Type::Func(l, r) => {
                    v.push(l);
                    worker(&r, v);
                }
                _ => v.push(*t),
            }
        }
        let mut v = vec![];
        worker(&t, &mut v);
        let (ret, args) = v.split_last().expect("Could not destructure arrow");
        (args.to_vec(), *ret)
    }

    pub fn to_row(self) -> Row {
        // TODO Replace with `Intern<Row>`
        match self {
            Self::Prod(row) | Self::Sum(row) => *row.0,
            _ => unreachable!("expected row, found {self:?}"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Num => write!(f, "num"),
            Self::Bool => write!(f, "bool"),
            Self::String => write!(f, "str"),
            Self::Unit => write!(f, "unit"),
            Self::Particle(p) => write!(f, "@{p}"),

            Self::Unifier(u) => write!(f, "%var{}", u.0),
            Self::Var(v) => write!(f, "?{}", v.0),
            Self::Func(l, r) => write!(f, "{l} -> {r}"),
            Self::Prod(r) => write!(f, "{{{r}}}"),
            Self::Sum(r) => write!(f, "|{r}|"),
            Self::Label(l, t) => write!(f, "{}: {t}", l.0.0),
            Self::TypeFun(l, r) => write!(f, "[{l}]{r}"),
            Self::TypeApp(l, r) => write!(f, "{l} {r}"),
            _ => write!(f, "{self:#?}"),
        }
    }
}
