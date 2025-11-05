use std::{hash::Hash, path::Path};

use super::{deserialize_static, deserialize_static_str};
use crate::resource::errors::{CompResult, DynamicErr};
use chumsky::span::SimpleSpan;
use ordered_float::OrderedFloat;
use serde::{Deserialize, Serialize};

use super::{
    quantifier::QualifierFragment,
    types::{EnumVariant, Ty},
    Spanned,
};

/// Type representing an atomic value within a pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Serialize)]
pub enum PatternAtom<'s> {
    // #[serde(deserialize_with = "deserialize_static_str")]
    Strlit(&'s str),
    Num(OrderedFloat<f64>),

    // #[serde(deserialize_with = "deserialize_static_str")]
    Variable(&'static str),

    // #[serde(deserialize_with = "deserialize_static")]
    Type(&'static Spanned<Ty>),
}

/// Type representing a Pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Serialize)]
pub enum Pattern {
    #[serde(borrow)]
    Atom(PatternAtom<'static>),

    #[serde(deserialize_with = "deserialize_static")]
    Tuple(&'static [Spanned<Self>]),

    #[serde(deserialize_with = "deserialize_static")]
    Variant(&'static Spanned<Expr>, &'static [Spanned<Self>]),
}

impl Pattern {
    pub fn get_ident(&self) -> Option<&'static str> {
        match self {
            Self::Variant(n, _) => n.0.get_ident(n.1).ok(),
            Self::Atom(a) => match a {
                PatternAtom::Type(t) => Some(t.0.get_raw_name()),
                PatternAtom::Variable(s) => Some(s),
                _ => None, // errors::bad_ident(expr, s),
            },
            Self::Tuple(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Serialize)]
pub enum ComparisonOp {
    Eq,
    Neq,
    Gt,
    Lt,
    Gte,
    Lte,
}

/// Type representing an Expression.
/// You will typically encounter ```Expr``` as a ```Spanned<Expr>```, which is decorated with a span for diagnostic information.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Serialize)]
pub enum Expr {
    Ident(&'static str),
    Number(ordered_float::OrderedFloat<f64>),
    String(&'static str),
    Bool(bool),

    ExternFunc(&'static [QualifierFragment]),

    Unit,
    // Constructor(&'static Spanned<Self>, Vec<Spanned<Self>>),
    Constructor(&'static Spanned<Self>, &'static [Spanned<Self>]),
    FieldedConstructor(
        &'static Spanned<Self>,
        &'static [(Spanned<Self>, Spanned<Self>)],
    ),

    Pat(Spanned<Pattern>),

    Mul(&'static Spanned<Self>, &'static Spanned<Self>),
    Div(&'static Spanned<Self>, &'static Spanned<Self>),
    Add(&'static Spanned<Self>, &'static Spanned<Self>),
    Sub(&'static Spanned<Self>, &'static Spanned<Self>),
    Comparison(&'static Spanned<Self>, ComparisonOp, &'static Spanned<Self>),

    Access(&'static Spanned<Self>),
    Call(&'static Spanned<Self>, &'static Spanned<Self>),
    FieldAccess(&'static Spanned<Self>, &'static Spanned<Self>),
    MethodAccess(&'static Spanned<Self>, &'static Spanned<Self>),
    Myself,

    If(
        &'static Spanned<Self>,
        &'static Spanned<Self>,
        &'static Spanned<Self>,
    ),
    Match(
        &'static Spanned<Self>,
        &'static [(Spanned<Pattern>, Spanned<Self>)],
    ),
    Lambda(&'static Spanned<Self>, &'static Spanned<Self>),
    Let(
        &'static Spanned<Self>,
        &'static Spanned<Self>,
        &'static Spanned<Self>,
    ),
    Struct(&'static [(Spanned<Self>, Spanned<Self>)]),
    Tuple(&'static [Spanned<Self>]),
}

impl Expr {
    pub fn get_ident(&self, span: SimpleSpan<usize, u64>) -> CompResult<&'static str> {
        match self {
            Expr::Ident(s) => Ok(s),
            Expr::FieldAccess(base, _field) => {
                Ok(base.0.get_ident(base.1)?)
                //todo!()
                //Some(base.0.get_ident()?.append(field.0.get_ident()?))
            }
            Expr::Access(expr) => expr.0.get_ident(expr.1),
            Expr::Call(func, _) => func.0.get_ident(func.1),
            Expr::Lambda(arg, _) => arg.0.get_ident(arg.1),
            Expr::Pat(p) => {
                if let Pattern::Atom(PatternAtom::Variable(s)) = p.0 {
                    Ok(s)
                } else {
                    Err(DynamicErr::new("cannot get ident")
                        .label((format!("{self:?}"), span))
                        .into())
                }
            }
            _ => Err(DynamicErr::new("cannot get ident")
                .label((format!("{self:?}"), span))
                .into()),
        }
    }

    pub fn get_num(&self, span: SimpleSpan<usize, u64>) -> CompResult<OrderedFloat<f64>> {
        match self {
            Self::Number(n) => Ok(*n),
            _ => Err(DynamicErr::new("Not a number").label(("here", span)).into()),
        }
    }

    pub fn inject_call_start(
        self,
        arg: Spanned<Self>,
        span: SimpleSpan<usize, u64>,
    ) -> Spanned<Self> {
        match self {
            Self::Call(l, r) => (
                Self::Call(Box::leak(Box::new(l.0.inject_call_start(arg, span))), r),
                span,
            ),
            Self::Ident(n) => (
                Self::Call(Box::leak(Box::new((self, span))), Box::leak(Box::new(arg))),
                span,
            ),
            _ => panic!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StructDef {
    pub the_ty: Spanned<Ty>,
    pub fields: Vec<(Spanned<Expr>, Spanned<Ty>)>,
    //pub generics: Vec<Spanned<Expr>>,
}

#[derive(Debug, PartialEq)]
pub struct EnumDef {
    pub the_ty: Spanned<Ty>,

    pub variants: Vec<Spanned<EnumVariant>>,
}

#[derive(Debug, PartialEq)]
pub struct ImportItem {
    pub items: Vec<Spanned<Expr>>,
}

#[derive(Debug, PartialEq)]
pub struct ImplDef {
    pub the_ty: Spanned<Ty>,
    pub methods: Vec<(Spanned<Expr>, Spanned<Expr>, Spanned<Ty>)>,
}

#[derive(Debug, PartialEq)]
pub enum Definition {
    Import(Spanned<Expr>),
    Struct(StructDef),
    Enum(EnumDef),
    Let(Spanned<Expr>, Spanned<Expr>, Option<Spanned<Ty>>),
    Extern(Spanned<Expr>, Spanned<Ty>),
    ImplDef(ImplDef),
}

#[derive(Debug, PartialEq)]
pub struct Package {
    pub name: Spanned<Expr>,
    pub items: Vec<Definition>,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub packages: Vec<(Package, &'static Path, &'static str)>,
}
