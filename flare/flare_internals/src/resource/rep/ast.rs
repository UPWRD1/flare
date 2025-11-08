use std::hash::Hash;

use crate::resource::{
    errors::{CompResult, DynamicErr},
    rep::files::FileID,
};
use chumsky::span::SimpleSpan;
use internment::Intern;
use ordered_float::OrderedFloat;

use super::{
    quantifier::QualifierFragment,
    types::{EnumVariant, Ty},
    Spanned,
};

/// Type representing an atomic value within a pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum PatternAtom {
    // #[serde(deserialize_with = "deserialize_static_str")]
    Strlit(Intern<String>),
    Num(OrderedFloat<f64>),

    // #[serde(deserialize_with = "deserialize_static_str")]
    Variable(Intern<String>),

    // #[serde(deserialize_with = "deserialize_static")]
    Type(Intern<Spanned<Ty>>),
}

/// Type representing a Pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Pattern {
    Atom(PatternAtom),

    Tuple(Intern<Vec<Spanned<Self>>>),

    Variant(Intern<Spanned<Expr>>, Intern<Vec<Spanned<Self>>>),
}

impl Pattern {
    pub fn get_ident(&self) -> Option<&Intern<String>> {
        match self {
            Self::Variant(n, _) => n.0.get_ident(n.1).ok(),
            Self::Atom(a) => match a {
                PatternAtom::Type(t) => Some(t.0.get_user_name()?),
                PatternAtom::Variable(s) => Some(s),
                _ => None, // errors::bad_ident(expr, s),
            },
            Self::Tuple(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Expr {
    Ident(Intern<String>),
    Number(ordered_float::OrderedFloat<f64>),
    String(Intern<String>),
    Bool(bool),

    ExternFunc(Intern<Vec<QualifierFragment>>),
    Unit,
    // Constructor(Intern<Spanned<Self>>, Vec<Spanned<Self>>),
    Constructor(Intern<Spanned<Self>>, Intern<Vec<Spanned<Self>>>),
    FieldedConstructor(
        Intern<Spanned<Self>>,
        Intern<Vec<(Spanned<Self>, Spanned<Self>)>>,
    ),

    Pat(Spanned<Pattern>),

    Mul(Intern<Spanned<Self>>, Intern<Spanned<Self>>),
    Div(Intern<Spanned<Self>>, Intern<Spanned<Self>>),
    Add(Intern<Spanned<Self>>, Intern<Spanned<Self>>),
    Sub(Intern<Spanned<Self>>, Intern<Spanned<Self>>),
    Comparison(Intern<Spanned<Self>>, ComparisonOp, Intern<Spanned<Self>>),

    Access(Intern<Spanned<Self>>),
    Call(Intern<Spanned<Self>>, Intern<Spanned<Self>>),
    FieldAccess(Intern<Spanned<Self>>, Intern<Spanned<Self>>),
    MethodAccess(Intern<Spanned<Self>>, Intern<Spanned<Self>>),
    Myself,

    If(
        Intern<Spanned<Self>>,
        Intern<Spanned<Self>>,
        Intern<Spanned<Self>>,
    ),
    Match(
        Intern<Spanned<Self>>,
        Intern<Vec<(Spanned<Pattern>, Spanned<Self>)>>,
    ),
    Lambda(Intern<Spanned<Self>>, Intern<Spanned<Self>>),
    Let(
        Intern<Spanned<Self>>,
        Intern<Spanned<Self>>,
        Intern<Spanned<Self>>,
    ),
    Struct(Intern<Vec<(Spanned<Self>, Spanned<Self>)>>),
    Tuple(Intern<Vec<Spanned<Self>>>),
}

impl Expr {
    pub fn get_ident(&self, span: SimpleSpan<usize, u64>) -> CompResult<&Intern<String>> {
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
                if let Pattern::Atom(PatternAtom::Variable(s)) = &p.0 {
                    Ok(s)
                } else {
                    Err(DynamicErr::new("cannot get ident")
                        .label(format!("{self:?}"), span)
                        .into())
                }
            }
            _ => Err(DynamicErr::new("cannot get ident")
                .label(format!("{self:?}"), span)
                .into()),
        }
    }

    pub fn get_num(&self, span: SimpleSpan<usize, u64>) -> CompResult<OrderedFloat<f64>> {
        match self {
            Self::Number(n) => Ok(*n),
            _ => Err(DynamicErr::new("Not a number").label("here", span).into()),
        }
    }

    pub fn inject_call_start(
        self,
        arg: Spanned<Self>,
        span: SimpleSpan<usize, u64>,
    ) -> Spanned<Self> {
        match self {
            Self::Call(l, r) => (
                Self::Call(Intern::from(l.0.inject_call_start(arg, span)), r),
                span,
            ),
            Self::Ident(n) => (
                Self::Call(Intern::from((self, span)), Intern::from(arg)),
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
    pub packages: Vec<(Package, FileID)>,
}
