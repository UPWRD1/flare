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
    Variable(Spanned<Intern<Expr>>),

    // #[serde(deserialize_with = "deserialize_static")]
    Type(Spanned<Intern<Ty>>),
}

/// Type representing a Pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Pattern {
    Atom(PatternAtom),

    Tuple(Intern<Vec<Spanned<Self>>>),

    Variant(Spanned<Intern<Expr>>, Intern<Vec<Spanned<Self>>>),
}

impl Pattern {
    pub fn get_ident(&self) -> Option<&Intern<String>> {
        match self {
            Self::Variant(n, _) => n.0.get_ident(n.1).ok(),
            Self::Atom(a) => match a {
                PatternAtom::Type(t) => Some(t.0.get_user_name()?),
                PatternAtom::Variable(s) => s.0.get_ident(s.1).ok(),
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
    // Constructor(Spanned<Intern<Self>>, Vec<Spanned<Self>>),
    Constructor(Spanned<Intern<Self>>, Intern<Vec<Spanned<Intern<Self>>>>),
    FieldedConstructor(
        Spanned<Intern<Self>>,
        Intern<Vec<(Spanned<Intern<Self>>, Spanned<Intern<Self>>)>>,
    ),

    Pat(Spanned<Pattern>),

    Mul(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Div(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Add(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Sub(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Comparison(Spanned<Intern<Self>>, ComparisonOp, Spanned<Intern<Self>>),

    Access(Spanned<Intern<Self>>),
    Call(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    FieldAccess(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    MethodAccess(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Myself,

    If(
        Spanned<Intern<Self>>,
        Spanned<Intern<Self>>,
        Spanned<Intern<Self>>,
    ),
    Match(
        Spanned<Intern<Self>>,
        Intern<Vec<(Spanned<Pattern>, Spanned<Intern<Self>>)>>,
    ),
    Lambda(Spanned<Intern<Self>>, Spanned<Intern<Self>>, bool),
    Let(
        Spanned<Intern<Self>>,
        Spanned<Intern<Self>>,
        Spanned<Intern<Self>>,
    ),
    Struct(Intern<Vec<(Spanned<Intern<Self>>, Spanned<Intern<Self>>)>>),
    Tuple(Intern<Vec<Spanned<Intern<Self>>>>),
}

impl Expr {
    #[inline]
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
            Expr::Lambda(arg, _, _) => arg.0.get_ident(arg.1),
            Expr::Pat(p) => {
                if let Pattern::Atom(PatternAtom::Variable(s)) = &p.0 {
                    Ok(s.0.get_ident(s.1)?)
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
        arg: Spanned<Intern<Self>>,
        span: SimpleSpan<usize, u64>,
    ) -> Spanned<Intern<Self>> {
        match self {
            Self::Call(l, r) => (
                Intern::from(Self::Call(l.0.inject_call_start(arg, span), r)),
                span,
            ),
            Self::Ident(n) => (
                Intern::from(Self::Call((Intern::from(self), span), arg)),
                span,
            ),
            _ => panic!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StructDef {
    pub the_ty: Spanned<Intern<Ty>>,
    pub fields: Vec<(Spanned<Intern<Expr>>, Spanned<Intern<Ty>>)>,
    //pub generics: Vec<Spanned<Expr>>,
}

#[derive(Debug, PartialEq)]
pub struct EnumDef {
    pub the_ty: Spanned<Intern<Ty>>,
    pub variants: Vec<Spanned<EnumVariant>>,
}

#[derive(Debug, PartialEq)]
pub struct ImportItem {
    pub items: Vec<Spanned<Intern<Expr>>>,
}

#[derive(Debug, PartialEq)]
pub struct ImplDef {
    pub the_ty: Spanned<Intern<Ty>>,
    pub methods: Vec<(
        Spanned<Intern<Expr>>,
        Spanned<Intern<Expr>>,
        Spanned<Intern<Ty>>,
    )>,
}

#[derive(Debug, PartialEq)]
pub enum Definition {
    Import(Spanned<Intern<Expr>>),
    Struct(StructDef),
    Enum(EnumDef),
    Let(
        Spanned<Intern<Expr>>,
        Spanned<Intern<Expr>>,
        Option<Spanned<Intern<Ty>>>,
    ),
    Extern(Spanned<Intern<Expr>>, Spanned<Intern<Ty>>),
    ImplDef(ImplDef),
}

#[derive(Debug, PartialEq)]
pub struct Package {
    pub name: Spanned<Intern<Expr>>,
    pub items: Vec<Definition>,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub packages: Vec<(Package, FileID)>,
}
