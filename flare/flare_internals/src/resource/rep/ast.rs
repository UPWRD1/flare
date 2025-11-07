use std::hash::Hash;

use crate::resource::{
    errors::{CompResult, DynamicErr},
    rep::files::FileID,
};
use chumsky::span::SimpleSpan;
use internment::Intern;
use ordered_float::OrderedFloat;
use serde::{Deserialize, Deserializer, Serialize};

use super::{
    quantifier::QualifierFragment,
    types::{EnumVariant, Ty},
    Spanned,
};

/// Type representing an atomic value within a pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Serialize, Deserialize)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Serialize, Deserialize)]
pub enum Pattern {
    // #[serde(borrow)]
    Atom(PatternAtom),

    // #[serde(deserialize_with = "deserialize_static")]
    Tuple(Intern<Vec<Spanned<Self>>>),

    // #[serde(deserialize_with = "deserialize_static")]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Serialize, Deserialize)]
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
#[serde(into = "ExprHelper")]
pub enum Expr {
    Ident(Intern<String>),
    Number(ordered_float::OrderedFloat<f64>),
    String(Intern<String>),
    Bool(bool),

    ExternFunc(Intern<[QualifierFragment]>),
    Unit,
    // Constructor(Intern<Spanned<Self>>, Vec<Spanned<Self>>),
    Constructor(Intern<Spanned<Self>>, Intern<[Spanned<Self>]>),
    FieldedConstructor(
        Intern<Spanned<Self>>,
        Intern<[(Spanned<Self>, Spanned<Self>)]>,
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
        Intern<[(Spanned<Pattern>, Spanned<Self>)]>,
    ),
    Lambda(Intern<Spanned<Self>>, Intern<Spanned<Self>>),
    Let(
        Intern<Spanned<Self>>,
        Intern<Spanned<Self>>,
        Intern<Spanned<Self>>,
    ),
    Struct(Intern<[(Spanned<Self>, Spanned<Self>)]>),
    Tuple(Intern<[Spanned<Self>]>),
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

impl<'de> Deserialize<'de> for Expr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        ExprHelper::deserialize(deserializer).map(Into::into)
    }
}

#[derive(Deserialize, Serialize)]
#[serde(rename = "Expr")]
enum ExprHelper {
    Ident(String),
    Number(ordered_float::OrderedFloat<f64>),
    String(String),
    Bool(bool),

    ExternFunc(Vec<QualifierFragment>),
    Unit,
    Constructor(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),
    FieldedConstructor(Box<Spanned<Expr>>, Vec<(Spanned<Expr>, Spanned<Expr>)>),

    Pat(Spanned<Pattern>),

    Mul(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Div(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Add(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Sub(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Comparison(Box<Spanned<Expr>>, ComparisonOp, Box<Spanned<Expr>>),

    Access(Box<Spanned<Expr>>),
    Call(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    FieldAccess(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    MethodAccess(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Myself,

    If(Box<Spanned<Expr>>, Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Match(Box<Spanned<Expr>>, Vec<(Spanned<Pattern>, Spanned<Expr>)>),
    Lambda(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Let(Box<Spanned<Expr>>, Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Struct(Vec<(Spanned<Expr>, Spanned<Expr>)>),
    Tuple(Vec<Spanned<Expr>>),
}

impl From<Expr> for ExprHelper {
    fn from(expr: Expr) -> Self {
        match expr {
            Expr::Ident(s) => ExprHelper::Ident((*s).to_string()),
            Expr::Number(n) => ExprHelper::Number(n),
            Expr::String(s) => ExprHelper::String((*s).to_string()),
            Expr::Bool(b) => ExprHelper::Bool(b),
            Expr::ExternFunc(frags) => ExprHelper::ExternFunc(frags.to_vec()),
            Expr::Unit => ExprHelper::Unit,
            Expr::Constructor(name, args) => {
                ExprHelper::Constructor(Box::new(*name), args.to_vec())
            }
            Expr::FieldedConstructor(name, fields) => {
                ExprHelper::FieldedConstructor(Box::new(*name), fields.to_vec())
            }
            Expr::Pat(p) => ExprHelper::Pat(p),
            Expr::Mul(l, r) => ExprHelper::Mul(Box::new(*l), Box::new(*r)),
            Expr::Div(l, r) => ExprHelper::Div(Box::new(*l), Box::new(*r)),
            Expr::Add(l, r) => ExprHelper::Add(Box::new(*l), Box::new(*r)),
            Expr::Sub(l, r) => ExprHelper::Sub(Box::new(*l), Box::new(*r)),
            Expr::Comparison(l, op, r) => ExprHelper::Comparison(Box::new(*l), op, Box::new(*r)),
            Expr::Access(e) => ExprHelper::Access(Box::new(*e)),
            Expr::Call(f, arg) => ExprHelper::Call(Box::new(*f), Box::new(*arg)),
            Expr::FieldAccess(e, field) => ExprHelper::FieldAccess(Box::new(*e), Box::new(*field)),
            Expr::MethodAccess(e, method) => {
                ExprHelper::MethodAccess(Box::new(*e), Box::new(*method))
            }
            Expr::Myself => ExprHelper::Myself,
            Expr::If(cond, then, els) => {
                ExprHelper::If(Box::new(*cond), Box::new(*then), Box::new(*els))
            }
            Expr::Match(scrutinee, arms) => ExprHelper::Match(Box::new(*scrutinee), arms.to_vec()),
            Expr::Lambda(param, body) => ExprHelper::Lambda(Box::new(*param), Box::new(*body)),
            Expr::Let(pat, val, body) => {
                ExprHelper::Let(Box::new(*pat), Box::new(*val), Box::new(*body))
            }
            Expr::Struct(fields) => ExprHelper::Struct(fields.to_vec()),
            Expr::Tuple(elems) => ExprHelper::Tuple(elems.to_vec()),
        }
    }
}

impl From<ExprHelper> for Expr {
    fn from(helper: ExprHelper) -> Self {
        match helper {
            ExprHelper::Ident(s) => Expr::Ident(s.into()),
            ExprHelper::Number(n) => Expr::Number(n),
            ExprHelper::String(s) => Expr::String(s.into()),
            ExprHelper::Bool(b) => Expr::Bool(b),
            ExprHelper::ExternFunc(frags) => {
                Expr::ExternFunc(Intern::<[_]>::from(frags.as_slice()))
            }
            ExprHelper::Unit => Expr::Unit,
            ExprHelper::Constructor(name, args) => {
                Expr::Constructor(Intern::from(name), Intern::<[_]>::from(args.as_slice()))
            }
            ExprHelper::FieldedConstructor(name, fields) => {
                Expr::FieldedConstructor(Intern::from(name), Intern::from(fields.as_slice()))
            }
            ExprHelper::Pat(p) => Expr::Pat(p),
            ExprHelper::Mul(l, r) => Expr::Mul(Intern::from(l), Intern::from(r)),
            ExprHelper::Div(l, r) => Expr::Div(Intern::from(l), Intern::from(r)),
            ExprHelper::Add(l, r) => Expr::Add(Intern::from(l), Intern::from(r)),
            ExprHelper::Sub(l, r) => Expr::Sub(Intern::from(l), Intern::from(r)),
            ExprHelper::Comparison(l, op, r) => {
                Expr::Comparison(Intern::from(l), op, Intern::from(r))
            }
            ExprHelper::Access(e) => Expr::Access(Intern::from(e)),
            ExprHelper::Call(f, arg) => Expr::Call(Intern::from(f), Intern::from(arg)),
            ExprHelper::FieldAccess(e, field) => {
                Expr::FieldAccess(Intern::from(e), Intern::from(field))
            }
            ExprHelper::MethodAccess(e, method) => {
                Expr::MethodAccess(Intern::from(e), Intern::from(method))
            }
            ExprHelper::Myself => Expr::Myself,
            ExprHelper::If(cond, then, els) => {
                Expr::If(Intern::from(cond), Intern::from(then), Intern::from(els))
            }
            ExprHelper::Match(scrutinee, arms) => {
                Expr::Match(Intern::from(scrutinee), Intern::from(arms.as_slice()))
            }
            ExprHelper::Lambda(param, body) => {
                Expr::Lambda(Intern::from(param), Intern::from(body))
            }
            ExprHelper::Let(pat, val, body) => {
                Expr::Let(Intern::from(pat), Intern::from(val), Intern::from(body))
            }
            ExprHelper::Struct(fields) => Expr::Struct(Intern::from(fields.as_slice())),
            ExprHelper::Tuple(elems) => Expr::Tuple(Intern::from(elems.as_slice())),
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
