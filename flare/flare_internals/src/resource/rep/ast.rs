use std::{hash::Hash, path::PathBuf};

use ordered_float::OrderedFloat;

use super::{
    quantifier::SimpleQuant,
    types::{EnumVariant, Ty},
    Spanned,
};

/// Type representing an atomic value within a pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PatternAtom {
    Strlit(String),
    Num(OrderedFloat<f64>),
    Variable(String),
    Type(Box<Spanned<Ty>>),
}

/// Type representing a Pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    Atom(PatternAtom),
    Tuple(Vec<Spanned<Self>>),
    Variant(Box<Spanned<Expr>>, Vec<Spanned<Self>>),
}

impl Pattern {
    pub fn get_ident(&self) -> Option<String> {
        match self {
            Self::Variant(n, _) => n.0.get_ident(),
            Self::Atom(a) => match a {
                PatternAtom::Type(t) => Some(t.0.get_raw_name()),
                PatternAtom::Variable(s) => Some(s.to_string()),
                _ => None,
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Ident(String),
    Number(ordered_float::OrderedFloat<f64>),
    String(String),
    Bool(bool),

    ExternFunc(Vec<SimpleQuant>),

    Unit,
    Constructor(Box<Spanned<Self>>, Vec<Spanned<Self>>),
    FieldedConstructor(Box<Spanned<Self>>, Vec<(Spanned<Self>, Spanned<Self>)>),

    Pat(Spanned<Pattern>),

    Mul(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Div(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Add(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Sub(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Comparison(Box<Spanned<Self>>, ComparisonOp, Box<Spanned<Self>>),

    Access(Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Box<Spanned<Self>>),
    FieldAccess(Box<Spanned<Self>>, Box<Spanned<Self>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Match(
        Box<Spanned<Self>>,
        Vec<(Spanned<Pattern>, Box<Spanned<Self>>)>,
    ),
    Lambda(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Let(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Struct(Vec<(Box<Spanned<Self>>, Spanned<Self>)>),
    Tuple(Vec<Spanned<Self>>),
}

impl Expr {
    pub fn get_ident(&self) -> Option<String> {
        match self {
            Expr::Ident(ref s) => Some(s.to_string()),
            Expr::FieldAccess(ref base, ref _field) => {
                Some(base.0.get_ident()?)
                //todo!()
                //Some(base.0.get_ident()?.append(field.0.get_ident()?))
            }
            Expr::Access(ref expr) => expr.0.get_ident(),
            Expr::Call(ref func, _) => func.0.get_ident(),
            Expr::Lambda(ref arg, _) => arg.0.get_ident(),
            Expr::Pat(p) => {
                if let Pattern::Atom(PatternAtom::Variable(ref s)) = p.0 {
                    Some(s.to_string())
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub the_ty: Spanned<Ty>,
    pub fields: Vec<(Spanned<Expr>, Spanned<Ty>)>,
    //pub generics: Vec<Spanned<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub the_ty: Spanned<Ty>,
    pub variants: Vec<Spanned<EnumVariant>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportItem {
    pub items: Vec<Spanned<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition {
    Import(Spanned<Expr>),
    Struct(StructDef),
    Enum(EnumDef),
    Let(Spanned<Expr>, Spanned<Expr>, Option<Spanned<Ty>>),
    Extern(Spanned<Expr>, Spanned<Ty>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Package {
    pub name: Spanned<Expr>,
    pub items: Vec<Definition>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub packages: Vec<(Package, PathBuf, String)>,
}
