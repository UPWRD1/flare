use std::{hash::Hash, path::PathBuf, rc::Rc};

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
    Type(Rc<Spanned<Ty>>),
}

/// Type representing a Pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    Atom(PatternAtom),
    Tuple(Vec<Spanned<Self>>),
    Variant(Rc<Spanned<Expr>>, Vec<Spanned<Self>>),
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
    Constructor(Rc<Spanned<Expr>>, Vec<Spanned<Expr>>),
    FieldedConstructor(Rc<Spanned<Expr>>, Vec<(Spanned<Expr>, Spanned<Expr>)>),

    Pat(Spanned<Pattern>),

    Mul(Rc<Spanned<Expr>>, Rc<Spanned<Expr>>),
    Div(Rc<Spanned<Expr>>, Rc<Spanned<Expr>>),
    Add(Rc<Spanned<Expr>>, Rc<Spanned<Expr>>),
    Sub(Rc<Spanned<Expr>>, Rc<Spanned<Expr>>),
    Comparison(Rc<Spanned<Expr>>, ComparisonOp, Rc<Spanned<Expr>>),

    Access(Rc<Spanned<Expr>>),
    Call(Rc<Spanned<Expr>>, Rc<Spanned<Expr>>),
    FieldAccess(Rc<Spanned<Expr>>, Rc<Spanned<Expr>>),
    If(Rc<Spanned<Expr>>, Rc<Spanned<Expr>>, Rc<Spanned<Expr>>),
    Match(
        Rc<Spanned<Expr>>,
        Vec<(Spanned<Pattern>, Rc<Spanned<Expr>>)>,
    ),
    Lambda(Rc<Spanned<Expr>>, Rc<Spanned<Expr>>),
    Let(Rc<Spanned<Expr>>, Rc<Spanned<Expr>>, Rc<Spanned<Expr>>),
    Struct(Vec<(Rc<Spanned<Expr>>, Spanned<Expr>)>),
    Tuple(Vec<Spanned<Expr>>),
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
    pub name: Spanned<Expr>,
    pub fields: Vec<(Spanned<Expr>, Spanned<Ty>)>,
    //pub generics: Vec<Spanned<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub name: Spanned<Expr>,
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
