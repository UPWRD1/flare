use std::path::PathBuf;

use chumsky::span::SimpleSpan;
use ordered_float::OrderedFloat;

pub type Spanned<T> = (T, SimpleSpan<usize>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OptSpanned<T> {
    pub t: T,
    pub span: Option<SimpleSpan<usize>>,
}

impl<T> From<Spanned<T>> for OptSpanned<T> {
    fn from(value: Spanned<T>) -> Self {
        OptSpanned {
            t: value.0,
            span: Some(value.1),
        }
    }
}

impl<T> From<OptSpanned<T>> for Spanned<T> {
    fn from(value: OptSpanned<T>) -> Self {
        //(value.0, value.1.unwrap_or(SimpleSpan::new(0, 0)))
        (value.t, value.span.expect("Shouldn't Happen!"))
    }
}

impl<T> OptSpanned<T> {
    pub fn new(t: T, span: Option<SimpleSpan<usize>>) -> Self {
        OptSpanned { t, span }
    }
}

/// Type representing an atomic value within a pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PatternAtom {
    Strlit(String),
    Num(OrderedFloat<f64>),
    Variable(String),
}

/// Type representing a Pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    Atom(PatternAtom),
    Tuple(Vec<Spanned<Self>>),
    Variant(Box<Spanned<Expr>>, Vec<Spanned<Self>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Num,
    Str,
    Bool,
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    Primitive(PrimitiveType),
    User(OptSpanned<Expr>, Vec<OptSpanned<Self>>),
    Tuple(Vec<OptSpanned<Self>>),
    Arrow(Box<OptSpanned<Self>>, Box<OptSpanned<Self>>),
    Generic(OptSpanned<Expr>),
}

impl Ty {
    pub fn get_arrow(&self) -> (Box<OptSpanned<Self>>, Box<OptSpanned<Self>>) {
        if let Self::Arrow(l, r) = self {
            (l.clone(), r.clone())
        } else {
            panic!()
        }
    }
}

/// Type representing an Expression.
/// You will typically encounter ```Expr``` as a ```Spanned<Expr>```, which is decorated with a span for diagnostic information.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Ident(String),
    Number(ordered_float::OrderedFloat<f64>),
    String(String),
    Bool(bool),

    Unit,
    Constructor(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),

    Pat(Pattern),

    Mul(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Div(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Add(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Sub(Box<Spanned<Expr>>, Box<Spanned<Expr>>),

    Access(Box<Spanned<Expr>>),
    Call(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    FieldAccess(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    If(Box<Spanned<Expr>>, Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Match(
        Box<Spanned<Expr>>,
        Vec<(Spanned<Pattern>, Box<Spanned<Expr>>)>,
    ),
    Lambda(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Let(Box<Spanned<Expr>>, Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Struct(Vec<(Box<Spanned<Expr>>, Spanned<Expr>)>),
    Tuple(Vec<Spanned<Expr>>),
}

impl Expr {
    pub fn get_ident(&self) -> Option<String> {
        match self {
            Expr::Ident(ref s) => Some(s.to_string()),
            Expr::FieldAccess(ref _base, ref _field) => {
                todo!()
                //Some(base.0.get_ident()?.append(field.0.get_ident()?))
            }
            Expr::Access(ref expr) => expr.0.get_ident(),
            Expr::Call(ref func, _) => func.0.get_ident(),
            Expr::Lambda(ref arg, _) => arg.0.get_ident(),
            Expr::Pat(Pattern::Atom(PatternAtom::Variable(ref s))) => Some(s.to_string()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: Spanned<Expr>,
    pub fields: Vec<(Spanned<Expr>, OptSpanned<Ty>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportItem {
    pub items: Vec<Spanned<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition {
    Import(ImportItem),
    Struct(StructDef),
    Let(Spanned<Expr>, Spanned<Expr>),
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

impl std::ops::Add for Program {
    type Output = Self;

    fn add(mut self, other: Self) -> Self::Output {
        self.packages.extend(other.packages);
        let mut the_prog = Self {
            packages: self.packages,
        };
        the_prog.packages.dedup();
        the_prog
    }
}
