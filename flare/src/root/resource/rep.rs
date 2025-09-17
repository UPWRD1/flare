use std::{path::PathBuf, rc::Rc};

use chumsky::span::SimpleSpan;
use ordered_float::OrderedFloat;
use pretty::RcDoc;

pub type Spanned<T> = (T, SimpleSpan<usize>);


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
    User(Spanned<Expr>, Vec<Spanned<Self>>),
    Tuple(Vec<Spanned<Self>>),
    Arrow(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Generic(Spanned<Expr>)
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
            Expr::FieldAccess(ref base, ref field) => {
                todo!()
                //Some(base.0.get_ident()?.append(field.0.get_ident()?))
            }
            Expr::Access(ref expr) => expr.0.get_ident(),
            Expr::Call(ref func, _) => func.0.get_ident(),
            Expr::Lambda(ref arg, _) => arg.0.get_ident(),
            _ => None,
        }
    }

    fn op_to_display_string(&self) -> Option<&'static str> {
        match self {
            Expr::Add(_, _) => Some(" + "),
            Expr::Sub(_, _) => Some(" - "),
            Expr::Mul(_, _) => Some(" * "),
            Expr::Div(_, _) => Some(" / "),
            Expr::FieldAccess(_, _) => Some("."),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: Spanned<Expr>,
    pub fields: Vec<(Spanned<Expr>, Spanned<Ty>)>,
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


