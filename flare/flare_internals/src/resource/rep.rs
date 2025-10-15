use std::{hash::Hash, path::PathBuf, rc::Rc};

use chumsky::span::{SimpleSpan};
use ordered_float::OrderedFloat;

use crate::passes::midend::environment::SimpleQuant;

/// Represents a file's unique identification code inside of a `Context`
pub type FileID = u64;

pub type Spanned<T> = (T, SimpleSpan<usize, FileID>);

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
    Variant(Rc<Spanned<Expr>>, Vec<Spanned<Self>>),
}

/// Represents a primitive type within `Ty`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Num,
    Str,
    Bool,
    Unit,
}

/// Represents a type in the parser and master environment.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    Primitive(PrimitiveType),
    User(Spanned<Expr>, Vec<Spanned<Self>>),
    Tuple(Vec<Spanned<Self>>, usize),
    Arrow(Rc<Spanned<Self>>, Rc<Spanned<Self>>),
    Generic(Spanned<Expr>),
}

impl Ty {
    pub fn get_arrow(&self) -> (Rc<Spanned<Self>>, Rc<Spanned<Self>>) {
        if let Self::Arrow(l, r) = self {
            (l.clone(), r.clone())
        } else {
            panic!()
        }
    }

    pub fn get_user_name(&self) -> Option<String> {
        if let Self::User(name, _) = self {
            //name.1;
            Some(name.0.get_ident()?)
        } else {
            None
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
            Expr::FieldAccess(ref _base, ref _field) => {
                todo!()
                //Some(base.0.get_ident()?.append(field.0.get_ident()?))
            }
            Expr::Access(ref expr) => expr.0.get_ident(),
            Expr::Call(ref func, _) => func.0.get_ident(),
            Expr::Lambda(ref arg, _) => arg.0.get_ident(),
            Expr::Pat(p) => if let Pattern::Atom(PatternAtom::Variable(ref s)) = p.0 {Some(s.to_string())} else {None},
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
pub struct ImportItem {
    pub items: Vec<Spanned<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition {
    Import(ImportItem),
    Struct(StructDef),
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

#[derive(Debug, Clone)]
pub struct FileSource {
    pub filename: PathBuf,
    pub src_text: String,
}