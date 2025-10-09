use std::{hash::Hash, path::PathBuf};

use chumsky::span::{SimpleSpan};
use ordered_float::OrderedFloat;

use crate::passes::midend::environment::SimpleQuant;


pub type FileID = u64;

pub type Spanned<T> = (T, SimpleSpan<usize, FileID>);
//#[derive(Debug, Clone, Copy)]
//pub struct Spanned<T>(T, SimpleSpan<usize>);
//pub struct Spanned<T>(T, SimpleSpan<usize, FileID>);



// #[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
// pub struct MySpan {
//     pub start: usize,
//     pub end: usize,
//     pub context: &'static str,
// }

// impl Span for MySpan {
//     type Context = &'static str;
//     type Offset = usize;

//     fn new(context: impl Into<&str>, range: std::ops::Range<Self::Offset>) -> Self {
//         let converted = Box::leak(Box::new(context.into()));
//         MySpan {
//             start: range.start,
//             end: range.end,
//             context: converted,
//         }
//     }
//     fn start(&self) -> usize {
//         self.start
//     }

//     fn end(&self) -> usize {
//         self.end
//     }

//     fn context(&self) -> &'static str {
//         self.context
//     }
// }


// impl<T> Spanned<T> {
//     pub fn new(t: T, ctx: FileID, span: impl Span) -> Self {
//         Self(t, SimpleSpan::new(ctx, span.start()..span.end()))
//     }

//     pub fn value(&self) -> &T {
//         &self.0
//     }

//     pub fn span(&self) -> &SimpleSpan<usize, FileID> {
//         &self.1
//     }
// }

// impl<T> Hash for Spanned<T>
// where
//     T: Hash,
// {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         self.0.hash(state);
//     }
// }

// impl<T: PartialEq> PartialEq for Spanned<T> {
//     fn eq(&self, other: &Self) -> bool {
//         self.0 == other.0
//     }
// }


// impl<T: Eq> Eq for Spanned<T> {}


// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pub struct OptSpanned<T> {
//     pub t: T,
//     pub span: Option<SimpleSpan<usize, u64>>,
// }

// impl<T> From<Spanned<T>> for OptSpanned<T> {
//     fn from(value: Spanned<T>) -> Self {
//         OptSpanned {
//             t: value.0,
//             span: Some(value.1),
//         }
//     }
// }



// impl<T> OptSpanned<T> {
//     pub fn new(t: T, span: Option<SimpleSpan<usize, u64>>) -> Self {
//         OptSpanned { t, span }
//     }
// }

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
    Arrow(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Generic(Spanned<Expr>),
}

impl Ty {
    pub fn get_arrow(&self) -> (Box<Spanned<Self>>, Box<Spanned<Self>>) {
        if let Self::Arrow(l, r) = self {
            (l.clone(), r.clone())
        } else {
            panic!()
        }
    }

    pub fn get_user_name(&self) -> Option<String> {
        if let Self::User(name, _) = self {
            name.1;
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