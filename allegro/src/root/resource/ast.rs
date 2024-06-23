use std::fmt::Display;

use super::itypes::Itype;

#[derive(Debug)]
pub enum BinOp {
    Plus, Minus,
    Mult, Div,
    Equal, NotEqual,
    And, Or,
    Less, Greater, LessEq, GreaterEq,
    Assign,
}

#[derive(Debug)]
pub enum UnaOp {
    Neg, Not
}

#[derive(Debug, Clone)]
pub enum VTypeKind {
    Int,
    Flt,
    Str,
    Bool,
    Custom(String),
    Generic(GenericType),
    Unknown,
}

impl Display for VTypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            VTypeKind::Int => "int",
            VTypeKind::Flt => "flt",
            VTypeKind::Str => "str",
            VTypeKind::Bool => "bool",
            VTypeKind::Custom(c) => &c,
            VTypeKind::Generic(g) => &g.name,
            VTypeKind::Unknown => "unknown",
        })
    }
}

pub struct VType {
    kind: VTypeKind,
    is_mut: bool,
}

#[derive(Debug, Clone)]
pub struct GenericType {
    name: String,
    restrictions: Vec<String>, // placeholder
}

#[derive(Debug, Clone)]
pub struct Pair {
    pub name: String,
    pub value: VTypeKind
}

#[derive(Debug)]
pub enum Expr {
    Scalar(Itype),
    Array(Vec<Expr>),
    Variable(String),
    BinaryOp(BinOp, Box<(Expr, Expr)>),
    UnaryOp(UnaOp, Box<Expr>),
    Call {name: String, on: Option<String>, args: Vec<Expr>},
    Assign(Variable),
    FnExpr(String, Vec<Pair>, Box<Expr>)
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<(Stmt, Option<Stmt>)>),
    While(Expr, Box<Stmt>),
    ForEach(String, String, Box<Stmt>),
    ForRange(String, Itype, Itype, Box<Stmt>),
    Return(Expr),
    Break,
    Continue,
}

#[derive(Debug)]
pub struct Function {
    name: Pair,
    extends: Option<String>,
    args: Vec<Pair>,
    code: Stmt,
}

#[derive(Debug)]
pub struct Variable {
    name: String,
    ini: Box<Expr>,
}

#[derive(Debug)]
pub struct Program {
    pub funcs: Vec<Function>,
}

impl Function {
    pub fn new(name: String, args: Vec<Pair>, code: Vec<Stmt>, extends: Option<String>) -> Function {
        Function {
            name: Pair { name, value: VTypeKind::Unknown }, extends, args, code: Stmt::Block(code)
        }
    }
}

impl Variable {
    pub fn new(name: String, ini: Expr) -> Variable {
        Variable {
            name, ini: Box::new(ini)
        }
    }
}

impl Program {
    pub fn new() -> Program {
        Program {
            funcs: Vec::new(),
        }
    }
    pub fn add_function(&mut self, f: Function) {
        self.funcs.push(f);
    }
}

#[derive(Debug, Clone)]
pub struct Loc {
    row: usize,
    col: usize,
}

#[macro_export]
macro_rules! loc {
    ($x: expr, $y:expr) => {
        Loc {
            row: $x,
            col: $y,
        }
    };
}