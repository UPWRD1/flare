use std::fmt::Display;

use super::itypes::Itype;

#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[allow(dead_code)]
pub enum BinOp {
    Plus,
    Minus,
    Mult,
    Div,
    Equal,
    NotEqual,
    And,
    Or,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Assign,
}

// impl BinOp {
//     pub fn to_char(&self) -> char {
//         match self {
//             BinOp::Plus => '+',
//             BinOp::Minus => '-',
//             BinOp::Mult => '*',
//             BinOp::Div => '/',
//             BinOp::Equal => '=',
//             BinOp::Less => '<',
//             BinOp::Greater => '>',

//             _ => todo!(),
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub enum UnaOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VTypeKind {
    Int,
    Flt,
    Str,
    Bool,
    Fn,
    Custom(String),
    Generic(String),
    Container(Box<VType>),
    Unknown,
}

impl VTypeKind {
    pub fn is_unknown(&self) -> bool {
        match self {
            VTypeKind::Unknown => true,
            _ => false,
        }
    }
}

impl Display for VTypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                VTypeKind::Int => "int",
                VTypeKind::Flt => "flt",
                VTypeKind::Str => "str",
                VTypeKind::Bool => "bool",
                VTypeKind::Fn => "Fn",
                VTypeKind::Custom(c) => &c,
                VTypeKind::Generic(g) => &g,
                VTypeKind::Container(_c) => "array",

                VTypeKind::Unknown => "unknown",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VType {
    pub kind: VTypeKind,
    is_mut: bool,
}

impl VType {
    pub fn new(kind: VTypeKind, is_mut: bool) -> Self {
        VType { kind, is_mut }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pair {
    pub name: String,
    pub value: VType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    //Range(Box<Expr>, Box<Expr>),
    Scalar(Itype),
    Array(Vec<Expr>),
    Variable(String),
    BinaryOp(BinOp, Box<(Expr, Expr)>),
    UnaryOp(UnaOp, Box<Expr>),
    Call {
        name: String,
        on: Option<String>,
        args: Vec<Expr>,
    },
    //Assign(Variable),
    FnExpr(String, Vec<Pair>, Box<Expr>),
    IfExpr(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl ToString for Expr {
    fn to_string(&self) -> String {
        match self {
            Expr::Variable(s) => s.to_string(),
            _ => panic!("Cannot get string of expr {self:?}")
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    // Block(Vec<Stmt>),
    // If(Expr, Box<(Stmt, Option<Stmt>)>),
    // While(Expr, Box<Stmt>),
    // ForEach(String, String, Box<Stmt>),
    // ForRange(String, Expr, Box<Stmt>),
    // Return(Expr),
    // Break,
    // Continue,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Pair,
    pub extends: Option<VType>,
    pub rtype: VType,
    pub args: Vec<Pair>,
    pub code: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub ini: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub funcs: Vec<Function>,
}

impl Function {
    pub fn new(name: String, args: Vec<Pair>, code: Vec<Stmt>, extends: Option<VType>) -> Function {
        if extends.is_none() {
            Function {
                name: Pair {
                    name,
                    value: VType::new(VTypeKind::Fn, false),
                },
                extends,
                rtype: VType {
                    kind: VTypeKind::Unknown,
                    is_mut: false,
                },
                args,
                code: code,
            }
        } else {
            Function {
                name: Pair {
                    name,
                    value: VType::new(VTypeKind::Fn, false),
                },
                extends: extends.clone(),
                rtype: extends.unwrap(),
                args,
                code: code,
            }
        }
    }

    pub fn new_rt(
        name: String,
        args: Vec<Pair>,
        code: Vec<Stmt>,
        extends: Option<VType>,
        rtype: VType,
    ) -> Function {
        if extends.is_none() {
            Function {
                name: Pair {
                    name,
                    value: VType::new(VTypeKind::Fn, false),
                },
                extends,
                rtype,
                args,
                code: code,
            }
        } else {
            panic!("Extension function cannot return a type other than its parent")
        }
    }
}

impl Variable {
    pub fn new(name: String, ini: Expr) -> Variable {
        Variable {
            name,
            ini: Box::new(ini),
        }
    }
}

impl Program {
    pub fn new() -> Program {
        Program { funcs: Vec::new() }
    }
    pub fn add_function(&mut self, f: Function) {
        self.funcs.push(f);
    }
}
