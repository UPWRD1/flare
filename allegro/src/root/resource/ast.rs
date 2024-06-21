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
    Number(i64),
    String(String),
    Scalar(Itype),
    Variable(String),
    BinaryOp(BinOp, Box<(Expr, Expr)>),
    UnaryOp(UnaOp, Box<Expr>),
    Call(String, Vec<Expr>),
    Assign(Variable),

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
    funcs: Vec<Function>,
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
