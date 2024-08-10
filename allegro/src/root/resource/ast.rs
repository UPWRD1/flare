use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    pub modules: Vec<Module>,
    pub dependencies: Vec<String>
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub body: Vec<Ast>
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Ast {
    FnDef {
        name: String,
        args: Vec<String>,
        body: Vec<Expr>
    },
    WithClause {
        include: Vec<Expr>
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Expr {
    BinAdd {
        l: Box<Expr>,
        r: Box<Expr>,
    },
    BinSub {
        l: Box<Expr>,
        r: Box<Expr>,
    },
    BinMul {
        l: Box<Expr>,
        r: Box<Expr>,
    },
    BinDiv {
        l: Box<Expr>,
        r: Box<Expr>,
    },
    Assignment {
        name: Box<Expr>,
        value: Box<Expr>,
    },
    MutableAssignment {
        name: Box<Expr>,
        value: Box<Expr>,
    },
    Closure {
        args: Vec<String>,
        body: Vec<Expr>,
    },
    Return {
        value: Box<Expr>
    },
    // Atomics
    Int(i32),
    Flt(f32),
    Str(String),
    Bool(bool),
    Symbol(String),
    Call{name: Box<Expr>, args: Vec<Expr>, namespace: Vec<Expr>},
}
impl Expr {
    pub fn get_symbol_name(&self) -> String {
        match self {
            Expr::Symbol(s) => s.to_string(),
            _ => panic!()
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
pub struct SymbolTableEntry {
    pub rawname: String,
    pub value: ASTType,
    pub version: usize,
}
#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
pub enum ASTType {
    Int,
    Flt,
    Str,
    Bool,
    Fn,
}