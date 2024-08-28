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
        limits: Option<Vec<FnArgLimit>>,
        body: Vec<Expr>
    },
    Record {
        name: String,
        members: Vec<(String, ASTType)>,
    },
    TypeDef {
        name: String,
        funcs: Vec<Self>,
    },
    WithClause {
        include: Vec<Expr>
    }
}
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FnArgLimit {
    pub name: String,
    pub limit: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Hash, Eq)]
pub enum LogicOp {
    CEQ,
    CLT,
    CLE,
    CGT,
    CGE,
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
    Logical {
        l: Box<Expr>,
        op: LogicOp,
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
    Composition {
        l: Box<Expr>,
        r: Box<Expr>,
    },
    Return {
        value: Box<Expr>
    },
    
    If {
        condition: Box<Expr>,
        then: Box<Expr>,
        otherwise: Option<Box<Expr>>
    },

    // Atomics
    Int(i32),
    Flt(f32),
    Str(String),
    Bool(bool),
    Symbol(String),
    FieldAccess(Vec<Expr>),
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolTable {
    scopes: Vec<SymbolTableScope>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolTableScope {
    entries: Vec<SymbolTableEntry>
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
pub struct SymbolTableEntry {
    pub rawname: String,
    pub kind: ASTType,
}
impl SymbolTableEntry {
    pub fn new(n: String, k: ASTType) -> Self {
        Self {
            rawname: n,
            kind: k
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
pub enum ASTType {
    Int,
    Flt,
    Str,
    Bool,
    Expr,
    Fn,
    Unknown,
    Custom(String),
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { scopes: vec![] }
    }

    pub fn open_scope(&mut self) {
        self.scopes.push(SymbolTableScope { entries: vec![] })
    } 

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, s: SymbolTableEntry) {
        dbg!(self.clone());
        let l = self.scopes.clone().len() - 1;
        println!("{l:?}");
        self.scopes[l].entries.push(s);
    }

    pub fn update(&mut self, s: SymbolTableEntry) {
        for i in self.scopes.len()..0 {
            let cs = &self.scopes[i];
            for mut j in &cs.entries {
                if j.rawname == s.rawname {
                    j = &s
                }
            }
        }
    }

    pub fn get(&mut self, n: String) -> Option<SymbolTableEntry> {
        for i in self.scopes.len()..0 {
            let cs = &self.scopes[i];
            dbg(cs.clone());
            for j in &cs.entries {
                if j.rawname == n {
                    return Some(j.clone())
                }
            }
        }
        return None;
    }
}