use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Program {
    pub modules: Vec<Module>,
    pub dependencies: Vec<String>
}

#[derive(Debug, Clone)]
pub struct Module {
    pub body: Vec<Ast>
}

#[derive(Debug, Clone)]
pub enum Ast {
    FnDef {
        name: String,
        rettype: SymbolType,
        args: Vec<(String, SymbolType)>,
        limits: Option<Vec<FnArgLimit>>,
        body: Vec<Expr>
    },
    Record {
        name: String,
        members: Vec<(String, SymbolType)>,
    },
    TypeDef {
        name: String,
        funcs: Vec<Self>,
    },
    WithClause {
        include: Vec<Expr>
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct FnArgLimit {
    pub name: String,
    pub limit: String,
}

#[derive(Debug, Clone,  Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum LogicOp {
    CEQ,
    CLT,
    CLE,
    CGT,
    CGE,
}

#[derive(Debug, Clone,  PartialEq)]
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
        args: Vec<(String, SymbolType)>,
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



#[derive(Debug, Clone)]
pub struct SymbolTable {
    scopes: Vec<SymbolTableScope>,
    top: usize,
}

#[derive(Debug, Clone)]
pub struct SymbolTableScope {
    entries: Vec<SymbolTableEntry>,
    pub closed: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTableEntry {
    pub rawname: String,
    pub kind: SymbolType,
}

impl SymbolTableEntry {
    pub fn new(n: String, k: SymbolType) -> Self {
        Self {
            rawname: n,
            kind: k
        }
    }
}


#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum SymbolType {
    Int,
    Flt,
    Str,
    Bool,
    Expr,
    Fn(Vec<SymbolType>, Box<SymbolType>),
    Naught,
    Unknown,
    Generic(String),
    Obj(Vec<(String, SymbolType)>)
}


impl SymbolType {
    pub fn is_unknown(&self) -> bool {
        match self {
            SymbolType::Unknown => true,
            _ => false,
        }
    }

    pub fn is_naught(&self) -> bool {
        match self {
            SymbolType::Naught => true,
            _ => false,
        }
    }

    pub fn is_generic(&self) -> bool {
        match self {
            SymbolType::Generic(_) => true,
            _ => false,
        }
    }

    pub fn get_generic_name(&self) -> String {
        match self {
            SymbolType::Generic(n) => n.to_string(),
            _ => panic!("{self:?} is not a generic")
        }
    }

    pub fn is_str(&self) -> bool {
        match self {
            SymbolType::Str => true,
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            SymbolType::Int => true,
            _ => false,
        }
    }

    pub fn is_flt(&self) -> bool {
        match self {
            SymbolType::Flt => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            SymbolType::Bool => true,
            _ => false,
        }
    }

    pub fn is_fn(&self) -> bool {
        match self {
            SymbolType::Fn(..) => true,
            _ => false,
        }
    }

    pub fn get_args(&self) -> Vec<SymbolType> {
        match self {
            SymbolType::Fn(args, ..) => args.clone(),
            _ => panic!("{self:?} is not a function"),
        }
    }

    pub fn get_rt(&self) -> SymbolType {
        match self {
            SymbolType::Fn(_, ret) => *ret.clone(),
            _ => panic!("{self:?} is not a function"),
        }
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { scopes: vec![], top: 0 }
    }

    pub fn open_scope(&mut self) {
        self.top += 1;
        self.scopes.push(SymbolTableScope { entries: vec![], closed: false})
    } 

    pub fn pop_scope(&mut self) {
        self.scopes[self.top].closed = true;
        self.top -= 1;
    }

    pub fn insert(&mut self, s: SymbolTableEntry)  {
        dbg!(s.clone());
        for i in self.scopes.len()..0 {
            let t = &mut self.scopes[i];
            if !t.closed {
                t.entries.push(s.clone());
                break
            }
        }
        
    }

    pub fn get(&mut self, n: String) -> Option<SymbolTableEntry> {
        for i in &self.scopes {
            if !i.closed {
                for j in &i.entries {
                    if j.rawname == n {
                        return Some(j.clone())
                    }
                }
            }
        }
        return None;
    }
}