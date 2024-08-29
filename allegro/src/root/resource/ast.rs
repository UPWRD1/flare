use std::rc::Rc;

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
    entries: Vec<SymbolTableEntry>
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTableEntry {
    pub rawname: String,
    pub kind: SymbolTableEntryKind,
}

impl SymbolTableEntry {
    pub fn new(n: String, k: SymbolTableEntryKind) -> Self {
        Self {
            rawname: n,
            kind: k
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolTableEntryKind {
    Variable(SymbolType),
    MutVariable(SymbolType),
    Fn {
        args: Vec<SymbolType>,
        ret: SymbolType,
    }
}

impl SymbolTableEntryKind {
    pub fn get_t(&self) -> SymbolType {
        match self {
            Self::Variable(a) => a.clone(),
            Self::MutVariable(a) => a.clone(),
            Self::Fn {ret, ..} => ret.clone(),
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
    Fn(Vec<RcType>, RcType),
    Naught,
    Unknown,
    Generic(String),
    Struct(Vec<(String, SymbolType)>)
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct RcType {
    pub inner: Rc<SymbolType>,
}

impl From<SymbolType> for RcType {
    fn from(src: SymbolType) -> RcType {
        RcType {
            inner: Rc::new(src),
        }
    }
}

impl SymbolType {
    pub fn is_unknown(&self) -> bool {
        match self {
            SymbolType::Unknown => true,
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
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { scopes: vec![], top: 0 }
    }

    pub fn open_scope(&mut self) {
        self.top += 1;
        self.scopes.push(SymbolTableScope { entries: vec![] })
    } 

    pub fn pop_scope(&mut self) {
        self.top -= 1;
    }

    pub fn insert(&mut self, s: SymbolTableEntry) {
        //dbg!(self.clone());
        self.scopes[self.top - 1].entries.push(s);
    }

    pub fn update(&mut self, s: SymbolTableEntry) {
        for i in self.top..0 {
            let cs = &self.scopes[i];
            for mut j in &cs.entries {
                if j.rawname == s.rawname {
                    j = &s
                }
            }
        }
    }

    pub fn get(&mut self, n: String) -> Option<SymbolTableEntry> {
        for i in &self.scopes {
            for j in &i.entries {
                if j.rawname == n {
                    return Some(j.clone())
                }
            }
        }
        return None;
    }
}