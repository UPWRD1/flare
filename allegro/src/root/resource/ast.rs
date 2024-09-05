#[derive(Debug, Clone)]
pub struct Program {
    pub modules: Vec<Module>,
    pub dependencies: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub body: Vec<Ast>,
}

#[derive(Debug, Clone)]
pub enum Ast {
    FnDef {
        name: String,
        rettype: SymbolType,
        args: Vec<(String, SymbolType)>,
        limits: Option<Vec<FnArgLimit>>,
        body: Vec<Expr>,
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
        include: Vec<Expr>,
    },
}
#[derive(Debug, Clone, PartialEq)]
pub struct FnArgLimit {
    pub name: String,
    pub limit: String,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum LogicOp {
    CEQ,
    CLT,
    CLE,
    CGT,
    CGE,
}

#[derive(Debug, Clone, PartialEq)]
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
        value: Box<Expr>,
    },

    If {
        condition: Box<Expr>,
        then: Box<Expr>,
        otherwise: Box<Expr>,
    },

    // Atomics
    Int(i32),
    Flt(f32),
    Str(String),
    Bool(bool),
    Symbol(String),
    FieldAccess(Vec<Expr>),
    Call {
        name: Box<Expr>,
        args: Vec<Expr>,
        namespace: Vec<Expr>,
    },
}

impl Expr {
    pub fn get_symbol_name(&self) -> String {
        match self {
            Expr::Symbol(s) => s.to_string(),
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum SymbolType {
    Int,
    Flt,
    Str,
    Bool,
    Mut(Box<Self>),
    Fn(Vec<Self>, Box<Self>),
    Naught,
    Unknown,
    Generic(String),
    Obj(Vec<(String, Self)>),
}

impl SymbolType {
    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }

    pub fn is_naught(&self) -> bool {
        matches!(self, Self::Naught)
    }

    pub fn is_generic(&self) -> bool {
        matches!(self, Self::Generic(_))
    }

    pub fn get_generic_name(&self) -> String {
        match self {
            SymbolType::Generic(n) => n.to_string(),
            _ => panic!("{self:?} is not a generic"),
        }
    }

    pub fn is_str(&self) -> bool {
        matches!(self, Self::Str)
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int)
    }

    pub fn is_flt(&self) -> bool {
        matches!(self, Self::Flt)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool)
    }

    pub fn is_fn(&self) -> bool {
        matches!(self, Self::Fn(..))
    }

    pub fn get_args(&self) -> Vec<Self> {
        match self {
            SymbolType::Fn(args, ..) => args.clone(),
            _ => panic!("{self:?} is not a function"),
        }
    }

    pub fn get_rt(&self) -> Self {
        match self {
            Self::Fn(_, ret) => *ret.clone(),
            _ => panic!("{self:?} is not a function"),
        }
    }

    pub fn is_mut(&self) -> bool {
        matches!(self, Self::Mut(_))
    }

    pub fn get_mut(&self) -> Self {
        match self {
            SymbolType::Mut(v) => *v.clone(),
            _ => panic!("{self:?} is not mutable"),
        }
    }

    pub fn get_raw(&self) -> Self {
        match self {
            Self::Naught | Self::Unknown | Self::Int | Self::Flt | Self::Str | Self::Bool => {
                self.clone()
            }
            Self::Mut(t) => t.clone().get_raw(),
            Self::Fn(_, t) => t.clone().get_raw(),
            Self::Generic(_) => self.clone(),
            Self::Obj(_) => todo!(),
        }
    }

    pub fn compare(&self, rhs: Self) -> bool {
        let r = rhs.get_raw();
        match self {
            Self::Int => r.is_int() || r.is_generic() || r.is_unknown(),
            Self::Flt => r.is_flt() || r.is_generic() || r.is_unknown(),
            Self::Str => r.is_str() || r.is_generic() || r.is_unknown(),
            Self::Bool => r.is_bool() || rhs.is_generic() || rhs.is_unknown(),
            Self::Mut(t) | Self::Fn(_, t) => t.compare(rhs),
            Self::Naught | Self::Unknown | Self::Generic(_) => true,
            Self::Obj(_) => todo!(),
        }
    }
}
