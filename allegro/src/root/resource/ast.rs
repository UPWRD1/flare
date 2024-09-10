use std::hash::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;

use serde::Deserialize;
use serde::Serialize;
use thin_vec::ThinVec;

pub fn calculate_hash<T: Hash>(t: &String) -> String {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    format!("{:x}", s.finish())
}

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
        args: ThinVec<(String, SymbolType)>,
        limits: Option<Vec<FnArgLimit>>,
        body: Vec<Expr>,
    },
    Struct {
        name: String,
        members: ThinVec<(String, SymbolType)>,
    },
    Enum {
        name: String,
        members: ThinVec<SymbolType>,
    },
    TypeDef {
        name: SymbolType,
        funcs: ThinVec<Self>,
    },
    WithClause {
        include: Vec<Expr>,
    },
}
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FnArgLimit {
    pub name: String,
    pub limit: String,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub enum LogicOp {
    CEQ,
    CLT,
    CLE,
    CGT,
    CGE,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
    StructInstance {
        name: Box<Expr>,
        fields: Vec<Expr>,
    },
    FieldAccess(Vec<Expr>),
    Call {
        name: Box<Expr>,
        args: Vec<Expr>,
    },
}

impl Expr {
    pub fn get_symbol_name(&self) -> String {
        match self {
            Expr::Symbol(s) => s.to_string(),
            _ => panic!(),
        }
    }

    pub fn get_assignment(&self) -> (String, Self) {
        match self {
            Expr::Assignment { name, value } => (name.get_symbol_name(), *value.clone()),
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub enum SymbolType {
    Int,
    Flt,
    Str,
    Bool,
    Mut(Box<Self>),
    Fn(ThinVec<Self>, Box<Self>),
    Naught,
    Unknown,
    Generic(String),
    Custom(String, ThinVec<Self>),
    Obj(ThinVec<(String, Self)>),
    Enum(usize, ThinVec<Self>),
    Variant(String, ThinVec<Self>),
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

    pub fn is_custom(&self) -> bool {
        matches!(self, Self::Custom(..))
    }

    pub fn get_custom_name(&self) -> String {
        match self {
            Self::Custom(v, ..) => v.to_string(),
            _ => panic!("{self:?} is not a custom type"),
        }
    }

    pub fn get_custom_generics(&self) -> ThinVec<Self> {
        match self {
            Self::Custom(_, v) => v.clone(),
            _ => panic!("{self:?} is not a custom type"),
        }
    }

    pub fn is_fn(&self) -> bool {
        matches!(self, Self::Fn(..))
    }

    pub fn get_args(&self) -> ThinVec<Self> {
        match self {
            SymbolType::Fn(args, ..) => args.clone(),
            _ => panic!("{self:?} is not a function"),
        }
    }

    pub fn extract(&self) -> Self {
        match self {
            SymbolType::Int
            | SymbolType::Flt
            | SymbolType::Str
            | SymbolType::Naught
            | SymbolType::Generic(..)
            | SymbolType::Obj(_)
            | SymbolType::Enum(..)
            | SymbolType::Variant(_, _)
            | SymbolType::Bool => self.clone(),
            SymbolType::Mut(t) => t.extract(),
            SymbolType::Fn(_, t) => t.extract(),
            SymbolType::Unknown => panic!(),
            SymbolType::Custom(_, _) => todo!(),
        }
    }
    #[must_use]
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

    pub fn is_obj(&self) -> bool {
        matches!(self, Self::Obj(..))
    }

    pub fn get_members(&self) -> ThinVec<(String, Self)> {
        match self {
            SymbolType::Obj(v) => v.clone(),
            _ => panic!("{self:?} is not an object"),
        }
    }

    pub fn is_enum(&self) -> bool {
        matches!(self, Self::Enum(..))
    }

    pub fn get_variants(&self) -> ThinVec<Self> {
        match self {
            SymbolType::Enum(_,v) => v.clone(),
            _ => panic!("{self:?} is not an enum"),
        }
    }

    pub fn get_generic_count(&self) -> usize {
        match self {
            SymbolType::Enum(c, ..) => c.clone(),
            _ => panic!("{self:?} is not an enum"),
        }
    }

    pub fn is_variant(&self) -> bool {
        matches!(self, Self::Variant(..))
    }

    pub fn get_variant_name(&self) -> String {
        match self {
            SymbolType::Variant(n, ..) => n.clone(),
            _ => panic!("{self:?} is not a variant"),
        }
    }

    pub fn get_variant_members(&self) -> ThinVec<Self> {
        match self {
            SymbolType::Variant(_, v) => v.clone(),
            _ => panic!("{self:?} is not a variant"),
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
            Self::Custom(..) => panic!("Custom type here!"),
            Self::Obj(_) | Self::Variant(..) => todo!(),
            Self::Enum(..) => todo!(),
        }
    }

    pub fn compare(&self, rhs: &Self) -> bool {
        //println!("{:?} vs {:?}", self, rhs);
        let r = rhs.clone(); //.get_raw();
        match self {
            Self::Int => r.is_int() || r.is_generic() || r.is_unknown(),
            Self::Flt => r.is_flt() || r.is_generic() || r.is_unknown(),
            Self::Str => r.is_str() || r.is_generic() || r.is_unknown(),
            Self::Bool => r.is_bool() || r.is_generic() || r.is_unknown(),
            Self::Mut(t) | Self::Fn(_, t) => t.compare(&r),
            Self::Naught | Self::Unknown | Self::Generic(_) => true,
            Self::Obj(m) => {
                if r.is_obj() {
                    for e in r.get_members().iter().enumerate() {
                        if m[e.0].1.compare(&e.1.1) {
                            continue
                        } else {
                            return false
                        }
                    }
                    return true
                } else {
                    return false
                }
            },
            Self::Custom(name, v) => {
                if r.get_variant_name() == *name {
                    for arg in v.iter().enumerate() {
                        let g = rhs.get_custom_generics();
                        if *arg.1 == g[arg.0] {
                            continue;
                        } else {
                            return false;
                        }
                    }
                    true
                } else if r.is_enum() {
                    r.get_generic_count() == v.len()
                } else {
                    false
                }
            }
            Self::Variant(..) => r.is_enum() && r.get_variants().contains(self) || r.is_variant() && r == *self,
            Self::Enum(_, v) => {
                if r.is_fn() && r.get_rt().is_variant() {
                    for var in v {
                        if !var.get_variant_members().is_empty()
                            && !r.get_rt().get_variant_members().is_empty()
                        {
                            for varg in var.get_variant_members().iter().enumerate() {
                                if varg
                                    .1
                                    .compare(&r.get_rt().get_variant_members()[varg.0])
                                {
                                    continue;
                                } else {
                                    return false;
                                }
                            }
                        }
                    }
                    true
                } else if r.is_enum() {
                    r.get_variants() == self.get_variants()
                } else{
                
                    false
                }
                
            }
        }
    }
}
