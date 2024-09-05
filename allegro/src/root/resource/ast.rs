use std::collections::HashMap;

use crate::root::passes::midend::typechecking::Typechecker;

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

pub trait Typecheck<T> {
    fn convert(value: T, t: &mut Typechecker) -> Self;
}

#[derive(Debug, Clone)]
pub struct TypedProgram {
    pub modules: Vec<TypedModule>,
    pub dependencies: Vec<String>,
}

impl From<Program> for TypedProgram {
    fn from(value: Program) -> Self {
        let mut t = Typechecker::new();
        t.s.new_scope();
        let mut vtm: Vec<TypedModule> = vec![];
        let mut vmr = value.modules.clone();
        vmr.reverse();
        for m in vmr {
            let tm = TypedModule::convert(m, &mut t);
            vtm.push(tm);
        }
        Self {
            modules: vtm,
            dependencies: value.dependencies,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedModule {
    pub body: Vec<TypedAst>,
}

impl Typecheck<Module> for TypedModule {
    fn convert(value: Module, t: &mut Typechecker) -> Self {
        let mut b: Vec<TypedAst> = vec![];
        for a in value.body {
            let ta = TypedAst::convert(a.clone(), t);
            b.push(ta)
        }
        Self { body: b }
    }
}

#[derive(Debug, Clone)]
pub enum TypedAst {
    FnDef {
        name: String,
        rettype: SymbolType,
        args: Vec<(String, SymbolType)>,
        limits: Option<Vec<FnArgLimit>>,
        body: Vec<TypedExpr>,
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
        include: Vec<String>,
    },
}

impl Typecheck<Ast> for TypedAst {
    fn convert(value: Ast, t: &mut Typechecker) -> Self {
        match value {
            Ast::FnDef {
                name,
                rettype,
                args,
                limits,
                body,
            } => {
                t.s.set(
                    name.clone(),
                    SymbolType::Fn(
                        args.clone().iter().map(|a| a.1.clone()).collect(),
                        Box::new(rettype.clone()),
                    ),
                );
                t.s.new_scope();
                for a in &args {
                    t.s.set(a.0.clone(), a.1.clone())
                }
                t.currentfunc = name.clone();
                let b: Vec<TypedExpr> = body
                    .iter()
                    .map(|e| TypedExpr::convert(e.clone(), t))
                    .collect();
                t.s.pop_scope();
                // if !t.compare_callargs(&rettype, &b.last().unwrap().t.clone().unwrap()) {
                //     panic!("Return type was not equal to the last expression in function body")
                // }
                Self::FnDef {
                    name,
                    rettype,
                    args,
                    limits,
                    body: b,
                }
            }
            Ast::Record {
                name: _,
                members: _,
            } => todo!(),
            Ast::TypeDef { name: _, funcs: _ } => todo!(),
            Ast::WithClause { include } => Self::WithClause {
                include: include.iter().map(|e| e.get_symbol_name()).collect(),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub e: Expr,
    pub t: Option<SymbolType>,
}

impl Typecheck<Expr> for TypedExpr {
    fn convert(value: Expr, t: &mut Typechecker) -> Self {
        Self {
            e: value.clone(),
            t: Some(t.synth_type(value)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    entries: HashMap<String, SymbolType>,
    parent: Box<Option<Self>>,
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
            _ => panic!("{self:?} is not a generic"),
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

    pub fn is_mut(&self) -> bool {
        match self {
            SymbolType::Mut(_) => true,
            _ => false,
        }
    }

    pub fn get_mut(&self) -> SymbolType {
        match self {
            SymbolType::Mut(v) => *v.clone(),
            _ => panic!("{self:?} is not mutable"),
        }
    }

    pub fn get_raw(&self) -> SymbolType {
        match self {
            SymbolType::Naught
            | SymbolType::Unknown
            | SymbolType::Int
            | SymbolType::Flt
            | SymbolType::Str
            | SymbolType::Bool => self.clone(),
            SymbolType::Mut(t) => t.clone().get_raw(),
            SymbolType::Fn(_, t) => t.clone().get_raw(),
            SymbolType::Generic(t) => todo!(),
            SymbolType::Obj(_) => todo!(),
        }
    }

    pub fn compare(&self, rhs: Self) -> bool {
        match self {
            SymbolType::Int => rhs.is_int(),
            SymbolType::Flt => rhs.is_flt() || rhs.is_generic(),
            SymbolType::Str => rhs.is_str() || rhs.is_generic(),
            SymbolType::Bool => rhs.is_bool() || rhs.is_generic(),
            SymbolType::Mut(_) => self.get_raw().compare(rhs),
            SymbolType::Fn(_, _) => self.get_raw().compare(rhs),
            SymbolType::Naught => false,
            SymbolType::Unknown => false,
            SymbolType::Generic(t) => true,
            SymbolType::Obj(_) => todo!(),
        }
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            parent: Box::new(None),
        }
    }

    pub fn get(&mut self, name: String) -> SymbolType {
        dbg!(self.clone());
        let x = self.entries.get(&name);
        if x.is_none() {
            if self.parent.is_some() {
                self.parent.clone().unwrap().get(name)
            } else {
                panic!("Undefined binding: {}", name)
            }
        } else {
            x.unwrap().clone()
        }
    }

    pub fn set(&mut self, name: String, t: SymbolType) {
        if self.entries.contains_key(&name) {
            if self.get(name.clone()).is_mut() {
                self.entries.insert(name, SymbolType::Mut(Box::new(t)));
            } else {
                panic!("Cannot redefine immutable value {name}")
            }
        } else {
            self.entries.insert(name, t);
        }
    }

    pub fn redefine(&mut self, name: String, nt: SymbolType) {
        if self.entries.contains_key(&name) {
            self.entries.insert(name, nt);
        } else {
            panic!("Cannot redefine {name}, is undefined!")
        }
    }

    pub fn new_scope(&mut self) {
        let temp = self.clone();
        self.parent = Box::new(Some(temp));
        self.entries = HashMap::new();
    }

    pub fn pop_scope(&mut self) {
        let temp = self.parent.clone().unwrap();
        self.entries = temp.entries;
        self.parent = temp.parent;
    }
}

#[derive(Debug, Clone)]
pub struct GenericTable {
    entries: HashMap<String, SymbolType>,
}

impl GenericTable {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    pub fn insert(&mut self, k: String, v: SymbolType) {
        self.entries.insert(k, v);
    }

    pub fn redefine(&mut self, k: String, v: SymbolType) {
        self.entries.remove(&k);
        self.insert(k, v);
    }

}
