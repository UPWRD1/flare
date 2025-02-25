use anyhow::format_err;
use ordered_float::OrderedFloat;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashSet;
use std::fmt::Display;
use std::hash::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;

use crate::root::passes::midend::environment::GenericValue;
use crate::root::passes::midend::environment::UserTypeKind;

pub fn calculate_hash<T: Hash>(t: &String) -> String {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    format!("{:x}", s.finish())
}

#[derive(Debug, Clone)]
pub struct Program {
    pub modules: Vec<FileModule>,
    pub dependencies: HashSet<String>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct FileModule {
    pub name: String,
    pub body: Vec<Ast>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ast {
    FnDef {
        name: String,
        rettype: SymbolType,
        args: Vec<(String, SymbolType)>,
        limits: Option<Vec<Expr>>,
        body: Vec<Expr>,
    },
    MethodDef {
        parent: String,
        name: String,
        rettype: SymbolType,
        args: Vec<(String, SymbolType)>,
        limits: Option<Vec<Expr>>,
        body: Vec<Expr>,
    },
    Struct {
        name: String,
        members: Vec<(String, SymbolType)>,
    },
    Enum {
        name: String,
        members: Vec<SymbolType>,
    },
    // TypeDef {
    //     name: SymbolType,
    //     funcs: Vec<Self>,
    // },
    WithClause {
        include: Vec<Expr>,
    },
    ExternClause {
        name: String,
        args: Vec<SymbolType>,
        variadic: bool, // TODO: unlimited variadics
        ret: SymbolType,
    },
    TypeAlias {
        name: String,
        is: SymbolType,
    },
    Propdef {
        p: Property,
    },
}

impl Ast {
    pub fn get_fnname(&self) -> String {
        match self {
            Self::FnDef { name, .. } => name.to_string(),
            Self::MethodDef { name, .. } => name.to_string(),

            _ => panic!("{:?} is not a function", self),
        }
    }

    pub fn convert_fn_to_methodfn(&self, parent: String) -> Self {
        match self {
            Self::FnDef {
                name,
                rettype,
                args,
                limits,
                body,
            } => Self::MethodDef {
                parent,
                name: name.to_string(),
                rettype: rettype.clone(),
                args: args.clone(),
                limits: limits.clone(),
                body: body.to_vec(),
            },
            _ => panic!("{:?} is not a function", self),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Property {
    pub name: String,
    pub req: Vec<FnSignature>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct FnSignature {
    pub name: String,
    pub rettype: SymbolType,
    pub args: Vec<SymbolType>,
    pub limits: Option<Vec<Expr>>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub enum LogicOp {
    CEQ,
    CLT,
    CLE,
    CGT,
    CGE,
    Is,
}
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Predicate {
    Comparison {
        op: LogicOp,
        rhs: Box<Expr>,
    },
    Variant {
        name: String,
        membervars: Vec<String>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Expr {
    BinAdd {
        l: Box<Self>,
        r: Box<Self>,
    },
    BinSub {
        l: Box<Self>,
        r: Box<Self>,
    },
    BinMul {
        l: Box<Self>,
        r: Box<Self>,
    },
    BinDiv {
        l: Box<Self>,
        r: Box<Self>,
    },
    Logical {
        l: Box<Self>,
        op: LogicOp,
        r: Box<Self>,
    },
    Assignment {
        name: Box<Self>,
        value: Box<Self>,
    },
    Closure {
        args: Vec<(String, SymbolType)>,
        body: Vec<Self>,
    },
    Composition {
        l: Box<Self>,
        r: Box<Self>,
    },
    Return {
        value: Box<Self>,
    },
    If {
        condition: Box<Self>,
        then: Box<Self>,
        otherwise: Box<Self>,
    },
    Match {
        matchee: Box<Self>,
        arms: Vec<(Predicate, Self)>,
    },
    // Atomics
    Naught,
    Int(i32),
    Uint(u32),
    Word(usize),
    Byte(u8),
    Flt(OrderedFloat<f32>),
    Str(String),
    Char(char),
    Bool(bool),
    AddressOf(Box<Self>),
    Symbol(String),
    StructInstance {
        name: Box<Self>,
        fields: Vec<(String, Self)>,
    },
    VariantInstance {
        name: Box<Self>,
        fields: Vec<Self>,
    },
    FieldAccess(Box<Self>, String),
    Path(Box<Self>, String),

    Call {
        name: Box<Self>,
        args: Vec<Self>,
    },
    MethodCall {
        obj: Box<Self>,
        name: Box<Self>,
        args: Vec<Self>,
    },
    ModuleCall {
        module: Box<Self>,
        name: Box<Self>,
        args: Vec<Self>,
    },
}

impl Expr {
    pub fn get_symbol_name(&self) -> anyhow::Result<String> {
        match self {
            Expr::Symbol(s) => Ok(s.to_string()),
            Expr::FieldAccess(_s, f) => Ok(f.to_string()),
            _ => Err(format_err!("{self:?} is not a symbol")) //panic!("{self:?} is not a symbol"),
        }
    }

    pub fn get_assignment(&self) -> (String, Self) {
        match self {
            Expr::Assignment { name, value } => (name.get_symbol_name().expect("cannot assign to a non-symbol"), *value.clone()),
            _ => panic!(),
        }
    }

    pub fn get_callee(&self) -> String {
        match self {
            Expr::Call { name, .. } => name.get_symbol_name().unwrap(),
            _ => panic!(),
        }
    }

    pub fn get_call_args(&self) -> Vec<Self> {
        match self {
            Expr::Call { name: _, args, .. } => args.to_vec(),
            _ => panic!(),
        }
    }

    pub fn get_parent_name(&self) -> String {
        match self {
            Expr::MethodCall { obj, .. } => obj.get_parent_name(),
            Expr::Symbol(s) => s.clone(),
            _ => panic!(),
        }
    }

    pub fn get_lit(&self) -> String {
        match self {
            Self::Str(s) => s
                .strip_prefix('"')
                .unwrap()
                .strip_suffix('"')
                .unwrap()
                .to_string(),
            _ => panic!("{:?} is not a string literal", self),
        }
    }
}

impl From<Box<Expr>> for Expr {
    fn from(value: Box<Expr>) -> Self {
        *value
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub enum SymbolType {
    Int,
    Uint,
    Word,
    Byte,
    Flt,
    Str,
    Char,
    Bool,
    Mut(Box<Self>),
    Fn(Vec<Self>, Box<Self>, bool), // bool if is variant constructor
    MethodFn { parent: String, f: Box<Self> },
    Naught,
    Pointer(Box<Self>),
    Unknown,
    Generic(GenericValue),
    Custom(String, Vec<Self>),
    User(UserTypeKind),
    StructRef(String),
    Enum(String, Vec<Self>),
    Variant(String, Vec<Self>),
    Property,
    TypeDefSelf,
}

#[derive(Debug, Clone)]
pub struct RecordDef {
    pub name: String,
    pub fields: Vec<(String, SymbolType)>,
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
            SymbolType::Generic(n) => match n {
                GenericValue::Ref(n) => n.to_string(),
                GenericValue::Perfect(_n, _symbol_type) => panic!(),
            },
            _ => panic!("{self:?} is not a generic"),
        }
    }

    pub fn get_generic_value(&self) -> GenericValue {
        match self {
            SymbolType::Generic(v) => match v {
                GenericValue::Ref(n) => panic!(),
                GenericValue::Perfect(_n, _symbol_type) => return v.clone(),            },
            _ => panic!("{self:?} is not a generic"),
        }
    }


    // pub fn is_str(&self) -> bool {
    //     matches!(self, Self::Str)
    // }

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
        match self.extract() {
            Self::Custom(v, ..) => v.to_string(),
            Self::StructRef(v, ..) => v.to_string(),
            //Self::Enum(v, ..) => v.to_string(),

            _ => panic!("{self:?} is not a custom type"),
        }
    }

    pub fn get_custom_generics(&self) -> Vec<Self> {
        match self {
            Self::Custom(_, v) => v.clone(),
            _ => panic!("{self:?} is not a custom type"),
        }
    }

    pub fn is_fn(&self) -> bool {
        matches!(self, Self::Fn(..)) || matches!(self, Self::MethodFn { .. })
    }

    pub fn get_args(&self) -> Vec<Self> {
        match self {
            SymbolType::Fn(args, ..) => args.clone(),
            SymbolType::MethodFn { parent: _, f } => f.get_args(),
            _ => panic!("{self:?} is not a function"),
        }
    }

    pub fn get_parent(&self) -> String {
        match self {
            SymbolType::MethodFn { parent: s, f: _ } => s.to_string(),
            _ => panic!("{self:?} is not a method"),
        }
    }

    pub fn extract(&self) -> Self {
        match self {
            SymbolType::Int
            | SymbolType::Uint
            | SymbolType::Byte
            | SymbolType::Flt
            | SymbolType::Str
            | SymbolType::Char
            | SymbolType::Bool
            | SymbolType::Naught
            | SymbolType::Generic(..)
            | SymbolType::StructRef(..)
            | SymbolType::Enum(..)
            | SymbolType::Variant(_, _) => self.clone(),
            SymbolType::Fn(.., rt, v) => {
                if *v {
                    *rt.clone()
                } else {
                    self.clone()
                }
            }
            SymbolType::Mut(t) => t.extract(),
            SymbolType::Unknown => self.clone(),
            SymbolType::Pointer(t) => t.extract(),
            Self::Custom(..) => self.clone(),
            _ => todo!("{:?}", self),
        }
    }
    #[must_use]
    pub fn get_rt(&self) -> Self {
        match self {
            Self::Fn(_, ret, _) => *ret.clone(),
            Self::MethodFn { parent: _, f } => f.get_rt(),
            _ => panic!("{self:?} is not a function"),
        }
    }

    pub fn is_variant_constructor(&self) -> bool {
        match self {
            SymbolType::Fn(_, _, t) => *t,
            Self::MethodFn { parent: _, f } => f.is_variant_constructor(),
            _ => panic!("{self:?} is not a variant constructor"),
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
        matches!(self.extract(), Self::StructRef(..))
    }

    // pub fn get_members(&self) -> Vec<(String, Self)> {
    //     match self.extract() {
    //         SymbolType::StructRef(_n, v) => v.clone(),
    //         _ => panic!("{self:?} is not an object"),
    //     }
    // }

    pub fn get_obj_name(&self) -> String {
        match self.extract() {
            SymbolType::StructRef(n, ..) => n.clone(),
            _ => panic!("{self:?} is not an object"),
        }
    }

    pub fn is_enum(&self) -> bool {
        matches!(self, Self::Enum(..))
    }

    pub fn get_variants(&self) -> Vec<Self> {
        match self.extract() {
            SymbolType::Enum(_, v) => v.clone(),
            _ => panic!("{self:?} is not an enum"),
        }
    }

    pub fn get_generic_count(&self) -> usize {
        match self.extract() {
            //SymbolType::Enum(_, c, ..) => c,
            SymbolType::Custom(_, x) => x.len(),
            _ => panic!("{self:?} is not an custom type"),
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

    pub fn get_variant_members(&self) -> Vec<Self> {
        match self {
            SymbolType::Variant(_, v) => v.clone(),
            _ => panic!("{self:?} is not a variant"),
        }
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Self::Pointer(..))
    }

    pub fn get_pointee(&self) -> Self {
        match self {
            SymbolType::Pointer(t) => *t.clone(),
            _ => panic!("{self:?} is not a pointer"),
        }
    }

    pub fn get_raw(&self) -> Self {
        match self {
            Self::Naught | Self::Unknown | Self::Int | Self::Flt | Self::Str | Self::Bool => {
                self.clone()
            }
            Self::Mut(t) => t.clone().get_raw(),
            Self::Fn(_, t, _) => t.clone().get_raw(),
            Self::Generic(_) => self.clone(),
            Self::Custom(..) => panic!("Custom type here!"),
            Self::StructRef(..) | Self::Variant(..) => todo!(),
            _ => todo!(),
        }
    }

    //     pub fn compare(&self, rhs: &Self) -> bool {
    //         //println!("{:?} vs {:?}", self, rhs);
    //         let r = rhs.clone(); //.get_raw();
    //         match self {
    //             Self::Mut(t) | Self::Fn(_, t, _) => t.compare(&r),
    //             Self::Naught | Self::Unknown | Self::Generic(_) => true,
    //             Self::StructRef(_ns) => {
    //                 if r.is_obj() {
    //                     for e in r.get_members().iter().enumerate() {
    //                         if m[e.0].1.compare(&e.1 .1) {
    //                             continue;
    //                         } else {
    //                             return false;
    //                         }
    //                     }
    //                     return true;
    //                 } else {
    //                     return false;
    //                 }
    //             }
    //             Self::Custom(name, v) => {
    //                 if r.is_custom() {
    //                     return *self == r;
    //                 }
    //                 if r.is_variant() {
    //                     if r.get_variant_name() == *name {
    //                         for arg in v.iter().enumerate() {
    //                             let g = rhs.get_custom_generics();
    //                             if *arg.1 == g[arg.0] {
    //                                 continue;
    //                             } else {
    //                                 return false;
    //                             }
    //                         }
    //                         true
    //                     } else {
    //                         false
    //                     }
    //                 } else if r.is_enum() {
    //                     r.get_generic_count() == v.len()
    //                 } else {
    //                     false
    //                 }
    //             }
    //             Self::Variant(..) => {
    //                 r.is_enum() && r.get_variants().contains(self) || r.is_variant() && r == *self
    //             }
    //             Self::Enum(_, _, v) => {
    //                 if r.is_fn() && r.get_rt().is_variant() {
    //                     for var in v {
    //                         if !var.get_variant_members().is_empty()
    //                             && !r.get_rt().get_variant_members().is_empty()
    //                         {
    //                             for varg in var.get_variant_members().iter().enumerate() {
    //                                 if varg.1.compare(&r.get_rt().get_variant_members()[varg.0]) {
    //                                     continue;
    //                                 } else {
    //                                     return false;
    //                                 }
    //                             }
    //                         }
    //                     }
    //                     true
    //                 } else if r.is_enum() {
    //                     r.get_variants() == self.get_variants()
    //                 } else {
    //                     false
    //                 }
    //             }
    //             Self::Property => panic!("Cannot compare properties"),
    //             Self::Pointer(t) => t.compare(rhs),
    //             _ => {
    //                 let _s = self.clone();
    //                 return matches!(r.clone(), _s) || r.is_generic() || r.is_unknown();
    //             }
    //         }
    //     }
}

impl Display for SymbolType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let the_string = match self {
            SymbolType::Int => "int",
            SymbolType::Flt => "flt",
            SymbolType::Str => "str",
            SymbolType::Bool => "bool",
            SymbolType::Bool => "char",

            SymbolType::Mut(_) => todo!(),
            SymbolType::Fn(_, _, _) => todo!(),
            SymbolType::MethodFn { .. } => todo!(),
            SymbolType::Naught => "naught",
            SymbolType::Pointer(_) => todo!(),
            SymbolType::Unknown => todo!(),
            SymbolType::Generic(name) => match name {
                GenericValue::Ref(n) => n,
                GenericValue::Perfect(n, _symbol_type) => todo!(),
            },
            SymbolType::Custom(_, _) => todo!(),
            SymbolType::StructRef(_) => todo!(),
            SymbolType::Enum(_, _) => todo!(),
            SymbolType::Variant(_, _) => todo!(),
            SymbolType::Property => todo!(),
            _ => todo!(),
        };
        write!(f, "{}", the_string)
    }
}
