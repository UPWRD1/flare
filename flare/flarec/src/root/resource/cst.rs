// use anyhow::format_err;
// use ordered_float::OrderedFloat;
// use serde::Deserialize;
// use serde::Serialize;
// use std::collections::HashSet;
// use std::fmt::Display;
// use std::hash::DefaultHasher;
// use std::hash::Hash;
// use std::hash::Hasher;

// use crate::root::passes::midend::environment::GenericValue;
// use crate::root::passes::midend::environment::Quantifier;
// use crate::root::passes::midend::environment::UserTypeKind;

// pub fn calculate_hash<T: Hash>(t: &String) -> String {
//     let mut s = DefaultHasher::new();
//     t.hash(&mut s);
//     format!("{:x}", s.finish())
// }

// #[derive(Debug, Clone)]
// pub struct Program {
//     pub modules: Vec<Cst>,
//     pub dependencies: HashSet<String>,
// }

// // #[derive(Debug, Clone, PartialEq, Hash)]
// // pub struct CodeModule {
// //     pub name: String,
// //     pub body: Vec<Cst>,
// // }

// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pub enum Cst {
//     Module {
//         name: String,
//         body: Vec<Self>,
//     },
    
//     // MethodDef {
//     //     parent: String,
//     //     name: String,
//     //     rettype: SymbolType,
//     //     args: Vec<(String, SymbolType)>,
//     //     limits: Option<Vec<Expr>>,
//     //     body: Vec<Expr>,
//     // },
//     // AssocDef {
//     //     parent: String,
//     //     name: String,
//     //     rettype: SymbolType,
//     //     args: Vec<(String, SymbolType)>,
//     //     limits: Option<Vec<Expr>>,
//     //     body: Vec<Expr>,
//     // },
//     Struct {
//         name: String,
//         members: Vec<(String, SymbolType)>,
//     },
//     Enum {
//         name: String,
//         members: Vec<(String, Vec<SymbolType>)>,
//     },
//     DefBlock {
//         name: SymbolType,
//         funcs: Vec<Self>,
//     },
//     WithClause {
//         include: Expr,
//     },
//     ExternClause {
//         name: Expr,
//         args: Vec<SymbolType>,
//         variadic: bool, // TODO: unlimited variadics
//         ret: SymbolType,
//         effect: Option<Expr>,
//     },
//     TypeAlias {
//         name: Expr,
//         is: SymbolType,
//     },
//     Propdef {
//         p: Property,
//     },

//         FnDef {
//         name: Expr,
//         rettype: SymbolType,
//         args: Vec<(Expr, SymbolType)>,
//         limits: Option<Vec<Expr>>,
//         effect: Option<Expr>,
//         //body: Vec<Expr>,
//         body: Expr,

//     },
// }

// impl Cst {
//     pub fn get_fnname(&self) -> String {
//         // match self {
//         //     Self::FnDef { name, .. } => name.to_string(),
//         //     //Self::MethodDef { name, .. } => name.to_string(),

//         //     _ => panic!("{:?} is not a function", self),
//         // }
//         todo!()
//     }

//     pub fn get_module_name(&self) -> String {
//         match self {
//             Self::Module { name, .. } => name.to_string(),
//             //Self::MethodDef { name, .. } => name.to_string(),

//             _ => panic!("{:?} is not a module", self),
//         }
//     }


//     pub fn get_module_body(&self) -> Vec<Self> {
//         match self {
//             Self::Module { name: _ , body } => body.clone(),
//             //Self::MethodDef { name, .. } => name.to_string(),

//             _ => panic!("{:?} is not a module", self),
//         }
//     }


//     // pub fn convert_fn_to_methodfn(&self, parent: String) -> Self {
//     //     match self {
//     //         Self::FnDef {
//     //             name,
//     //             rettype,
//     //             args,
//     //             limits,
//     //             body,
//     //         } => Self::MethodDef {
//     //             parent,
//     //             name: name.to_string(),
//     //             rettype: rettype.clone(),
//     //             args: args.clone(),
//     //             limits: limits.clone(),
//     //             body: body.to_vec(),
//     //         },
//     //         _ => panic!("{:?} is not a function", self),
//     //     }
//     // }
// }

// #[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
// pub struct Property {
//     pub name: String,
//     pub req: Vec<FnSignature>,
// }

// #[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
// pub struct FnSignature {
//     pub name: String,
//     pub rettype: SymbolType,
//     pub args: Vec<SymbolType>,
//     pub limits: Option<Vec<Expr>>,
// }

// #[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
// pub enum LogicOp {
//     CEQ,
//     CLT,
//     CLE,
//     CGT,
//     CGE,
//     Is,
//     And,
//     Or
// }
// #[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
// pub enum Predicate {
//     Comparison {
//         op: LogicOp,
//         rhs: Box<Expr>,
//     },
//     Variant {
//         name: String,
//         membervars: Vec<String>,
//     },
//     Wildcard
// }

// #[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
// pub enum Expr {
//     BinAdd (Box<Self>, Box<Self>),
//     BinSub (Box<Self>, Box<Self>),
//     BinMul (Box<Self>, Box<Self>),
//     BinDiv (Box<Self>, Box<Self>),
//     Logical {
//         l: Box<Self>,
//         op: LogicOp,
//         r: Box<Self>,
//     },
//     Assignment {
//         name: Box<Self>,
//         value: Box<Self>,
//         and_in: Box<Self>,
//     },
//     Closure {
//         args: Vec<(String, SymbolType)>,
//         body: Vec<Self>,
//     },
//     SeqComp {
//         l: Box<Self>,
//         r: Box<Self>,
//     },
//     If {
//         condition: Box<Self>,
//         then: Box<Self>,
//         otherwise: Box<Self>,
//     },
//     Match {
//         matchee: Box<Self>,
//         arms: Vec<(Predicate, Self)>,
//     },
//     // Atomics
//     Naught,
//     // Int(i32),
//     // Uint(u32),
//     // Word(usize),
//     // Byte(u8),
//     Num(OrderedFloat<f64>),
//     Str(String),
//     Char(char),
//     Bool(bool),
//     AddressOf(Box<Self>),
//     Symbol(String),
//     Selff,
//     StructInstance {
//         name: Box<Self>,
//         fields: Vec<(String, Self)>,
//     },
//     VariantInstance {
//         name: Box<Self>,
//         fields: Vec<Self>,
//     },
//     FieldAccess(Box<Self>, Box<Self>),
//     Path(Box<Self>, Box<Self>),
//     Apply {
//         func: Box<Self>,
//         arg: Box<Self>,
//     },
//     Call {
//         name: Box<Self>,
//         args: Vec<Self>,
//     },
//     MethodCall {
//         obj: Box<Self>,
//         name: Box<Self>,
//         args: Vec<Self>,
//     },
//     ModuleCall {
//         module: Box<Self>,
//         name: Box<Self>,
//         args: Vec<Self>,
//     },
//     LetDef {
//         lhs: Box<Self>,
//         rhs: Box<Self>,
//         then: Box<Self>,

//     },
//     FnDef {
//         name: Box<Self>,
//         rettype: SymbolType,
//         args: Vec<(Expr, SymbolType)>,
//         limits: Option<Vec<Expr>>,
//         effect: Option<Box<Self>>,
//         //body: Vec<Expr>,
//         body: Box<Self>,

//     },
// }

// impl Expr {
//     pub fn get_symbol_name(&self) -> Option<String> {
//         match self {
//             Expr::Symbol(s) => Some(s.to_string()),
//             Expr::FieldAccess(_s, f) => Some(f.get_symbol_name()?),
//             Expr::Num(i) => Some(i.to_string()),
//             _ => None //Err(format_err!("{self:?} is not a symbol")) //panic!("{self:?} is not a symbol"),
//         }
//     }

//     pub fn get_assignment(&self) -> (String, Self) {
//         match self {
//             Expr::Assignment { name, value, and_in: _ } => (name.get_symbol_name().expect("cannot assign to a non-symbol"), *value.clone()),
//             _ => panic!(),
//         }
//     }

//     pub fn get_callee(&self) -> String {
//         match self {
//             Expr::Call { name, .. } => name.get_symbol_name().unwrap(),
//             _ => panic!(),
//         }
//     }

//     pub fn get_call_args(&self) -> Vec<Self> {
//         match self {
//             Expr::Call { name: _, args, .. } => args.to_vec(),
//             _ => panic!(),
//         }
//     }

//     pub fn get_parent_name(&self) -> String {
//         match self {
//             Expr::MethodCall { obj, .. } => obj.get_parent_name(),
//             Expr::Symbol(s) => s.clone(),
//             Expr::StructInstance { name, fields } => name.get_parent_name(),
//             Expr::If { condition, then, otherwise } => then.get_parent_name(),
//             _ => panic!("{self:?}"),
//         }
//     }

//     pub fn get_lit(&self) -> String {
//         match self {
//             Self::Str(s) => s
//                 .strip_prefix('"')
//                 .unwrap()
//                 .strip_suffix('"')
//                 .unwrap()
//                 .to_string(),
//             _ => panic!("{:?} is not a string literal", self),
//         }
//     }

//     pub fn is_constant(&self) -> bool {
//         match self {
//             Expr::Num(_) => true,
//             Expr::Str(_) => true,
//             Expr::Char(_) => true,
//             Expr::Bool(_) => true,
//             _ => false,
//         }
//     }

//     pub fn get_fields(&self) -> Vec<(String, Self)>{
//         if let Expr::StructInstance { name: _, fields } = self {
//             fields.to_vec()
//         } else {
//             panic!("{self:?}")
//         }
//     }

//     pub fn get_numeric(&self) -> Option<OrderedFloat<f64>> {
//         match self {
//             Expr::Num(v) => Some(v.clone()),
//             _ => None,
//         }
//     }

//     pub fn get_variant_name(&self) -> String {
//         if let Expr::VariantInstance { name, fields: _ } = self {
//             name.get_symbol_name().unwrap()
//                 } else {
//             panic!("{self:?}")
//         }
//     }

//     pub fn get_variant_fields(&self) -> Vec<String> {
//         if let Expr::VariantInstance { name: _, fields } = self {
//             fields.clone().iter().map(|x| x.get_symbol_name().unwrap()).collect()
//         } else {
//             panic!("{self:?}")
//         }

//     }

// }

// impl Display for Expr {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         todo!()
//     }
// }

// impl From<Box<Expr>> for Expr {
//     fn from(value: Box<Expr>) -> Self {
//         *value
//     }
// }

// impl std::ops::Add for Expr {
//     type Output = Self;

//     fn add(self, rhs: Self) -> Self::Output {
//         match (self, rhs) {
//             (Expr::Num(l), Expr::Num(r)) => Expr::Num(l + r),
//             _ => panic!()
//         }
//     }
// }

// impl std::ops::Sub for Expr {
//     type Output = Self;

//     fn sub(self, rhs: Self) -> Self::Output {
//         match (self, rhs) {
//             (Expr::Num(l), Expr::Num(r)) => Expr::Num(l - r),
//             _ => panic!()
//         }
//     }
// }

// impl std::ops::Mul for Expr {
//     type Output = Self;

//     fn mul(self, rhs: Self) -> Self::Output {
//         match (self, rhs) {
//             (Expr::Num(l), Expr::Num(r)) => Expr::Num(l * r),
//             _ => panic!()
//         }
//     }
// }

// impl std::ops::Div for Expr {
//     type Output = Self;

//     fn div(self, rhs: Self) -> Self::Output {
//         match (self, rhs) {
//             (Expr::Num(l), Expr::Num(r)) => Expr::Num(l / r),
//             _ => panic!()
//         }
//     }
// }


// #[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
// pub enum SymbolType {
//     // Int,
//     // Usize,
//     // Word,
//     // Byte,
//     Num,
//     Str,
//     Char,
//     Bool,
//     Mut(Box<Self>),
//     Fn(Vec<Self>, Box<Self>), // bool if is variant constructor
//     MethodFn { parent: String, f: Box<Self> },
//     Unit,
//     Selff,
//     Pointer(Box<Self>),
//     Unknown,
//     Generic(GenericValue),
//     Quant(Quantifier),
//     Custom(String, Vec<Self>),
//     //User(UserTypeKind),
//     //Enum(String, Vec<Self>),
//     //Variant(String, Vec<Self>),
//     //EnumInstance(Box<Self>, Box<Self>),
//     Property,
//     TypeDefSelf,
// }

// #[derive(Debug, Clone)]
// pub struct RecordDef {
//     pub name: String,
//     pub fields: Vec<(String, SymbolType)>,
// }

// impl SymbolType {
//     pub fn is_unknown(&self) -> bool {
//         matches!(self, Self::Unknown)
//     }

//     pub fn is_unit(&self) -> bool {
//         matches!(self, Self::Unit)
//     }

//     pub fn is_generic(&self) -> bool {
//         matches!(self, Self::Generic(_))
//     }

//     pub fn get_generic_name(&self) -> String {
//         match self {
//             SymbolType::Generic(n) => match n {
//                 GenericValue::Ref(n) => n.to_string(),
//                 GenericValue::Perfect(_n, _symbol_type) => panic!(),
//             },
//             _ => panic!("{self:?} is not a generic"),
//         }
//     }

//     pub fn get_generic_value(&self) -> GenericValue {
//         match self {
//             SymbolType::Generic(v) => match v {
//                 GenericValue::Ref(n) => panic!(),
//                 GenericValue::Perfect(_n, _symbol_type) => return v.clone(),            },
//             _ => panic!("{self:?} is not a generic"),
//         }
//     }


//     // pub fn is_str(&self) -> bool {
//     //     matches!(self, Self::Str)
//     // }

//     pub fn is_num(&self) -> bool {
//         matches!(self, Self::Num)
//     }



//     pub fn is_bool(&self) -> bool {
//         matches!(self, Self::Bool)
//     }

//     pub fn is_custom(&self) -> bool {
//         matches!(self, Self::Custom(..))
//     }

//     pub fn is_self(&self) -> bool {
//         matches!(self, Self::Selff)
//     }

//     pub fn get_custom_name(&self) -> String {
//         match self.extract() {
//             Self::Custom(v, ..) => v.to_string(),
//             // Self::StructRef(v, ..) => v.to_string(),
//             //Self::Enum(v, ..) => v.to_string(),

//             _ => panic!("{self:?} is not a custom type"),
//         }
//     }

//     pub fn get_custom_generics(&self) -> Vec<Self> {
//         match self {
//             Self::Custom(_, v) => v.clone(),
//             _ => panic!("{self:?} is not a custom type"),
//         }
//     }

//     pub fn is_fn(&self) -> bool {
//         matches!(self, Self::Fn(..)) || matches!(self, Self::MethodFn { .. })
//     }

//     pub fn get_args(&self) -> Vec<Self> {
//         match self {
//             SymbolType::Fn(args, ..) => args.clone(),
//             SymbolType::MethodFn { parent: _, f } => f.get_args(),
//             _ => panic!("{self:?} is not a function"),
//         }
//     }

//     pub fn get_parent(&self) -> String {
//         match self {
//             SymbolType::MethodFn { parent: s, f: _ } => s.to_string(),
//             _ => panic!("{self:?} is not a method"),
//         }
//     }

//     pub fn extract(&self) -> Self {
//         match self {
//             SymbolType::Num
//             | SymbolType::Str
//             | SymbolType::Char
//             | SymbolType::Bool
//             | SymbolType::Unit
//             | SymbolType::Generic(..) => self.clone(),
//             // | SymbolType::StructRef(..)
//             // | SymbolType::Enum(..)
//             // | SymbolType::Variant(_, _) => self.clone(),
//             SymbolType::Fn(.., _rt, _v) => {
//                     self.clone()
                
//             }
//             SymbolType::Mut(t) => t.extract(),
//             SymbolType::Unknown => self.clone(),
//             SymbolType::Pointer(t) => t.extract(),
//             Self::Custom(..) => self.clone(),
//             Self::Quant(..) => self.clone(),

//             _ => todo!("{:?}", self),
//         }
//     }
//     #[must_use]
//     pub fn get_rt(&self) -> Self {
//         match self {
//             Self::Fn(_, ret) => *ret.clone(),
//             Self::MethodFn { parent: _, f } => f.get_rt(),
//             _ => panic!("{self:?} is not a function"),
//         }
//     }

//     pub fn is_mut(&self) -> bool {
//         matches!(self, Self::Mut(_))
//     }

//     pub fn get_mut(&self) -> Self {
//         match self {
//             SymbolType::Mut(v) => *v.clone(),
//             _ => panic!("{self:?} is not mutable"),
//         }
//     }

//     // pub fn is_obj(&self) -> bool {
//     //     matches!(self.extract(), Self::StructRef(..))
//     // }

//     // pub fn get_members(&self) -> Vec<(String, Self)> {
//     //     match self.extract() {
//     //         SymbolType::StructRef(_n, v) => v.clone(),
//     //         _ => panic!("{self:?} is not an object"),
//     //     }
//     // }

//     // pub fn get_obj_name(&self) -> String {
//     //     match self.extract() {
//     //         SymbolType::StructRef(n, ..) => n.clone(),
//     //         _ => panic!("{self:?} is not an object"),
//     //     }
//     // }

//     // pub fn is_enum(&self) -> bool {
//     //     matches!(self, Self::Enum(..))
//     // }

//     // pub fn get_variants(&self) -> Vec<Self> {
//     //     match self.extract() {
//     //         SymbolType::Enum(_, v) => v.clone(),
//     //         _ => panic!("{self:?} is not an enum"),
//     //     }
//     // }

//     pub fn get_generic_count(&self) -> usize {
//         match self.extract() {
//             //SymbolType::Enum(_, c, ..) => c,
//             SymbolType::Custom(_, x) => x.len(),
//             _ => panic!("{self:?} is not an custom type"),
//         }
//     }

//     // pub fn is_variant(&self) -> bool {
//     //     matches!(self, Self::Variant(..))
//     // }

//     // pub fn get_variant_name(&self) -> String {
//     //     match self {
//     //         SymbolType::Variant(n, ..) => n.clone(),
//     //         _ => panic!("{self:?} is not a variant"),
//     //     }
//     // }

//     // pub fn get_variant_members(&self) -> Vec<Self> {
//     //     match self {
//     //         SymbolType::Variant(_, v) => v.clone(),
//     //         _ => panic!("{self:?} is not a variant"),
//     //     }
//     // }

//     pub fn is_pointer(&self) -> bool {
//         matches!(self, Self::Pointer(..))
//     }

//     pub fn get_pointee(&self) -> Self {
//         match self {
//             SymbolType::Pointer(t) => *t.clone(),
//             _ => panic!("{self:?} is not a pointer"),
//         }
//     }

//     pub fn get_quant(&self) -> Quantifier {
//         match self {
//             SymbolType::Quant(q) => q.clone(),
//             _ => panic!("{self:?} is not a quantified type"),
//         }
//     }

//     pub fn get_raw(&self) -> Self {
//         match self {
//             Self::Unit | Self::Unknown | Self::Num | Self::Str | Self::Bool => {
//                 self.clone()
//             }
//             Self::Mut(t) => t.clone().get_raw(),
//             Self::Fn(_, t ) => t.clone().get_raw(),
//             Self::Generic(_) => self.clone(),
//             Self::Custom(..) => panic!("Custom type here!"),
//             // Self::StructRef(..) | Self::Variant(..) => todo!(),
//             _ => todo!(),
//         }
//     }

//     //     pub fn compare(&self, rhs: &Self) -> bool {
//     //         //println!("{:?} vs {:?}", self, rhs);
//     //         let r = rhs.clone(); //.get_raw();
//     //         match self {
//     //             Self::Mut(t) | Self::Fn(_, t, _) => t.compare(&r),
//     //             Self::Naught | Self::Unknown | Self::Generic(_) => true,
//     //             Self::StructRef(_ns) => {
//     //                 if r.is_obj() {
//     //                     for e in r.get_members().iter().enumerate() {
//     //                         if m[e.0].1.compare(&e.1 .1) {
//     //                             continue;
//     //                         } else {
//     //                             return false;
//     //                         }
//     //                     }
//     //                     return true;
//     //                 } else {
//     //                     return false;
//     //                 }
//     //             }
//     //             Self::Custom(name, v) => {
//     //                 if r.is_custom() {
//     //                     return *self == r;
//     //                 }
//     //                 if r.is_variant() {
//     //                     if r.get_variant_name() == *name {
//     //                         for arg in v.iter().enumerate() {
//     //                             let g = rhs.get_custom_generics();
//     //                             if *arg.1 == g[arg.0] {
//     //                                 continue;
//     //                             } else {
//     //                                 return false;
//     //                             }
//     //                         }
//     //                         true
//     //                     } else {
//     //                         false
//     //                     }
//     //                 } else if r.is_enum() {
//     //                     r.get_generic_count() == v.len()
//     //                 } else {
//     //                     false
//     //                 }
//     //             }
//     //             Self::Variant(..) => {
//     //                 r.is_enum() && r.get_variants().contains(self) || r.is_variant() && r == *self
//     //             }
//     //             Self::Enum(_, _, v) => {
//     //                 if r.is_fn() && r.get_rt().is_variant() {
//     //                     for var in v {
//     //                         if !var.get_variant_members().is_empty()
//     //                             && !r.get_rt().get_variant_members().is_empty()
//     //                         {
//     //                             for varg in var.get_variant_members().iter().enumerate() {
//     //                                 if varg.1.compare(&r.get_rt().get_variant_members()[varg.0]) {
//     //                                     continue;
//     //                                 } else {
//     //                                     return false;
//     //                                 }
//     //                             }
//     //                         }
//     //                     }
//     //                     true
//     //                 } else if r.is_enum() {
//     //                     r.get_variants() == self.get_variants()
//     //                 } else {
//     //                     false
//     //                 }
//     //             }
//     //             Self::Property => panic!("Cannot compare properties"),
//     //             Self::Pointer(t) => t.compare(rhs),
//     //             _ => {
//     //                 let _s = self.clone();
//     //                 return matches!(r.clone(), _s) || r.is_generic() || r.is_unknown();
//     //             }
//     //         }
//     //     }
// }

// impl Display for SymbolType {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let the_string = match self {
//             SymbolType::Num => "num",
//             SymbolType::Str => "str",
//             SymbolType::Bool => "bool",
//             SymbolType::Char => "char",
//             SymbolType::Selff => "self",

//             SymbolType::Mut(_) => todo!(),
//             SymbolType::Fn(_, _) => todo!(),
//             SymbolType::MethodFn { .. } => todo!(),
//             SymbolType::Unit => "unit",
//             SymbolType::Pointer(t) => Box::leak(Box::new(format!("pointer:{}", t))),
//             SymbolType::Unknown => todo!(),
//             SymbolType::Generic(name) => match name {
//                 GenericValue::Ref(n) => n,
//                 GenericValue::Perfect(_n, _symbol_type) => todo!(),
//             },
//             SymbolType::Quant(q) => Box::leak(Box::new(format!("{q}"))),
//             SymbolType::Custom(n, _v) => Box::leak(Box::new(format!("{n}"))),
//             // SymbolType::Enum(_, _) => todo!(),
//             // SymbolType::Variant(_, _) => todo!(),
//             SymbolType::Property => todo!(),
//             _ => todo!("{self:?}"),
//         };
//         write!(f, "{}", the_string)
//     }
// }
