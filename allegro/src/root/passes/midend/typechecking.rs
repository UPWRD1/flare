// //use std::ops::Add;

// use std::collections::HashMap;

// use petgraph::{
//     algo::{self}, graph::{DiGraph, NodeIndex}, prelude::{DiGraphMap, GraphMap}, visit::DfsPostOrder, Directed
// };

// use crate::root::resource::ast::{Ast, Expr, Module, Program, SymbolType};

// #[derive(Clone, Debug, Default, Hash, Eq, PartialEq, Copy, PartialOrd, Ord)]
// pub struct TypeVar(pub usize);

// #[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
// pub enum PartialType {
//     Root,
//     Module,
//     Instance,
//     TypeDef,
//     StandardScope,
//     Variable(TypeVar),
//     Record,
//     Field(TypeVar, &'static Self),
//     Enum,
//     Variant,
//     Int,
//     Uint,
//     Byte,
//     Flt,
//     Bool,
//     Char,
//     Str,
//     Naught,
//     Fn(&'static [Self], &'static Self),
//     Mut(&'static Self),
//     Pointer(&'static Self),
//     Custom(usize),
// }

// impl PartialType {
//     pub fn get_fn_args(&self) -> &'static [Self] {
//         match self {
//             Self::Fn(args, _) => args,
//             _ => panic!("{self:?} is not a function!"),
//         }
//     }

//     pub fn get_fn_rt(&self) -> &Self {
//         match self {
//             Self::Fn(_, rt) => rt,
//             _ => panic!("{self:?} is not a function!"),
//         }
//     }
// }

// trait ConvertToPartial {
//     fn convert(&self, t: &mut Environment) -> GraphItem
//     where
//         Self: Sized;
// }

// impl ConvertToPartial for SymbolType {
//     fn convert(&self, t: &mut Environment) -> GraphItem {
//         //dbg!(self.clone());
//         match self {
//             SymbolType::Int => GraphItem::new(Some("$int".into())),
//             SymbolType::Uint => GraphItem::new(Some("$uint".into())),
//             SymbolType::Byte => GraphItem::new(Some("$byte".into())),
//             SymbolType::Flt => GraphItem::new(Some("$flt".into())),
//             SymbolType::Str => GraphItem::new(Some("$str".into())),
//             SymbolType::Bool => GraphItem::new(Some("$bool".into())),
//             SymbolType::Custom(n, _c) => {
//                 // dbg!(t.current_node);
//                 t.get_name(n.to_string(), QueryType::Type)
//                     .expect(&format!("Type '{:?}' does not exist!", n))
//             }
//             _ => GraphItem::new(Some(t.tvg.generate().0.to_string())), //todo!("{:?}", self),
//         }
//     }
// }

// #[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
// pub struct GraphItem {
//     n: Option<&'static str>,
// }

// impl GraphItem {
//     pub fn new(n: Option<String>) -> Self {
//         if n.is_some() {
//             Self {
//                 n: Some(Box::leak(Box::new(n.unwrap()))),
//             }
//         } else {
//             Self { n: None }
//         }
//     }
//     pub fn new_instance(n: String) -> Self {
//         Self {
//             n: Some(Box::leak(Box::new(n))),
//             //t: PartialType::Instance,
//         }
//     }

//     pub fn get_lit_name(&self) -> Option<String> {
//         return if self.n.is_some() {
//             return Some(self.n.unwrap().to_string());
//         } else {
//             None
//         };
//     }
// }

// #[derive(Debug, Default, Clone, PartialEq, Copy, PartialOrd, Eq, Ord)]
// pub enum GraphEdge {
//     Parent,
//     Empty,
//     Module,
//     Definition,
//     TypeDef,
//     ExplicitImportSink,
//     WildcardImport,
//     #[default]
//     Substitution,
// }

// // impl Add for GraphEdge {
// //     type Output = GraphEdge;

// //     fn add(self, rhs: Self) -> Self::Output {
// //         return self;
// //     }
// // }

// #[derive(Debug, Clone, Copy)]
// pub struct TyVarGenerator {
//     current: usize,
// }

// impl Default for TyVarGenerator {
//     fn default() -> Self {
//         Self::new()
//     }
// }

// impl TyVarGenerator {
//     pub fn new() -> Self {
//         Self { current: 0 }
//     }

//     pub fn generate(&mut self) -> TypeVar {
//         self.current += 1;
//         TypeVar(self.current)
//     }
// }

// macro_rules! path_query {
//     ($vec:ident, $variant:ident) => {
//         (|| {
//             use crate::root::passes::midend::typechecking::GraphEdge::*;
//             let c = $vec.iter().filter(|x| **x == $variant).count();
//             (c == 1)
//         })()
//     };

//     ($vec:ident, $variant:ident?) => {
//         (|| {
//             use crate::root::passes::midend::typechecking::GraphEdge::*;
//             let c = $vec.iter().filter(|x| **x == $variant).count();
//             //dbg!(c);

//             (c < 1)
//         })()
//     };
//     ($vec:ident, $variant:ident+) => {
//         (|| {
//             use crate::root::passes::midend::typechecking::GraphEdge::*;
//             let c = $vec.iter().filter(|x| **x == $variant).count();
//             //dbg!(c);

//             (c > 0)
//         })()
//     };
// }

// #[derive(Debug, Clone, Copy)]
// pub enum QueryType {
//     Symbol,
//     Type,
//     Field,
// }

// #[derive(Debug, Clone)]
// struct Environment {
//     g: DiGraph<GraphItem, GraphEdge>,
//     m: HashMap<String, PartialType>,
//     tvg: TyVarGenerator,
//     current_node: Option<NodeIndex>,
//     current_func: Option<GraphItem>,
//     current_typeclass: Option<GraphItem>,
// }

// impl Environment {
//     pub fn new() -> Self {
//         Self {
//             g: DiGraph::new(),
//             m: HashMap::new(),
//             tvg: TyVarGenerator::new(),
//             current_node: None,
//             current_func: None,
//             current_typeclass: None,
//         }
//     }

//     fn extend_curr(&mut self, p: GraphItem, e: GraphEdge) -> GraphItem {
//         let curr = self.current_node.unwrap();
//         //println!("extend {:?} with {:?} via {:?}", curr, p, e);
//         let idx = self.g.add_node(p);
//         self.g.add_edge(curr, idx, e);
//         //        self.current_node = Some(curr);
//         p
//     }

//     // fn extend_enter(&mut self, p: PartialType, e: GraphEdge) {
//     //     let nindx = self.extend_curr(p, e);
//     //     self.current_node = Some(nindx);
//     // }

//     // fn enter_do<F, T>(&mut self, n: PartialType, mut exec: F) -> T
//     // where
//     //     F: FnMut(&mut Self) -> T,
//     // {
//     //     let current_current = self.current_node;
//     //     self.current_node = Some(n);
//     //     let res = exec(self);
//     //     self.current_node = current_current;
//     //     res
//     // }

//     fn get_name(&mut self, n: String, query_type: QueryType) -> Result<GraphItem, String> {
//         // println!("get: {n} with query {:?}", query_type);
//         // println!(
//         //     "{:?}",
//         //     petgraph::dot::Dot::with_config(&(self.clone().g.clone()), &[])
//         // );

//         if n == "self" {
//             return Ok(self.current_typeclass.unwrap());
//         }
//         //let filtered = NodeFiltered::from_fn(&self.g, |node| node.n.is_some_and(|x| x == n));
//         let mut dfs = DfsPostOrder::new(&self.g, GraphItem::new(Some("$ROOT".to_string())));
//         let mut destination: Option<GraphItem> = None;
//         while let Some(visited) = dfs.next(&self.g) {
//             if visited.n.is_some_and(|v| v == n) {
//                 destination = Some(visited)
//             }
//         }

//         match destination {
//             Some(destination) => {
//                 let ways = algo::all_simple_paths::<Vec<_>, _>(
//                     &self.g,
//                     GraphItem::new(Some("$ROOT".to_string())), // start
//                     destination,
//                     0,
//                     None,
//                 )
//                 .collect::<Vec<_>>();

//                 for v in ways {
//                     let mut ep: Vec<GraphEdge> = vec![];
//                     for n in v.iter().enumerate() {
//                         if n.0 < v.len() - 1 {
//                             ep.push(*self.g.edge_weight(*n.1, v[n.0 + 1]).unwrap())
//                         }
//                     }
//                     //dbg!(ep.clone());
//                     //dbg!(query_type);
//                     match query_type {
//                         QueryType::Symbol => {
//                             if path_query!(ep, Definition+) {
//                                 let res = v.last().unwrap().clone();
//                                 return Ok(res);
//                             }
//                         }
//                         QueryType::Type => {
//                             if path_query!(ep, TypeDef) && path_query!(ep, Parent+) {
//                                 let res = v.last().unwrap().clone();
//                                 return Ok(res);
//                             }
//                         }
//                         QueryType::Field => {
//                             if path_query!(ep, Definition) {
//                                 let res = v.last().unwrap().clone();
//                                 return Ok(res);
//                             }
//                         }
//                     }
//                 }
//                 Err(format!("{n} is not in scope!"))
//             }
//             None => Err(format!("{n} does not exist!")),
//         }
//     }

//     fn get_ty_from_name(&mut self, n: &str) -> PartialType {
//         //dbg!(self.m.clone());
//         dbg!(n);
//         if n.starts_with("$") {
//             match n {
//                 "$int" => return PartialType::Int,
//                 "$flt" => return PartialType::Flt,

//                 &_ => panic!(),
//             }
//         } else {
//             *self.m.get(&n.to_string()).unwrap()
//         }
//     }

//     fn unify_helper(&mut self, l: PartialType, r: PartialType) -> Result<PartialType, String> {
//         dbg!(l == r);
//         dbg!(l, r);
//         match (l, r) {
//             (t, u) if t == u && u.eq(&t) => Ok(t),

//             (t, PartialType::Fn(_x, y)) => {
//                 let res = self.unify_helper(t, *y).unwrap();
//                 Ok(res)
//             }
//             (PartialType::Fn(_, y), t) => {
//                 let res = self.unify_helper(*y, t).unwrap();
//                 Ok(res)
//             }
//             (PartialType::Variable(_), _t) => Ok(r),
//             (_t, PartialType::Variable(_)) => Ok(l),

//             (PartialType::Mut(t), u) => {
//                 let res = self.unify_helper(*t, u).unwrap();
//                 Ok(PartialType::Mut(Box::leak(Box::new(res))))
//             }
//             (t, PartialType::Mut(u)) => {
//                 let res = self.unify_helper(t, *u).unwrap();
//                 Ok(PartialType::Mut(Box::leak(Box::new(res))))
//             }

//             (PartialType::Field(_, t), PartialType::Field(_, u)) => {
//                 let res = self.unify_helper(*t, *u);
//                 if res.is_ok() {
//                     return Ok(l);
//                 }
//                 Err(format!("Cannot unify {:?} with {:?}", l, r))
//             }
//             (PartialType::Field(_, t), u) => {
//                 let res = self.unify_helper(*t, u);
//                 if res.is_ok() {
//                     return Ok(l);
//                 }
//                 Err(format!("Cannot unify {:?} with {:?}", l, r))
//             }

//             (t, PartialType::Field(_, u)) => {
//                 let res = self.unify_helper(t, *u);
//                 if res.is_ok() {
//                     return res;
//                 }
//                 Err(format!("Cannot unify {:?} with {:?}", l, r))
//             }

//             _ => Err(format!("Cannot unify {:?} with {:?}", l, r)),
//         }
//     }

//     fn unify(&mut self, l: GraphItem, r: GraphItem) -> Result<GraphItem, String> {
//         let lt = self.get_ty_from_name(l.n.unwrap());
//         let rt = self.get_ty_from_name(r.n.unwrap());
//         //dbg!(lt);

//         let x = self.unify_helper(lt, rt).unwrap();
//         //dbg!(x);
//         if matches!(lt, PartialType::Variable(_)) {
//             self.g.add_edge(l, r, GraphEdge::Substitution);
//             Ok(r)
//         } else if matches!(rt, PartialType::Variable(_)) {
//             self.g.add_edge(r, l, GraphEdge::Substitution);
//             Ok(l)
//         } else {
//             //self.g.add_edge(l, r, GraphEdge::Substitution);
//             Ok(l)
//         }

//         // let temp_condensed = condensation(self.g.clone().into_graph::<usize>(), true);
//         // self.g = DiGraphMap::from_graph(
//         //     temp_condensed.map(|_, n| n.last().unwrap().clone(), |_, e| e.clone()),
//         //);
//     }
// }

// #[derive(Debug, Clone)]
// pub struct Typechecker {
//     env: Environment,
// }

// impl Default for Typechecker {
//     fn default() -> Self {
//         Self::new()
//     }
// }

// impl Typechecker {
//     pub fn new() -> Self {
//         Self {
//             env: Environment::new(),
//         }
//     }

//     fn extend_enter_do<F, T>(&mut self, p: GraphItem, e: GraphEdge, mut exec: F) -> T
//     where
//         F: FnMut(&mut Self) -> T,
//     {
//         let current_current = self.env.current_node;
//         let nindx = self.env.extend_curr(p, e);
//         self.env.current_node = Some(nindx);
//         let res = exec(self);
//         self.env.current_node = current_current;
//         res
//     }

//     fn check_expr(&mut self, e: &Expr) -> Result<PartialType, String> {
//         //dbg!(e);
//         let res = match e {
//             Expr::Int(_) => PartialType::Int,
//             Expr::Uint(_) => PartialType::Uint,
//             Expr::Flt(_) => PartialType::Flt,
//             Expr::Bool(_) => PartialType::Bool,
//             Expr::Str(_) => PartialType::Str,
//             Expr::Symbol(name) => {
//                 let v = self
//                     .env
//                     .get_name(name.to_string(), QueryType::Symbol)
//                     .expect(&format!("{} is not defined", name));
//                 self.env.get_ty_from_name(v.n.unwrap())
//             }
//             Expr::Logical { l, op: _, r } => {
//                 let lty = self.check_expr(l)?;
//                 let rty = self.check_expr(r)?;
//                 self.env.unify_helper(lty, rty)?
//             }
//             Expr::BinAdd { l, r }
//             | Expr::BinSub { l, r }
//             | Expr::BinMul { l, r }
//             | Expr::BinDiv { l, r } => {
//                 let lty = self.check_expr(l)?;
//                 let rty = self.check_expr(r)?;
//                 self.env.unify_helper(lty, rty)?
//             }

//             Expr::Assignment { name, value } => {
//                 let nty = self.check_expr(&value)?;
//                 let name = name.get_symbol_name();
//                 self.add_name(&name, nty);
//                 nty
//             }

//             Expr::Call { name, args } => {
//                 let name = name.get_symbol_name();
//                 let funcsig = self.env.get_ty_from_name(&name);
//                 assert!(matches!(funcsig, PartialType::Fn(..)));
//                 assert_eq!(funcsig.get_fn_args().len(), args.len());
//                 for arg in args.iter().zip(funcsig.get_fn_args()) {
//                     assert_eq!(self.check_expr(arg.0).unwrap(), *arg.1)
//                 }

//                 *funcsig.get_fn_rt()
//             }

//             _ => todo!("{:?}", e),
//         };
//         // println!(
//         //     "{:?}",
//         //     petgraph::dot::Dot::with_config(&(self.env.clone().g.clone()), &[])
//         // );
//         Ok(res)
//     }

//     fn add_item(&mut self, n: &GraphItem, t: PartialType) {
//         self.env.extend_curr(*n, GraphEdge::Definition);
//         self.env.m.insert(n.n.unwrap().to_string(), t);
//     }

//     fn add_name(&mut self, n: &str, t: PartialType) -> GraphItem {
//         let item = GraphItem::new(Some(n.to_string()));
//         self.env.extend_curr(item, GraphEdge::Definition);
//         self.env.m.insert(n.to_string(), t);
//         return item;
//     }

//     fn add_name_noext(&mut self, n: &String, t: PartialType) -> GraphItem {
//         let item = GraphItem::new(Some(n.clone()));
//         self.env.m.insert(n.to_string(), t);
//         return item;
//     }

//     fn check_ast(&mut self, a: &Ast) {
//         match a {
//             Ast::FnDef {
//                 name,
//                 rettype,
//                 args,
//                 limits: _,
//                 body,
//             } => {
//                 let curr_curr: Option<GraphItem> = self.env.current_node;
//                 let nargs: Vec<PartialType> = args
//                     .iter()
//                     .map(|a| {
//                         let t = a.1.convert(&mut self.env).n.unwrap();
//                         self.env.get_ty_from_name(t)
//                     })
//                     .collect();
//                 let crt = rettype.convert(&mut self.env).n.unwrap();
//                 let nrt = self.env.get_ty_from_name(crt);

//                 let n = GraphItem::new(Some(name.to_string()));
//                 self.add_item(
//                     &n,
//                     PartialType::Fn(Box::leak(Box::new(nargs.clone())), Box::leak(Box::new(nrt))),
//                 );
//                 self.env.current_node = Some(n);
//                 self.env.current_func = self.env.current_node;

//                 for arg in nargs.iter().zip(args.clone()) {
//                     self.add_name(&arg.1 .0, *arg.0);
//                 }
//                 let mut a: Vec<PartialType> = vec![];
//                 for b in body {
//                     a.push(self.check_expr(b).unwrap());
//                 }
//                 //let r = rettype.clone().convert(&mut self.env);
//                 let newname = self.env.get_ty_from_name(n.n.unwrap());
//                 self.env.unify_helper(newname, *a.last().unwrap()).unwrap();
//                 self.env.current_node = curr_curr;
//             }
//             Ast::WithClause { include: _ } => (),
//             Ast::Enum { name, members } => {
//                 let n = self.add_name(name, PartialType::Enum);

//                 self.extend_enter_do(n, GraphEdge::TypeDef, |this| {
//                     for m in members {
//                         let nt = this.env.tvg.generate();
//                         let m2 = m.convert(&mut this.env);
//                         let bf = this.env.get_ty_from_name(m2.n.unwrap());
//                         let last = this.add_name(
//                             &m.get_variant_name(),
//                             PartialType::Field(nt, Box::leak(Box::new(bf))),
//                         );

//                         this.env.unify(last, m2).unwrap();
//                     }
//                 });
//             }
//             Ast::Struct { name, members } => {
//                 let n = self.add_name_noext(name, PartialType::Record);
//                 self.extend_enter_do(n, GraphEdge::TypeDef, |this| {
//                     for m in members {
//                         let nt = this.env.tvg.generate();
//                         let m2 = m.1.convert(&mut this.env);
//                         let bf = this.env.get_ty_from_name(m2.n.unwrap());
//                         let last =
//                             this.add_name(&m.0, PartialType::Field(nt, Box::leak(Box::new(bf))));
//                         this.env.unify(last, m2);
//                     }
//                 });
//             }
//             Ast::TypeDef { name, funcs } => {
//                 let prevcur = self.env.current_node;
//                 let prevtc = self.env.current_typeclass;
//                 let c = name.convert(&mut self.env);

//                 self.env.current_node = Some(c);
//                 self.env.current_typeclass = Some(c);

//                 for a in funcs.clone() {
//                     self.check_ast(&a);
//                 }
//                 self.env.current_node = prevcur;

//                 self.env.current_typeclass = prevtc;
//             }
//             _ => todo!("{:?}", a),
//         }
//     }

//     pub fn check_module(&mut self, m: &Module) {
//         for a in &m.body {
//             self.check_ast(a);
//         }
//     }

//     pub fn check(&mut self, mut p: Box<Program>) {
//         //let thisp: &'static Program = Box::leak(p);
//         let rootidx = self
//             .env
//             .g
//             .add_node(GraphItem::new(Some("$ROOT".to_string())));
//         self.env.current_node = Some(rootidx);
//         p.modules.reverse();
//         for module in &p.modules {
//             let ns = self.add_name(&module.name, PartialType::Module);
//             self.env
//                 .g
//                 .add_edge(self.env.current_node.unwrap(), ns, GraphEdge::Module);
//             self.env
//                 .g
//                 .add_edge(ns, self.env.current_node.unwrap(), GraphEdge::Parent);

//             self.env.current_node = Some(ns);
//             self.check_module(module);
//         }
//         println!(
//             "{:?}",
//             petgraph::dot::Dot::with_config(&(self.env.clone().g.clone()), &[])
//         );
//         //dbg!(&self.env.m);
//     }
// }
