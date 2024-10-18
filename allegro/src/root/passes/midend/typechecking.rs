//use std::ops::Add;

use std::collections::HashMap;

use petgraph::{
    algo::{self},
    prelude::{DiGraphMap, GraphMap},
    visit::DfsPostOrder,
    Directed,
};

use crate::root::resource::ast::{Ast, Expr, Module, Program, SymbolType};

#[derive(Clone, Debug, Default, Hash, Eq, PartialEq, Copy, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum PartialType {
    Root,
    Module,
    Instance,
    TypeDef,
    StandardScope,
    Variable(TypeVar),
    Record,
    Field(TypeVar, &'static Self),
    Enum,
    Variant,
    Int,
    Uint,
    Byte,
    Flt,
    Bool,
    Char,
    Str,
    Naught,
    Fn(&'static [Self], &'static Self),
    Mut(&'static Self),
    Pointer(&'static Self),
    Custom(usize),
}

impl PartialType {
    pub fn get_fn_args(&self) -> &'static [Self] {
        match self {
            Self::Fn(args, _) => args,
            _ => panic!("{self:?} is not a function!"),
        }
    }

    pub fn get_fn_rt(&self) -> &Self {
        match self {
            Self::Fn(_, rt) => rt,
            _ => panic!("{self:?} is not a function!"),
        }
    }
}

trait ConvertToPartial {
    fn convert(&self, t: &mut Environment) -> GraphType
    where
        Self: Sized;
}

impl ConvertToPartial for SymbolType {
    fn convert(&self, t: &mut Environment) -> GraphType {
        //dbg!(self.clone());
        match self {
            SymbolType::Int => GraphType::new(Some("$int".into())),
            SymbolType::Uint => GraphType::new(Some("$uint".into())),
            SymbolType::Byte => GraphType::new(Some("$byte".into())),
            SymbolType::Flt => GraphType::new(Some("$flt".into())),
            SymbolType::Str => GraphType::new(Some("$str".into())),
            SymbolType::Bool => GraphType::new(Some("$bool".into())),
            SymbolType::Generic(_n) => {
                GraphType::new(Some(t.tvg.generate().0.to_string()))
            }
            SymbolType::Custom(n, _c) => {
                // dbg!(t.current_node);
                t.get_name(n.to_string(), QueryType::Type)
                    .expect(&format!("Type '{:?}' does not exist!", n))
            }
            _ =>                 GraphType::new(Some(t.tvg.generate().0.to_string()))
            , //todo!("{:?}", self),
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct GraphType {
//    t: PartialType,
    n: Option<&'static str>,
}

impl GraphType {
    pub fn new(n: Option<String>) -> Self {
        if n.is_some() {
            Self {
                n: Some(Box::leak(Box::new(n.unwrap()))),
            }
        } else {
            Self { n: None }
        }
    }
    pub fn new_instance(n: String) -> Self {
        Self {
            n: Some(Box::leak(Box::new(n))),
            //t: PartialType::Instance,
        }
    }

    pub fn get_lit_name(&self) -> Option<String> {
        return if self.n.is_some() {
            return Some(self.n.unwrap().to_string());
        } else {
            None
        };
    }
}

#[derive(Debug, Default, Clone, PartialEq, Copy, PartialOrd, Eq, Ord)]
pub enum GraphEdge {
    Parent,
    Empty,
    Module,
    Definition,
    TypeDef,
    ExplicitImportSink,
    WildcardImport,
    #[default]
    Substitution,
}

// impl Add for GraphEdge {
//     type Output = GraphEdge;

//     fn add(self, rhs: Self) -> Self::Output {
//         return self;
//     }
// }

#[derive(Debug, Clone, Copy)]
pub struct TyVarGenerator {
    current: usize,
}

impl Default for TyVarGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl TyVarGenerator {
    pub fn new() -> Self {
        Self { current: 0 }
    }

    pub fn generate(&mut self) -> TypeVar {
        self.current += 1;
        TypeVar(self.current)
    }
}

macro_rules! path_query {
    ($vec:ident, $variant:ident) => {
        (|| {
            use crate::root::passes::midend::typechecking::GraphEdge::*;
            let c = $vec.iter().filter(|x| **x == $variant).count();
            (c == 1)
        })()
    };

    ($vec:ident, $variant:ident?) => {
        (|| {
            use crate::root::passes::midend::typechecking::GraphEdge::*;
            let c = $vec.iter().filter(|x| **x == $variant).count();
            //dbg!(c);

            (c < 1)
        })()
    };
    ($vec:ident, $variant:ident+) => {
        (|| {
            use crate::root::passes::midend::typechecking::GraphEdge::*;
            let c = $vec.iter().filter(|x| **x == $variant).count();
            //dbg!(c);

            (c > 0)
        })()
    };
}

#[derive(Debug, Clone, Copy)]
pub enum QueryType {
    Symbol,
    Type,
    Field,
}

#[derive(Debug, Clone)]
struct Environment {
    g: DiGraphMap<GraphType, GraphEdge>,
    m: HashMap<String, PartialType>,
    tvg: TyVarGenerator,
    current_node: Option<GraphType>,
    current_func: Option<GraphType>,
    current_typeclass: Option<GraphType>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            g: GraphMap::<GraphType, GraphEdge, Directed>::new(),
            m: HashMap::new(),
            tvg: TyVarGenerator::new(),
            current_node: None,
            current_func: None,
            current_typeclass: None,
        }
    }

    fn extend_curr(&mut self, p: GraphType, e: GraphEdge) -> GraphType {
        let curr = self.current_node.unwrap();
        //println!("extend {:?} with {:?} via {:?}", curr, p, e);
        self.g.add_edge(curr, p, e);
        //        self.current_node = Some(curr);
        p
    }

    // fn extend_enter(&mut self, p: PartialType, e: GraphEdge) {
    //     let nindx = self.extend_curr(p, e);
    //     self.current_node = Some(nindx);
    // }

    // fn enter_do<F, T>(&mut self, n: PartialType, mut exec: F) -> T
    // where
    //     F: FnMut(&mut Self) -> T,
    // {
    //     let current_current = self.current_node;
    //     self.current_node = Some(n);
    //     let res = exec(self);
    //     self.current_node = current_current;
    //     res
    // }

    fn get_name(&mut self, n: String, query_type: QueryType) -> Result<GraphType, String> {
        // println!("get: {n} with query {:?}", query_type);
        // println!(
        //     "{:?}",
        //     petgraph::dot::Dot::with_config(&(self.clone().g.clone()), &[])
        // );

        if n == "self" {
            return Ok(self.current_typeclass.unwrap());
        }
        //let filtered = NodeFiltered::from_fn(&self.g, |node| node.n.is_some_and(|x| x == n));
        let mut dfs = DfsPostOrder::new(&self.g, self.current_node.unwrap());
        let mut destination: Option<GraphType> = None;
        while let Some(visited) = dfs.next(&self.g) {
            if visited.n.is_some_and(|v| v == n) {
                destination = Some(visited)
            }
        }

        match destination {
            Some(destination) => {
                let ways = algo::all_simple_paths::<Vec<_>, _>(
                    &self.g,
                    self.current_node.unwrap(), // start
                    destination,
                    0,
                    None,
                )
                .collect::<Vec<_>>();

                for v in ways {
                    let mut ep: Vec<GraphEdge> = vec![];
                    for n in v.iter().enumerate() {
                        if n.0 < v.len() - 1 {
                            ep.push(*self.g.edge_weight(*n.1, v[n.0 + 1]).unwrap())
                        }
                    }
                    //dbg!(ep.clone());
                    //dbg!(query_type);
                    match query_type {
                        QueryType::Symbol => {
                            if path_query!(ep, Definition) && path_query!(ep, Parent+) {
                                return Ok(*v.last().unwrap());
                            }
                        }
                        QueryType::Type => {
                            if path_query!(ep, TypeDef) && path_query!(ep, Parent+) {
                                return Ok(*v.last().unwrap());
                            }
                        }
                        QueryType::Field => {
                            if path_query!(ep, Definition) {
                                return Ok(*v.last().unwrap());
                            }
                        }
                    }
                }
                Err(format!("{n} is not in scope!"))
            }
            None => Err(format!("{n} does not exist!")),
        }
    }

    fn unify_helper(&mut self, l: PartialType, r: PartialType) -> Result<PartialType, String> {
        match (l, r) {
            (PartialType::Int, PartialType::Int)
            | (PartialType::Flt, PartialType::Flt)
            | (PartialType::Str, PartialType::Str)
            | (PartialType::Bool, PartialType::Bool) => Ok(l),

            (t, PartialType::Fn(_x, y)) => {
                let res = self.unify_helper(t, *y).unwrap();
                Ok(res)
            }
            (PartialType::Fn(_, y), t) => {
                let res = self.unify_helper(*y, t).unwrap();
                Ok(res)
            }
            (PartialType::Variable(_), _t) => Ok(r),
            (_t, PartialType::Variable(_)) => Ok(l),

            (PartialType::Mut(t), u) => {
                let res = self.unify_helper(*t, u).unwrap();
                Ok(PartialType::Mut(Box::leak(Box::new(res))))
            }
            (t, PartialType::Mut(u)) => {
                let res = self.unify_helper(t, *u).unwrap();
                Ok(PartialType::Mut(Box::leak(Box::new(res))))
            }

            (PartialType::Field(_, t), PartialType::Field(_, u)) => {
                let res = self.unify_helper(*t, *u);
                if res.is_ok() {
                    return Ok(l);
                }
                Err(format!("Cannot unify {:?} with {:?}", l, r))
            }
            (PartialType::Field(_, t), u) => {
                let res = self.unify_helper(*t, u);
                if res.is_ok() {
                    return Ok(l);
                }
                Err(format!("Cannot unify {:?} with {:?}", l, r))
            }

            (t, PartialType::Field(_, u)) => {
                let res = self.unify_helper(t, *u);
                if res.is_ok() {
                    return res;
                }
                Err(format!("Cannot unify {:?} with {:?}", l, r))
            }
            (t, u) if t == u => Ok(t),

            _ => Err(format!("Cannot unify {:?} with {:?}", l, r)),
        }
    }

    fn unify(&mut self, l: GraphType, r: GraphType) -> Result<GraphType, String> {
        self.unify_helper(l.t, r.t)?;
        if matches!(l.t, PartialType::Variable(_)) {
            self.g.add_edge(l, r, GraphEdge::Substitution);
            Ok(r)
        } else if matches!(r.t, PartialType::Variable(_)) {
            self.g.add_edge(r, l, GraphEdge::Substitution);
            Ok(l)
        } else {
            //self.g.add_edge(l, r, GraphEdge::Substitution);
            Ok(l)
        }

        // let temp_condensed = condensation(self.g.clone().into_graph::<usize>(), true);
        // self.g = DiGraphMap::from_graph(
        //     temp_condensed.map(|_, n| n.last().unwrap().clone(), |_, e| e.clone()),
        //);
    }
}

#[derive(Debug, Clone)]
pub struct Typechecker {
    env: Environment,
}

impl Default for Typechecker {
    fn default() -> Self {
        Self::new()
    }
}

impl Typechecker {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
        }
    }

    fn extend_enter_do<F, T>(&mut self, p: GraphType, e: GraphEdge, mut exec: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        let current_current = self.env.current_node;
        let nindx = self.env.extend_curr(p, e);
        self.env.current_node = Some(nindx);
        let res = exec(self);
        self.env.current_node = current_current;
        res
    }

    fn check_expr(&mut self, e: &Expr) -> Result<GraphType, String> {
        //dbg!(e);
        let res: GraphType = match e {
            Expr::Int(_) => GraphType::new(None, PartialType::Int),
            Expr::Uint(_) => GraphType::new(None, PartialType::Uint),
            Expr::Flt(_) => GraphType::new(None, PartialType::Flt),
            Expr::Bool(_) => GraphType::new(None, PartialType::Bool),
            Expr::Str(_) => GraphType::new(None, PartialType::Str),
            Expr::Symbol(name) => self
                .env
                .get_name(name.to_string(), QueryType::Symbol)
                .expect(&format!("{} is not defined", name)),
            Expr::Logical { l, op: _, r } => {
                let lty = self.check_expr(l)?;
                let rty = self.check_expr(r)?;
                self.env.unify(lty, rty)?
            }
            Expr::BinAdd { l, r }
            | Expr::BinSub { l, r }
            | Expr::BinMul { l, r }
            | Expr::BinDiv { l, r } => {
                let lty = self.check_expr(l)?;
                let rty = self.check_expr(r)?;
                self.env.unify(lty, rty)?
            }
            Expr::Assignment { name, value } => {
                let fname = name.get_symbol_name();
 // get the String value of the name
                match self.env.get_name(fname.clone(), QueryType::Symbol) {
                    Ok(t) => {
                        println!("exists");
                        match t.t {
                            PartialType::Mut(_) => {
                                let temp_rhs =self.check_expr(value)?;
                                let rhs: GraphType = GraphType { t: PartialType::Mut(Box::leak(Box::new(temp_rhs.t))), n: temp_rhs.n }; // get the type of the assignment expr
                                let node = GraphType::new(
                                    Some(fname),
                                    rhs.t,
                                ); // create a new node
                                let lnode = self.env.extend_curr(node, GraphEdge::Definition); // add an edge to the current node and our new node
                                let t = self.env.unify(lnode, rhs);

                                t?
                            }
                            _ => {
                                return Err(format!(
                                    "{fname} is already defined, but is not mutable!"
                                ))
                            }
                        }
                    }
                    Err(_) => {
                        let rhs: GraphType = self.check_expr(value)?; // get the type of the assignment expr
                        let node = GraphType::new(Some(fname), rhs.t); // create a new node
                        let lnode = self.env.extend_curr(node, GraphEdge::Definition); // add an edge to the current node and our new node
                        let t = self.env.unify(lnode, rhs);

                        t?
                    }
                }
            }
            Expr::MutableAssignment { name, value } => {
                let fname = name.get_symbol_name(); // get the String value of the name
                let rhs = self.check_expr(value)?; // get the type of the assignment expr
                let node =
                    GraphType::new(Some(fname), PartialType::Mut(Box::leak(Box::new(rhs.t)))); // create a new node
                let lnode = self.env.extend_curr(node, GraphEdge::Definition); // add an edge to the current node and our new node
                let t = self.env.unify(lnode, rhs);

                t?
            }
            Expr::If {
                condition,
                then,
                otherwise,
            } => {
                let cond = self.check_expr(condition)?;
                let thenty = self.check_expr(then)?;
                let elsety = self.check_expr(otherwise)?;
                //assert!(matches!(cond.t, PartialType::Bool));
                self.env.unify(thenty, elsety)?
            }

            Expr::StructInstance { name, fields } => {
                let t = self.env.get_name(name.get_symbol_name(), QueryType::Type)?;
                for f in fields.iter() {
                    let fieldtype = self.check_expr(&f.1)?;
                    //dbg!(f.0.clone());
                    let resty = self.env.get_name(f.0.clone(), QueryType::Field)?;
                    //dbg!(fieldtype);
                    //dbg!(resty);
                    self.env.unify(fieldtype, resty);
                }
                t
            }

            Expr::Call { name, args } => {
                let callt = self
                    .env
                    .get_name(name.get_symbol_name(), QueryType::Symbol)?;

                assert!(matches!(callt.t, PartialType::Fn(_, _)));
                let nargs: Vec<PartialType> =
                    args.iter().map(|a| self.check_expr(a).unwrap().t).collect();
                let targs = callt.t.get_fn_args();
                assert!(nargs.len() == targs.len());
                for arg in targs.iter().zip(nargs) {
                    self.env.unify_helper(*arg.0, arg.1).unwrap();
                }
                callt
            }
            Expr::Return { value } => {
                let cunwrap = self.env.current_func.unwrap();
                let val = self.check_expr(value)?;
                self.env.unify(val, cunwrap)?
            }
            Expr::FieldAccess(par, name) => {
                self.check_expr(par).unwrap();
                //self.env.get_name(name.to_string(), true);
                let nf = self.env.get_name(name.to_string(), QueryType::Field);
                return nf;
                //todo!()
            }
            Expr::AddressOf(s) => {
                let t = self.check_expr(s)?;
                let nt = GraphType::new(
                    Some(t.n.unwrap().to_string()),
                    PartialType::Pointer(Box::leak(Box::new(t.t))),
                );
                nt
            }
            _ => todo!("{:?}", e),
        };
        println!(
            "{:?}",
            petgraph::dot::Dot::with_config(&(self.env.clone().g.clone()), &[])
        );
        Ok(res)
    }

    fn check_ast(&mut self, a: &Ast) {
        match a {
            Ast::FnDef {
                name,
                rettype,
                args,
                limits: _,
                body,
            } => {
                let curr_curr: Option<GraphType> = self.env.current_node;
                let nargs: Vec<PartialType> =
                    args.iter().map(|a| a.1.convert(&mut self.env).t).collect();
                let nrt = rettype.convert(&mut self.env).t;
                let n = GraphType::new(
                    Some(name.to_string()),
                    PartialType::Fn(Box::leak(Box::new(nargs.clone())), Box::leak(Box::new(nrt))),
                );
                self.env.extend_curr(n, GraphEdge::Definition);
                self.env.current_node = Some(n);
                self.env.current_func = self.env.current_node;

                for arg in nargs.iter().zip(args.clone()) {
                    let nt = GraphType::new(Some(arg.1 .0.clone()), *arg.0);
                    self.env.extend_curr(nt, GraphEdge::Definition);
                }
                let mut a: Vec<GraphType> = vec![];
                for b in body {
                    a.push(self.check_expr(b).unwrap());
                }
                //let r = rettype.clone().convert(&mut self.env);
                self.env.unify(n, *a.last().unwrap());
                self.env.current_node = curr_curr;
            }
            Ast::WithClause { include: _ } => (),
            Ast::Enum { name, members } => {
                let n = GraphType::new(Some(name.to_string()), PartialType::Enum);

                self.extend_enter_do(n, GraphEdge::TypeDef, |this| {
                    for m in members {
                        let nt = this.env.tvg.generate();
                        let m2 = m.convert(&mut this.env);
                        let last = this.env.extend_curr(
                            GraphType::new(
                                Some(m.get_variant_name()),
                                PartialType::Field(nt, Box::leak(Box::new(m2.t))),
                            ),
                            GraphEdge::Parent,
                        );
                        this.env.unify(last, m2).unwrap();
                    }
                });
            }
            Ast::Struct { name, members } => {
                let n = GraphType::new(Some(name.to_string()), PartialType::Record);
                self.extend_enter_do(n, GraphEdge::TypeDef, |this| {
                    for m in members {
                        let nt = this.env.tvg.generate();
                        let m2 = m.1.convert(&mut this.env);
                        let last = this.env.extend_curr(
                            GraphType::new(
                                Some(m.0.clone()),
                                PartialType::Field(nt, Box::leak(Box::new(m2.t))),
                            ),
                            GraphEdge::Parent,
                        );
                        this.env.unify(last, m2);
                    }
                });
            }
            Ast::TypeDef { name, funcs } => {
                let prevcur = self.env.current_node;
                let prevtc = self.env.current_typeclass;
                let c = name.convert(&mut self.env);

                self.env.current_node = Some(c);
                self.env.current_typeclass = Some(c);

                for a in funcs.clone() {
                    self.check_ast(&a);
                }
                self.env.current_node = prevcur;

                self.env.current_typeclass = prevtc;
            }
            Ast::MethodDef {
                parent,
                name,
                rettype,
                args,
                limits: _,
                body,
            } => {
                let nargs: Vec<PartialType> =
                    args.iter().map(|a| a.1.convert(&mut self.env).t).collect();
                let nrt = rettype.convert(&mut self.env).t;
                let n = GraphType::new(
                    Some(name.to_string()),
                    PartialType::Fn(Box::leak(Box::new(nargs.clone())), Box::leak(Box::new(nrt))),
                );
                self.env.current_node = Some(
                    self.env
                        .get_name(parent.to_string(), QueryType::Type)
                        .unwrap(),
                );
                let _ = self.extend_enter_do(n, GraphEdge::Parent, |this| {
                    for arg in nargs.iter().zip(args.clone()) {
                        this.env.extend_curr(
                            GraphType::new(Some(arg.1 .0), *arg.0),
                            GraphEdge::Definition,
                        );
                    }
                    this.env.current_func = this.env.current_node;
                    let a: Vec<GraphType> =
                        body.iter().map(|b| this.check_expr(b).unwrap()).collect();
                    let r = rettype.clone().convert(&mut this.env);
                    let rn = GraphType::new(
                        Some(name.to_string()),
                        PartialType::Fn(
                            Box::leak(Box::new(nargs.clone())),
                            Box::leak(Box::new(this.env.unify(*a.last().unwrap(), r).unwrap().t)),
                        ),
                    );
                    this.env.unify(n, rn)
                });
            }
            _ => todo!("{:?}", a),
        }
    }

    pub fn check_module(&mut self, m: &Module) {
        for a in &m.body {
            self.check_ast(a);
        }
    }

    pub fn check(&mut self, mut p: Box<Program>) {
        //let thisp: &'static Program = Box::leak(p);
        let rootidx = self.env.g.add_node(GraphType::new(None, PartialType::Root));
        self.env.current_node = Some(rootidx);
        p.modules.reverse();
        for module in &p.modules {
            let ns = GraphType::new(Some(module.name.clone()), PartialType::Module);
            self.env
                .g
                .add_edge(self.env.current_node.unwrap(), ns, GraphEdge::Module);
            self.env
                .g
                .add_edge(ns, self.env.current_node.unwrap(), GraphEdge::Parent);

            self.env.current_node = Some(ns);
            self.check_module(module);
        }
        println!(
            "{:?}",
            petgraph::dot::Dot::with_config(&(self.env.clone().g.clone()), &[])
        );
    }
}
