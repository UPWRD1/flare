use std::collections::HashMap;

use petgraph::{
    prelude::{DiGraphMap, GraphMap},
    visit::{Bfs, Dfs, DfsPostOrder, EdgeFiltered, EdgeRef},
    Directed,
    Direction::{Incoming, Outgoing},
};

use crate::root::resource::ast::{Ast, Expr, Program, SymbolType};

#[derive(Clone, Debug, Default, Hash, Eq, PartialEq, Copy, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum PartialType {
    Root,
    Module,
    TypeDef,
    StandardScope,
    Variable(TypeVar),
    Record(&'static str),
    Field(TypeVar, &'static Self),
    Enum(&'static str),
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
    Custom(&'static str, usize),
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
    fn convert(&self, t: &mut Environment) -> PartialType
    where
        Self: Sized;
}

impl ConvertToPartial for SymbolType {
    fn convert(&self, t: &mut Environment) -> PartialType {
        //dbg!(self.clone());
        match self {
            SymbolType::Int => PartialType::Int,
            SymbolType::Uint => PartialType::Uint,
            SymbolType::Byte => PartialType::Byte,
            SymbolType::Flt => PartialType::Flt,
            SymbolType::Str => PartialType::Str,
            SymbolType::Bool => PartialType::Bool,

            SymbolType::Mut(x) => PartialType::Mut(Box::leak(Box::new(x.convert(t)))),
            SymbolType::Generic(_n) => PartialType::Variable(t.tvg.generate()),
            SymbolType::Custom(n, c) => {
                PartialType::Custom(Box::leak(Box::new(n.clone())), c.len())
            }
            _ => PartialType::Variable(t.tvg.generate()), //todo!("{:?}", self),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GraphEdge {
    Name(String),
    Expr(Expr),
    Substitution,
    Scope,
    Module,
}

impl From<Expr> for GraphEdge {
    fn from(value: Expr) -> Self {
        match value {
            Expr::Symbol(s) => Self::Name(s.to_string()),
            _ => Self::Expr(value.clone()),
        }
    }
}

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
#[derive(Debug, Clone)]
struct Environment {
    g: DiGraphMap<PartialType, GraphEdge>,
    m: HashMap<String, PartialType>,
    tvg: TyVarGenerator,
    current_node: Option<PartialType>,
    current_func: Option<PartialType>,
    current_typeclass: Option<PartialType>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            g: GraphMap::<PartialType, GraphEdge, Directed>::new(),
            m: HashMap::new(),
            tvg: TyVarGenerator::new(),
            current_node: None,
            current_func: None,
            current_typeclass: None,
        }
    }

    fn extend_curr(&mut self, p: PartialType, e: GraphEdge) -> PartialType {
        let curr = self.current_node.unwrap();
        self.g.add_edge(curr, p, e);
        p
    }

    fn extend_enter(&mut self, p: PartialType, e: GraphEdge) {
        let nindx = self.extend_curr(p, e);
        self.current_node = Some(nindx);
    }

    fn enter_do<F, T>(&mut self, n: PartialType, mut exec: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        let current_current = self.current_node;
        self.current_node = Some(n);
        let res = exec(self);
        self.current_node = current_current;
        res
    }

    fn get_name(&mut self, e: GraphEdge) -> Option<PartialType> {
        // println!(
        //     "{:?}",
        //     petgraph::dot::Dot::with_config(&(self.g.clone()), &[])
        // );
        let c: PartialType = self.current_node.unwrap();
        assert!(matches!(e, GraphEdge::Name(_)));
        if e == GraphEdge::Name("self".to_string()) {
            return self.current_typeclass;
        }
        let filtered = EdgeFiltered::from_fn(&self.g, |edge_ref| *edge_ref.weight() == e );

        let mut dfs = Bfs::new(&filtered, self.current_node?);
        dfs.next(&self.g)
    //     while let Some(visited) = dfs.next(&self.g) {
            
    //         println!("{:?} {:?}", visited, e);
    // }
    // todo!()
        // while let Some(visited) = dfs.next(&self.g) {
        //     let w: Vec<&GraphEdge> = self.g.edges(visited).into_iter().map(|x| x.2).collect();
        //     println!("{:?} {:?}",visited, w);

        //     if w.contains(&&e) {

        //         return Some(visited);
        //     }
        // }
        // let mut res = None;
        // for n in &self.g.neighbors_directed(c, Incoming).collect::<Vec<PartialType>>() {
        //     res = self.enter_do(*n, |this| {this.get_name(e.clone())});
        //     if res.is_some() {
        //         break
        //     }
        // }
        
        // res
        
    }

    fn get_parented_name(&mut self, p: PartialType, e: GraphEdge) -> Option<PartialType> {
        let curr_curr = self.current_node;
        self.current_node = Some(p);
        let res = self.get_name(e);
        self.current_node = curr_curr;
        res
    }

    fn unify(&mut self, l: PartialType, r: PartialType) -> Result<PartialType, String> {
        match (l, r) {
            (PartialType::Int, PartialType::Int)
            | (PartialType::Flt, PartialType::Flt)
            | (PartialType::Str, PartialType::Str)
            | (PartialType::Bool, PartialType::Bool) => Ok(l),
            (PartialType::Variable(_), t) => {
                self.g.add_edge(l, r, GraphEdge::Substitution);
                Ok(t)
            }
            (t, PartialType::Variable(_)) => {
                self.g.add_edge(r, l, GraphEdge::Substitution);
                Ok(t)
            }
            (t, PartialType::Fn(_, _)) => {
                self.g.add_edge(
                    PartialType::Fn(r.get_fn_args(), Box::leak(Box::new(t))),
                    l,
                    GraphEdge::Substitution,
                );
                Ok(t)
            }
            (PartialType::Fn(_, _), t) => {
                self.g.add_edge(
                    PartialType::Fn(l.get_fn_args(), Box::leak(Box::new(t))),
                    r,
                    GraphEdge::Substitution,
                );
                Ok(t)
            }

            (PartialType::Mut(t), u) => {
                let res = self.unify(*t, u).unwrap();
                self.g.add_edge(res, r, GraphEdge::Substitution);
                Ok(PartialType::Mut(Box::leak(Box::new(res))))
            }

            (t, u) if t == u => Ok(t),

            (PartialType::Field(_, t), PartialType::Field(_, u)) => {
                Ok(self.unify(*t, *u).unwrap())

            }

            (PartialType::Field(_, t), u) => {
                let res = self.unify(*t, u).unwrap();
                return Ok(res)

            }

            (t, PartialType::Field(_, u)) => {
                return Ok(self.unify(t, *u).unwrap())
            }


            
            _ => Err(format!("Cannot unify {:?} with {:?}", l, r)),
        }
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

    fn extend_enter_do<F, T>(&mut self, p: PartialType, e: GraphEdge, mut exec: F) -> T
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

    fn check_expr(&mut self, e: &Expr) -> PartialType {

        dbg!(e.clone());
        let res = match e {
            Expr::Int(_) => PartialType::Int,
            Expr::Uint(_) => PartialType::Uint,
            Expr::Flt(_) => PartialType::Flt,
            Expr::Bool(_) => PartialType::Bool,
            Expr::Str(_) => PartialType::Str,
            Expr::Symbol(name) => self
                .env
                .get_name(GraphEdge::Name(name.to_string()))
                .unwrap_or_else(|| panic!("Symbol {} is not defined!", name)),
            Expr::Logical { l, op: _, r } => {
                let lty: PartialType = self.check_expr(l);
                let rty: PartialType = self.check_expr(r);
                self.env.unify(lty, rty).unwrap()
            }
            Expr::BinAdd { l, r }
            | Expr::BinSub { l, r }
            | Expr::BinMul { l, r }
            | Expr::BinDiv { l, r } => {
                let lty: PartialType = self.check_expr(l);
                let rty: PartialType = self.check_expr(r);
                self.env.unify(lty, rty).unwrap()
            }
            Expr::Assignment { name, value } => {
                let name = name.get_symbol_name();
                let rhs: PartialType = self.check_expr(value);
                let lnode = self.env.extend_curr(rhs, GraphEdge::Name(name));
                self.env.unify(lnode, rhs).unwrap();
                rhs
            }
            Expr::MutableAssignment { name, value } => {
                let name = name.get_symbol_name();
                let rhs: PartialType = self.check_expr(value);
                let lnode = self.env.extend_curr(rhs, GraphEdge::Name(name));
                self.env.unify(lnode, rhs).unwrap();

                rhs
            }
            Expr::If {
                condition,
                then,
                otherwise: _,
            } => {
                let _cond = self.check_expr(condition);
                let thenty: Box<PartialType> =
                    self.extend_enter_do(PartialType::StandardScope, GraphEdge::Scope, |this| {
                        Box::new(this.check_expr(then))
                    });
                let elsety: Box<PartialType> =
                    self.extend_enter_do(PartialType::StandardScope, GraphEdge::Scope, |this| {
                        Box::new(this.check_expr(e))
                    });

                self.env.unify(*thenty, *elsety).unwrap()
            }

            Expr::StructInstance { name, fields } => {
                //dbg!(self.env.current_node);
                let t = self
                .env
                .get_name(GraphEdge::Name(name.get_symbol_name().to_string()))
                .unwrap();
                // let children: Vec<PartialType> = self.env.g.neighbors_directed(t, Outgoing).collect();
                // for f in fields.iter().enumerate() {
                //     dbg!(f.1.1.clone());
                //     let ft = self.check_expr(&f.1.1);
                //     //self.env.unify(ft, children[f.0]).unwrap();
                // }
                t
            }
                
                ,//.expect(&format!("Struct {name:?} is not defined!")),
            Expr::Call { name, args } => match *name.clone() {
                Expr::Symbol(s) => {
                    let t = self
                        .env
                        .get_name(GraphEdge::Name(s.to_string()))
                        .expect("Function is not defined!");
                    let _nargs: Vec<PartialType> =
                        args.iter().map(|a| self.check_expr(a)).collect();
                    assert!(matches!(t, PartialType::Fn(_, _)));
                    t
                }
                Expr::FieldAccess(f, c) => {
                    //let nf = self.check_expr(&f);
                    ///dbg!(f);
                    let t = self
                        .env
                        .get_parented_name(self.env.current_node.unwrap(), GraphEdge::Name(c.to_string()))
                        .expect("Function is not defined!");
                    let _nargs: Vec<PartialType> =
                        args.iter().map(|a| self.check_expr(a)).collect();
                    assert!(matches!(t, PartialType::Fn(_, _)));
                    t
                }
                _ => panic!("{:?} is not a function!", name),
            },
            Expr::Return { value } => {
                let cunwrap = self.env.current_func.unwrap();
                let val = self.check_expr(value);
                let a = self.env.unify(val, cunwrap);
                a.unwrap_or_else(|_| panic!("Error when returning {:?}", self.env.current_func))
            }
            Expr::FieldAccess(par, name) => {
                let nf = par.get_symbol_name();

                let nft = self
                .env
                .get_name(GraphEdge::Name(nf.clone())).unwrap();
                let t = self
                    .env
                    .get_parented_name(nft, GraphEdge::Name(name.to_string()))
                    .unwrap_or_else(|| panic!("Field {:?} is not defined for {:?}", name, nf));

                return t;
            }
            Expr::ModuleCall { module, name, args } => {
                //dbg!(module);

                
                self
                    .env
                    .get_name(GraphEdge::Name(module.get_symbol_name()))
                    .unwrap()
            }
            _ => todo!("{:?}", e),
        };
        println!(
            "{:?}",
            petgraph::dot::Dot::with_config(&(self.env.clone().g), &[])
        );

        res
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
                let nargs: Vec<PartialType> =
                    args.iter().map(|a| a.1.convert(&mut self.env)).collect();
                let nrt = rettype.convert(&mut self.env);

                self.extend_enter_do(
                    PartialType::Fn(
                        Box::leak(nargs.clone().into_boxed_slice()),
                        Box::leak(Box::new(nrt)),
                    ),
                    GraphEdge::Name(name.to_string()),
                    |this| {
                        for arg in nargs.iter().zip(args.clone()) {
                            this.env
                                .extend_curr(*arg.0, GraphEdge::Name(arg.1 .0));
                        }
                        this.env.current_func = this.env.current_node;
                        let a: Vec<PartialType> =
                            body.iter().map(|b| this.clone().check_expr(b)).collect();
                        let r = rettype.clone().convert(&mut this.env);
                        this.env.unify(*a.last().unwrap(), r).unwrap()
                    },
                );
            }
            Ast::WithClause { include: _ } => (),
            Ast::Enum { name, members } => {
                self.extend_enter_do(
                    PartialType::Enum(Box::leak(Box::new(name.clone()))),
                    GraphEdge::Name(name.to_string()),
                    |this| {
                        for m in &members.clone() {
                            //dbg!(m);
                            this.env.extend_curr(
                                PartialType::Variant,
                                GraphEdge::Name(m.get_variant_name()),
                            );
                        }
                    },
                );
            }
            Ast::Struct { name, members } => {
                let p = PartialType::Record(Box::leak(Box::new(name.clone())));
                let current_current = self.env.current_node;
                self.env
                    .extend_enter(p, GraphEdge::Name(name.to_string()));
                for a in members.to_vec() {
                    let nt = self.env.tvg.generate();
                    let m2 = a.1.convert(&mut self.env);
                    self.env.extend_curr(PartialType::Field(nt, Box::leak(Box::new(m2))), GraphEdge::Name(a.0.clone()));
                }
                self.env.current_node = current_current;
    
                
            }
            Ast::TypeDef { name, funcs } => {
                let prevcur = self.env.current_node;
                let d: PartialType = self
                    .env
                    .get_name(GraphEdge::Name(name.get_custom_name()))
                    .unwrap();
                //self.env.extend_curr(d, GraphEdge::Module);
                self.env.current_node = Some(d);
                self.env.current_typeclass = Some(PartialType::Custom(
                    Box::leak(Box::new(name.get_custom_name())),
                    0,
                ));
                for a in funcs.clone() {
                    self.check_ast(&a);
                }
                self.env.current_node = prevcur;
                self.env.current_typeclass = None;
            }
            Ast::MethodDef {
                parent,
                name,
                rettype,
                args,
                limits,
                body,
            } => {
                let nargs: Vec<PartialType> =
                    args.iter().map(|a| a.1.convert(&mut self.env)).collect();
                let nrt = rettype.convert(&mut self.env);

                self.extend_enter_do(
                    PartialType::Fn(
                        Box::leak(nargs.clone().into_boxed_slice()),
                        Box::leak(Box::new(nrt)),
                    ),
                    GraphEdge::Name(name.to_string()),
                    |this| {
                        for arg in nargs.iter().zip(args.clone()) {
                            this.env
                                .extend_curr(*arg.0, GraphEdge::Name(arg.1 .0));
                        }
                        this.env.current_func = this.env.current_node;
                        let a: Vec<PartialType> =
                            body.iter().map(|b| this.clone().check_expr(b)).collect();
                    },
                );
            }
            _ => todo!("{:?}", a),
        }
    }

    pub fn check(&mut self, mut p: Box<Program>) {
        //let thisp: &'static Program = Box::leak(p);
        let rootidx = self.env.g.add_node(PartialType::Root);
        self.env.current_node = Some(rootidx);
        p.modules.reverse();
        for module in &p.modules {
            let current_current = self.env.current_node;
            self.env
                .extend_enter(PartialType::Module, GraphEdge::Name(module.name.clone()));
            for a in &module.body {
                self.check_ast(a)
            }
            self.env.current_node = current_current;
        }
    }
}
