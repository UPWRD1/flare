use petgraph::{
    prelude::{DiGraphMap, GraphMap},
    visit::{
        DfsPostOrder, EdgeFiltered,
        EdgeRef,
    },
    Directed, Direction::Outgoing,
};

use crate::root::resource::ast::{Ast, Expr, Program, SymbolType};

#[derive(Clone, Debug, Default, Hash, Eq, PartialEq, Copy, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum PartialType {
    Root,
    Module(&'static str),
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

    pub fn get_lit_name(&self) -> Option<String> {
        match self {
            PartialType::Module(n) => Some(n.to_string()),
            PartialType::Record(n) => Some(n.to_string()),
            PartialType::Enum(n) => Some(n.to_string()),
            PartialType::Custom(n, _) => Some(n.to_string()),
            _ => None,
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
                // dbg!(t.current_node);
                let x = t.get_name(GraphEdge::Name(n.to_string()), true);
                if x.is_some() {
                    //dbg!(n);
                    //dbg!(x);
                    return x.unwrap();
                } else {
                    return PartialType::Custom(Box::leak(Box::new(n.clone())), c.len());
                }
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
    Module(String),
}

impl From<Expr> for GraphEdge {
    fn from(value: Expr) -> Self {
        match value {
            Expr::Symbol(s) => Self::Name(s.to_string()),
            _ => Self::Expr(value.clone()),
        }
    }
}

impl GraphEdge {
    pub fn get_name(&self) -> String {
        match self {
            GraphEdge::Name(n) => n.clone(),
            GraphEdge::Module(n) => n.clone(),
            _ => todo!(),
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
    //m: HashMap<String, PartialType>,
    tvg: TyVarGenerator,
    current_node: Option<PartialType>,
    current_func: Option<PartialType>,
    current_typeclass: Option<PartialType>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            g: GraphMap::<PartialType, GraphEdge, Directed>::new(),
           // m: HashMap::new(),
            tvg: TyVarGenerator::new(),
            current_node: None,
            current_func: None,
            current_typeclass: None,
        }
    }

    fn extend_curr(&mut self, p: PartialType, e: GraphEdge) -> PartialType {
        let curr = self.current_node.unwrap();
        println!("extend {:?} with {:?} via {:?}", curr, p, e);
        self.g.add_edge(curr, p, e);
        self.current_node = Some(curr);
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

    fn get_name(&mut self, e: GraphEdge, is_type: bool) -> Option<PartialType> {
        // println!(
        //     "{:?}",
        //     petgraph::dot::Dot::with_config(&(self.g.clone()), &[])
        // );
        //dbg!(e.clone());
        assert!(matches!(e, GraphEdge::Name(_)));
        if e == GraphEdge::Name("self".to_string()) {
            return self.current_typeclass;
        }
        let filtered = EdgeFiltered::from_fn(&self.g, |edge_ref| {
            *edge_ref.weight() == e //&& edge_ref.target() == self.current_node.unwrap()
        });

        let mut dfs = DfsPostOrder::new(&filtered, self.current_node?);
        let mut r: Vec<PartialType> = vec![];
        while let Some(t) = dfs.next(&self.g) {
            r.push(t)
        }
        //dbg!(r.clone());
        if is_type {
            r = r
                .iter()
                .filter(|x| {
                    x.get_lit_name()
                        .map_or_else(|| false, |v| v == e.get_name())
                })
                .cloned()
                .collect();
            //dbg!(r.clone());
        }
        r.first().copied()
    }

    fn get_parented_name(
        &mut self,
        p: PartialType,
        e: GraphEdge,
        is_type: bool,
    ) -> Option<PartialType> {
        let curr_curr = self.current_node;
        self.current_node = Some(p);
        let res = self.get_name(e, is_type);
        self.current_node = curr_curr;
        res
    }

    fn unify(&mut self, l: PartialType, r: PartialType) -> Result<PartialType, String> {
        //dbg!(l, r);
        match (l, r) {
            (PartialType::Int, PartialType::Int)
            | (PartialType::Flt, PartialType::Flt)
            | (PartialType::Str, PartialType::Str)
            | (PartialType::Bool, PartialType::Bool) => Ok(l),
            
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
            (PartialType::Variable(_), t) => {
                self.g.add_edge(l, r, GraphEdge::Substitution);
                Ok(t)
            }
            (t, PartialType::Variable(_)) => {
                self.g.add_edge(r, l, GraphEdge::Substitution);
                Ok(t)
            }
            

            (PartialType::Mut(t), u) => {
                let res = self.unify(*t, u).unwrap();
                self.g.add_edge(res, r, GraphEdge::Substitution);
                Ok(PartialType::Mut(Box::leak(Box::new(res))))
            }

            (PartialType::Field(_, t), PartialType::Field(_, u)) => {
                let res = self.unify(*t, *u);
                if res.is_ok() {
                    self.g.add_edge(res.clone()?, r, GraphEdge::Substitution);
                    return res
                }
                Err(format!("Cannot unify {:?} with {:?}", l, r))            
            }

            (PartialType::Field(_, t), u) => {
                let res = self.unify(*t, u);
                if res.is_ok() {
                    self.g.add_edge(l, res.clone()?, GraphEdge::Substitution);
                    return res
                }
                Err(format!("Cannot unify {:?} with {:?}", l, r))            
            }

            (t, PartialType::Field(_, u)) => {
                let res = self.unify(t, *u);
                if res.is_ok() {
                    //self.g.add_edge(res.clone()?, *u, GraphEdge::Substitution);
                    self.g.add_edge(l, r, GraphEdge::Substitution);

                    return res
                }
                Err(format!("Cannot unify {:?} with {:?}", l, r))            
            }
            (t, u) if t == u => Ok(t),

            _ => Err(format!("Cannot unify {:?} with {:?}", l, r)),
        }
    }

    fn get_children(&mut self, t: PartialType) -> Vec<String> {
        self.g.edges_directed(t, Outgoing).map(|x| x.2.get_name()).collect()
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
        //dbg!(e.clone());
        let res = match e {
            Expr::Int(_) => PartialType::Int,
            Expr::Uint(_) => PartialType::Uint,
            Expr::Flt(_) => PartialType::Flt,
            Expr::Bool(_) => PartialType::Bool,
            Expr::Str(_) => PartialType::Str,
            Expr::Symbol(name) => self
                .env
                .get_name(GraphEdge::Name(name.to_string()), false)
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
                    .get_name(GraphEdge::Name(name.get_symbol_name().to_string()), false)
                    .unwrap();
                let tfields: Vec<String> = self.env.get_children(t);
                dbg!(fields.clone());
                for f in fields.iter() {
                    let fieldtype = self.check_expr(&f.1);
                    let resty = self.env.get_parented_name(t, GraphEdge::Name(f.0.clone()), false).unwrap();
                    self.env.unify(fieldtype, resty).unwrap();
                }
                t
            }

            Expr::Call { name, args } => match *name.clone() {
                Expr::Symbol(s) => {
                    let t = self
                        .env
                        .get_name(GraphEdge::Name(s.to_string()), false)
                        .expect("Function is not defined!");
                    let nargs: Vec<PartialType> = args.iter().map(|a| self.check_expr(a)).collect();
                    assert!(matches!(t, PartialType::Fn(_, _)));
                    let targs = t.get_fn_args();
                    for arg in targs.iter().zip(nargs) {
                        self.env.unify(*arg.0, arg.1).unwrap();
                    }
                    t
                }
                Expr::FieldAccess(f, c) => {
                    //let nf = self.check_expr(&f);
                    //dbg!(f.clone());
                    let t = self
                        .env
                        .get_name( GraphEdge::Name(c.to_string()), false)
                        .expect(&format!("Function {:?} is not defined!", c));
                    //dbg!(t);
                    let nargs: Vec<PartialType> =
                        args.iter().map(|a| self.check_expr(a)).collect();
                    assert!(matches!(t, PartialType::Fn(_, _)));
                    let targs = t.get_fn_args();
                    //dbg!(nargs.clone());
                    //dbg!(targs);
                    assert!(nargs.len() == targs.len());

                    for arg in targs.iter().zip(nargs) {
                        self.env.unify(*arg.0, arg.1).unwrap();
                    }
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
                let nf = self.check_expr(par);
                // let nf = self
                //     .env
                //     .get_parented_name(np, GraphEdge::Name(name.get_symbol_name()), false)
                //     .unwrap();
                let t = self
                    .env
                    .get_parented_name(nf, GraphEdge::Name(name.to_string()), false)
                    .unwrap_or_else(|| panic!("Field {:?} is not defined for {:?}", name, nf));
                return t;
            }
            Expr::ModuleCall { module, name, args } => {
                let t = self.check_expr(module);
                dbg!(t);
                let nargs: Vec<PartialType> = args.iter().map(|a| self.check_expr(a)).collect();
                assert!(matches!(t, PartialType::Fn(_, _)));
                let targs = t.get_fn_args();
                for arg in targs.iter().zip(nargs) {
                    self.env.unify(*arg.0, arg.1).unwrap();
                }
                t
            }
            _ => todo!("{:?}", e),
        };

        res
    }

    fn check_ast(&mut self, a: &Ast) {
        //dbg!(self.env.current_node);
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
                            this.env.extend_curr(*arg.0, GraphEdge::Name(arg.1 .0));
                        }
                        this.env.current_func = this.env.current_node;
                        let a: Vec<PartialType> =
                            body.iter().map(|b| this.clone().check_expr(b)).collect();
                        let r = rettype.clone().convert(&mut this.env);
                        this.env.unify(r, *a.last().unwrap()).unwrap()
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
                self.extend_enter_do(p, GraphEdge::Name(name.to_string()), |this| {
                    for a in members.to_vec() {
                        let nt = this.env.tvg.generate();
                        let m2 = a.1.convert(&mut this.env);
                        let last = this.env.extend_curr(
                            PartialType::Field(nt, Box::leak(Box::new(m2))),
                            GraphEdge::Name(a.0.clone()),
                        );
                        this.env.unify(last, m2).unwrap();
                    }
                });
            }
            Ast::TypeDef { name, funcs } => {
                let prevcur = self.env.current_node;
                let prevtc = self.env.current_typeclass;
                let c = name.convert(&mut self.env);

                self.env.current_node = Some(c);

                self.env.current_typeclass = Some(c);

                //dbg!(name);
                let d: PartialType = self
                    .env
                    .get_name(GraphEdge::Name(name.get_custom_name()), true)
                    .unwrap();
                dbg!(d);
                //dbg!(c);

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
                        let prevfunc = this.env.current_func;
                        this.env.current_func = this.env.current_node;
                        for arg in nargs.iter().zip(args.clone()) {
                            this.env.extend_curr(*arg.0, GraphEdge::Name(arg.1 .0));
                        }
                        let a: Vec<PartialType> =
                            body.iter().map(|b| this.clone().check_expr(b)).collect();
                        //this.env.current_func = prevfunc;
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
            self.extend_enter_do(
                PartialType::Module(Box::leak(Box::new(module.name.clone()))),
                GraphEdge::Name(module.name.clone()),
                |this| {
                    for a in &module.body {
                        this.check_ast(a);
                    }
                },
            );
        }
        println!(
            "{:?}",
            petgraph::dot::Dot::with_config(&(self.env.clone().g), &[])
        );
    }
}
