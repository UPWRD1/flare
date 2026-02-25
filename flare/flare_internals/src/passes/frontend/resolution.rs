use chumsky::span::{SimpleSpan, Span};
use internment::Intern;
use petgraph::{
    algo::toposort,
    dot::Config,
    graph::NodeIndex,
    visit::{Dfs, IntoNodeReferences, Walker},
};
use rustc_hash::FxHashSet;

// const INTRINSIC_FUNC_ADD: usize = 0;
// const INTRINSIC_FUNC_SUB: usize = 1;
// const INTRINSIC_FUNC_MUL: usize = 2;
// const INTRINSIC_FUNC_DIV: usize = 3;
// const INTRINSIC_FUNC_CEQ: usize = 4;
// const INTRINSIC_FUNC_NEQ: usize = 5;
// const INTRINSIC_FUNC_CLT: usize = 6;
// const INTRINSIC_FUNC_CLE: usize = 7;
// const INTRINSIC_FUNC_CGT: usize = 8;
// const INTRINSIC_FUNC_CGE: usize = 9;

type DiGraph<N, E> = petgraph::graph::DiGraph<N, E>;

const INACCESSIBLE_IDENTIFIER: &str = "%INACCESSIBLE%";
use crate::{
    passes::frontend::{
        environment::Environment,
        typing::{ClosedRow, Row, Type},
    },
    resource::{
        errors::{self, CompResult, CompilerErr, DynamicErr, ErrorCollection},
        rep::{
            common::{Ident, Spanned},
            frontend::{
                ast::{Direction, Expr, ItemId, Kind, Label, Untyped, UntypedAst},
                cst::{CstExpr, MatchArm, Pattern, UntypedCst},
                csttypes::{CstClosedRow, CstType},
                entry::{FunctionItem, Item, ItemKind, PackageEntry},
                quantifier::QualifierFragment,
            },
        },
    },
};

/// The name resolution engine.
/// Once the environment is built from the parser, the `Resolver` takes ownership
/// of the graph.
///
/// The engine works in a two-pass system. First, each `Item` is analyzed
/// for names that need resolving. This information becomes a dependency
/// graph / DAG that is used to determine the order in which the items should
/// be typechecked.
///
/// Finally the AST in each item is modified to reference the `Expr::Item`
/// instead of plain Identifiers.
/// In theory, this whole process could be built into the intial graph building.
/// However, I'd rather maintain a separation of concerns,
/// even if it would be more efficient to use a single pass over the environment.
pub struct Resolver {
    env: Environment<UntypedCst>,
    // new_env: Environment<UntypedAst>
    current_parent: QualifierFragment,
    current_dag_node: Option<NodeIndex>,
    pub dag: DiGraph<DagIdx, ()>,
    main_dag_idx: Option<NodeIndex>,
    errors: Vec<CompilerErr>,
}

type DagIdx = usize;

impl Resolver {
    pub fn new(env: Environment<UntypedCst>) -> Self {
        Self {
            env,
            current_parent: QualifierFragment::Root,
            current_dag_node: None,
            dag: DiGraph::new(),
            main_dag_idx: None,
            // generic_scope: im::HashMap::with_hasher(FxBuildHasher),
            errors: Vec::new(),
            // intrinsics,
        }
    }

    pub fn analyze(mut self) -> CompResult<(Environment<UntypedAst>, Vec<NodeIndex>)> {
        let filtered: Vec<(NodeIndex, PackageEntry<UntypedCst>)> = self
            .env
            .graph
            .node_indices()
            .filter_map(|idx| {
                self.env
                    .value(idx)
                    .map(|x| {
                        if let ItemKind::Package(p) = x.kind {
                            Some((idx, p))
                        } else {
                            None
                        }
                    })
                    .ok()
                    .flatten()
            })
            .collect();

        let err_no_main = DynamicErr::new("Could not find a main function").label(
            "not found in this package",
            filtered.first().expect("No packages").1.name.1,
        );
        for (idx, p) in filtered {
            self.analyze_package(idx, p)
        }

        let reachable: FxHashSet<NodeIndex> =
            Dfs::new(&self.dag.clone(), self.main_dag_idx.ok_or(err_no_main)?)
                .iter(&self.dag)
                .collect();
        self.dag.reverse();
        // dbg!(&reachable);
        // let sorted = reachable;
        let sorted: Vec<NodeIndex> = toposort(&self.dag, None)
            // self.debug();
            // let sorted: Vec<NodeIndex> = kosaraju_scc(&self.dag)
            .into_iter()
            .flatten()
            .filter(|x| reachable.contains(x))
            .map(|x| NodeIndex::new(*self.dag.node_weight(x).expect("Node should exist")))
            .collect();

        let g = self.env.graph.map_owned(|idx, n| todo!(), |idx, e| e);
        let env = Environment::from_graph_and_root(g, self.env.root);
        Ok((env, sorted))
    }

    fn analyze_package(&mut self, idx: NodeIndex, p: PackageEntry<UntypedCst>) {
        self.current_parent = QualifierFragment::Package(p.name.0);
        let children = self
            .env
            .graph
            .neighbors_directed(idx, petgraph::Direction::Outgoing)
            .map(NodeIndex::index)
            .collect::<Vec<usize>>();

        for child in children {
            let node_idx = NodeIndex::new(child);
            // dbg!(child);
            let dag_idx = if let Some((node_idx, _)) =
                self.dag.node_references().find(|(_, x)| **x == child)
            {
                // dbg!(node_idx);
                node_idx
            } else {
                self.dag
                    .try_add_node(child)
                    .unwrap_or_else(|_| unreachable!("Graph overflow in resolution"))
            };

            self.analyze_item(child, node_idx, dag_idx);
        }
    }

    fn analyze_item(
        &mut self,
        child: usize,
        node_idx: NodeIndex,
        dag_idx: NodeIndex,
    ) -> Item<UntypedAst> {
        let item = self
            .env
            .graph
            .node_weight(node_idx)
            .expect("Node should exist");

        match item.kind {
            ItemKind::Package(PackageEntry { name, id }) => {
                Item::new(ItemKind::Package(PackageEntry { name, id }))
            }
            ItemKind::Function(f) => {
                if *f.name.0 == "main" {
                    self.main_dag_idx = Some(dag_idx);
                }

                let f = self.analyze_func(f, dag_idx);
                Item::new(ItemKind::Function(f))
            }
            ItemKind::Type(n, g, t) => {
                // self.generic_scope.clear();
                let t = self.analyze_type(t);
                Item::new(ItemKind::Type(n, vec![].leak(), t))
                // self.generic_scope.clear();

                // dbg!(t); /* do nothing */
            }

            ItemKind::Extern { name, args, sig } => {
                let sig = self.in_context(|me| me.analyze_type(sig), dag_idx);

                Item::new(ItemKind::Extern { name, args, sig })
            }
            _ => unreachable!("{child:?}, {:?}", item.kind),
        }
    }

    fn in_context<T>(
        &mut self,
        mut f: impl FnMut(&mut Self) -> T,
        dag_idx: NodeIndex,
        // dag: &mut DiGraph<usize, ()>,
    ) -> T {
        let old = self.current_dag_node;
        self.current_dag_node = Some(dag_idx);

        let out = f(self);
        self.current_dag_node = old;
        out
    }

    fn analyze_func(
        &mut self,
        the_func: FunctionItem<UntypedCst>,
        idx: NodeIndex,
    ) -> FunctionItem<UntypedAst> {
        self.in_context(
            |me| {
                // me.generic_scope.clear();
                let sig = me.analyze_type(the_func.sig);
                let body = me.analyze_expr(the_func.body, &[]);
                // me.generic_scope.clear();
                FunctionItem {
                    sig,
                    body,
                    name: the_func.name,
                }
            },
            idx,
        )
    }

    fn analyze_type(&mut self, t: Spanned<Intern<CstType>>) -> Spanned<Intern<Type>> {
        // dbg!(self.current_node);
        // dbg!(&self.generic_scope);
        // dbg!(t);
        match *t.0 {
            CstType::Func(l, r) => {
                let l = self.analyze_type(l);
                let r = self.analyze_type(r);

                t.convert(Type::Func(l, r))
            }
            CstType::Label(l, the_r) => {
                let new_t = self.analyze_type(the_r);
                // dbg!(new_t);
                t.convert(Type::Label(l, new_t))
            }
            CstType::User(name, instanced_generics) => {
                let the_item = self.resolve_name_type(&name);
                // dbg!(the_item.get_type_universal());
                if let Ok(item_id) = the_item {
                    // self.dag_add(item_id);
                    let the_item = self
                        .env
                        .value(NodeIndex::new(item_id.0))
                        .expect("Item has not been defined in the environment");

                    if let ItemKind::Type(_, _generics, new_t) = the_item.kind {
                        // if !instanced_generics.is_empty() {
                        let analyzed_instances: Vec<_> = instanced_generics
                            .iter()
                            .map(|ty| self.analyze_type(*ty))
                            .collect();
                        let new_t = self.analyze_type(new_t);

                        let mut final_t = analyzed_instances
                            .into_iter()
                            .fold(new_t, |x, y| Spanned(Type::TypeApp(x, y).into(), x.1));
                        final_t.1 = t.1;
                        final_t
                        // self.analyze_type(final_t)
                    } else {
                        panic!("not a type")
                    }
                } else {
                    let err = errors::not_defined(name.0, &name.1);
                    self.errors.push(err);
                    name.convert(Type::Hole)
                }
            }
            CstType::Prod(r) => {
                let new_r = Row::Closed(
                    ClosedRow {
                        values: r
                            .values
                            .iter()
                            .map(|t| -> Spanned<Intern<Type>> {
                                // let t = self.resolve_name_generic(&n.0)?.get_ty()?.0;
                                self.analyze_type(*t)
                            })
                            .collect::<Vec<_>>()
                            .leak(),
                        fields: r.fields,
                    }
                    .sort(),
                );

                t.convert(Type::Prod(t.convert(new_r)))
            }
            CstType::Sum(r) => {
                let new_r = Row::Closed(
                    ClosedRow {
                        values: r
                            .values
                            .iter()
                            .map(|t| -> Spanned<Intern<Type>> {
                                // let t = self.resolve_name_generic(&n.0)?.get_ty()?.0;
                                self.analyze_type(*t)
                            })
                            .collect::<Vec<_>>()
                            .leak(),
                        fields: r.fields,
                    }
                    .sort(),
                );
                t.convert(Type::Sum(t.convert(new_r)))
            }
            CstType::GenericApp(l, r) => {
                // let l = self.analyze_type(l);
                // let r = self.analyze_type(r);

                if let CstType::GenericFun(param, body) = *l.0 {
                    self.analyze_type(subst_generic_type(body, param.0, r.0))
                } else {
                    panic!("Not a generic function")
                    // t.modify(Type::TypeApp(l, r))
                }
                // t.modify(Type::TypeApp(l, r))
            }
            CstType::GenericFun(l, r) => {
                let l = self.analyze_type(l);
                let r = self.analyze_type(r);

                t.convert(Type::TypeFun(l, r))
            }
            CstType::Generic(spanned) => panic!("Escaped Generic {spanned:?}"),
            CstType::Particle(p) => t.convert(Type::Particle(p)),
            CstType::Unit => t.convert(Type::Unit),
            CstType::Num => t.convert(Type::Num),
            CstType::Bool => t.convert(Type::Bool),
            CstType::String => t.convert(Type::String),
            CstType::Hole => t.convert(Type::Hole),
        }
    }

    #[allow(unused_variables)]
    fn analyze_expr(
        &mut self,
        expr: Spanned<Intern<CstExpr<Untyped>>>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> Spanned<Intern<Expr<Untyped>>> {
        // dbg!(&expr);

        match *expr.0 {
            CstExpr::Ident(u) => {
                if let Some(expr) = vars
                    .iter()
                    .rev()
                    .find(|n| u.ident().is_ok_and(|name| n.0 == name.0))
                {
                    expr.1
                } else {
                    // dbg!(expr);
                    self.resolve_name_expr(expr)
                }
            }
            CstExpr::Concat(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                // let t = Type::Prod(crate::passes::midend::typing::Row::Closed(());
                expr.convert(Expr::Concat(l, r))
            }
            CstExpr::Project(direction, ex) => {
                let ex = self.analyze_expr(ex, vars);
                expr.convert(Expr::Project(direction, ex))
            }
            CstExpr::Inject(direction, ex) => {
                let ex = self.analyze_expr(ex, vars);
                expr.convert(Expr::Inject(direction, ex))
            }
            CstExpr::Branch(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);

                expr.convert(Expr::Branch(l, r))
            }
            CstExpr::Label(l, v) => {
                let v = self.analyze_expr(v, vars);
                // let new_vars = [vars, &[(l.0.0, v)]].concat();

                expr.convert(Expr::Label(l, v))
            }
            CstExpr::Unlabel(v, l) => {
                let v = self.analyze_expr(v, vars);
                expr.convert(Expr::Unlabel(v, l))
            }
            CstExpr::Pat(spanned) => todo!(),
            CstExpr::Mul(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                expr.convert(Expr::Mul(l, r))
            }
            CstExpr::Div(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                expr.convert(Expr::Div(l, r))
            }
            CstExpr::Add(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                expr.convert(Expr::Add(l, r))
            }
            CstExpr::Sub(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                expr.convert(Expr::Sub(l, r))
            }
            CstExpr::Comparison(l, comparison_op, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                expr.convert(Expr::Comparison(l, comparison_op, r))
            }
            CstExpr::Call(func, arg) => {
                let func = self.analyze_expr(func, vars);

                let arg = self.analyze_expr(arg, vars);
                if let Expr::Item(id, Kind::Ty) = *func.0 {
                    todo!(
                        "This would be a type constructor, but it lowk isn't being used right now"
                    )
                };
                expr.convert(Expr::Call(func, arg))
            }
            CstExpr::FieldAccess(l, r) => self.resolve_field_access(expr, l, r, vars),
            CstExpr::If(cond, then, otherwise) => {
                let cond = self.analyze_expr(cond, vars);
                let then_vars = vars;
                let then = self.analyze_expr(then, then_vars);
                let otherwise_vars = vars;
                let otherwise = self.analyze_expr(otherwise, otherwise_vars);
                expr.convert(Expr::If(cond, then, otherwise))
            }
            CstExpr::Match(matchee, branches) => {
                let matchee = self.analyze_expr(matchee, vars);

                let branches: Vec<_> = branches
                    .iter()
                    .map(|b| self.resolve_branch(*b, vars))
                    .collect();
                assert!(!branches.is_empty());
                let branches = branches
                    .into_iter()
                    .reduce(|l, r| Spanned(Expr::Branch(l, r).into(), l.1.union(r.1)))
                    .expect("Branches was empty; match has no arms");

                expr.convert(Expr::Call(branches, matchee))
            }
            CstExpr::Lambda(arg, body) => self.resolve_lambda(expr, arg, body, vars),
            CstExpr::Let(id, body, and_in) => self.resolve_let(expr, id, body, and_in, vars),
            CstExpr::Number(n) => expr.convert(Expr::Number(n)),
            CstExpr::String(s) => expr.convert(Expr::String(s)),
            CstExpr::Bool(b) => expr.convert(Expr::Bool(b)),
            CstExpr::Unit => expr.convert(Expr::Unit),
            CstExpr::Particle(p) => expr.convert(Expr::Particle(p)),
            CstExpr::Hole(v) => expr.convert(Expr::Hole(v)),
            CstExpr::Item(item_id, kind) => expr.convert(Expr::Item(item_id, kind)),
            CstExpr::Myself => todo!(),
            CstExpr::MethodAccess { obj, prop, method } => todo!(),
        }
    }

    fn resolve_let(
        &mut self,
        expr: Spanned<Intern<CstExpr<Untyped>>>,
        id: Untyped,
        body: Spanned<Intern<CstExpr<Untyped>>>,
        and_in: Spanned<Intern<CstExpr<Untyped>>>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> Spanned<Intern<Expr<Untyped>>> {
        let body = self.analyze_expr(body, vars);

        let new_vars = [vars, &[(id.0.0, body)]].concat();
        let and_in = self.analyze_expr(and_in, &new_vars);
        // let lambda = Spanned(Expr::Lambda(id, and_in, LambdaInfo::Anon).into(), expr.1);
        expr.convert(Expr::Let(id, body, and_in))
    }

    fn resolve_lambda(
        &mut self,
        expr: Spanned<Intern<CstExpr<Untyped>>>,
        arg: Untyped,
        body: Spanned<Intern<CstExpr<Untyped>>>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> Spanned<Intern<Expr<Untyped>>> {
        let new_vars = &[
            vars,
            &[(
                arg.0.0,
                arg.ident()
                    .expect("Expected expression to be namable")
                    .convert(Expr::Ident(arg)),
            )],
        ]
        .concat();
        let body = self.analyze_expr(body, new_vars);
        // *vars.iter_mut().find(|x| x.0 == arg.0 .0).unwrap() = (arg.0 .0, body);
        expr.convert(Expr::Lambda(arg, body))
    }

    fn resolve_field_access(
        &mut self,
        expr: Spanned<Intern<CstExpr<Untyped>>>,
        l: Spanned<Intern<CstExpr<Untyped>>>,
        r: Spanned<Intern<CstExpr<Untyped>>>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> Spanned<Intern<Expr<Untyped>>> {
        let l = self.analyze_expr(l, vars);
        if let Expr::Item(_, _) = *l.0 {
            self.resolve_name_expr(r)
        } else if let Expr::Ident(n) = *l.0 {
            if let Some((_variable, val)) = vars.iter().find(|x| x.0 == n.0.0) {
                let projection: Spanned<Intern<Expr<Untyped>>> = {
                    let combo = *val;
                    let id = r.ident().expect("Expression should be nameable");
                    {
                        let ex = combo.convert(Expr::Project(Direction::Right, combo));

                        combo.convert(Expr::Unlabel(ex, Label(id)))
                    }
                };
                expr.convert(projection.0)
            } else {
                todo!()
            }
        } else if let Expr::Unlabel(_combo, _labell) = *l.0 {
            self.resolve_name_expr(r)
        } else {
            let ex = l.convert(Expr::Project(Direction::Right, l));

            let id = r.ident().expect("Expression should be nameable");
            l.convert(Expr::Unlabel(ex, Label(id)))
            // todo!("{l:?}")
        }
    }

    fn resolve_pattern(
        &mut self,
        p: Spanned<Intern<Pattern<Untyped>>>,
        vars: Vec<Untyped>,
    ) -> (Spanned<Intern<Expr<Untyped>>>, Vec<Untyped>) {
        // dbg!(&p);

        match *p.0 {
            Pattern::Wildcard => (
                p.convert(Expr::Ident(Untyped(p.convert(p.1.to_string())))),
                vars,
            ),
            Pattern::Var(v) => (p.convert(Expr::Ident(v)), [vars, [v].to_vec()].concat()),
            Pattern::Number(_) => todo!(),
            Pattern::String(_) => todo!(),
            Pattern::Particle(p) => (p.convert(Expr::Particle(p)), vars),
            Pattern::Bool(_) => todo!(),
            Pattern::Unit => (p.convert(Expr::Unit), vars),
            Pattern::Ctor(label, ex) => {
                if *ex.0 == Pattern::Unit {
                    let inaccessible = Untyped(p.convert(INACCESSIBLE_IDENTIFIER.to_string()));

                    (
                        p.convert(Expr::Unit),
                        [vars, [inaccessible].to_vec()].concat(),
                    )
                } else {
                    let (ex, vars) = self.resolve_pattern(ex, vars);

                    (p.convert(Expr::Unlabel(ex, label)), vars)
                }
            }
            _ => todo!(),
        }
    }

    fn resolve_branch(
        &mut self,
        b: MatchArm<Untyped>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> Spanned<Intern<Expr<Untyped>>> {
        let body = self.analyze_expr(b.body, vars);
        let (pat_expr, bindings) = self.resolve_pattern(b.pat, vec![]);

        bindings.into_iter().fold(body, |prev, v| {
            if *v.0.0 == INACCESSIBLE_IDENTIFIER {
                prev.modify(Expr::Lambda(v, prev))
            } else {
                let unwrapped = prev.modify(Expr::Let(v, pat_expr, prev));
                prev.modify(Expr::Lambda(v, unwrapped))
            }
        })
    }

    #[allow(dead_code, clippy::unwrap_used, clippy::dbg_macro)]
    #[deprecated]
    /// Pretty-print GraphViz for the internal state of the dependancy graph.
    fn debug(&self) {
        let render = |_, (_, v): (_, &usize)| {
            format!(
                "label = \"{}\"",
                self.env
                    .value(NodeIndex::from(*v as u32))
                    .unwrap()
                    .ident()
                    .unwrap()
                    .0
            )
        };
        let dot = petgraph::dot::Dot::with_attr_getters(
            &self.dag,
            &[
                Config::EdgeNoLabel,
                Config::NodeNoLabel,
                Config::RankDir(petgraph::dot::RankDir::LR),
            ],
            &|_, _| String::new(),
            &render,
        );
        dbg!(dot);
    }

    fn dag_add(&mut self, env_node: usize) {
        let n = if let Some((idx, _)) = self.dag.node_references().find(|(_, x)| **x == env_node) {
            idx
        } else {
            self.dag.add_node(env_node)
        };

        // dbg!(self.current_dag_node, n);

        if let Some(current) = self.current_dag_node
            && n != current
        // && !self.dag.contains_edge(current, n)
        {
            self.dag.update_edge(current, n, ());
        }
    }

    fn search_masterenv(
        &mut self,
        q: &QualifierFragment,
        s: &SimpleSpan<usize, u64>,
    ) -> CompResult<ItemId> {
        let search = self.env.get_from_context(q, &self.current_parent);
        search.map_or_else(
            |_| Err(errors::not_defined(q, s)),
            |node| {
                // if !matches!(q, QualifierFragment::Type(_)) {
                self.dag_add(node.index());
                // }
                Ok(ItemId(node.index()))
            },
        )
    }

    fn resolve_name_expr(
        &mut self,
        expr: Spanned<Intern<CstExpr<Untyped>>>,
    ) -> Spanned<Intern<Expr<Untyped>>> {
        let name = expr.ident().expect("Expression should be nameable");
        // self.env.debug();

        if let Ok(e) = self.search_masterenv(&QualifierFragment::Func(name.0), &expr.1) {
            expr.convert(Expr::Item(e, Kind::Func))
        } else if let Ok(e) = self.search_masterenv(&QualifierFragment::Type(name.0), &expr.1) {
            expr.convert(Expr::Item(e, Kind::Ty))
        } else if let Ok(e) = self.search_masterenv(&QualifierFragment::Package(name.0), &expr.1) {
            expr.convert(Expr::Item(e, Kind::Package))
        } else {
            let err = errors::not_defined(QualifierFragment::Wildcard(name.0), &expr.1);
            self.errors.push(err);
            expr.convert(Expr::Hole(Untyped(name)))
        }
    }

    fn resolve_name_type(&mut self, name: &impl Ident) -> CompResult<ItemId> {
        let name = name.ident()?;

        self.search_masterenv(&QualifierFragment::Type(name.0), &name.1)
            .map_or_else(
                |_| {
                    Err(errors::not_defined(
                        QualifierFragment::Wildcard(name.0),
                        &name.1,
                    ))
                },
                Ok,
            )
    }
}

pub fn subst_generic_type(
    subject: Spanned<Intern<CstType>>,
    target: Intern<CstType>,
    replacement: Intern<CstType>,
) -> Spanned<Intern<CstType>> {
    let mut accum: Spanned<Intern<CstType>> = subject;
    // dbg!(t);
    match (*subject.0, *target) {
        // t if t == *target => accum = accum.modify(replacement),
        (CstType::Generic(subj_name), CstType::Generic(target_name))
            if subj_name == target_name =>
        {
            accum = subject.modify(replacement);
        }

        (CstType::Func(l, r), _) => {
            accum = accum.modify(CstType::Func(
                subst_generic_type(l, target, replacement),
                subst_generic_type(r, target, replacement),
            ));
        }
        (CstType::Prod(r), _) => {
            let r = {
                let values: Vec<_> = r
                    .values
                    .iter()
                    .map(|x| subst_generic_type(*x, target, replacement))
                    .collect();

                CstClosedRow {
                    values: values.leak(),
                    ..r
                }
            };
            accum = accum.modify(CstType::Prod(r))
        }

        (CstType::Sum(r), _) => {
            let r = {
                let values: Vec<_> = r
                    .values
                    .iter()
                    .map(|x| subst_generic_type(*x, target, replacement))
                    .collect();

                CstClosedRow {
                    values: values.leak(),
                    ..r
                }
            };
            accum = accum.modify(CstType::Sum(r))
        }

        (CstType::Label(label, value), _) => {
            accum = accum.modify(CstType::Label(
                label,
                subst_generic_type(value, target, replacement),
            ))
        }
        // (Type::TypeApp(l, r), _) => {
        //     let l = subst_generic_type(l, target, replacement);
        //     let r = subst_generic_type(r, target, replacement);
        //     accum = accum.modify(Type::TypeApp(l, r))
        // }
        // GENERATED: Claude
        (CstType::GenericFun(param, body), _) => {
            if *param.0 == *target {
                // The parameter shadows the target, don't substitute in body
                accum = subject
            } else {
                // Safe to substitute in body
                let new_body = subst_generic_type(body, target, replacement);
                accum = accum.modify(CstType::GenericFun(param, new_body))
            }
            // let r = subst_generic_type(r, target, replacement);
            // accum = accum.modify(Type::TypeFun(l, r))
        }

        // Type::TypeApp(l, r) => {
        //     accum = accum.modify(Type::TypeApp(
        //         subst_generic_type(l, target, replacement),
        //         subst_generic_type(r, target, replacement),
        //     ));
        // }
        // Type::TypeFun(l, r) => {
        //     accum = accum.modify(Type::TypeFun(l, subst_generic_type(r, target, replacement)));
        // }
        _ => accum = subject,
    }

    accum
}
