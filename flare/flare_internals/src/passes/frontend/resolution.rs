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
        errors::{self, CompResult, DynamicErr},
        rep::{
            common::Ident,
            common::Spanned,
            frontend::{
                ast::{Direction, Expr, ItemId, Kind, Label, LambdaInfo, MatchArm, Pattern},
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
pub struct Resolver<'db> {
    db: &'db dyn salsa::Database,
    env: Environment<'db>,
    current_parent: QualifierFragment,
    current_dag_node: Option<NodeIndex>,
    pub dag: DiGraph<DagIdx, ()>,
    main_dag_idx: Option<NodeIndex>,
    // generic_scope: im::HashMap<String, Spanned<Intern<Type>>, FxBuildHasher>,
    // intrinsics: [(ItemId, Intern<Expr<Untyped>>); N],
}

type DagIdx = usize;

impl<'db> Resolver<'db> {
    pub fn new(db: &'db dyn salsa::Database, mut env: Environment) -> Self {
        // let intrinsics =vec![];
        //  for id in intrinsics {
        //      let name = Intern::from(id.into());
        //      // env.debug();
        //      //
        //      let e = env.add(
        //          env.root(db),
        //          QualifierFragment::Func(name),
        //          Item {
        //              kind: ItemKind::Extern {
        //                  name: Spanned(name, SimpleSpan::new(0, 0..0)),
        //                  args,
        //                  sig: Spanned(sig.into(), SimpleSpan::new(0, 0..0)),
        //              },
        //          },
        //      );
        //      let itemid = ItemId(e.index());
        //      (
        //          itemid,
        //          Expr::Item(itemid, Kind::Extern(name)).into(), // Expr::ExternFunc(itemid, (*name).clone().leak(), ty).into(),
        //      )
        //  }

        Self {
            db,
            env,
            current_parent: QualifierFragment::Root,
            current_dag_node: None,
            dag: DiGraph::new(),
            main_dag_idx: None,
            // generic_scope: im::HashMap::with_hasher(FxBuildHasher),
        }
    }

    pub fn build(&mut self) -> CompResult<Vec<NodeIndex>> {
        self.analyze()
        // let out = self.modify()
    }

    fn analyze(&mut self) -> CompResult<Vec<NodeIndex>> {
        let filtered: Vec<(NodeIndex, PackageEntry)> = self
            .env
            .graph(self.db)
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
        // self.debug();

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
        // sorted.reverse();
        // dbg!(&sorted);
        // self.debug();
        Ok(sorted)
    }

    fn analyze_package(&mut self, idx: NodeIndex, p: PackageEntry) {
        self.current_parent = QualifierFragment::Package(p.name.0);
        let children = self
            .env
            .graph(self.db)
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

            let item_kind = self
                .env
                .graph(self.db)
                .node_weight(node_idx)
                .expect("Node should exist")
                .kind;

            match item_kind {
                ItemKind::Package(_) => { /* do nothing */ }
                ItemKind::Function(mut f) => {
                    if *f.name.0.0 == "main" {
                        self.main_dag_idx = Some(dag_idx);
                    }
                    f = self.analyze_func(f, dag_idx);

                    let item = self
                        .env
                        .graph(self.db)
                        .node_weight_mut(node_idx)
                        .expect("Node should exist");
                    if let ItemKind::Function(ref mut func) = item.kind {
                        // println!("Analyzed {} : {}", f.name, f.sig);
                        *func = f;
                    };
                }
                ItemKind::Type(.., t) => {
                    // self.generic_scope.clear();
                    let t = self.analyze_type(t);
                    let item = self
                        .env
                        .graph(self.db)
                        .node_weight_mut(node_idx)
                        .expect("Node should exist");

                    if let ItemKind::Type(_, _, ref mut ty) = item.kind {
                        *ty = t;
                    };
                    // self.generic_scope.clear();

                    // dbg!(t); /* do nothing */
                }

                ItemKind::Extern { sig, .. } => {
                    let old_sig = sig;
                    let t = self.in_context(|me| me.analyze_type(sig), dag_idx);

                    let item = self
                        .env
                        .graph(self.db)
                        .node_weight_mut(node_idx)
                        .expect("Node should exist");
                    if let ItemKind::Extern { ref mut sig, .. } = item.kind {
                        *sig = old_sig.convert(t.0);
                    };
                }
                _ => unreachable!("{child:?}, {item_kind:?}"),
            }
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
        the_func: FunctionItem<Untyped>,
        idx: NodeIndex,
    ) -> FunctionItem<Untyped> {
        self.in_context(
            |me| {
                // me.generic_scope.clear();
                let sig = me.analyze_type(the_func.sig);
                let body = me.analyze_expr(the_func.body, &[]);
                // me.generic_scope.clear();
                FunctionItem {
                    sig,
                    body,
                    ..the_func
                }
            },
            idx,
        )
    }

    fn analyze_type(&mut self, t: Spanned<Intern<Type>>) -> Spanned<Intern<Type>> {
        // dbg!(self.current_node);
        // dbg!(&self.generic_scope);
        // dbg!(t);
        match *t.0 {
            // Type::Generic(name) => {
            //     if let Some(existing) = self.generic_scope.get(&*name.0) {
            //         *existing
            //     } else {
            //         self.generic_scope.insert(name.0.to_string(), t);
            //         t
            //     }
            // }
            Type::Func(l, r) => {
                let l = self.analyze_type(l);
                let r = self.analyze_type(r);

                t.modify(Type::Func(l, r))
            }
            Type::Label(l, the_r) => {
                let new_t = self.analyze_type(the_r);
                // dbg!(new_t);
                t.modify(Type::Label(l, new_t))
            }
            Type::User(name, instanced_generics) => {
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

                        let mut final_t = analyzed_instances
                            .into_iter()
                            .fold(new_t, |x, y| Spanned(Type::TypeApp(x, y).into(), x.1));
                        final_t.1 = t.1;
                        self.analyze_type(final_t)
                    } else {
                        panic!("not a type")
                    }
                } else {
                    let err = errors::not_defined(name.0, &name.1);
                    self.errors.push(err);
                    name.convert(Type::Hole)
                }
            }
            Type::Prod(r) => {
                let new_r = r.map(|r| match *r {
                    Row::Closed(closed_row) => {
                        Row::Closed(
                            ClosedRow {
                                values: closed_row
                                    .values
                                    .iter()
                                    .map(|t| -> Spanned<Intern<Type>> {
                                        // let t = self.resolve_name_generic(&n.0)?.get_ty()?.0;
                                        self.analyze_type(*t)
                                    })
                                    .collect::<Vec<_>>()
                                    .leak(),
                                ..closed_row
                            }
                            .sort(),
                        )
                        .into()
                    }
                    _ => unreachable!("All rows should be closed"),
                });

                t.modify(Type::Prod(new_r))
            }

            Type::Sum(r) => {
                let new_r = r.map(|r| match *r {
                    Row::Closed(closed_row) => {
                        Row::Closed(
                            ClosedRow {
                                values: closed_row
                                    .values
                                    .iter()
                                    .map(|t| -> Spanned<Intern<Type>> {
                                        // let t = self.resolve_name_generic(&n.0)?.get_ty()?.0;
                                        self.analyze_type(*t)
                                    })
                                    .collect::<Vec<_>>()
                                    .leak(),
                                ..closed_row
                            }
                            .sort(),
                        )
                        .into()
                    }
                    _ => unreachable!("All rows should be closed"),
                });
                t.modify(Type::Sum(new_r))
            }
            Type::Subtable(fields, s) => {
                let new_fields = self.analyze_type(fields);
                t.modify(Type::Subtable(new_fields, s))
            }
            // Type::TypeFun(g, l)
            Type::TypeApp(l, r) => {
                let l = self.analyze_type(l);
                let r = self.analyze_type(r);

                if let Type::TypeFun(param, body) = *l.0 {
                    self.analyze_type(subst_generic_type(body, param.0, r.0))
                } else {
                    t.modify(Type::TypeApp(l, r))
                }
                // t.modify(Type::TypeApp(l, r))
            }
            Type::TypeFun(l, r) => {
                let l = self.analyze_type(l);
                let r = self.analyze_type(r);

                t.modify(Type::TypeFun(l, r))
            }
            _ => t,
        }
    }

    #[allow(unused_variables)]
    fn analyze_expr(
        &mut self,
        expr: Spanned<Intern<Expr<Untyped>>>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> Spanned<Intern<Expr<Untyped>>> {
        // dbg!(&expr);

        match *expr.0 {
            Expr::Ident(u) => {
                if vars
                    .iter()
                    .rev()
                    .any(|n| u.ident().is_ok_and(|name| n.0 == name.0))
                {
                    expr
                } else {
                    // dbg!(expr);
                    self.resolve_name_expr(expr)
                }
            }
            Expr::Concat(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                // let t = Type::Prod(crate::passes::midend::typing::Row::Closed(());
                expr.convert(Expr::Concat(l, r))
            }
            Expr::Project(direction, ex) => {
                let ex = self.analyze_expr(ex, vars);
                expr.convert(Expr::Project(direction, ex))
            }

            Expr::Inject(direction, ex) => {
                let ex = self.analyze_expr(ex, vars);
                expr.convert(Expr::Inject(direction, ex))
            }
            Expr::Branch(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);

                expr.convert(Expr::Branch(l, r))
            }
            Expr::Label(l, v) => {
                let new_vars = [vars, &[(l.0.0, v)]].concat();

                let v = self.analyze_expr(v, &new_vars);
                expr.convert(Expr::Label(l, v))
            }
            Expr::Unlabel(v, l) => expr.convert(Expr::Unlabel(v, l)),
            Expr::Pat(spanned) => todo!(),
            Expr::Mul(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                expr.modify(Expr::Mul(l, r))
            }
            Expr::Div(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                expr.modify(Expr::Div(l, r))
            }
            Expr::Add(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                expr.modify(Expr::Add(l, r))
            }
            Expr::Sub(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                expr.modify(Expr::Sub(l, r))
            }
            Expr::Comparison(l, comparison_op, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                expr.modify(Expr::Comparison(l, comparison_op, r))
            }

            Expr::Call(func, arg) => {
                let func = self.analyze_expr(func, vars);

                let arg = self.analyze_expr(arg, vars);
                if let Expr::Item(id, Kind::Ty) = *func.0 {
                    todo!(
                        "This would be a type constructor, but it lowk isn't being used right now"
                    )
                };
                expr.convert(Expr::Call(func, arg))
            }
            Expr::FieldAccess(l, r) => self.resolve_field_access(expr, l, r, vars),
            Expr::If(cond, then, otherwise) => {
                let cond = self.analyze_expr(cond, vars);
                let then_vars = vars;
                let then = self.analyze_expr(then, then_vars);
                let otherwise_vars = vars;
                let otherwise = self.analyze_expr(otherwise, otherwise_vars);
                expr.convert(Expr::If(cond, then, otherwise))
            }
            Expr::Match(matchee, branches) => {
                let matchee = self.analyze_expr(matchee, vars);

                let branches: Vec<_> = branches.iter().map(|b| self.resolve_branch(*b)).collect();
                assert!(!branches.is_empty());
                let branches = branches
                    .into_iter()
                    .reduce(|l, r| Spanned(Expr::Branch(l, r).into(), l.1.union(r.1)))
                    .expect("Branches was empty; match has no arms");

                expr.convert(Expr::Call(branches, matchee))
            }
            Expr::Lambda(arg, body, is_anon) => self.resolve_lambda(expr, arg, body, is_anon, vars),
            Expr::Let(id, body, and_in) => self.resolve_let(expr, id, body, and_in, vars),

            _ => expr,
        }
    }

    fn resolve_let(
        &mut self,
        expr: Spanned<Intern<Expr<Untyped>>>,
        id: Untyped,
        body: Spanned<Intern<Expr<Untyped>>>,
        and_in: Spanned<Intern<Expr<Untyped>>>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> Spanned<Intern<Expr<Untyped>>> {
        let body = self.analyze_expr(body, vars);

        let new_vars = [vars, &[(id.0.0, body)]].concat();
        let and_in = self.analyze_expr(and_in, &new_vars);
        // let lambda = Spanned(Expr::Lambda(id, and_in, LambdaInfo::Anon).into(), expr.1);
        expr.modify(Expr::Let(id, body, and_in))
    }

    fn resolve_lambda(
        &mut self,
        expr: Spanned<Intern<Expr<Untyped>>>,
        arg: Untyped,
        body: Spanned<Intern<Expr<Untyped>>>,
        is_anon: LambdaInfo,
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
        expr.convert(Expr::Lambda(arg, body, is_anon))
    }

    fn resolve_field_access(
        &mut self,
        expr: Spanned<Intern<Expr<Untyped>>>,
        l: Spanned<Intern<Expr<Untyped>>>,
        r: Spanned<Intern<Expr<Untyped>>>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> Spanned<Intern<Expr<Untyped>>> {
        let l = self.analyze_expr(l, vars);
        if let Expr::Item(_, _) = *l.0 {
            self.resolve_name_expr(r)
        } else if let Expr::Ident(n) = *l.0 {
            if let Some((_variable, val)) = vars.iter().find(|x| x.0 == n..0) {
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
        // vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> Spanned<Intern<Expr<Untyped>>> {
        let (pat_expr, bindings) = self.resolve_pattern(b.pat, vec![]);

        bindings.into_iter().fold(b.body, |prev, v| {
            if *v.0.0 == INACCESSIBLE_IDENTIFIER {
                prev.modify(Expr::Lambda(v, prev, LambdaInfo::Anon))
            } else {
                let unwrapped = prev.modify(Expr::Let(v, pat_expr, prev));
                prev.modify(Expr::Lambda(v, unwrapped, LambdaInfo::Anon))
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
        expr: Spanned<Intern<Expr<Untyped>>>,
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

    pub fn finish(self) -> CompResult<Environment> {
        if self.errors.is_empty() {
            Ok(self.env)
        } else {
            Err(ErrorCollection::new(self.errors).into())
        }
    }
}

pub fn subst_generic_type(
    subject: Spanned<Intern<Type>>,
    target: Intern<Type>,
    replacement: Intern<Type>,
) -> Spanned<Intern<Type>> {
    let mut accum: Spanned<Intern<Type>> = subject;
    // dbg!(t);
    match (*subject.0, *target) {
        // t if t == *target => accum = accum.modify(replacement),
        (Type::Generic(subj_name), Type::Generic(target_name)) if subj_name == target_name => {
            accum = subject.modify(replacement);
        }

        (Type::Var(subj_var), Type::Var(target_var)) if subj_var == target_var => {
            accum = subject.modify(replacement);
        }
        (Type::Func(l, r), _) => {
            accum = accum.modify(Type::Func(
                subst_generic_type(l, target, replacement),
                subst_generic_type(r, target, replacement),
            ));
        }
        (Type::Prod(r), _) => {
            let r = r.map(|r| match *r {
                Row::Closed(closed_row) => {
                    let values: Vec<_> = closed_row
                        .values
                        .iter()
                        .map(|x| subst_generic_type(*x, target, replacement))
                        .collect();

                    Row::Closed(ClosedRow {
                        values: values.leak(),
                        ..closed_row
                    })
                    .into()
                }
                _ => r,
            });
            accum = accum.modify(Type::Prod(r))
        }

        (Type::Sum(r), _) => {
            let r = r.map(|r| match *r {
                Row::Closed(closed_row) => {
                    let values: Vec<_> = closed_row
                        .values
                        .iter()
                        .map(|x| subst_generic_type(*x, target, replacement))
                        .collect();

                    Row::Closed(ClosedRow {
                        values: values.leak(),
                        ..closed_row
                    })
                    .into()
                }
                _ => r,
            });
            accum = accum.modify(Type::Sum(r))
        }

        (Type::Label(label, value), _) => {
            accum = accum.modify(Type::Label(
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
        (Type::TypeFun(param, body), _) => {
            if *param.0 == *target {
                // The parameter shadows the target, don't substitute in body
                accum = subject
            } else {
                // Safe to substitute in body
                let new_body = subst_generic_type(body, target, replacement);
                accum = accum.modify(Type::TypeFun(param, new_body))
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
