use chumsky::span::{SimpleSpan, Span};
use internment::Intern;
use petgraph::{
    algo::kosaraju_scc,
    dot::Config,
    graph::NodeIndex,
    visit::{Dfs, IntoNodeReferences, NodeRef, Walker},
};
use rustc_hash::{FxBuildHasher, FxHashSet};
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

use crate::{
    passes::midend::{
        environment::Environment,
        typing::{ClosedRow, Row, Type},
    },
    resource::{
        errors::{self, CompResult, CompilerErr, DynamicErr},
        rep::{
            Spanned,
            ast::{Direction, Expr, ItemId, Kind, Label, LambdaInfo, MatchArm, Pattern, Untyped},
            common::Ident,
            entry::{FunctionItem, Item, ItemKind, PackageEntry},
            quantifier::QualifierFragment,
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
pub struct Resolver<const N: usize> {
    env: Environment,
    current_parent: QualifierFragment,
    current_dag_node: Option<NodeIndex>,
    pub dag: DiGraph<DagIdx, ()>,
    main_dag_idx: Option<NodeIndex>,
    // intrinsics: [(ItemId, Intern<Expr<Untyped>>); N],
}

type DagIdx = usize;

impl<const N: usize> Resolver<N> {
    pub fn new(
        mut env: Environment,
        intrinsics: [(
            impl Into<String>,
            &'static [Untyped],
            impl Into<Intern<Type>>,
        ); N],
    ) -> Self {
        fn register_intrinsic(
            (id, args, sig): (
                impl Into<String>,
                &'static [Untyped],
                impl Into<Intern<Type>>,
            ),
            env: &mut Environment,
        ) -> (ItemId, Intern<Expr<Untyped>>) {
            let name = Intern::from(id.into());
            // env.debug();
            //
            let e = env.add(
                env.root,
                QualifierFragment::Func(name),
                Item {
                    kind: ItemKind::Extern {
                        name: Spanned(name, SimpleSpan::new(0, 0..0)),
                        args,
                        sig: Spanned(sig.into(), SimpleSpan::new(0, 0..0)),
                    },
                },
            );
            let itemid = ItemId(e.index());
            (
                itemid,
                Expr::Item(itemid, Kind::Extern((*name).clone().leak())).into(), // Expr::ExternFunc(itemid, (*name).clone().leak(), ty).into(),
            )
        }

        for id in intrinsics {
            register_intrinsic(id, &mut env);
        }

        Self {
            env,
            current_parent: QualifierFragment::Root,
            current_dag_node: None,
            dag: DiGraph::new(),
            main_dag_idx: None,
            // intrinsics,
        }
    }

    pub fn build(&mut self) -> CompResult<Vec<NodeIndex>> {
        self.analyze()
        // let out = self.modify()
    }

    fn analyze(&mut self) -> CompResult<Vec<NodeIndex>> {
        let filtered: Vec<(NodeIndex, PackageEntry)> = self
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
            self.analyze_package(idx, p)?
        }
        // self.debug();
        // self.dag.reverse();

        let reachable: FxHashSet<NodeIndex> =
            Dfs::new(&self.dag.clone(), self.main_dag_idx.ok_or(err_no_main)?)
                .iter(&self.dag)
                .collect();
        let sorted: Vec<NodeIndex> = kosaraju_scc(&self.dag)
            .into_iter()
            .flatten()
            .filter(|x| reachable.contains(x))
            .map(|x| NodeIndex::new(*self.dag.node_weight(x).expect("Node should exist")))
            .collect();

        Ok(sorted)
    }

    fn analyze_package(&mut self, idx: NodeIndex, p: PackageEntry) -> CompResult<()> {
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

            // Extract the data we need to analyze (without holding a borrow)
            let item_kind = self
                .env
                .graph
                .node_weight(node_idx)
                .expect("Node should exist")
                .kind;

            match item_kind {
                ItemKind::Package(_) => Ok::<(), CompilerErr>(()),
                ItemKind::Function(mut f) => {
                    if *f.name.0.0 == "main" {
                        self.main_dag_idx = Some(dag_idx);
                    }
                    // dbg!(&f.name);
                    // Analyze the function (doesn't touch the graph)
                    self.analyze_func(&mut f, dag_idx)?;

                    // Now write back the result
                    let item = self
                        .env
                        .graph
                        .node_weight_mut(node_idx)
                        .expect("Node should exist");
                    if let ItemKind::Function(ref mut func) = item.kind {
                        *func = f;
                    }

                    Ok(())
                }
                ItemKind::Type(_, _, t) => {
                    // Analyze the type
                    // dbg!(t);
                    let t = self.in_context(|me| me.analyze_type(t), dag_idx)?;
                    // dbg!(t);
                    // Write back the result
                    let item = self
                        .env
                        .graph
                        .node_weight_mut(node_idx)
                        .expect("Node should exist");
                    if let ItemKind::Type(_, _, ref mut ty) = item.kind {
                        *ty = t;
                    }
                    Ok(())
                }

                ItemKind::Extern { sig, .. } => {
                    let old_sig = sig;
                    let t = self.in_context(|me| me.analyze_type(sig), dag_idx)?;

                    // Write back the result
                    let item = self
                        .env
                        .graph
                        .node_weight_mut(node_idx)
                        .expect("Node should exist");
                    if let ItemKind::Extern { ref mut sig, .. } = item.kind {
                        *sig = old_sig.convert(t.0);
                    }
                    Ok(())
                }
                _ => unreachable!("{child:?}, {item_kind:?}"),
            }?
        }

        Ok(())
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
        the_func: &mut FunctionItem<Untyped>,
        idx: NodeIndex,
    ) -> CompResult<()> {
        // let f_old = *the_func;
        self.in_context(
            |me| {
                let sig = me.analyze_type(the_func.sig)?;
                let body = me.analyze_expr(the_func.body, &[])?;
                the_func.sig = sig;
                the_func.body = body;
                Ok(())
            },
            idx,
        )
    }

    fn subst_generic_type(
        &mut self,
        t: Spanned<Intern<Type>>,
        gens: im::HashMap<Intern<Type>, Spanned<Intern<Type>>, FxBuildHasher>,
        // mut gens: &[Spanned<Intern<Type>>],
    ) -> CompResult<Spanned<Intern<Type>>> {
        let mut accum: Spanned<Intern<Type>> = t;

        match *t.0 {
            Type::Func(l, r) => {
                accum = accum.modify(Type::Func(
                    self.subst_generic_type(l, gens.clone())?,
                    self.subst_generic_type(r, gens)?,
                ));
            }
            Type::Generic(_) => {
                if let Some(g) = gens.get(&t.0) {
                    accum = *g
                } else {
                    panic!()
                    // accum = t
                }
            }
            Type::Prod(r) => {
                let r = match r {
                    Row::Closed(closed_row) => {
                        let values: Vec<_> = closed_row
                            .values
                            .iter()
                            .map(|x| self.subst_generic_type(*x, gens.clone()).unwrap())
                            .collect();

                        Row::Closed(ClosedRow {
                            values: values.leak(),
                            ..closed_row
                        })
                    }
                    _ => r,
                };
                accum = accum.modify(Type::Prod(r))
            }

            Type::Sum(r) => {
                let r = match r {
                    Row::Closed(closed_row) => {
                        let values: Vec<_> = closed_row
                            .values
                            .iter()
                            .map(|x| self.subst_generic_type(*x, gens.clone()).unwrap())
                            .collect();

                        Row::Closed(ClosedRow {
                            values: values.leak(),
                            ..closed_row
                        })
                    }
                    _ => r,
                };
                accum = accum.modify(Type::Sum(r))
            }
            _ => accum = t,
        }

        Ok(accum)
    }

    fn analyze_type(&mut self, t: Spanned<Intern<Type>>) -> CompResult<Spanned<Intern<Type>>> {
        // dbg!(self.current_node);
        // dbg!(t);
        match *t.0 {
            Type::Func(l, r) => {
                let l = self.analyze_type(l)?;
                let r = self.analyze_type(r)?;
                let new_t = t.modify(Type::Func(l, r));
                Ok(new_t)
            }
            Type::Label(l, the_r) => {
                let new_t = self.analyze_type(the_r)?;
                Ok(t.modify(Type::Label(l, new_t)))
            }
            Type::User(name, instanced_generics) => {
                let the_item = self.resolve_name_type(&name)?;
                // dbg!(the_item.get_type_universal());
                if let ItemKind::Type(_, generics, new_t) = the_item.kind {
                    let t = if !instanced_generics.is_empty() {
                        // First, analyze each instanced generic type
                        let analyzed_instances: CompResult<Vec<_>> = instanced_generics
                            .iter()
                            .map(|ty| self.analyze_type(*ty))
                            .collect();
                        let analyzed_instances = analyzed_instances?;

                        let generic_instances: im::HashMap<
                            Intern<Type>,
                            Spanned<Intern<Type>>,
                            FxBuildHasher,
                        > = generics
                            .iter()
                            .zip(analyzed_instances.iter())
                            .map(|(l, r)| (l.0, *r))
                            .collect();

                        self.subst_generic_type(new_t, generic_instances)?
                    } else {
                        new_t
                    };
                    self.analyze_type(t) // This recursively analyzes the substituted type
                } else {
                    Err(DynamicErr::new(format!("{} is not a type", name.0))
                        .label("", name.1)
                        .into())
                }
            }
            Type::Prod(r) => {
                let new_r = match r {
                    super::typing::Row::Closed(closed_row) => {
                        let new_values: CompResult<Vec<Spanned<Intern<Type>>>> = closed_row
                            .values
                            .iter()
                            .map(|t| -> CompResult<Spanned<Intern<Type>>> {
                                // let t = self.resolve_name_generic(&n.0)?.get_ty()?.0;
                                self.analyze_type(*t)
                            })
                            .collect();
                        let values = new_values?.leak();
                        let cr = ClosedRow {
                            values,
                            ..closed_row
                        }
                        .sort();
                        crate::passes::midend::typing::Row::Closed(cr)
                    }
                    _ => r,
                };
                // self.rows.insert(, value)
                Ok(t.modify(Type::Prod(new_r)))
            }

            Type::Sum(r) => {
                let new_r = match r {
                    super::typing::Row::Closed(closed_row) => {
                        let new_values: CompResult<Vec<Spanned<Intern<Type>>>> = closed_row
                            .values
                            .iter()
                            .map(|t| -> CompResult<Spanned<Intern<Type>>> {
                                // let t = self.resolve_name_generic(&n.0)?.get_ty()?;
                                self.analyze_type(*t)
                            })
                            .collect();
                        let values = new_values?.leak();
                        let cr = ClosedRow {
                            values,
                            ..closed_row
                        }
                        .sort();
                        crate::passes::midend::typing::Row::Closed(cr)
                    }
                    _ => unreachable!("All rows should be closed"),
                };
                Ok(t.modify(Type::Sum(new_r)))
            }

            _ => Ok(t),
        }
    }

    #[allow(unused_variables)]
    fn analyze_expr(
        &mut self,
        expr: Spanned<Intern<Expr<Untyped>>>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> CompResult<Spanned<Intern<Expr<Untyped>>>> {
        // dbg!(&expr);

        match *expr.0 {
            Expr::Ident(u) => {
                if vars
                    .iter()
                    .rev()
                    .any(|n| u.ident().is_ok_and(|name| n.0 == name.0))
                {
                    Ok(expr)
                } else {
                    // dbg!(expr);
                    self.resolve_name_expr(expr)
                }
            }
            Expr::Concat(l, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;
                // let t = Type::Prod(crate::passes::midend::typing::Row::Closed(());
                Ok(expr.convert(Expr::Concat(l, r)))
            }
            Expr::Project(direction, ex) => {
                let ex = self.analyze_expr(ex, vars)?;
                Ok(expr.convert(Expr::Project(direction, ex)))
            }

            Expr::Inject(direction, ex) => {
                let ex = self.analyze_expr(ex, vars)?;
                Ok(expr.convert(Expr::Inject(direction, ex)))
            }
            Expr::Branch(l, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;

                Ok(expr.convert(Expr::Branch(l, r)))
            }
            Expr::Label(l, v) => {
                let new_vars = [vars, &[(l.0.0, v)]].concat();

                let v = self.analyze_expr(v, &new_vars)?;
                Ok(expr.convert(Expr::Label(l, v)))
            }
            Expr::Unlabel(v, l) => Ok(expr.convert(Expr::Unlabel(v, l))),
            Expr::Pat(spanned) => todo!(),
            Expr::Mul(l, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;

                Ok(expr.modify(Expr::Mul(l, r)))
            }
            Expr::Div(l, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;
                Ok(expr.modify(Expr::Div(l, r)))
            }
            Expr::Add(l, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;
                Ok(expr.modify(Expr::Add(l, r)))
            }
            Expr::Sub(l, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;
                Ok(expr.modify(Expr::Sub(l, r)))
            }
            Expr::Comparison(l, comparison_op, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;
                Ok(expr.modify(Expr::Comparison(l, comparison_op, r)))
            }

            Expr::Call(func, arg) => {
                let func = self.analyze_expr(func, vars)?;

                let arg = self.analyze_expr(arg, vars)?;
                if let Expr::Item(id, Kind::Ty) = *func.0 {
                    todo!()
                };
                Ok(expr.convert(Expr::Call(func, arg)))
            }
            Expr::FieldAccess(l, r) => {
                // dbg!(l, r);
                let l = self.analyze_expr(l, vars)?;
                if let Expr::Item(id, k) = *l.0 {
                    dbg!(id, k);
                    let res = self.resolve_name_expr(r)?;
                    Ok(res)
                } else if let Expr::Ident(n) = *l.0 {
                    if let Some((variable, val)) = vars.iter().find(|x| x.0 == n.0.0) {
                        let projection: Spanned<Intern<Expr<Untyped>>> = {
                            let combo = *val;
                            let id = r.ident()?;
                            {
                                // let ex = path.into_iter().fold(combo, |prev, dir| {
                                let ex = combo.convert(Expr::Project(Direction::Right, combo));
                                // });

                                combo.convert(Expr::Unlabel(ex, Label(id)))
                            }

                            // dbg!(combo);
                        };
                        Ok(expr.convert(projection.0))
                    } else {
                        todo!()
                    }
                } else if let Expr::Unlabel(combo, label) = *l.0 {
                    self.resolve_name_expr(r)
                } else {
                    todo!("{l:?}")
                }
                // Ok(expr.update(Expr::Project(Direction::Left, l))?;
                // if matches!(*l.0, Expr::Con) {}
                // Ok(expr.update(Expr::FieldAccess(l, r)))
            }
            Expr::If(cond, then, otherwise) => {
                let cond = self.analyze_expr(cond, vars)?;
                let then_vars = vars;
                let then = self.analyze_expr(then, then_vars)?;
                let otherwise_vars = vars;
                let otherwise = self.analyze_expr(otherwise, otherwise_vars)?;
                Ok(expr.convert(Expr::If(cond, then, otherwise)))
            }
            Expr::Match(matchee, branches) => {
                let matchee = self.analyze_expr(matchee, vars)?;

                let branches: CompResult<Vec<_>> = branches
                    .iter()
                    .map(|b| self.resolve_branch(*b, vars))
                    .collect();

                let branches = branches?
                    .into_iter()
                    .reduce(|l, r| l.modify(Expr::Branch(l, r)))
                    .unwrap();

                Ok(expr.convert(Expr::Call(branches, matchee)))
            }
            Expr::Lambda(arg, body, is_anon) => {
                let new_vars =
                    &[vars, &[(arg.0.0, arg.ident()?.convert(Expr::Ident(arg)))]].concat();
                let body = self.analyze_expr(body, new_vars)?;
                // *vars.iter_mut().find(|x| x.0 == arg.0 .0).unwrap() = (arg.0 .0, body);
                Ok(expr.convert(Expr::Lambda(arg, body, is_anon)))
            }
            Expr::Let(id, body, and_in) => {
                let body = self.analyze_expr(body, vars)?;

                let new_vars = [vars, &[(id.0.0, body)]].concat();
                let and_in = self.analyze_expr(and_in, &new_vars)?;
                // let lambda = Spanned(Expr::Lambda(id, and_in, LambdaInfo::Anon).into(), expr.1);

                // Ok(expr.convert(Expr::Call(lambda, body)))

                Ok(expr.modify(Expr::Let(id, body, and_in)))
            }

            _ => Ok(expr),
        }
        //.inspect(|x| {

        // dbg!(x);
        // })
    }

    fn resolve_pattern(
        &mut self,
        p: Spanned<Intern<Pattern<Untyped>>>,
        vars: Vec<Untyped>,
    ) -> CompResult<(Spanned<Intern<Expr<Untyped>>>, Vec<Untyped>)> {
        let res = match *p.0 {
            Pattern::Wildcard => (
                p.convert(Expr::Ident(Untyped(p.convert(p.1.to_string())))),
                vars,
            ),
            Pattern::Var(v) => (p.convert(Expr::Ident(v)), [vars, [v].to_vec()].concat()),
            Pattern::Number(_) => todo!(),
            Pattern::String(_) => todo!(),
            Pattern::Particle(p) => (p.convert(Expr::Particle(p)), vars),
            Pattern::Bool(_) => todo!(),
            Pattern::Unit => todo!(),
            Pattern::Ctor(label, ex) => {
                let (ex, vars) = self.resolve_pattern(ex, vars)?;
                (p.convert(Expr::Unlabel(ex, label)), vars)
            }
            _ => todo!(),
        };
        Ok(res)
    }

    fn resolve_branch(
        &mut self,
        b: MatchArm<Untyped>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> CompResult<Spanned<Intern<Expr<Untyped>>>> {
        let (pat, bindings) = self.resolve_pattern(b.pat, vec![])?;
        // dbg!(pat);
        // dbg!(&vars);
        // dbg!(b.body);

        let body = bindings.into_iter().fold(b.body, |prev, v| {
            let unwrapped = prev.modify(Expr::Let(v, pat, prev));
            prev.modify(Expr::Lambda(v, unwrapped, LambdaInfo::Anon))
        });

        self.analyze_expr(body, vars)

        // dbg!(arm);
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
        // dbg!(self.current_parent, q);
        // self.env.debug();
        let search = self.env.get_from_context(q, &self.current_parent);
        search.map_or_else(
            |_| Err(errors::not_defined(q, s)),
            |node| {
                self.dag_add(node.index());
                Ok(ItemId(node.index()))
            },
        )
    }

    fn resolve_name_expr(
        &mut self,
        expr: Spanned<Intern<Expr<Untyped>>>,
    ) -> CompResult<Spanned<Intern<Expr<Untyped>>>> {
        let name = expr.ident()?;
        // self.env.debug();

        if let Ok(e) = self.search_masterenv(&QualifierFragment::Func(name.0), &expr.1) {
            Ok(expr.convert(Expr::Item(e, Kind::Func)))
        } else if let Ok(e) = self.search_masterenv(&QualifierFragment::Type(name.0), &expr.1) {
            Ok(expr.convert(Expr::Item(e, Kind::Ty)))
        } else if let Ok(e) = self.search_masterenv(&QualifierFragment::Package(name.0), &expr.1) {
            Ok(expr.convert(Expr::Item(e, Kind::Package)))
        } else {
            Err(errors::not_defined(
                QualifierFragment::Wildcard(name.0),
                &expr.1,
            ))
        }
    }

    fn resolve_name_type(&mut self, name: &impl Ident) -> CompResult<&Item<Untyped>> {
        let name = name.ident()?;

        if let Ok(e) = self.search_masterenv(&QualifierFragment::Type(name.0), &name.1) {
            self.env.value(NodeIndex::from(e.0 as u32))
        } else {
            Err(errors::not_defined(
                QualifierFragment::Wildcard(name.0),
                &name.1,
            ))
        }
    }

    pub fn finish(self) -> Environment {
        self.env
    }
}
