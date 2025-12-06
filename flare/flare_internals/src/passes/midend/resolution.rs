use std::collections::{BTreeMap, BTreeSet};

use chumsky::span::SimpleSpan;
use internment::Intern;
use petgraph::{
    dot::Config,
    graph::NodeIndex,
    visit::{EdgeRef, IntoNodeReferences, Topo, Walker},
};
use rustc_hash::FxHashSet;

type DiGraph<N, E> = petgraph::graph::DiGraph<N, E>;

use crate::{
    passes::midend::{
        environment::Environment,
        typechecker::Typechecker,
        typing::{self, ClosedRow, Evidence, Row, RowVar, Solver, Type, TypeScheme, TypeVar},
    },
    resource::{
        errors::{self, not_defined, CompResult, CompilerErr, DynamicErr, FatalErr},
        rep::{
            ast::{Direction, Expr, ItemId, Kind, Label, LambdaInfo, Untyped},
            common::Ident,
            entry::{FunctionItem, Item, ItemKind, PackageEntry},
            quantifier::QualifierFragment,
            Spanned,
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
    env: Environment,
    current_parent: QualifierFragment,
    current_dag_node: Option<NodeIndex>,
    pub dag: DiGraph<usize, ()>,
    main_dag_idx: Option<usize>,
    unbound_ty_count: u32,
    // rows: BTreeMap<RowVar, Row>,
    unbound_row_count: u32, // new_rows: BTreeSet<RowVar>,
    guess_evidence: Vec<Evidence>,
}

impl Resolver {
    pub fn new(env: Environment) -> Self {
        Self {
            env,
            current_parent: QualifierFragment::Root,
            current_dag_node: None,
            dag: DiGraph::new(),
            main_dag_idx: None,
            unbound_ty_count: 0,
            // rows: BTreeMap::new(),
            unbound_row_count: 0,
            guess_evidence: vec![],
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
                let t = self
                    .env
                    .value(idx)
                    .map(|x| {
                        if let ItemKind::Package(p) = x.kind {
                            Some((idx, p))
                        } else {
                            None
                        }
                    })
                    .ok()
                    .flatten();
                t
            })
            // .map(|x| if let Some(x) = x { x } else { unreachable!() })
            .collect();

        for (idx, p) in filtered {
            // dbg!(&name);
            self.analyze_package(idx, p)?
        }

        self.dag = self.dag.clone().filter_map_owned(
            |idx, n| {
                let var_name = self.dag.neighbors_undirected(idx).count();
                // dbg!(n); // dbg!(self.env.value(NodeIndex::from(n as u32)), var_name);
                if var_name > 0 || Some(n) == self.main_dag_idx {
                    Some(n)
                } else {
                    None
                }
            },
            |_, e| Some(e),
        );
        // self.debug();

        let flat: Vec<NodeIndex> = Topo::new(&self.dag).iter(&self.dag).collect();
        let flat: Vec<NodeIndex> = flat
            .into_iter()
            .map(|x| NodeIndex::new(*self.dag.node_weight(x).expect("Node should exist")))
            .collect();
        // self.env.debug();
        // self.debug();
        // dbg!(&flat);
        Ok(flat)
    }

    fn analyze_package(&mut self, idx: NodeIndex, p: PackageEntry) -> CompResult<()> {
        self.current_parent = QualifierFragment::Package(p.name.0);
        let children = self
            .env
            .graph
            .neighbors_directed(idx, petgraph::Direction::Outgoing)
            .map(|x| x.index())
            .collect::<Vec<usize>>();

        for child in children {
            let node_idx = NodeIndex::new(child);

            let dag_idx = if let Some((node_idx, _)) =
                self.dag.node_references().find(|(_, x)| **x == child)
            {
                node_idx
            } else {
                self.dag.add_node(child)
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
                    if *f.name.0 .0 == "main" {
                        self.main_dag_idx = Some(child);
                    }
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
                ItemKind::Type(n, t, _) => {
                    // Analyze the type
                    let t = self.in_context(|me| me.analyze_type(t), dag_idx)?;

                    // Write back the result
                    let item = self
                        .env
                        .graph
                        .node_weight_mut(node_idx)
                        .expect("Node should exist");
                    if let ItemKind::Type(_, ref mut ty, _) = item.kind {
                        *ty = t;
                    }
                    Ok(())
                }
                ItemKind::Field { name, value } => {
                    // let t = self.in_context(|me| me.analyze_type(value), dag_idx)?;
                    let t = self.analyze_type(value)?;
                    // dbg!(value, t); // Write back the result
                    let item = self
                        .env
                        .graph
                        .node_weight_mut(node_idx)
                        .expect("Node should exist");
                    if let ItemKind::Field {
                        name,
                        ref mut value,
                    } = item.kind
                    {
                        *value = t;
                    } else {
                        unreachable!()
                    }
                    Ok(())
                }

                ItemKind::Extern { name, sig } => {
                    let old_sig = sig;
                    let t = self.in_context(|me| me.analyze_type(sig.0), dag_idx)?;

                    // Write back the result
                    let item = self
                        .env
                        .graph
                        .node_weight_mut(node_idx)
                        .expect("Node should exist");
                    if let ItemKind::Extern { name, ref mut sig } = item.kind {
                        *sig = old_sig.replace(t);
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
        self.guess_evidence = vec![];
        out
    }

    fn new_rowvar(&mut self) -> u32 {
        let v = self.unbound_row_count;
        self.unbound_row_count += 1;
        v
    }

    fn new_typvar(&mut self) -> Intern<String> {
        let v = self.unbound_ty_count;
        self.unbound_ty_count += 1;
        v.to_string().into()
    }

    fn analyze_func(
        &mut self,
        the_func: &mut FunctionItem<Untyped>,
        idx: NodeIndex,
    ) -> CompResult<()> {
        // let f_old = *the_func;
        self.in_context(
            |me| {
                let sig = me.analyze_type(the_func.sig.0)?;

                let body = me.analyze_expr(the_func.body, &[])?;
                // dbg!(body);
                the_func.unbound_rows = (0..me.unbound_row_count)
                    .map(RowVar)
                    .collect::<BTreeSet<_>>()
                    .into();

                the_func.unbound_types = (0..me.unbound_ty_count)
                    .map(|x| TypeVar(x.to_string().into()))
                    .collect::<BTreeSet<_>>()
                    .into();

                the_func.sig = Spanned(sig, the_func.sig.1);
                the_func.evidence = me.guess_evidence.clone().into();

                the_func.body = body;
                Ok(())
            },
            idx,
        )
    }

    fn analyze_type(&mut self, t: Intern<Type>) -> CompResult<Intern<Type>> {
        // dbg!(self.current_node);
        // dbg!(t);
        match *t {
            Type::Func(l, r) => {
                let l = self.analyze_type(l)?;
                let r = self.analyze_type(r)?;
                let new_t = Type::Func(Intern::from(*l), Intern::from(*r)).into();
                // *t = new_t;
                Ok(new_t)
            }
            Type::Label(l, the_r) => {
                let new_t = self.analyze_type(the_r)?;
                // *t = new_t;
                Ok(Type::Label(l, new_t).into())
            }
            Type::User(name) => {
                let the_item = self.resolve_name_generic(&name)?;
                if let ItemKind::Type(_, new_t, _) = the_item.kind {
                    self.analyze_type(new_t)
                } else {
                    Err(DynamicErr::new(format!("{} is not a type", name.0))
                        .label("", name.1)
                        .into())
                }
            }
            Type::Prod(r) => {
                let new_r = match r {
                    super::typing::Row::Closed(closed_row) => {
                        let new_values: CompResult<Vec<Intern<Type>>> = closed_row
                            .values
                            .iter()
                            .map(|t| -> CompResult<Intern<Type>> {
                                // let t = self.resolve_name_generic(&n.0)?.get_ty()?.0;
                                self.analyze_type(*t)
                            })
                            .collect();

                        // let new_values: CompResult<Vec<Intern<Type>>> = closed_row
                        //     .values
                        //     .iter()
                        //     .map(|t| -> CompResult<Intern<Type>> { self.analyze_type(*t) })
                        //     .collect();
                        let values = new_values?.leak();
                        let cr = ClosedRow {
                            values,
                            ..closed_row
                        };
                        crate::passes::midend::typing::Row::Closed(cr)
                    }
                    _ => unreachable!("All rows should be closed"),
                };
                // self.rows.insert(, value)
                Ok(Type::Prod(new_r).into())
            }

            Type::Sum(r) => {
                let new_r = match r {
                    super::typing::Row::Closed(closed_row) => {
                        let new_values: CompResult<Vec<Intern<Type>>> = closed_row
                            .fields
                            .iter()
                            .map(|n| -> CompResult<Intern<Type>> {
                                let t = self.resolve_name_generic(&n.0)?.get_ty()?.0;
                                self.analyze_type(t)
                            })
                            .collect();
                        let values = new_values?.leak();
                        let cr = ClosedRow {
                            values,
                            ..closed_row
                        };
                        crate::passes::midend::typing::Row::Closed(cr)
                    }
                    _ => unreachable!("All rows should be closed"),
                };
                Ok(Type::Sum(new_r).into())
            }
            _ => Ok(t),
        }
        // .inspect(|x| {
        //     dbg!(t, x);
        // })
    }

    #[allow(unused_variables)]
    fn analyze_expr(
        &mut self,
        expr: Spanned<Intern<Expr<Untyped>>>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> CompResult<Spanned<Intern<Expr<Untyped>>>> {
        // dbg!(&expr);
        let ex = match *expr.0 {
            Expr::Ident(u) => {
                // dbg!(vars);

                if vars
                    .iter()
                    .rev()
                    .any(|n| u.ident().is_ok_and(|name| n.0 == name.0))
                {
                    Ok(expr)
                } else {
                    self.resolve_name_expr(expr)
                }
            }
            Expr::Concat(l, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;
                // let t = Type::Prod(crate::passes::midend::typing::Row::Closed(());
                Ok(expr.update(Expr::Concat(l, r)))
            }
            // Expr::Project(direction, spanned) => todo!(),
            Expr::Inject(direction, ex) => {
                let ex = self.analyze_expr(ex, vars)?;
                Ok(expr.update(Expr::Inject(direction, ex)))
            }
            // Expr::Branch(spanned, spanned1) => todo!(),
            // Expr::Label(l, v) => {
            //     let new_vars = [vars, &[(l.0 .0, v)]].concat();

            //     let v = self.analyze_expr(v, &new_vars)?;
            //     Ok(expr.update(Expr::Label(l, v)))
            // }
            // Expr::Unlabel(spanned, label) => todo!(),
            Expr::ExternFunc(intern) => todo!(),
            Expr::Pat(spanned) => todo!(),
            Expr::Mul(l, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;
                Ok(expr.update(Expr::Mul(l, r)))
            }
            Expr::Div(l, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;
                Ok(expr.update(Expr::Div(l, r)))
            }
            Expr::Add(l, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;
                Ok(expr.update(Expr::Add(l, r)))
            }
            Expr::Sub(l, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;
                Ok(expr.update(Expr::Sub(l, r)))
            }
            Expr::Comparison(spanned, comparison_op, spanned1) => todo!(),
            Expr::Call(func, arg) => {
                let func = self.analyze_expr(func, vars)?;

                let arg = self.analyze_expr(arg, vars)?;
                if let Expr::Item(id, Kind::Ty) = *func.0 {
                    todo!()
                };
                Ok(expr.update(Expr::Call(func, arg)))
            }
            Expr::FieldAccess(l, r) => {
                dbg!(expr, l);
                let l = self.analyze_expr(l, vars)?;
                if let Expr::Item(id, k) = *l.0 {
                    let res = self.resolve_name_expr(r)?;

                    Ok(res)
                } else if let Expr::Ident(n) = *l.0 {
                    if let Some((variable, val)) = vars.iter().find(|x| x.0 == n.0 .0) {
                        let projection = {
                            let combo = *val;
                            let id = r.ident()?;

                            {
                                let mut path = Vec::new();
                                self.row_addr_helper(&combo, id, vars, &mut path)
                                    .map_err(|e| {
                                        if let Some(err) = e.downcast_ref::<DynamicErr>() {
                                            err.clone()
                                                .extra_labels(vec![(
                                                    "in this object".to_string(),
                                                    combo.1,
                                                )])
                                                .into()
                                        } else {
                                            e
                                        }
                                    })?;
                                // dbg!(&path);

                                let ex = path.into_iter().fold(combo, |prev, dir| {
                                    combo.replace(Expr::Project(dir, prev))
                                });

                                ex.replace(Expr::Unlabel(ex, Label(id)))
                            }

                            // dbg!(combo);
                        };
                        Ok(expr.update(projection.0))
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
            Expr::If(spanned, spanned1, spanned2) => todo!(),
            Expr::Match(spanned, intern) => {
                todo!()
            }
            Expr::Lambda(arg, body, is_anon) => {
                let new_vars =
                    &[vars, &[(arg.0 .0, arg.ident()?.update(Expr::Ident(arg)))]].concat();
                let body = self.analyze_expr(body, new_vars)?;
                // *vars.iter_mut().find(|x| x.0 == arg.0 .0).unwrap() = (arg.0 .0, body);
                Ok(expr.update(Expr::Lambda(arg, body, is_anon)))
            }
            Expr::Let(id, body, and_in) => {
                let body = self.analyze_expr(body, vars)?;

                let new_vars = [vars, &[(id.0 .0, body)]].concat();
                let and_in = self.analyze_expr(and_in, &new_vars)?;
                let lambda = Spanned(Expr::Lambda(id, and_in, LambdaInfo::Anon).into(), expr.1);

                Ok(expr.update(Expr::Call(lambda, body)))

                // Ok(expr.replace(Expr::Let(id, body, and_in)))
            }

            _ => Ok(expr),
        };
        ex
        //.inspect(|x| {

        // dbg!(x);
        // })
    }

    fn row_addr_helper(
        &mut self,
        combo: &Spanned<Intern<Expr<Untyped>>>,
        id: Spanned<Intern<String>>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
        accum: &mut Vec<Direction>,
    ) -> CompResult<()> {
        match *combo.0 {
            Expr::Concat(l, r) => {
                if matches!(*r.0, Expr::Label(a, _) if a.0.0 == id.0) {
                    accum.push(Direction::Right);
                    Ok(())
                } else {
                    accum.push(Direction::Left);
                    let l = self.analyze_expr(l, vars)?;
                    self.row_addr_helper(&l, id, vars, accum)
                }
            }

            Expr::Branch(l, r) => todo!(),
            Expr::Label(a, r) => {
                if a.0 .0 == id.0 {
                    Ok(())
                } else {
                    let r = self.analyze_expr(r, vars)?;
                    self.row_addr_helper(&r, id, vars, accum)
                }
            }
            Expr::Call(l, r) => {
                if matches!(*l.0, Expr::Item(_, Kind::Ty)) {
                    todo!()
                } else {
                    self.row_addr_helper(&r, id, vars, accum)
                }
            }
            Expr::Ident(n) => {
                let ty = Type::Var(TypeVar(self.new_typvar())).into();
                let goal = Row::Open(RowVar(self.new_rowvar()));

                let right = Row::Open(RowVar(self.new_rowvar()));
                let left = Row::Open(RowVar(self.new_rowvar()));
                let remainder = Row::Open(RowVar(self.new_rowvar()));
                // evidence we can produce a row containing the field and other types
                let ev1 = Evidence::RowEquation {
                    left: Row::single(Label(id.ident()?), ty),
                    right: remainder,
                    goal,
                };

                let ev2 = Evidence::RowEquation { left, right, goal };
                self.guess_evidence.push(ev1);
                self.guess_evidence.push(ev2);
                accum.push(Direction::Left);
                Ok(())
            }

            _ => Err(
                DynamicErr::new(format!("The field {} is not available here", id.0))
                    .label("this", id.1)
                    .into(),
            ),
        }
    }

    // fn find_row_address(
    //     &mut self,
    //     combo: VariableKind,
    //     id: Spanned<Intern<Expr<Untyped>>>,
    //     vars: &[(Intern<String>, VariableKind)],
    // ) -> CompResult<Spanned<Intern<Expr<Untyped>>>> {
    //     let id = id.ident()?;
    //     match combo {
    //         VariableKind::Local(combo) => {
    //             let mut path = Vec::new();
    //             self.row_addr_helper(&combo, id, vars, &mut path)
    //                 .map_err(|e| {
    //                     if let Some(err) = e.downcast_ref::<DynamicErr>() {
    //                         err.clone()
    //                             .extra_labels(vec![("in this object".to_string(), combo.1)])
    //                             .into()
    //                     } else {
    //                         e
    //                     }
    //                 })?;
    //             // dbg!(&path);

    //             let ex = path
    //                 .into_iter()
    //                 .fold(combo, |prev, dir| combo.replace(Expr::Project(dir, prev)));

    //             Ok(ex.replace(Expr::Unlabel(ex, Label(id))))
    //         }

    //         VariableKind::Param(combo, t) => {
    //             let rv: Result<Intern<Type>, CompilerErr> = match *t {
    //                 Type::Prod(row) | Type::Sum(row) => match row {
    //                     Row::Closed(closed_row) => {
    //                         dbg!(combo, id, t, row);
    //                         let idx = closed_row.fields.iter().position(|x| x.0 == id).ok_or(
    //                             DynamicErr::new(format!(
    //                                 "The field {} is not available here",
    //                                 id.0
    //                             ))
    //                             .label("this", id.1)
    //                             .extra_labels(vec![("in this object".to_string(), combo.1)]),
    //                         )?;
    //                         Ok(closed_row.values[idx])
    //                     }
    //                     _ => panic!(),
    //                 },
    //                 _ => Err(DynamicErr::new(format!("{t:?} is not a row type")).into()),
    //             };

    //             let left = typing::Row::Closed(ClosedRow {
    //                 fields: vec![Label(combo.ident()?)].leak(),
    //                 values: vec![rv?].leak(),
    //             });
    //             let right = typing::Row::Open(RowVar(self.new_rowvar()));

    //             let goal = typing::Row::Open(RowVar(self.new_rowvar()));
    //             self.guess_evidence
    //                 .push(Evidence::RowEquation { left, right, goal });

    //             Ok(combo.update(Expr::FieldAccess(combo, id)))
    //         }
    //     }

    //     // dbg!(combo);
    // }

    #[allow(dead_code, clippy::unwrap_used, clippy::dbg_macro)]
    #[deprecated]
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

        if let Some(current) = self.current_dag_node {
            if n != current && !self.dag.contains_edge(current, n) {
                self.dag.update_edge(n, current, ());
            }
        }
    }

    fn search_masterenv(
        &mut self,
        q: &QualifierFragment,
        s: &SimpleSpan<usize, u64>,
    ) -> CompResult<ItemId> {
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

        if let Ok(e) = self.search_masterenv(&QualifierFragment::Func(name.0), &expr.1) {
            Ok(expr.update(Expr::Item(e, Kind::Func)))
        } else if let Ok(e) = self.search_masterenv(&QualifierFragment::Type(name.0), &expr.1) {
            Ok(expr.update(Expr::Item(e, Kind::Ty)))
        } else {
            Err(errors::not_defined(
                QualifierFragment::Wildcard(name.0),
                &expr.1,
            ))
        }
    }

    fn resolve_name_generic(&mut self, name: &impl Ident) -> CompResult<&Item<Untyped>> {
        let name = name.ident()?;

        if let Ok(e) = self.search_masterenv(&QualifierFragment::Func(name.0), &name.1) {
            self.env.value(NodeIndex::from(e.0 as u32))
        } else if let Ok(e) = self.search_masterenv(&QualifierFragment::Type(name.0), &name.1) {
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
