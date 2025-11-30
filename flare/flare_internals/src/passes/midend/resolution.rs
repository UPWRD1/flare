use std::cmp::Ordering;

use chumsky::span::SimpleSpan;
use internment::Intern;
use log::info;
use petgraph::{
    dot::{self, Config, Dot},
    graph::{EdgeReference, NodeIndex},
    visit::{EdgeRef, IntoNodeReferences},
};
use rustc_hash::{FxHashMap, FxHashSet};

type DiGraph<N, E> = petgraph::csr::Csr<N, E>;

use crate::{
    passes::midend::{environment::Environment, typing::Type},
    resource::{
        errors::{self, CompResult, DynamicErr},
        rep::{
            ast::{Expr, ItemId, Kind, NodeId, Untyped},
            common::Ident,
            entry::{FunctionItem, ItemKind, PackageEntry},
            quantifier::QualifierFragment,
            Spanned,
        },
    },
};

pub struct Mentioned {
    mentions: FxHashSet<Expr<Untyped>>,
}

pub struct NameOut {
    items: FxHashMap<NodeId, Mentioned>,
}

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
    current_node: u32,
    // pub dag: DiGraph<NodeIndex, ()>,
}

impl Resolver {
    pub fn new(env: Environment) -> Self {
        let mut s = Self {
            env,
            current_parent: QualifierFragment::Root,
            current_node: 0_u32,
            // dag: DiGraph::new(),
        };
        s
    }

    pub fn build(&mut self) -> CompResult<()> {
        self.analyze()?;
        // let out = self.modify()
        todo!()
    }

    fn analyze(&mut self) -> CompResult<()> {
        if cfg!(debug_assertions) {
            let render =
                |_, k: EdgeReference<QualifierFragment>| format!("label = \"{}\"", k.weight());
            let dot = petgraph::dot::Dot::with_attr_getters(
                &self.env.graph,
                &[
                    Config::EdgeNoLabel,
                    Config::NodeNoLabel,
                    Config::RankDir(petgraph::dot::RankDir::LR),
                ],
                &render,
                &|_, _| String::new(),
            );
            info!("{dot:?}");
        }
        let mut dag: DiGraph<NodeIndex, ()> = DiGraph::new();
        let mut filtered: Vec<(NodeIndex, PackageEntry)> = self
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
        filtered.sort_by(|x, y| {
            if *x.1.name.0 == "Main" {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        });
        for (idx, p) in filtered {
            // dbg!(&name);
            self.analyze_package(idx, p, &mut dag)?
        }
        todo!()
    }

    fn analyze_package(
        &mut self,
        idx: NodeIndex,
        p: PackageEntry,
        dag: &mut DiGraph<NodeIndex, ()>,
    ) -> CompResult<()> {
        self.current_parent = QualifierFragment::Package(p.name.0);
        let children = self
            .env
            .graph
            .edges_directed(idx, petgraph::Direction::Outgoing);
        for child in children {
            // dbg!(child.weight());
            let target_idx = child.target();
            self.current_node = dag.add_node(target_idx);
            let item = self.env.value(target_idx)?;
            match item.kind {
                ItemKind::Root => todo!(),
                ItemKind::Filename(intern) => todo!(),
                ItemKind::Package(package_entry) => Ok(()),
                //self.analyze_package(idx, package_entry, dag),
                ItemKind::Function(f) => self.analyze_func(&f, dag),
                ItemKind::Type(t, s) => {
                    self.analyze_type(t.into(), dag)?;
                    Ok(())
                }
                ItemKind::Extern { name, sig } => todo!(),
                ItemKind::Dummy(_) => todo!(),
                _ => todo!(),
            }?
        }
        self.debug(dag);
        Ok(())
    }

    fn analyze_func(
        &self,
        f: &FunctionItem<Untyped>,
        dag: &mut DiGraph<NodeIndex, ()>,
    ) -> CompResult<()> {
        // dbg!(&f);
        let sig = self.analyze_type(f.sig.0, dag)?;
        let body = self.analyze_expr(f.body, &[], dag)?;
        self.debug(dag);
        Ok(())
    }

    fn analyze_type(
        &self,
        t: Intern<Type>,
        dag: &mut DiGraph<NodeIndex, ()>,
    ) -> CompResult<Intern<Type>> {
        match *t {
            Type::Func(l, r) => {
                let l = self.analyze_type(l, dag)?;
                let r = self.analyze_type(r, dag)?;
                let t = Type::Func(Intern::from(*l), Intern::from(*r)).into();
                Ok(t)
            }
            Type::Label(l, t) => {
                let t = self.analyze_type(t, dag)?;
                Ok(Type::Label(l, t).into())
            }
            Type::User(name) => {
                let frag = &QualifierFragment::Type(name.0);
                let the_item_idx = self.env.get_from_context(frag, &self.current_parent)?;
                let the_item = self.env.value(the_item_idx)?;
                if let ItemKind::Type(t, _) = the_item.kind {
                    self.dag_add(dag, the_item_idx);
                    Ok(t.into())
                } else {
                    Err(DynamicErr::new(format!("{} is not a type", name.0))
                        .label("", name.1)
                        .into())
                }
            }
            _ => Ok(t),
        }
    }

    fn analyze_expr(
        &self,
        expr: Spanned<Intern<Expr<Untyped>>>,
        vars: &[Intern<String>],
        dag: &mut DiGraph<NodeIndex, ()>,
    ) -> CompResult<Spanned<Intern<Expr<Untyped>>>> {
        // dbg!(&expr.0);
        match *expr.0 {
            Expr::Ident(name) => {
                //dbg!(&self.env);
                if vars
                    .iter()
                    .rev()
                    .any(|n| name.ident().is_ok_and(|name| *n == name.0))
                {
                    Ok(expr)
                } else {
                    self.resolve_name(&expr, dag)
                }
            }
            Expr::Concat(l, r) => {
                let l = self.analyze_expr(l, vars, dag)?;
                let r = self.analyze_expr(r, vars, dag)?;
                Ok(expr.replace(Expr::Concat(l, r)))
            }
            Expr::Project(direction, spanned) => todo!(),
            Expr::Inject(direction, ex) => {
                let ex = self.analyze_expr(ex, vars, dag)?;
                Ok(expr.replace(Expr::Inject(direction, ex)))
            }
            Expr::Branch(spanned, spanned1) => todo!(),
            Expr::Unlabel(spanned, label) => todo!(),
            Expr::ExternFunc(intern) => todo!(),
            Expr::Pat(spanned) => todo!(),
            Expr::Mul(l, r) => {
                let l = self.analyze_expr(l, vars, dag)?;
                let r = self.analyze_expr(r, vars, dag)?;
                Ok(expr.replace(Expr::Mul(l, r)))
            }
            Expr::Div(spanned, spanned1) => todo!(),
            Expr::Add(spanned, spanned1) => todo!(),
            Expr::Sub(spanned, spanned1) => todo!(),
            Expr::Comparison(spanned, comparison_op, spanned1) => todo!(),
            Expr::Call(func, arg) => {
                let func = self.analyze_expr(func, vars, dag)?;
                let arg = self.analyze_expr(arg, vars, dag)?;
                Ok(expr.replace(Expr::Call(func, arg)))
            }
            Expr::FieldAccess(l, r) => {
                let l = self.analyze_expr(l, vars, dag)?;
                let r = self.analyze_expr(r, vars, dag)?;
                Ok(expr.replace(Expr::FieldAccess(l, r)))
            }
            Expr::If(spanned, spanned1, spanned2) => todo!(),
            Expr::Match(spanned, intern) => todo!(),
            Expr::Lambda(arg, body, is_anon) => {
                let new_vars = [vars, &[arg.0 .0]].concat();
                let body = self.analyze_expr(body, &new_vars, dag)?;
                Ok(expr.replace(Expr::Lambda(arg, body, is_anon)))
            }
            Expr::Let(id, body, and_in) => {
                let body = self.analyze_expr(body, vars, dag)?;

                let new_vars = [vars, &[id.0 .0]].concat();
                let and_in = self.analyze_expr(and_in, &new_vars, dag)?;
                Ok(expr.replace(Expr::Let(id, body, and_in)))
            }

            Expr::Tuple(intern) => todo!(),
            _ => Ok(expr),
        }
        // .inspect(|x| {
        //     dbg!(x);
        // })
    }

    fn debug(&self, dag: &mut DiGraph<NodeIndex, ()>) {
        let render = |_, (_, v): (_, &NodeIndex)| {
            format!(
                "label = \"{}\"",
                self.env.value(*v).unwrap().ident().unwrap().0
            )
        };
        let dot = petgraph::dot::Dot::with_attr_getters(
            &*dag,
            &[
                Config::EdgeNoLabel,
                Config::NodeNoLabel,
                Config::RankDir(petgraph::dot::RankDir::LR),
            ],
            &|_, _| String::new(),
            &render,
        );
        info!("{dot:?}");
    }

    fn dag_add(&self, dag: &mut DiGraph<NodeIndex, ()>, node: NodeIndex) {
        if dag.node_references().any(|x| *x.1 == node) {
            return;
        }
        let n = dag.add_node(node);
        dag.add_edge(n, self.current_node, ());
    }

    fn search_masterenv(
        &self,
        q: &QualifierFragment,
        s: &SimpleSpan<usize, u64>,
        dag: &mut DiGraph<NodeIndex, ()>,
    ) -> CompResult<ItemId> {
        // self.debug(dag);
        let search = self.env.get_from_context(q, &self.current_parent);
        search.map_or_else(
            |_| Err(errors::not_defined(q, s)),
            |node| {
                self.dag_add(dag, node);
                Ok(ItemId(node.index()))
            },
        )
    }

    fn resolve_name(
        &self,
        expr: &Spanned<Intern<Expr<Untyped>>>,
        dag: &mut DiGraph<NodeIndex, ()>,
    ) -> CompResult<Spanned<Intern<Expr<Untyped>>>> {
        let name = expr.ident()?;

        if let Ok(e) = self.search_masterenv(&QualifierFragment::Func(name.0), &expr.1, dag) {
            Ok(expr.replace(Expr::Item(e, Kind::Func)))
        } else if let Ok(e) = self.search_masterenv(&QualifierFragment::Type(name.0), &expr.1, dag)
        {
            Ok(expr.replace(Expr::Item(e, Kind::Ty)))
        } else {
            Err(errors::not_defined(
                QualifierFragment::Wildcard(name.0),
                &expr.1,
            ))
        }
    }

    pub fn finish(self) -> Environment {
        self.env
    }
}
