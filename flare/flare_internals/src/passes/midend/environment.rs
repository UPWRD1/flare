use chumsky::span::{SimpleSpan, Span};
use core::panic;
use log::info;
use petgraph::graph::EdgeReference;
use petgraph::Graph;
use petgraph::{
    dot::Config,
    graph::{DiGraph, NodeIndex},
    visit::EdgeRef,
};
use std::cell::OnceCell;
use std::collections::HashMap;
use std::hash::RandomState;

use crate::passes::midend::typechecking::Solver;

use crate::resource::{
    errors::CompResult,
    rep::{
        ast::{Definition, EnumDef, Expr, Program, StructDef},
        entry::{EnumEntry, Item, PackageEntry, StructEntry},
        quantifier::QualifierFragment,
        Spanned,
    },
};

#[derive(Debug)]
pub struct Environment {
    //pub items: Trie<SimpleQuant, Index>,
    //pub arena: Arena<Node>,
    pub graph: DiGraph<Item, QualifierFragment>,
    //    pub cache: RefCell<HashMap<(SimpleQuant, SimpleQuant), NodeIndex>>,
    pub root: NodeIndex,
}

impl Environment {
    fn add(&mut self, parent_node: NodeIndex, k: QualifierFragment, v: Item) -> NodeIndex {
        let ix = self.graph.add_node(v);
        self.graph.add_edge(parent_node, ix, k);
        ix
    }
    #[must_use]
    pub fn value(&self, idx: NodeIndex) -> Option<&Item> {
        self.graph.node_weight(idx)
    }

    pub fn build(p: &Program) -> CompResult<Environment> {
        let mut g = Graph::new();
        let mut current_node = g.add_node(Item::Root);

        let mut me = Self {
            graph: g,
            root: current_node,
        };
        let mut pack_imports: HashMap<QualifierFragment, Vec<Spanned<Expr>>> = HashMap::new();

        // Start building each package's contents
        for package in &p.packages {
            let package_name = package.0.name.0.get_ident(package.0.name.1)?;

            let mut deps = Vec::new();
            // let mut subpackages = HashMap::new();
            // let mut structs = HashMap::new();
            // let mut enums = HashMap::new();
            // let mut funcs = HashMap::new();
            let package_entry = Item::Package(PackageEntry {
                name: package.0.name,
                file: package.1,
                src: package.2,
            });
            let package_quant = QualifierFragment::Package(package_name);

            let p_id = me.add(current_node, package_quant.clone(), package_entry);

            let old_current = current_node;
            current_node = p_id;

            for item in &package.0.items {
                match item {
                    Definition::Import(import_item) => {
                        deps.push(*import_item);
                    }
                    Definition::Struct(StructDef { the_ty, fields }) => {
                        let ident = QualifierFragment::Type(the_ty.0.get_user_name().unwrap());

                        let struct_entry = Item::Struct(StructEntry { ty: *the_ty });

                        let struct_node = me.add(current_node, ident, struct_entry);
                        for f in fields {
                            let field_name =
                                QualifierFragment::Field(f.0 .0.get_ident(f.0 .1).unwrap());
                            let field_entry = Item::Field(*f);
                            me.add(struct_node, field_name, field_entry);
                        }
                    }
                    Definition::Enum(EnumDef { the_ty, variants }) => {
                        let ident = QualifierFragment::Type(the_ty.0.get_user_name().unwrap());
                        //let the_ty = (Ty::User(name.clone(), vec![]), name.1);
                        let enum_entry = Item::Enum(EnumEntry { ty: *the_ty });
                        let enum_node = me.add(current_node, ident, enum_entry);
                        for v in variants {
                            let variant_name =
                                QualifierFragment::Variant(v.0.name.0.get_ident(v.1).unwrap());
                            let variant_entry = Item::Variant(*v);
                            me.add(enum_node, variant_name, variant_entry);
                        }
                    }
                    Definition::Let(name, body, ty) => {
                        let ident = QualifierFragment::Func(name.0.get_ident(name.1)?);
                        let cell = OnceCell::new();
                        if let Some(ty) = ty {
                            let _ = cell.set(*ty);
                        }
                        let leak_cell: &'static OnceCell<_> = Box::leak(Box::new(cell));
                        let entry = Item::Let {
                            name: *name,
                            sig: leak_cell,
                            body: *body,
                        };
                        me.add(current_node, ident, entry);
                    }
                    Definition::Extern(n, ty) => {
                        let ident = QualifierFragment::Func(n.0.get_ident(n.1)?);
                        let entry = Item::Extern {
                            //parent: current_parent.clone(),
                            name: *n,
                            sig: *ty,
                        };
                        me.add(current_node, ident, entry);
                    }
                }
            }
            current_node = old_current;
            pack_imports.insert(package_quant, deps);
        }
        //dbg!(&pack_imports);
        for (name, deps) in pack_imports.iter() {
            let package = me.get(&[name.clone()][..]).unwrap();

            for dep in deps {
                let path = QualifierFragment::from_expr(dep);
                let imports: Vec<NodeIndex> = if let Some(n) = me.get(&path?) {
                    if matches!(me.value(n).unwrap(), Item::Package(_)) {
                        me.graph
                            .edges_directed(n, petgraph::Direction::Outgoing)
                            .map(|e| e.target())
                            .collect()
                    } else {
                        vec![n]
                    }
                } else {
                    panic!()
                };
                for import in imports {
                    let the_name = me
                        .graph
                        .edges_directed(import, petgraph::Direction::Incoming)
                        .map(|x| x.weight())
                        .next()
                        .cloned()
                        .unwrap();
                    me.graph.add_edge(package, import, the_name);
                }
            }
        }

        Ok(me)
    }

    #[must_use]
    pub fn get_from_context(
        &self,
        q: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> Option<NodeIndex> {
        let paths = self.search_for_edge(q)?;
        //dbg!(&paths);
        for path in &paths {
            if path.first()?.is(packctx) {
                return self.get(path);
            }
        }
        None
    }
    #[must_use]
    fn raw_get_node_and_children(
        &self,
        q: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> Option<(NodeIndex, Vec<EdgeReference<'_, QualifierFragment>>)> {
        let parent = self.get_from_context(q, packctx)?;
        let children: Vec<_> = self
            .graph
            .edges_directed(parent, petgraph::Direction::Outgoing)
            .collect();
        Some((parent, children))
    }

    #[must_use]
    pub fn get_node_and_children(
        &self,
        q: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> Option<(&Item, Vec<(&QualifierFragment, &Item)>)> {
        let (node, children) = self.raw_get_node_and_children(q, packctx)?;
        let node_w = self.value(node)?;

        Some((
            node_w,
            children
                .iter()
                .map(|c| {
                    (
                        c.weight(),
                        // # SAFETY: `self.value()` returns none only if the node
                        // doesn't exist. We know the node exists because we
                        // are iterating over the children of the parent.
                        unsafe { self.value(c.target()).unwrap_unchecked() },
                    )
                })
                .collect(),
        ))
    }

    #[must_use]
    // #[allow(dead_code)]
    fn raw_get_children(
        &self,
        q: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> Option<Vec<EdgeReference<'_, QualifierFragment>>> {
        let parent = self.get_from_context(q, packctx)?;
        let children: Vec<_> = self
            .graph
            .edges_directed(parent, petgraph::Direction::Outgoing)
            .collect();
        Some(children)
    }

    #[must_use]
    /// Get the children of a node given the the node's quantifier and context to search in.
    pub fn get_children(
        &self,
        q: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> Option<Vec<(&QualifierFragment, &Item)>> {
        let children = self.raw_get_children(q, packctx)?;

        Some(
            children
                .iter()
                .map(|c| (c.weight(), self.value(c.target()).unwrap()))
                .collect(),
        )
    }

    pub fn get_node(&self, q: &QualifierFragment, packctx: &QualifierFragment) -> Option<&Item> {
        let node = self.get_from_context(q, packctx)?;
        let node_w = self.value(node)?;

        Some(node_w)
    }

    fn get_all_targets_edge<'envi, 'k>(
        &'envi self,
        k: &'k QualifierFragment,
    ) -> impl Iterator<Item = NodeIndex> + use<'envi, 'k> {
        let edges = self.graph.edge_references().filter(|x| x.weight().is(k));
        edges.map(|x| x.target())
    }

    /// Optionally returns a vector of the possible paths to an item fragment.
    fn search_for_edge(&self, k: &QualifierFragment) -> Option<Vec<Vec<QualifierFragment>>> {
        use petgraph::algo::*;
        use petgraph::prelude::*;
        let mut paths: Vec<Vec<QualifierFragment>> = vec![];
        let targets = self.get_all_targets_edge(k);
        for target in targets {
            let real_paths: Vec<Vec<NodeIndex>> =
                all_simple_paths::<Vec<_>, _, RandomState>(&self.graph, self.root, target, 0, None)
                    .collect::<Vec<_>>();
            for p in real_paths {
                let mut path = Vec::new();
                for node_pair in p.windows(2) {
                    let a = node_pair[0];
                    let b = node_pair[1];
                    let edge = self
                        .graph
                        .edges_connecting(a, b)
                        .next()
                        .unwrap()
                        .weight()
                        .clone();
                    path.push(edge);
                }
                paths.push(path);
            }
        }
        Some(paths)
    }

    /// Gets an absolute path and verifies it
    fn get<'graph>(&'graph self, q: &[QualifierFragment]) -> Option<NodeIndex> {
        //let _ = self.graph.edges(self.root).map(|x| dbg!(x));
        struct Rec<'s, 'graph, T> {
            f: &'s dyn Fn(
                &Rec<'s, 'graph, T>,
                &'graph T,
                NodeIndex,
                &[QualifierFragment],
            ) -> Option<NodeIndex>,
        }
        let rec = Rec {
            f: &|rec: &Rec<'_, 'graph, _>,
                 graph_self: &'graph Self,
                 n: NodeIndex,
                 q: &[QualifierFragment]|
             -> Option<NodeIndex> {
                //dbg!(&q);
                match q {
                    [] => Some(n),

                    [q, qs @ ..] => {
                        let candidate = graph_self.graph.edges(n).find(|e| e.weight().is(q))?;
                        (rec.f)(rec, graph_self, candidate.target(), qs)
                        //self.graph.node_weight(n).cloned()
                    }
                }
            },
        };
        (rec.f)(&rec, self, self.root, q) //.inspect(|x| {dbg!(&self.graph.node_weight(*x));})
    }

    pub fn check(&self) -> CompResult<()>
// where
        // 'env: 'src,
    {
        let main_idx = self
            .get_from_context(
                &QualifierFragment::Func("main"),
                &QualifierFragment::Package("Main"),
            )
            //.get(&quantifier!(Root, Package("Main"), Func("main"), End).into_simple())
            .unwrap();
        let main_item = self.value(main_idx).unwrap();
        if cfg!(debug_assertions) {
            let dot = petgraph::dot::Dot::with_config(
                &self.graph,
                &[
                    Config::NodeNoLabel,
                    Config::RankDir(petgraph::dot::RankDir::LR),
                ],
            );
            info!("{:?}", dot);
        }

        self.check_item(main_item, &QualifierFragment::Package("Main"))?;
        Ok(())
        //dbg!(&main);
    }

    pub fn check_item<'env>(
        &self,
        item: &'env Item,
        packctx: &QualifierFragment,
    ) -> CompResult<&'env Item>
// where
        // 'src: 'env, // 'env: 'src, // 'env: 'src,
    {
        //println!("Checking {:?}", entry);
        //match *entry.borrow_mut() {
        if let Item::Let {
            ref name,
            ref mut sig,
            ref body,
            ..
        } = if item.get_sig().is_none() {
            *item
        } else {
            // Early Return
            return Ok(item);
        } {
            //let the_sig: &OnceCell<Spanned<crate::resource::rep::types::Ty<'src>>> = sig;
            let mut tc = Solver::new(self, packctx); //WTF?
            let tv = tc.check_expr(body)?;
            let fn_sig/*: Spanned<crate::resource::rep::types::Ty<'src>>*/ = tc.solve(tv)?;

            //dbg!(&fn_sig);
            // sig = Some((
            //     fn_sig.0,
            //     SimpleSpan::new(name.1.context, name.1.into_range()),
            // ));
            let val = (
                fn_sig.0,
                SimpleSpan::new(name.1.context, name.1.into_range()),
            );
            let _ = sig.set(val);
            // let _ = sig.replace(val);
        }
        info!("Checked {}: {}", item.name(), item.get_ty().unwrap().0);
        Ok(item)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        passes::midend::environment::Environment,
        // quantifier,
        resource::rep::{entry::Item, quantifier::QualifierFragment},
    };

    use petgraph::prelude::*;

    fn make_graph() -> Environment {
        let mut graph: DiGraph<Item, QualifierFragment> = DiGraph::new();
        let root = graph.add_node(Item::Root);
        let lib_foo = graph.add_node(Item::Dummy("libFoo"));
        let foo = graph.add_node(Item::Dummy("foo"));
        let lib_bar = graph.add_node(Item::Dummy("libBar"));
        let bar = graph.add_node(Item::Dummy("Bar"));
        let baz = graph.add_node(Item::Dummy("baz"));
        let bar_foo = graph.add_node(Item::Dummy("fooooo"));

        graph.extend_with_edges(&[
            (root, lib_foo, QualifierFragment::Package("Foo")),
            (lib_foo, foo, QualifierFragment::Func("foo")),
            (root, lib_bar, QualifierFragment::Package("Bar")),
            (lib_bar, bar, QualifierFragment::Type("Bar")),
            (bar, baz, QualifierFragment::Field("f1")),
            (lib_bar, bar_foo, QualifierFragment::Func("foo")),
        ]);

        Environment { graph, root }
    }

    // #[test]
    // fn exists() {
    //     let e = make_graph();
    //     assert!(e
    //         .get(&quantifier!(Root, Package("Foo"), Func("foo"), End).into_simple())
    //         .is_some())
    // }

    #[test]
    fn get_node_exists() {
        let e = make_graph();
        let search1 = e.get_node(
            &QualifierFragment::Func("foo"),
            &QualifierFragment::Package("Foo"),
        );
        let search2 = e.get_node(
            &QualifierFragment::Type("Bar"),
            &QualifierFragment::Package("Bar"),
        );

        assert!(search1.is_some());
        assert!(search2.is_some());
    }

    #[test]
    fn get_node_dne() {
        let e = make_graph();
        let found = e.get_node(
            &QualifierFragment::Func("Bar"),
            &QualifierFragment::Package("libFoo"),
        );
        assert!(found.is_none())
    }

    #[test]
    fn get_paths() {
        let e = make_graph();
        let res = e.search_for_edge(&QualifierFragment::Func("foo"));
        assert_eq!(
            res,
            Some(vec![
                vec![
                    QualifierFragment::Package("Foo"),
                    QualifierFragment::Func("foo")
                ],
                vec![
                    QualifierFragment::Package("Bar"),
                    QualifierFragment::Func("foo")
                ]
            ])
        );
    }
}
