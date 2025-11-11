use internment::Intern;
// use petgraph::Graph;
use petgraph::{
    graph::{DiGraph, Edge, EdgeReference, Edges, NodeIndex},
    visit::EdgeRef,
};
use rustc_hash::FxHashMap;
// use serde::{Deserialize, Serialize};
// use std::cell::OnceCell;
// use std::collections::HashMap;
use std::{cell::Cell, hash::RandomState};

use crate::resource::rep::entry::FunctionItem;
use crate::resource::rep::types::EnumVariant;
use crate::resource::rep::{ast::ImplDef, common::Ident};
use crate::resource::{
    errors::CompResult,
    rep::{
        ast::{Definition, EnumDef, Expr, Program, StructDef},
        common::Named,
        entry::{EnumEntry, Item, PackageEntry, StructEntry},
        quantifier::QualifierFragment,
        Spanned,
    },
};
use crate::resource::{errors::DynamicErr, rep::types::Ty};

#[derive(Debug)]
/// The main environment graph structure. Holds all the objects produced by
/// the  parser, and the index of the root.
///
/// Obviously, `Environment` does not implement `Copy`, but it also does not
/// implement `Clone`, since it is ridiculously expensive, and there is no
/// real reason to clone the environment.
pub struct Environment {
    pub graph: DiGraph<Item, QualifierFragment>,
    pub root: NodeIndex,
}

impl Environment {
    /// Add an `item` to the environment as a child of a `parent_node`, accessible via a `qualifier`. Returns the `NodeIndex` of the newly-created child.
    /// # Panics
    /// Panics if the internal graph is full.
    /// Panics if the root node doesn't exist.
    ///
    /// # Examples
    /// ```rust
    /// let env: Environment::new();
    /// let foo = env.add(env.root, QualifierFragment::Dummy("libFoo"), Item::Dummy("Foo"));
    /// assert!(env.graph.contains_edge(env.root, foo))
    /// ```
    fn add(
        &mut self,
        parent_node: NodeIndex,
        qualifier: QualifierFragment,
        item: Item,
    ) -> NodeIndex {
        let child_idx = self.graph.add_node(item);
        self.graph.add_edge(parent_node, child_idx, qualifier);
        child_idx
    }

    #[must_use]
    /// Get the item value of an index.
    /// # Examples
    /// ```rust   
    /// let foo = env.add(env.root, QualifierFragment::Dummy("libFoo"), Item::Dummy("Foo"));
    /// assert_eq!(Some(Item::Dummy("Foo")), env.value(foo))
    /// ```
    pub fn value(&self, idx: NodeIndex) -> Option<&Item> {
        self.graph.node_weight(idx)
    }

    #[must_use]
    /// Get the item value of an index.
    /// # Examples
    /// ```rust   
    /// let foo = env.add(env.root, QualifierFragment::Dummy("libFoo"), Item::Dummy("Foo"));
    /// assert_eq!(Some(Item::Dummy("Foo")), env.value(foo))
    /// ```
    pub fn mut_value(&mut self, idx: NodeIndex) -> Option<&mut Item> {
        self.graph.node_weight_mut(idx)
    }

    /// Build the environment from a given `Program`
    /// # Errors
    /// - on invalid names,
    ///
    pub fn build(p: &Program) -> CompResult<Self> {
        let mut g = DiGraph::new();
        let mut current_node = g.add_node(Item::Root);

        let mut me = Self {
            graph: g,
            root: current_node,
        };
        let mut pack_imports: FxHashMap<QualifierFragment, Vec<Spanned<Intern<Expr>>>> =
            FxHashMap::default();

        // Start building each package's contents
        for package in &p.packages {
            let package_name = package.0.name.name()?;

            let mut deps = Vec::new();
            let package_entry = Item::Package(PackageEntry {
                name: package.0.name,
                id: package.1, // file: fsource.filename,
                               // src: fsource.src_text,
            });
            let package_quant = QualifierFragment::Package(package_name.ident()?);

            let p_id = me.add(current_node, package_quant, package_entry);

            let old_current = current_node;
            current_node = p_id;

            for item in &package.0.items {
                match item {
                    Definition::Import(import_item) => {
                        deps.push(*import_item);
                    }
                    Definition::Struct(StructDef { the_ty, fields }) => {
                        let ident = QualifierFragment::Type(the_ty.name()?.ident()?);

                        let struct_entry = Item::Struct(StructEntry { ty: *the_ty });

                        let struct_node = me.add(current_node, ident, struct_entry);
                        for f in fields {
                            let field_name = QualifierFragment::Field(f.0.name()?.ident()?);
                            let field_entry = Item::Field(*f);
                            me.add(struct_node, field_name, field_entry);
                        }
                    }
                    Definition::Enum(EnumDef { the_ty, variants }) => {
                        // let parent_name = the_ty.0.get_user_name().unwrap();
                        let parent_name = the_ty.name()?;
                        let ident = QualifierFragment::Type(parent_name.ident()?);
                        //let the_ty = (Ty::User(name.clone(), vec![]), name.1);
                        let enum_entry = Item::Enum(EnumEntry { ty: *the_ty });
                        let enum_node = me.add(current_node, ident, enum_entry);
                        for v in variants {
                            let variant_name =
                                QualifierFragment::Variant(v.0.name.name()?.ident()?);
                            let v = (
                                EnumVariant {
                                    parent_name: Some(parent_name),
                                    ..v.0
                                },
                                v.1,
                            );
                            let variant_entry = Item::Variant(v);
                            me.add(enum_node, variant_name, variant_entry);
                        }
                    }
                    Definition::Let(name, body, ty) => {
                        let ident = QualifierFragment::Func(name.name()?.ident()?);
                        let entry = Item::Function(FunctionItem {
                            name: *name,
                            sig: Cell::from(*ty),
                            body: *body,
                            checked: Cell::from(false),
                        });
                        me.add(current_node, ident, entry);
                    }
                    Definition::Extern(n, ty) => {
                        let ident = QualifierFragment::Func(n.name()?.ident()?);
                        let entry = Item::Extern {
                            //parent: current_parent.clone(),
                            name: *n,
                            sig: *ty,
                        };
                        me.add(current_node, ident, entry);
                    }
                    Definition::ImplDef(ImplDef { the_ty, methods }) => {
                        me.build_impl_def(package_quant, the_ty, methods)?;
                    }
                }
            }
            current_node = old_current;
            pack_imports.insert(package_quant, deps);
        }
        //dbg!(&pack_imports);
        for (name, deps) in &pack_imports {
            let package = me.get(&[*name][..]).unwrap();

            for dep in deps {
                // dbg!(dep);
                let path = QualifierFragment::from_expr(dep);
                // dbg!(&path);
                let imports: Vec<NodeIndex> = if let Some(n) = me.get(&path?) {
                    // if matches!(me.value(n).unwrap(), Item::Package(_)) {
                    //     me.graph
                    //         .edges_directed(n, petgraph::Direction::Outgoing)
                    //         .map(|e| e.target())
                    //         .collect()
                    // } else {
                    vec![n]
                    // }
                } else {
                    return Err(DynamicErr::new("Import does not exist")
                        .label("this", dep.1)
                        .into());
                };
                for import in imports {
                    let the_name = me
                        .graph
                        .edges_directed(import, petgraph::Direction::Incoming)
                        .map(|x| x.weight())
                        .next()
                        .copied()
                        .unwrap();
                    me.graph.add_edge(package, import, the_name);
                }
            }
        }

        Ok(me)
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    fn build_impl_def(
        &mut self,
        package_quant: QualifierFragment,
        the_ty: &Spanned<Intern<Ty>>,
        methods: &Vec<(
            Spanned<Intern<Expr>>,
            Spanned<Intern<Expr>>,
            Spanned<Intern<Ty>>,
        )>,
    ) -> CompResult<()> {
        let type_name = QualifierFragment::Type(the_ty.name()?.ident()?);

        let type_node = self.get(&[package_quant, type_name]).unwrap();
        for (method_name, method_body, method_ty) in methods {
            let method_qual = QualifierFragment::Method(method_name.name()?.ident()?);
            // let cell = OnceCell::from(*method_ty);
            // let leak_cell: &'static OnceCell<_> = Box::leak(Box::new(cell));
            let the_method = Item::Function(FunctionItem {
                name: *method_name,
                sig: Cell::from(Some(*method_ty)),
                body: *method_body,
                checked: Cell::from(false),
            });
            self.add(type_node, method_qual, the_method);
        }
        Ok(())
    }

    #[must_use]
    pub fn get_from_context(
        &self,
        q: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> Option<NodeIndex> {
        let paths = self.search_for_edge(q);
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
    fn raw_get_mut_node_and_children(
        &mut self,
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
    pub fn raw_get_node_and_children_indexes(
        &self,
        q: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> Option<(NodeIndex, Vec<NodeIndex>)> {
        let parent = self.get_from_context(q, packctx)?;
        let children: Vec<_> = self
            .graph
            .edges_directed(parent, petgraph::Direction::Outgoing)
            .map(|c| c.target())
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

    #[must_use]
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
    #[must_use]
    pub fn search_for_edge(&self, k: &QualifierFragment) -> Vec<Vec<QualifierFragment>> {
        use petgraph::algo::all_simple_paths;
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
                    let edge = *self.graph.edges_connecting(a, b).next().unwrap().weight();
                    path.push(edge);
                }
                paths.push(path);
            }
        }
        paths
    }

    #[cfg(feature = "testing")]
    pub fn test_get<'graph>(&'graph self, q: &[QualifierFragment]) -> Option<NodeIndex> {
        self.get(q)
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

    // #[cfg(test)]
    // #[cfg(debug_assertions)]
    #[cfg(feature = "testing")]
    #[allow(clippy::disallowed_names)]
    pub fn make_graph() -> Self {
        // if cfg!(test) {
        let mut graph: DiGraph<Item, QualifierFragment> = DiGraph::new();
        let root = graph.add_node(Item::Root);
        let lib_foo = graph.add_node(Item::Dummy("libFoo"));
        let foo = graph.add_node(Item::Dummy("foo"));
        let lib_bar = graph.add_node(Item::Dummy("libBar"));
        let bar = graph.add_node(Item::Dummy("Bar"));
        let baz = graph.add_node(Item::Dummy("baz"));
        let bar_foo = graph.add_node(Item::Dummy("fooooo"));
        graph.extend_with_edges([
            (
                root,
                lib_foo,
                QualifierFragment::Package(Intern::from_ref("Foo")),
            ),
            (
                lib_foo,
                foo,
                QualifierFragment::Func(Intern::from_ref("foo")),
            ),
            (
                root,
                lib_bar,
                QualifierFragment::Package(Intern::from_ref("Bar")),
            ),
            (
                lib_bar,
                bar,
                QualifierFragment::Type(Intern::from_ref("Bar")),
            ),
            (bar, baz, QualifierFragment::Field(Intern::from_ref("f1"))),
            (
                lib_bar,
                bar_foo,
                QualifierFragment::Func(Intern::from_ref("foo")),
            ),
        ]);

        Self { graph, root }
    }
}
