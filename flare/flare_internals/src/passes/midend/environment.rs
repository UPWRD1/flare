use internment::Intern;
// use petgraph::Graph;
use petgraph::{
    graph::{DiGraph, EdgeReference, NodeIndex},
    visit::EdgeRef as _,
};
use rustc_hash::FxHashMap;
// use serde::{Deserialize, Serialize};
// use std::cell::OnceCell;
// use std::collections::HashMap;
use std::{cell::Cell, hash::RandomState};

use crate::resource::{
    errors::{self, CompResult, CompilerErr, DynamicErr, FatalErr},
    rep::{
        ast::{
            Definition,
            EnumDef,
            Expr,
            ImplDef,
            Program,
            StructDef,
            Untyped,
            // Untyped, Variable
        },
        common::{Ident as _, Named},
        entry::{EnumEntry, FunctionItem, Item, ItemKind, PackageEntry, StructEntry},
        quantifier::QualifierFragment,
        concretetypes::{EnumVariant, Ty},
        Spanned,
    },
};

#[derive(Debug)]
/// The main environment graph structure. Holds all the objects produced by
/// the  parser, and the index of the root.
///
/// Obviously, `Environment` does not implement `Copy`, but it also does not
/// implement `Clone`, since it is ridiculously expensive, and there is no
/// real reason to clone the environment.
#[non_exhaustive]
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

    #[inline]
    /// Get the item value of an index.
    /// # Examples
    /// ```rust   
    /// let foo = env.add(env.root, QualifierFragment::Dummy("libFoo"), Item::Dummy("Foo"));
    /// assert_eq!(Some(Item::Dummy("Foo")), env.value(foo))
    /// ```
    pub fn value(&self, idx: NodeIndex) -> CompResult<&Item> {
        self.graph
            .node_weight(idx)
            .ok_or_else(|| FatalErr::new(format!("Bad node index: {:?}", idx)))
    }

    /// Build the environment from a given `Program`
    /// # Errors
    /// - on invalid names,
    ///
    pub fn build(program: &Program<Untyped>) -> CompResult<Self> {
        use ItemKind::*;
        let mut graph = DiGraph::new();
        let mut current_node = graph.add_node(Item::new(ItemKind::Root, true));

        let mut me = Self {
            graph,
            root: current_node,
        };
        let mut pack_imports: FxHashMap<
            Spanned<QualifierFragment>,
            Vec<Spanned<Intern<Expr<Untyped>>>>,
        > = FxHashMap::default();

        // Start building each package's contents
        for package in &program.packages {
            let package_name = package.0.name;

            let mut deps = Vec::new();
            let package_entry = Item::new(
                Package(PackageEntry {
                    name: package.0.name,
                    id: package.1, // file: fsource.filename,
                                   // src: fsource.src_text,
                }),
                false,
            );
            let package_quant = QualifierFragment::Package(package_name.0);

            let p_id = me.add(current_node, package_quant, package_entry);

            let old_current = current_node;
            current_node = p_id;

            for item in &package.0.items {
                match item {
                    Definition::Import(import_item) => {
                        deps.push(*import_item);
                    }
                    Definition::Struct(StructDef { the_ty, fields }) => {
                        me.build_struct(current_node, the_ty, fields)?;
                    }
                    Definition::Enum(EnumDef { the_ty, variants }) => {
                        me.build_enum(current_node, the_ty, variants)?;
                    }
                    Definition::Let(name, body, ty) => {
                        let ident = QualifierFragment::Func(name.0 .0);
                        let entry = Item::new(
                            Function(FunctionItem {
                                name: *name,
                                sig: Cell::from(*ty),
                                body: *body,
                            }),
                            false,
                        );
                        me.add(current_node, ident, entry);
                    }
                    Definition::Extern(n, ty) => {
                        let ident = QualifierFragment::Func(n.0);
                        let entry = Item::new(
                            Extern {
                                //parent: current_parent.clone(),
                                name: *n,
                                sig: *ty,
                            },
                            true,
                        );
                        me.add(current_node, ident, entry);
                    }
                    Definition::ImplDef(ImplDef { the_ty, methods }) => {
                        me.build_impl_def(package_quant, the_ty, methods)?;
                    }
                }
            }
            current_node = old_current;
            pack_imports.insert(Spanned(package_quant, package_name.1), deps);
        }
        //dbg!(&pack_imports);
        for (name, deps) in &pack_imports {
            let package = me
                .get(&[name.0][..])
                .map_err(|_| errors::not_defined(name.0, &name.1))?;

            for dep in deps {
                // dbg!(dep);
                let path = QualifierFragment::from_expr(dep);
                // dbg!(&path);
                let imports: Vec<NodeIndex> = if let Ok(n) = me.get(&path?) {
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
                        .ok_or_else(|| {
                            DynamicErr::new("Could not locate import within graph")
                                .label("here", dep.1)
                        })?;
                    me.graph.add_edge(package, import, the_name);
                }
            }
        }

        Ok(me)
    }

    /// Builds an impl definition
    /// # Errors
    /// On invalid names.
    fn build_impl_def(
        &mut self,
        package_quant: QualifierFragment,
        the_ty: &Spanned<Intern<Ty>>,
        methods: &Vec<(
            Spanned<Intern<String>>,
            Spanned<Intern<Expr<Untyped>>>,
            Spanned<Intern<Ty>>,
        )>,
    ) -> CompResult<()> {
        use ItemKind::Function;
        let type_name = QualifierFragment::Type(the_ty.ident()?.0);

        let type_node = self
            .get(&[package_quant, type_name])
            .map_err(|_| errors::not_defined(type_name, &the_ty.1))?;
        for &(method_name, method_body, method_ty) in methods {
            let method_qual = QualifierFragment::Method(method_name.0);
            let the_method = Item::new(
                Function(FunctionItem {
                    name: Untyped(method_name),
                    sig: Cell::from(Some(method_ty)),
                    body: method_body,
                }),
                false,
            );
            self.add(type_node, method_qual, the_method);
        }
        Ok(())
    }

    /// Builds a struct.
    ///
    /// # Errors
    ///
    /// On invalid names
    fn build_struct(
        &mut self,
        current_node: NodeIndex,
        the_ty: &Spanned<Intern<Ty>>,
        fields: &Vec<(Spanned<Intern<String>>, Spanned<Intern<Ty>>)>,
    ) -> Result<(), CompilerErr> {
        use ItemKind::{Field, Struct};
        let ident = QualifierFragment::Type(the_ty.ident()?.0);
        let struct_entry = Item::new(Struct(StructEntry { ty: *the_ty }), false);
        let struct_node = self.add(current_node, ident, struct_entry);

        for field in fields {
            let field_name = QualifierFragment::Field(field.0 .0);
            let field_entry = Item::new(Field(*field), false);
            self.add(struct_node, field_name, field_entry);
        }
        Ok(())
    }
    /// Builds an enum
    fn build_enum(
        &mut self,
        current_node: NodeIndex,
        the_ty: &Spanned<Intern<Ty>>,
        variants: &Vec<Spanned<EnumVariant>>,
    ) -> Result<(), CompilerErr> {
        use ItemKind::{Enum, Variant};
        let parent_name = the_ty.ident()?;
        let ident = QualifierFragment::Type(parent_name.0);
        let enum_entry = Item::new(Enum(EnumEntry { ty: *the_ty }), false);
        let enum_node = self.add(current_node, ident, enum_entry);

        for variant in variants {
            let variant_name = QualifierFragment::Variant(variant.0.name.0);
            let the_variant = (
                EnumVariant {
                    parent_name: Some(parent_name),
                    ..variant.0
                },
                variant.1,
            );
            let variant_entry = Item::new(Variant(the_variant.into()), false);
            self.add(enum_node, variant_name, variant_entry);
        }
        Ok(())
    }

    #[inline]
    pub fn get_from_context(
        &self,
        frag: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> CompResult<NodeIndex> {
        let err = || FatalErr::new(format!("{} does not exist in {}", frag, packctx));
        let paths = self.search_for_edge(frag);
        for path in &paths {
            if path.first().ok_or_else(err)?.is(packctx) {
                return self.get(path);
            }
        }
        Err(err())
    }

    /// Internal helper function for `get_node_and_children`
    fn raw_get_node_and_children(
        &self,
        frag: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> CompResult<(NodeIndex, Vec<EdgeReference<'_, QualifierFragment>>)> {
        let parent = self.get_from_context(frag, packctx)?;
        let children: Vec<_> = self
            .graph
            .edges_directed(parent, petgraph::Direction::Outgoing)
            .collect();
        Ok((parent, children))
    }

    #[inline]
    pub fn raw_get_node_and_children_indexes(
        &self,
        frag: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> CompResult<(NodeIndex, Vec<NodeIndex>)> {
        let parent = self.get_from_context(frag, packctx)?;
        let children: Vec<_> = self
            .graph
            .edges_directed(parent, petgraph::Direction::Outgoing)
            .map(|edge| edge.target())
            .collect();
        Ok((parent, children))
    }

    #[inline]
    pub fn get_node_and_children(
        &self,
        frag: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> CompResult<(&Item, Vec<(&QualifierFragment, &Item)>)> {
        let (node, children) = self.raw_get_node_and_children(frag, packctx)?;
        let node_w = self.value(node)?;

        Ok((
            node_w,
            children
                .iter()
                .map(|edge| {
                    (
                        edge.weight(),
                        // # SAFETY: `self.value()` returns none only if the node
                        // doesn't exist. We know the node exists because we
                        // are iterating over the children of the parent.
                        unsafe { self.value(edge.target()).unwrap_unchecked() },
                    )
                })
                .collect(),
        ))
    }

    /// Internal helper function for `get_children()`
    fn raw_get_children(
        &self,
        frag: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> CompResult<Vec<EdgeReference<'_, QualifierFragment>>> {
        let parent = self.get_from_context(frag, packctx)?;
        let children: Vec<_> = self
            .graph
            .edges_directed(parent, petgraph::Direction::Outgoing)
            .collect();
        Ok(children)
    }

    #[inline]
    /// Get the children of a node given the the node's quantifier and context to search in.
    pub fn get_children(
        &self,
        frag: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> CompResult<Vec<(&QualifierFragment, &Item)>> {
        let children = self.raw_get_children(frag, packctx)?;

        // children
        //     .iter()
        //     .map(|edge| self.value(edge.target()).map(|item| (edge.weight(), item)))
        //     .collect()
        Ok(children
            .iter()
            .map(|edge| {
                // # SAFETY: `self.value()` returns none only if the node
                // doesn't exist. We know the node exists because we
                // are iterating over the children of the parent.
                (edge.weight(), unsafe {
                    self.value(edge.target()).unwrap_unchecked()
                })
            })
            .collect())
    }

    #[inline]
    /// Given a qualifier and a context to search in, get the `Item` value of the qualifier's target.
    pub fn get_node(
        &self,
        target: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> CompResult<&Item> {
        let node = self.get_from_context(target, packctx)?;
        let node_w = self.value(node)?;

        Ok(node_w)
    }

    /// Gets all the targets for all edges labeled `frag`
    fn get_all_targets_edge<'envi, 'fragment>(
        &'envi self,
        frag: &'fragment QualifierFragment,
    ) -> impl Iterator<Item = NodeIndex> + use<'envi, 'fragment> {
        let edges = self.graph.edge_references().filter(|x| x.weight().is(frag));
        edges.map(|x| x.target())
    }

    /// Optionally returns a vector of the possible paths to an item fragment.
    #[must_use]
    #[inline]
    pub fn search_for_edge(&self, fragment: &QualifierFragment) -> Vec<Vec<QualifierFragment>> {
        use petgraph::algo::all_simple_paths;
        use petgraph::prelude::*;
        let mut paths: Vec<Vec<QualifierFragment>> = vec![];
        let targets = self.get_all_targets_edge(fragment);
        for target in targets {
            let real_paths: Vec<Vec<NodeIndex>> =
                all_simple_paths::<Vec<_>, _, RandomState>(&self.graph, self.root, target, 0, None)
                    .collect::<Vec<_>>();
            for frag_path in real_paths {
                let mut path = Vec::new();
                for node_pair in frag_path.windows(2) {
                    debug_assert_eq!(
                        node_pair.len(),
                        2,
                        "p.windows(2) failed to produce a window of size 2"
                    );

                    // SAFETY: The length of the windows is always 2, therefore
                    // we can always index into the first two elements,
                    // without panicking.
                    let first = unsafe { node_pair.get_unchecked(0) };
                    // SAFETY: Same as above
                    let second = unsafe { node_pair.get_unchecked(1) };
                    let edge = *self
                        .graph
                        .edges_connecting(*first, *second)
                        .next()
                        .unwrap_or_else(|| FatalErr::new(format!("Index {:?} is not a child of index {:?}; could not build dependancy graph.", first, second)))
                        .weight();
                    path.push(edge);
                }
                paths.push(path);
            }
        }
        paths
    }

    /// Gets an absolute path and verifies it
    fn get<'graph>(&'graph self, qualifier_path: &[QualifierFragment]) -> CompResult<NodeIndex> {
        //let _ = self.graph.edges(self.root).map(|x| dbg!(x));
        struct Rec<'s, 'graph, T> {
            f: &'s dyn Fn(
                &Rec<'s, 'graph, T>,
                &'graph T,
                NodeIndex,
                &[QualifierFragment],
            ) -> CompResult<NodeIndex>,
        }
        let rec = Rec {
            f: &|rec: &Rec<'_, 'graph, _>,
                 graph_self: &'graph Self,
                 n: NodeIndex,
                 q: &[QualifierFragment]|
             -> CompResult<NodeIndex> {
                //dbg!(&q);
                match q {
                    [] => Ok(n),

                    [head, tail @ ..] => {
                        let candidate = graph_self
                            .graph
                            .edges(n)
                            .find(|e| e.weight().is(head))
                            .ok_or_else(|| DynamicErr::new("Does not Exist"))?;

                        (rec.f)(rec, graph_self, candidate.target(), tail)
                        //self.graph.node_weight(n).cloned()
                    }
                }
            },
        };
        (rec.f)(&rec, self, self.root, qualifier_path) //.inspect(|x| {dbg!(&self.graph.node_weight(*x));})
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

    pub fn remove_unused(self) -> Self {
        let g = self
            .graph
            .filter_map_owned(|_, n| n.is_checked.get().then_some(n), |_, e| Some(e));
        Self { graph: g, ..self }
    }
}
