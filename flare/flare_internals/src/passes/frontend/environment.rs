// use petgraph::Graph;
use petgraph::{
    dot::Config,
    prelude::StableDiGraph,
    stable_graph::{EdgeReference, NodeIndex},
    visit::EdgeRef as _,
};
use rustc_hash::{FxHashMap, FxHashSet};

use std::hash::RandomState;

use crate::resource::{
    errors::CompResult,
    rep::{
        // concretetypes::{EnumVariant, Ty},
        common::Syntax,
        frontend::{
            cst::{Package, PackageCollection, UntypedCst},
            entry::{FunctionItem, Item, ItemKind, PackageEntry},
            quantifier::QualifierFragment,
        },
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
pub struct Environment<S: Syntax> {
    pub graph: StableDiGraph<Item<S>, QualifierFragment>,
    pub root: NodeIndex,
}

impl Environment<UntypedCst> {
    /// Build the environment from a given `Program`
    /// # Errors
    /// - on invalid names,
    ///
    pub fn build(program: &PackageCollection<UntypedCst>) -> CompResult<Self> {
        dbg!(program);
        todo!()
    }

    /// Builds an impl definition
    /// # Errors
    /// On invalid names.
    #[allow(dead_code, unused_variables)]
    fn build_impl_def(
        &self,
        package_quant: &QualifierFragment,
        the_ty: <UntypedCst as Syntax>::Name,
        methods: &[(
            <UntypedCst as Syntax>::Name,
            <UntypedCst as Syntax>::Expr,
            <UntypedCst as Syntax>::Type,
        )],
    ) -> CompResult<()> {
        todo!()
    }
}

impl<S: Syntax> Environment<S> {
    pub fn from_graph_and_root(
        graph: impl Into<StableDiGraph<Item<S>, QualifierFragment>>,
        root: NodeIndex,
    ) -> Self {
        let graph = graph.into();
        Self { graph, root }
    }

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
    pub fn add(
        &mut self,
        parent_node: NodeIndex,
        qualifier: QualifierFragment,
        item: Item<S>,
    ) -> NodeIndex {
        let child_idx = self.graph.add_node(item);
        self.graph.add_edge(parent_node, child_idx, qualifier);
        child_idx
    }

    #[allow(dead_code, clippy::unwrap_used, clippy::dbg_macro)]
    #[deprecated]
    pub fn debug(&self) {
        let render =
            |_, v: EdgeReference<'_, QualifierFragment>| format!("label = \"{}\"", v.weight());
        let dot = petgraph::dot::Dot::with_attr_getters(
            &self.graph,
            &[
                Config::EdgeNoLabel,
                Config::NodeNoLabel,
                Config::RankDir(petgraph::dot::RankDir::LR),
            ],
            // &|_, _| String::new(),
            &render,
            &|_, _| String::new(),
        );
        dbg!(dot);
    }
    #[inline]
    /// Get the item value of an index.
    /// # Examples
    /// ```rust   
    /// let foo = env.add(env.root, QualifierFragment::Dummy("libFoo"), Item::Dummy("Foo"));
    /// assert_eq!(Some(Item::Dummy("Foo")), env.value(foo))
    /// ```
    pub fn value(&self, idx: NodeIndex) -> Option<&Item<S>> {
        self.graph.node_weight(idx)
    }

    fn trace_import_path(
        &self,
        path: &[QualifierFragment],
        package_to_exports: &FxHashMap<
            QualifierFragment,
            FxHashSet<(QualifierFragment, NodeIndex)>,
        >,
    ) -> NodeIndex {
        path.windows(2)
            .map(|path| {
                let [left, right] = path else {
                    panic!("Path is invalid")
                };

                if let QualifierFragment::Package(_) = left {
                    if let Some(exports) = package_to_exports.get(left) {
                        if let Some(n) = exports.iter().find(|x| x.0.is(right)) {
                            n.1
                        } else {
                            panic!("import not found, {left}.{right}")
                        }
                    } else {
                        panic!("Package has no exports")
                    }
                } else {
                    panic!("Import path should be a package or subpackage")
                }
            })
            .next_back()
            .expect("Path was empty")
    }

    #[inline]
    pub fn get_from_context(
        &self,
        frag: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> Option<NodeIndex> {
        let paths = self.search_for_edge(frag);
        for path in &paths {
            // dbg!(path);
            // if path.first().ok_or_else(err)?.is(packctx) {
            if let Some(first) = path.first()
                && first.is(packctx)
            {
                return self.get(path);
            }
        }
        None
    }

    /// Internal helper function for `get_node_and_children`
    fn raw_get_node_and_children(
        &self,
        frag: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> Option<(NodeIndex, Vec<EdgeReference<'_, QualifierFragment>>)> {
        let parent = self.get_from_context(frag, packctx)?;
        let children: Vec<_> = self
            .graph
            .edges_directed(parent, petgraph::Direction::Outgoing)
            .collect();
        Some((parent, children))
    }

    #[inline]
    pub fn raw_get_node_and_children_indexes(
        &self,
        frag: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> Option<(NodeIndex, Vec<NodeIndex>)> {
        let parent = self.get_from_context(frag, packctx)?;
        let children: Vec<_> = self
            .graph
            .edges_directed(parent, petgraph::Direction::Outgoing)
            .map(|edge| edge.target())
            .collect();
        Some((parent, children))
    }

    #[inline]
    pub fn get_node_and_children(
        &self,
        frag: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> Option<(&Item<S>, Vec<(&QualifierFragment, &Item<S>)>)> {
        let (node, children) = self.raw_get_node_and_children(frag, packctx)?;
        let node_w = self.value(node)?;

        Some((
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
    ) -> Option<Vec<EdgeReference<'_, QualifierFragment>>> {
        let parent = self.get_from_context(frag, packctx)?;
        let children: Vec<_> = self
            .graph
            .edges_directed(parent, petgraph::Direction::Outgoing)
            .collect();
        Some(children)
    }

    #[inline]
    /// Get the children of a node given the the node's quantifier and context to search in.
    pub fn get_children(
        &self,
        frag: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> Option<Vec<(&QualifierFragment, &Item<S>)>> {
        let children = self.raw_get_children(frag, packctx)?;

        // children
        //     .iter()
        //     .map(|edge| self.value(edge.target()).map(|item| (edge.weight(), item)))
        //     .collect()
        Some(
            children
                .iter()
                .map(|edge| {
                    // # SAFETY: `self.value()` returns none only if the node
                    // doesn't exist. We know the node exists because we
                    // are iterating over the children of the parent.
                    (edge.weight(), unsafe {
                        self.value(edge.target()).unwrap_unchecked()
                    })
                })
                .collect(),
        )
    }

    #[inline]
    /// Given a qualifier and a context to search in, get the `Item` value of the qualifier's target.
    pub fn get_node(
        &self,
        target: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> Option<&Item<S>> {
        let node = self.get_from_context(target, packctx)?;
        let node_w = self.value(node)?;

        Some(node_w)
    }

    /// Gets all the targets for all edges labeled `frag`
    fn get_all_targets_edge<'envi, 'fragment>(
        &'envi self,
        frag: &'fragment QualifierFragment,
    ) -> impl Iterator<Item = NodeIndex> + use<'envi, 'fragment, S> {
        let edges = self
            .graph
            .edge_indices()
            .filter(move |x| self.graph.edge_weight(*x).unwrap() == (frag));

        edges.map(|x| self.graph.edge_endpoints(x).unwrap().1)
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
                    let edge = self
                        .graph
                        .edges_connecting(*first, *second)
                        .next()
                        .unwrap_or_else(|| {
                            unreachable!(
                                "Index {:?} is not a child of index {:?}; could not build dependancy graph.",
                                first, second
                            )
                        })
                        .weight().clone();
                    path.push(edge);
                }
                paths.push(path);
            }
        }
        paths
    }

    /// Gets an absolute path and verifies it
    fn get<'graph>(&'graph self, qualifier_path: &[QualifierFragment]) -> Option<NodeIndex> {
        //let _ = self.graph.edges(self.root).map(|x| dbg!(x));
        struct Rec<'s, 'graph, T> {
            f: &'s dyn Fn(&Self, &'graph T, NodeIndex, &[QualifierFragment]) -> Option<NodeIndex>,
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

                    [head, tail @ ..] => {
                        let candidate = graph_self.graph.edges(n).find(|e| e.weight().is(head))?;

                        (rec.f)(rec, graph_self, candidate.target(), tail)
                        //self.graph.node_weight(n).cloned()
                    }
                }
            },
        };
        (rec.f)(&rec, self, self.root, qualifier_path) //.inspect(|x| {dbg!(&self.graph.node_weight(*x));})
    }

    pub fn get_parent(&self, idx: NodeIndex) -> Option<QualifierFragment> {
        let parents = self
            .graph
            .edges_directed(idx, petgraph::Direction::Incoming);
        let mut the_edge = None;
        for parent in parents {
            let source = parent.source();
            let edge_idx = self.graph.find_edge(source, idx)?;
            let edge = self.graph.edge_weight(edge_idx).cloned()?;
            if matches!(edge, QualifierFragment::Package(_)) {
                the_edge = Some(edge);
            } else if !matches!(edge, QualifierFragment::Root) {
                the_edge = self.get_parent(source);
            } else {
                continue;
            }
        }

        the_edge
    }

    // #[cfg(test)]
    // #[cfg(debug_assertions)]
    // #[cfg(feature = "testing")]
    // #[allow(clippy::disallowed_names)]
    // pub fn make_graph() -> Self {
    //     // if cfg!(test) {
    //     let mut graph: DiGraph<Item, QualifierFragment> = DiGraph::new();
    //     let root = graph.add_node(Item::Root);
    //     let lib_foo = graph.add_node(Item::Dummy("libFoo"));
    //     let foo = graph.add_node(Item::Dummy("foo"));
    //     let lib_bar = graph.add_node(Item::Dummy("libBar"));
    //     let bar = graph.add_node(Item::Dummy("Bar"));
    //     let baz = graph.add_node(Item::Dummy("baz"));
    //     let bar_foo = graph.add_node(Item::Dummy("fooooo"));
    //     graph.extend_with_edges([
    //         (
    //             root,
    //             lib_foo,
    //             QualifierFragment::Package(Intern::from_ref("Foo")),
    //         ),
    //         (
    //             lib_foo,
    //             foo,
    //             QualifierFragment::Func(Intern::from_ref("foo")),
    //         ),
    //         (
    //             root,
    //             lib_bar,
    //             QualifierFragment::Package(Intern::from_ref("Bar")),
    //         ),
    //         (
    //             lib_bar,
    //             bar,
    //             QualifierFragment::Type(Intern::from_ref("Bar")),
    //         ),
    //         (bar, baz, QualifierFragment::Field(Intern::from_ref("f1"))),
    //         (
    //             lib_bar,
    //             bar_foo,
    //             QualifierFragment::Func(Intern::from_ref("foo")),
    //         ),
    //     ]);

    //     Self { graph, root }
    // }
}
