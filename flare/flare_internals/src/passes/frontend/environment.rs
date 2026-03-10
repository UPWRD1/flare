use internment::Intern;
// use petgraph::Graph;
use radix_trie::{Trie, TrieCommon};
use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};
use std::hash::RandomState;

use crate::resource::{
    errors::{CompResult, DynamicErr},
    rep::{
        // concretetypes::{EnumVariant, Ty},
        common::{HasSpan, Ident, Spanned, Syntax},
        frontend::{
            ast::{ItemId, Untyped},
            cst::{CstExpr, Definition, ImplDef, Program, UntypedCst},
            csttypes::CstType,
            entry::{FunctionItem, Item, ItemKind, PackageEntry},
            quantifier::{FullQualifier, QualifierFragment},
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
    // pub graph: DiGraph<Item<S>, QualifierFragment>,
    pub trie: Trie<FullQualifier, ItemId>,
    // pub root: NodeIndex,
    pub items: FxHashMap<ItemId, Item<S>>,
}

pub struct EnvironmentBuilder<S: Syntax> {
    current_id: usize,
    // pub graph: DiGraph<Item<S>, QualifierFragment>,
    pub trie: Trie<FullQualifier, ItemId>,
    // pub qual_to_items: FxHashMap<Vec<QualifierFragment>, ItemId>,
    pub items: FxHashMap<ItemId, Item<S>>,
}

impl Default for EnvironmentBuilder<UntypedCst> {
    fn default() -> Self {
        Self {
            current_id: Default::default(),
            trie: Default::default(),
            items: Default::default(),
            // qual_to_items: Default::default(),
        }
    }
}

impl EnvironmentBuilder<UntypedCst> {
    fn add_node(&mut self, path: &[QualifierFragment], item: Item<UntypedCst>) -> ItemId {
        let id = self.current_id;
        let itemid = ItemId(id);
        self.trie.insert(path.to_vec().into(), itemid);
        self.items.insert(itemid, item);
        self.current_id += 1;
        itemid
    }

    fn add_import_edge(
        &mut self,
        importer_path: &[QualifierFragment],
        mut importee_path: Vec<QualifierFragment>,
        imported_item_id: ItemId,
    ) {
        if importee_path
            .iter()
            .any(|x| matches!(x, QualifierFragment::Root))
        {
            importee_path.remove(0);
        };
        let new_path = [importer_path, &importee_path].concat();
        self.trie.insert(new_path.to_vec().into(), imported_item_id);
    }
    /// Build the environment from a given `Program`
    /// # Errors
    /// - on invalid names,
    ///
    pub fn build(program: &Program<UntypedCst>) -> CompResult<Environment<UntypedCst>> {
        // use ItemKind::*;
        let mut me = Self::default();

        let root_path = [QualifierFragment::Root].as_slice();
        me.add_node(root_path, Item::new(ItemKind::Root));

        let num_packages = program.packages.len();
        let mut package_to_imports: FxHashMap<
            Spanned<Vec<QualifierFragment>>,
            FxHashSet<Spanned<Intern<CstExpr<Untyped>>>>,
        > = FxHashMap::with_capacity_and_hasher(num_packages, FxBuildHasher);

        let mut package_to_exports: FxHashMap<
            QualifierFragment,
            FxHashSet<(QualifierFragment, ItemId)>,
        > = FxHashMap::with_capacity_and_hasher(num_packages, FxBuildHasher);

        // Start building each package's contents
        for package in &program.packages {
            let package_name = package.0.name;

            let mut imports = FxHashSet::default();
            let mut exports = FxHashSet::default();

            let package_entry = Item::new(ItemKind::Package(PackageEntry {
                name: package.0.name,
                id: package.1,
            }
                as PackageEntry<UntypedCst>));
            let package_qual = QualifierFragment::Package(package_name.0);
            let package_path = [root_path, &[package_qual]].concat();
            let p_id = me.add_node(&package_path, package_entry);

            for item in &package.0.items {
                let mut item_nodeindex: NodeIndex = NodeIndex::new(0);
                let f: Option<(Item<UntypedCst>, QualifierFragment)> = match item.def {
                    Definition::Import(import_item) => {
                        imports.insert(import_item);
                        None
                    }
                    Definition::Type(name, generics, t) => {
                        let qfrag = QualifierFragment::Type(name.ident()?.0);

                        let t = if generics.is_empty() {
                            t
                        } else {
                            let mut new_t = generics.iter().fold(t, |g, t| {
                                Spanned(CstType::GenericFun(*t, g).into(), g.span())
                            });
                            new_t.1 = t.1;
                            new_t
                        };
                        let entry = Item::new(ItemKind::Type(name, generics, t));
                        Some((entry, qfrag))
                    }
                    Definition::Let(name, body, sig) => {
                        let qfrag = QualifierFragment::Func(name.0);
                        let entry = Item::new(ItemKind::Function(FunctionItem { name, sig, body }));
                        Some((entry, qfrag))
                    }
                    Definition::Extern(name, args, sig) => {
                        let qfrag = QualifierFragment::Func(name.0);
                        let entry = Item::new(ItemKind::Extern {
                            //parent: current_parent.clone(),
                            name,
                            args,
                            sig,
                        });
                        Some((entry, qfrag))
                    }
                    Definition::ImplDef(ImplDef { the_ty, methods }) => {
                        me.build_impl_def(package_qual, the_ty, methods)?;
                        None
                    }
                };
                if let Some((entry, qual)) = f {
                    let item_path = [root_path, &[qual]].concat();
                    let id = me.add_node(&item_path, entry);
                    if item.is_pub {
                        exports.insert((qual, id));
                    }
                }
            }
            package_to_imports.insert(Spanned(package_path.clone(), package_name.1), imports);
            package_to_exports.insert(package_qual, exports);
        }

        let new_package_to_imports: FxHashMap<
            Spanned<Vec<QualifierFragment>>,
            FxHashSet<Vec<QualifierFragment>>,
        > = package_to_imports
            .into_iter()
            .map(|(package_name, package_imports)| {
                let new_package_imports: FxHashSet<_> = package_imports
                    .into_iter()
                    .flat_map(|import_expr| {
                        QualifierFragment::from_expr(&import_expr)
                            .expect("Could not generate qualifier fragment")
                    })
                    .collect();
                Ok((package_name, new_package_imports))
            })
            .collect::<CompResult<_>>()?;

        for (importer_package_qual, imports) in new_package_to_imports {
            // let importing_package = me.qual_to_items.get(&importer_package_qual.0).ok_or_else(|| {
            //     errors::not_defined(importer_package_qual.0.last().unwrap(), &importer_package_qual.1)
            // })?;

            for importee_path in imports {
                dbg!(&importee_path);
                let importee = me.trace_import_path(&importee_path, &package_to_exports);
                me.add_import_edge(&importer_package_qual.0, importee_path, importee);
            }
        }
        let env = Environment {
            trie: me.trie,
            items: me.items,
        };
        Ok(env)
    }

    fn trace_import_path(
        &self,
        path: &[QualifierFragment],
        package_to_exports: &FxHashMap<QualifierFragment, FxHashSet<(QualifierFragment, ItemId)>>,
    ) -> ItemId {
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
    /// Builds an impl definition
    /// # Errors
    /// On invalid names.
    #[allow(dead_code, unused_variables)]
    fn build_impl_def(
        &self,
        package_quant: QualifierFragment,
        the_ty: <UntypedCst as Syntax>::Name,
        methods: &[(
            <UntypedCst as Syntax>::Name,
            <UntypedCst as Syntax>::Expr,
            <UntypedCst as Syntax>::Type,
        )],
    ) -> CompResult<()> {
        todo!()
        // use ItemKind::Function;
        // let type_name = QualifierFragment::Type(the_ty.0);

        // let type_node = self
        //     .get(&[package_quant, type_name])
        //     .map_err(|_| errors::not_defined(type_name, &the_ty.1))?;
        // for &(method_name, method_body, method_ty) in methods {
        //     let method_qual = QualifierFragment::Method(method_name.0);
        //     let the_method = Item::new(
        //         Function(FunctionItem {
        //             name: Untyped(method_name),
        //             sig: method_ty,
        //             body: method_body,
        //         }),
        //         false,
        //     );
        //     self.add(type_node, method_qual, the_method);
        // }
        // Ok(())
    }
}

impl<S: Syntax> Environment<S> {
    pub fn from_trie_and_items(
        trie: Trie<FullQualifier, ItemId>,
        items: FxHashMap<ItemId, Item<S>>,
    ) -> Self {
        Self { trie, items }
    }

    // / Add an `item` to the environment as a child of a `parent_node`, accessible via a `qualifier`. Returns the `NodeIndex` of the newly-created child.
    // / # Panics
    // / Panics if the internal graph is full.
    // / Panics if the root node doesn't exist.
    // /
    // / # Examples
    // / ```rust
    // / let env: Environment::new();
    // / let foo = env.add(env.root, QualifierFragment::Dummy("libFoo"), Item::Dummy("Foo"));
    // / assert!(env.graph.contains_edge(env.root, foo))
    // / ```
    // pub fn add(
    //     &mut self,
    //     parent_node: NodeIndex,
    //     qualifier: QualifierFragment,
    //     item: Item<S>,
    // ) -> NodeIndex {
    //     let child_idx = self.graph.add_node(item);
    //     self.graph.add_edge(parent_node, child_idx, qualifier);
    //     child_idx
    // }

    #[allow(dead_code, clippy::unwrap_used, clippy::dbg_macro)]
    #[deprecated]
    pub fn debug(&self) {
        // let render =
        //     |_, v: EdgeReference<'_, QualifierFragment>| format!("label = \"{}\"", v.weight());
        // let dot = petgraph::dot::Dot::with_attr_getters(
        //     &self.graph,
        //     &[
        //         Config::EdgeNoLabel,
        //         Config::NodeNoLabel,
        //         Config::RankDir(petgraph::dot::RankDir::LR),
        //     ],
        //     // &|_, _| String::new(),
        //     &render,
        //     &|_, _| String::new(),
        // );
        // dbg!(dot);
    }

    #[inline]
    /// Get the item value of an index.
    /// # Examples
    /// ```rust   
    /// let foo = env.add(env.root, QualifierFragment::Dummy("libFoo"), Item::Dummy("Foo"));
    /// assert_eq!(Some(Item::Dummy("Foo")), env.value(foo))
    /// ```
    pub fn value(&self, idx: &ItemId) -> CompResult<&Item<S>> {
        self.items
            .get(idx)
            .ok_or_else(|| unreachable!("Bad node index: {:?}", idx))
    }

    #[inline]
    pub fn get_from_context(
        &self,
        frag: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> CompResult<ItemId> {
        let err = || DynamicErr::new(format!("{} does not exist in {}", frag, packctx));
        let subtrie = self.trie.subtrie(packctx);
        if let Some(subtrie) = subtrie {
            let v = subtrie
                .children()
                .find(|c| c.key().unwrap() == frag)
                .map(|c| c.value())
                .or_else(|| {
                    subtrie
                        .children()
                        .filter_map(|s| self.get_children(frag, s.key()).ok())
                        .next()
                        .unwrap()
                });
            Ok(dbg!(v))
        } else {
            return Err(err);
        }
    }

    fn raw_get_children(&self, frag: &QualifierFragment, packctx: &QualifierFragment) {
        self.trie.subtrie(packctx).unwrap_or_default().get()
    }

    #[inline]
    /// Get the children of a node given the the node's quantifier and context to search in.
    pub fn get_children(
        &self,
        frag: &QualifierFragment,
        packctx: &QualifierFragment,
    ) -> CompResult<Vec<(&QualifierFragment, &Item<S>)>> {
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
    ) -> CompResult<&Item<S>> {
        let node = self.get_from_context(target, packctx)?;
        let node_w = self.value(node)?;

        Ok(node_w)
    }

    /// Gets all the targets for all edges labeled `frag`
    fn get_all_targets_edge<'envi, 'fragment>(
        &'envi self,
        frag: &'fragment QualifierFragment,
    ) -> impl Iterator<Item = NodeIndex> + use<'envi, 'fragment, S> {
        let edges = self
            .graph
            .edge_references()
            .filter(move |x| x.weight() == (frag));
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
                        .unwrap_or_else(|| {
                            unreachable!(
                                "Index {:?} is not a child of index {:?}; could not build dependancy graph.",
                                first, second
                            )
                        })
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
                &Self,
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

    pub fn get_parent(&self, idx: NodeIndex) -> Option<QualifierFragment> {
        let parents = self
            .graph
            .edges_directed(idx, petgraph::Direction::Incoming);
        let mut the_edge = None;
        for parent in parents {
            let source = parent.source();
            let edge_idx = self.graph.find_edge(source, idx)?;
            let edge = self.graph.edge_weight(edge_idx).copied()?;
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
