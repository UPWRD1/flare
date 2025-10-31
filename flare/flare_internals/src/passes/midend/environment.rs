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
use std::collections::HashMap;
use std::hash::RandomState;
use std::{cell::OnceCell, fmt::Debug};

use crate::passes::midend::typechecking::Solver;

use crate::resource::{
    errors::CompResult,
    rep::{
        ast::{Definition, EnumDef, Expr, Program, StructDef},
        entry::{EnumEntry, Item, PackageEntry, StructEntry},
        quantifier::SimpleQuant,
        Spanned,
    },
};

#[derive(Debug)]
pub struct Environment {
    //pub items: Trie<SimpleQuant, Index>,
    //pub arena: Arena<Node>,
    pub graph: DiGraph<Item, SimpleQuant>,
    //    pub cache: RefCell<HashMap<(SimpleQuant, SimpleQuant), NodeIndex>>,
    pub root: NodeIndex,
}

impl Environment {
    fn add(&mut self, parent_node: NodeIndex, k: SimpleQuant, v: Item) -> NodeIndex {
        let ix = self.graph.add_node(v);
        self.graph.add_edge(parent_node, ix, k);
        ix
    }

    pub fn value(&self, idx: NodeIndex) -> Option<&Item> {
        self.graph.node_weight(idx)
    }

    pub fn build(p: Program) -> CompResult<Environment> {
        let mut g = Graph::new();
        let mut current_node = g.add_node(Item::Root);

        let mut me = Self {
            graph: g,
            root: current_node,
        };
        let mut pack_imports: HashMap<SimpleQuant, Vec<Spanned<Expr>>> = HashMap::new();

        // Start building each package's contents
        for package in p.packages {
            let package_name = package.0.name.0.get_ident().unwrap();

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
            let package_quant = SimpleQuant::Package(package_name.clone());

            let p_id = me.add(current_node, package_quant.clone(), package_entry);

            let old_current = current_node;
            current_node = p_id;

            for item in package.0.items {
                match item {
                    Definition::Import(import_item) => {
                        deps.push(import_item);
                    }
                    Definition::Struct(StructDef { the_ty, fields }) => {
                        let ident = SimpleQuant::Type(the_ty.0.get_user_name().unwrap());

                        let struct_entry = Item::Struct(StructEntry { ty: the_ty });

                        let struct_node = me.add(current_node, ident, struct_entry);
                        for f in fields {
                            let field_name = SimpleQuant::Field(f.0 .0.get_ident().unwrap());
                            let field_entry = Item::Field(f);
                            me.add(struct_node, field_name, field_entry);
                        }
                    }
                    Definition::Enum(EnumDef { the_ty, variants }) => {
                        let ident = SimpleQuant::Type(the_ty.0.get_user_name().unwrap());
                        //let the_ty = (Ty::User(name.clone(), vec![]), name.1);
                        let enum_entry = Item::Enum(EnumEntry { ty: the_ty });
                        let enum_node = me.add(current_node, ident, enum_entry);
                        for v in variants {
                            let variant_name =
                                SimpleQuant::Variant(v.0.name.0.get_ident().unwrap().clone());
                            let variant_entry = Item::Variant(v);
                            me.add(enum_node, variant_name, variant_entry);
                        }
                    }
                    Definition::Let(name, body, ty) => {
                        let ident = SimpleQuant::Func(name.0.get_ident().unwrap());

                        let cell = OnceCell::new();
                        if let Some(ty) = ty {
                            let _ = cell.set(ty);
                        }
                        let entry = Item::Let {
                            name,
                            sig: cell,
                            body,
                        };
                        me.add(current_node, ident, entry);
                    }
                    Definition::Extern(n, ty) => {
                        let ident = SimpleQuant::Func(n.0.get_ident().unwrap());
                        let entry = Item::Extern {
                            //parent: current_parent.clone(),
                            name: n,
                            sig: ty,
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
                let path = SimpleQuant::from_expr(dep);
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

    pub fn get_from_context(&self, q: &SimpleQuant, packctx: &SimpleQuant) -> Option<NodeIndex> {
        let paths = self.search_for_edge(q)?;
        //dbg!(&paths);
        for path in &paths {
            if path.first()?.is(packctx) {
                return self.get(path);
            }
        }
        None
    }

    pub fn raw_get_node_and_children(
        &self,
        q: &SimpleQuant,
        packctx: &SimpleQuant,
    ) -> Option<(NodeIndex, Vec<EdgeReference<'_, SimpleQuant>>)> {
        let parent = self.get_from_context(q, packctx)?;
        let children: Vec<_> = self
            .graph
            .edges_directed(parent, petgraph::Direction::Outgoing)
            .collect();
        Some((parent, children))
    }

    pub fn get_node_and_children(
        &self,
        q: &SimpleQuant,
        packctx: &SimpleQuant,
    ) -> Option<(&Item, Vec<(&SimpleQuant, &Item)>)> {
        let (node, children) = self.raw_get_node_and_children(q, packctx)?;
        let node_w = self.value(node)?;

        Some((
            node_w,
            children
                .iter()
                .map(|c| (c.weight(), self.value(c.target()).unwrap()))
                .collect(),
        ))
    }

    pub fn raw_get_children(
        &self,
        q: &SimpleQuant,
        packctx: &SimpleQuant,
    ) -> Option<Vec<EdgeReference<'_, SimpleQuant>>> {
        let parent = self.get_from_context(q, packctx)?;
        let children: Vec<_> = self
            .graph
            .edges_directed(parent, petgraph::Direction::Outgoing)
            .collect();
        Some(children)
    }

    pub fn get_children(
        &self,
        q: &SimpleQuant,
        packctx: &SimpleQuant,
    ) -> Option<Vec<(&SimpleQuant, &Item)>> {
        let (_, children) = self.raw_get_node_and_children(q, packctx)?;

        Some(
            children
                .iter()
                .map(|c| (c.weight(), self.value(c.target()).unwrap()))
                .collect(),
        )
    }

    pub fn get_node(&self, q: &SimpleQuant, packctx: &SimpleQuant) -> Option<&Item> {
        let node = self.get_from_context(q, packctx)?;
        let node_w = self.value(node)?;

        Some(node_w)
    }

    fn get_all_targets_edge<'env, 'k>(
        &'env self,
        k: &'k SimpleQuant,
    ) -> impl Iterator<Item = NodeIndex> + use<'env, 'k> {
        let edges = self.graph.edge_references().filter(|x| x.weight().is(k));
        edges.map(|x| x.target())
    }

    /// Optionally returns a vector of the possible paths to an item fragment.
    fn search_for_edge(&self, k: &SimpleQuant) -> Option<Vec<Vec<SimpleQuant>>> {
        use petgraph::algo::*;
        use petgraph::prelude::*;
        let mut paths: Vec<Vec<SimpleQuant>> = vec![];
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
    fn get<'graph>(&'graph self, q: &[SimpleQuant]) -> Option<NodeIndex> {
        //let _ = self.graph.edges(self.root).map(|x| dbg!(x));
        struct Rec<'s, 'graph, T> {
            f: &'s dyn Fn(
                &Rec<'s, 'graph, T>,
                &'graph T,
                NodeIndex,
                &[SimpleQuant],
            ) -> Option<NodeIndex>,
        }
        let rec = Rec {
            f: &|rec: &Rec<'_, 'graph, _>,
                 graph_self: &'graph Self,
                 n: NodeIndex,
                 q: &[SimpleQuant]|
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

    pub fn check(&self) -> CompResult<()> {
        let main_idx = self
            .get_from_context(
                &SimpleQuant::Func("main".to_string()),
                &SimpleQuant::Package("Main".to_string()),
            )
            //.get(&quantifier!(Root, Package("Main"), Func("main"), End).into_simple())
            .unwrap();
        let main_item = self.graph.node_weight(main_idx).unwrap();
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

        self.check_item(main_item, &SimpleQuant::Package("Main".to_string()))?;
        //dbg!(&main);
        Ok(())
    }

    pub fn check_item<'e>(&self, item: &'e Item, packctx: &SimpleQuant) -> CompResult<&'e Item> {
        //println!("Checking {:?}", entry);
        //match *entry.borrow_mut() {

        if let Item::Let {
            ref name,
            ref sig,
            ref body,
            ..
        } = if item.get_sig().is_none() {
            item
        } else {
            return Ok(item);
        } {
            let mut tc = Solver::new(self, packctx.clone());
            let tv = tc.check_expr(body)?;
            let fn_sig = tc.solve(tv)?.clone();
            //dbg!(&fn_sig);
            let _ = sig.set((
                fn_sig.0,
                SimpleSpan::new(name.1.context, name.1.into_range()),
            ));
        }
        info!("Checked {}: {}", item.name(), item.get_ty().unwrap().0);
        Ok(item)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        passes::midend::environment::Environment,
        quantifier,
        resource::rep::{
            entry::Item,
            quantifier::{Quantifier, SimpleQuant},
        },
    };

    use petgraph::prelude::*;

    fn make_graph() -> Environment {
        let mut graph: DiGraph<Item, SimpleQuant> = DiGraph::new();
        let root = graph.add_node(Item::Root);
        let lib_foo = graph.add_node(Item::Dummy("libFoo".to_string()));
        let foo = graph.add_node(Item::Dummy("foo".to_string()));
        let lib_bar = graph.add_node(Item::Dummy("libBar".to_string()));
        let bar = graph.add_node(Item::Dummy("Bar".to_string()));
        let baz = graph.add_node(Item::Dummy("baz".to_string()));
        let bar_foo = graph.add_node(Item::Dummy("fooooo".to_string()));

        graph.extend_with_edges(&[
            (root, lib_foo, SimpleQuant::Package(String::from("Foo"))),
            (lib_foo, foo, SimpleQuant::Func(String::from("foo"))),
            (root, lib_bar, SimpleQuant::Package(String::from("Bar"))),
            (lib_bar, bar, SimpleQuant::Type(String::from("Bar"))),
            (bar, baz, SimpleQuant::Field(String::from("f1"))),
            (lib_bar, bar_foo, SimpleQuant::Func(String::from("foo"))),
        ]);

        Environment { graph, root }
    }

    #[test]
    fn exists() {
        let e = make_graph();
        assert!(e
            .get(&quantifier!(Root, Package("Foo"), Func("foo"), End).into_simple())
            .is_some())
    }

    #[test]
    fn get_node_exists() {
        let e = make_graph();
        let search1 = e.get_node(
            &SimpleQuant::Func("foo".to_string()),
            &SimpleQuant::Package("Foo".to_string()),
        );
        let search2 = e.get_node(
            &SimpleQuant::Type("Bar".to_string()),
            &SimpleQuant::Package("Bar".to_string()),
        );

        assert!(search1.is_some());
        assert!(search2.is_some());
    }

    #[test]
    fn get_node_dne() {
        let e = make_graph();
        let found = e.get_node(
            &SimpleQuant::Func("Bar".to_string()),
            &SimpleQuant::Package("libFoo".to_string()),
        );
        assert!(found.is_none())
    }

    #[test]
    fn get_paths() {
        let e = make_graph();
        let res = e.search_for_edge(&SimpleQuant::Func("foo".to_string()));
        assert_eq!(
            res,
            Some(vec![
                vec![
                    SimpleQuant::Package("Foo".to_string()),
                    SimpleQuant::Func("foo".to_string())
                ],
                vec![
                    SimpleQuant::Package("Bar".to_string()),
                    SimpleQuant::Func("foo".to_string())
                ]
            ])
        );
    }
}
