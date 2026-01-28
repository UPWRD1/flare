use core::fmt;

use itertools::Itertools;
use petgraph::{dot::Config, prelude::*};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::passes::backend::{
    lowering::{
        ir::{Branch, IR, ItemId, Kind, TyApp, Type},
        lower_ast::ItemSupply,
    },
    simplify,
};

// How to Monomorph
// Guarantees:
// 1. The last function is always main().
// 2. main() never takes any type parameters.
// 3. Therefore, start from the back.
//
// Algorithm:
// 1. Read the IR:
// 2. If we find a tyapp chain, fold all the tyapps into a new Monomorph
// 3. If the mononomorph is cached:
//     - use the cached ID
//     - else
//         1. recursively solve the new monomorph
//         2. generate and use a new ID
// 4. Replace the application chain with the id

pub fn monomorph(the_ir: Vec<IR>) -> Vec<IR> {
    // the_ir
    // dbg!(the_ir.last().unwrap().who_do_i_call());

    let main_id = the_ir.len() as u32 - 1;
    let mut m = Monomorpher::new(the_ir.into_iter(), main_id);

    let init_monomorph = Monomorph {
        ref_item: ItemId(main_id),
        apps: vec![].leak(),
    };
    m.solve_monomorph(&init_monomorph);
    m.debug_graph();
    // printn!("{res}");
    // todo!()
    // the_ir
    let res: Vec<_> = m
        .ref_ir
        .into_iter()
        .sorted_by_key(|(id, _)| *id)
        .map(|(_, ir)| ir)
        .collect();
    // the_ir;
    res
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
struct Monomorph {
    ref_item: ItemId,
    apps: &'static [TyApp],
    // output: IR,
}

impl fmt::Display for Monomorph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}[{}]", self.ref_item.0, self.apps.iter().join(", "))
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Replacement {
    ref_item: ItemId,
    apps: &'static [TyApp],
    replacement: ItemId,
    // output: IR,
}

#[derive(Default)]
struct Monomorpher {
    next_id: u32,
    main_id: u32,
    ref_ir: FxHashMap<ItemId, IR>,
    graph: DiGraph<Monomorph, ()>,
    graph_cache: FxHashMap<Monomorph, NodeIndex>,
}

impl Monomorpher {
    fn new(ref_ir: impl Iterator<Item = IR>, main_id: u32) -> Self {
        Self {
            ref_ir: ref_ir
                .enumerate()
                .map(|(i, ir)| (ItemId(i as u32), ir))
                .collect(),
            next_id: main_id,
            main_id,
            ..Default::default()
        }
    }

    fn new_id(&mut self) -> ItemId {
        let n = self.next_id;
        self.next_id += 1;
        ItemId(n)
    }

    #[allow(dead_code, clippy::unwrap_used, clippy::dbg_macro)]
    #[deprecated]
    pub fn debug_graph(&self) {
        let render = |_, v: (_, &Monomorph)| format!("label = \"{}\"", v.1);

        let dot = petgraph::dot::Dot::with_attr_getters(
            &self.graph,
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

    fn get_or_insert(&mut self, mono: &Monomorph) -> NodeIndex {
        if let Some(x) = self.graph_cache.get(mono) {
            *x
        } else {
            let idx = self.graph.add_node(*mono);
            self.graph_cache.insert(*mono, idx);
            idx
        }
    }

    fn solve_monomorph(&mut self, mono: &Monomorph) {
        let ir = self.ref_ir.get(&mono.ref_item).expect("IR should exist");
        let sub_morphs = self.collect_needed_morphs(ir, mono);

        let parent_node = self.get_or_insert(mono);
        dbg!(&sub_morphs);
        for sub_morph in sub_morphs {
            let sub_morph_node = self.get_or_insert(&sub_morph);
            self.graph.add_edge(parent_node, sub_morph_node, ());
            self.solve_monomorph(&sub_morph);
        }
    }

    fn collect_needed_morphs(&self, ir: &IR, parent_mono: &Monomorph) -> FxHashSet<Monomorph> {
        let mut result = FxHashSet::default();
        let mut iter = ir.iter();

        while let Some(node) = iter.next() {
            if let IR::TyApp(_, _) = node {
                let mut apps = vec![];
                let mut sub_iter = node.iter();

                // Collect all consecutive TyApp nodes
                for sub_node in sub_iter.by_ref() {
                    match sub_node {
                        IR::TyApp(_, app) => {
                            let app = match app {
                                TyApp::Ty(Type::Var(v)) => parent_mono.apps[v.0].clone(),
                                _ => app.clone(),
                            };
                            apps.push(app);
                            iter.next();
                        }
                        IR::Item(_, id) => {
                            result.insert(Monomorph {
                                ref_item: *id,
                                apps: apps.leak(),
                            });
                            break;
                        }
                        _ => unreachable!("Monomorph was not on an item: {sub_node:?}"),
                    }
                }
            }
        }
        result
    }
}
