use core::fmt;

use itertools::Itertools;
use petgraph::{dot::Config, prelude::*};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    passes::midend::simplify,
    resource::rep::midend::{
        ir::{Branch, IR, ItemId},
        irtype::{IRType, TyApp},
    },
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
    let main_id = the_ir.len() as u32 - 1;
    let mut m = Monomorpher::new(the_ir.into_iter(), main_id);

    let init_monomorph = Monomorph {
        ref_item: ItemId(main_id),
        apps: Vec::new().leak(),
    };
    m.solve_monomorph(&init_monomorph);
    // m.debug_graph();
    let Some(main_idx) = m.graph_cache.get(&init_monomorph) else {
        unreachable!("Main was not added to cache");
    };
    m.generate_irs(*main_idx)
    // the_ir
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
struct Monomorph {
    ref_item: ItemId,
    apps: &'static [TyApp],
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

impl fmt::Display for Replacement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "#{}[{}] ==> #{}",
            self.ref_item.0,
            self.apps.iter().join(", "),
            self.replacement.0,
        )
    }
}

#[derive(Default)]
struct Monomorpher {
    ref_ir: FxHashMap<ItemId, IR>,
    graph: DiGraph<Monomorph, ()>,
    graph_cache: FxHashMap<Monomorph, NodeIndex>,
    mono_ids_to_ty: FxHashMap<Monomorph, ItemId>,
}

impl Monomorpher {
    fn new(ref_ir: impl Iterator<Item = IR>, main_id: u32) -> Self {
        Self {
            ref_ir: ref_ir
                .enumerate()
                .map(|(i, ir)| (ItemId(i as u32), ir))
                .collect(),

            ..Default::default()
        }
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
        // dbg!(ir);
        let sub_morphs = self.collect_needed_morphs(ir, mono);

        let raw_calls: Vec<_> = ir
            .who_do_i_call()
            .into_iter()
            .filter(|id| !sub_morphs.iter().any(|mono| mono.ref_item == *id))
            .collect();

        let parent_node = self.get_or_insert(mono);

        for ref_item in raw_calls.iter().rev() {
            let imaginary_morph = Monomorph {
                ref_item: *ref_item,
                apps: Vec::new().leak(),
            };

            let imaginary_morph_node = self.get_or_insert(&imaginary_morph);
            self.graph.add_edge(parent_node, imaginary_morph_node, ());
            self.solve_monomorph(&imaginary_morph);
        }

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
                                TyApp::Ty(IRType::Var(v)) => {
                                    parent_mono.apps.get(v.0).cloned().unwrap_or(app.clone())
                                }
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
                        _ => continue,
                        // _ => unreachable!("Monomorph was not on an item: {sub_node:?}"),
                    }
                }
            }
        }
        result
    }

    fn generate_irs(&mut self, main_idx: NodeIndex) -> Vec<IR> {
        let mut dfs = DfsPostOrder::new(&self.graph, main_idx);
        let mut new_irs: Vec<IR> = Vec::with_capacity(self.graph_cache.len() * 2);
        let mut id = 0;
        let mut replacements: Vec<Replacement> = Vec::with_capacity(self.graph_cache.len());
        while let Some(n) = dfs.next(&self.graph) {
            let mono = self.graph.node_weight(n).expect("Monomorph should exist");
            let replacement = Replacement {
                ref_item: mono.ref_item,
                apps: mono.apps,
                replacement: ItemId(id),
            };
            replacements.push(replacement);
            let un_monomorphed_ir = self.ref_ir[&mono.ref_item].clone();
            let morphed_ir = self.instantiate_monomorph(un_monomorphed_ir, *mono);
            // println!("{morphed_ir}\n");

            self.mono_ids_to_ty.insert(*mono, ItemId(id));
            new_irs.push(morphed_ir);
            id += 1;
        }

        new_irs
            .into_iter()
            .map(|ir| {
                let mut ir = ir;
                let needs_updating = ir.who_do_i_call();
                for replacement in replacements
                    .iter()
                    .filter(|rep| needs_updating.contains(&rep.ref_item))
                {
                    // dbg!(replacement);
                    // ir = ir
                    ir = self.instantiate_replacement(ir, *replacement)
                }
                ir
            })
            .collect()
    }

    fn instantiate_monomorph(&self, ir: IR, morph: Monomorph) -> IR {
        match ir {
            IR::TyFun(k, body) => self.instantiate_monomorph(*body, morph),
            IR::TyApp(body, app) => {
                // dbg!(&t);
                let new_app = match app {
                    TyApp::Ty(IRType::Var(v)) => morph.apps[v.0].clone(),
                    _ => app,
                };
                let body = match new_app {
                    TyApp::Ty(ref t) => simplify::subst_ty(*body, t.clone()),
                    TyApp::Row(ref row) => simplify::subst_row(*body, row.clone()),
                };

                IR::ty_app(self.instantiate_monomorph(body, morph), new_app)
            }
            IR::Local(v, d, b) => {
                let v = v.map_ty(|f| {
                    morph
                        .apps
                        .iter()
                        .fold(f, |ty, tyapp| ty.subst_app(tyapp.clone()))
                });
                let defn = self.instantiate_monomorph(*d, morph);

                let body = self.instantiate_monomorph(*b, morph);
                IR::local(v, defn, body)
            }
            IR::App(l, r) => IR::app(
                self.instantiate_monomorph(*l, morph),
                self.instantiate_monomorph(*r, morph),
            ),
            IR::Fun(v, body) => IR::fun(
                v.map_ty(|f| {
                    morph
                        .apps
                        .iter()
                        .fold(f, |ty, tyapp| ty.subst_app(tyapp.clone()))
                }),
                self.instantiate_monomorph(*body, morph),
            ),
            IR::Tuple(v) => IR::Tuple(
                v.into_iter()
                    .map(|ir| self.instantiate_monomorph(ir, morph))
                    .collect(),
            ),
            IR::Case(t, ir, b) => {
                let t = morph
                    .apps
                    .iter()
                    .fold(t, |ty, tyapp| ty.subst_app(tyapp.clone()));
                IR::case(
                    t,
                    self.instantiate_monomorph(*ir, morph),
                    b.into_iter().map(|b| Branch {
                        param: b.param.map_ty(|t| {
                            morph
                                .apps
                                .iter()
                                .fold(t, |ty, tyapp| ty.subst_app(tyapp.clone()))
                        }),
                        body: self.instantiate_monomorph(b.body, morph),
                    }),
                )
            }
            IR::Field(ir, u) => IR::field(self.instantiate_monomorph(*ir, morph), u),
            IR::Tag(t, u, ir) => IR::tag(
                morph
                    .apps
                    .iter()
                    .fold(t, |ty, tyapp| ty.subst_app(tyapp.clone())),
                u,
                self.instantiate_monomorph(*ir, morph),
            ),
            IR::Var(v) => IR::Var(v.map_ty(|t| {
                morph
                    .apps
                    .iter()
                    .fold(t.clone(), |ty, tyapp| ty.subst_app(tyapp.clone()))
            })),
            IR::Item(t, id) => {
                let new_t = morph
                    .apps
                    .iter()
                    .fold(t, |ty, tyapp| ty.subst_app(tyapp.clone()));
                IR::Item(new_t, id)
            }
            IR::Extern(n, t) => {
                let t = morph
                    .apps
                    .iter()
                    .fold(t.clone(), |ty, tyapp| ty.subst_app(tyapp.clone()));
                IR::Extern(n, t)
            }
            IR::Num(_) | IR::Str(_) | IR::Bool(_) | IR::Unit | IR::Particle(_) => ir,

            IR::If(ir, ir1, ir2) => todo!(),
            IR::Bin(l, op, r) => IR::bin(
                self.instantiate_monomorph(*l, morph),
                op,
                self.instantiate_monomorph(*r, morph),
            ),
        }
    }

    fn instantiate_replacement(&self, ir: IR, replacement: Replacement) -> IR {
        match ir {
            IR::TyFun(k, body) => self.instantiate_replacement(*body, replacement),
            IR::TyApp(body, app) => {
                fn probe_item(ir: &IR, app_accum: Vec<TyApp>) -> Option<(Monomorph, IRType)> {
                    // dbg!(ir);
                    match ir {
                        IR::TyApp(ir, t) => probe_item(ir, [app_accum, vec![t.clone()]].concat()),
                        IR::Item(t, item_id) => Some((
                            Monomorph {
                                ref_item: *item_id,
                                apps: app_accum.leak(),
                            },
                            t.clone(),
                        )),
                        _ => None,
                    }
                }

                if let Some((morph, og_ty)) = probe_item(&body, vec![app.clone()]) {
                    if morph.ref_item == replacement.ref_item && morph.apps == replacement.apps {
                        // dbg!(&og_ty);
                        let new_ty = replacement
                            .apps
                            .iter()
                            .fold(og_ty.clone(), |ty, tyapp| ty.subst_app_final(tyapp.clone()));

                        IR::Item(new_ty, replacement.replacement)
                    } else {
                        //This is not the correct item to replace
                        IR::TyApp(body, app)
                    }
                } else {
                    // The types should have been applied in monomorphing
                    println!("here! {body}");
                    self.instantiate_replacement(*body, replacement)
                }
            }
            IR::Local(v, d, b) => {
                let defn = self.instantiate_replacement(*d, replacement);

                let body = self.instantiate_replacement(*b, replacement);
                IR::local(v, defn, body)
            }
            IR::App(l, r) => IR::app(
                self.instantiate_replacement(*l, replacement),
                self.instantiate_replacement(*r, replacement),
            ),
            IR::Fun(v, body) => IR::fun(v, self.instantiate_replacement(*body, replacement)),

            IR::Tuple(v) => IR::tuple(
                v.into_iter()
                    .map(|ir| self.instantiate_replacement(ir, replacement)),
            ),
            IR::Case(t, ir, b) => IR::case(
                t,
                self.instantiate_replacement(*ir, replacement),
                b.into_iter().map(|b| Branch {
                    param: b.param,
                    body: self.instantiate_replacement(b.body, replacement),
                }),
            ),
            IR::Field(ir, u) => IR::field(self.instantiate_replacement(*ir, replacement), u),
            IR::Tag(t, u, ir) => IR::tag(t, u, self.instantiate_replacement(*ir, replacement)),

            IR::Item(ref t, id) => {
                if id == replacement.ref_item {
                    // Recursive item call
                    let t = replacement
                        .apps
                        .iter()
                        .fold(t.clone(), |ty, tyapp| ty.subst_app(tyapp.clone()));
                    IR::Item(t, replacement.replacement)
                } else {
                    ir
                }
            }
            // IR::Num(_) | IR::Str(_) | IR::Bool(_) | IR::Unit | IR::Particle(_) => ir,
            _ => ir,
            // IR::If(ir, ir1, ir2) => todo!(),
            // IR::Bin(ir, bin_op, ir1) => todo!(),
            // IR::Extern(intern, _) => todo!(),
        }
    }
}
