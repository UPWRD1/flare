use itertools::Itertools;
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
    dbg!(the_ir.last().unwrap().who_do_i_call());

    let main_id = the_ir.len() as u32 - 1;
    let mut m = Monomorpher::new(the_ir.clone().into_iter(), main_id);

    let init_monomorph = Monomorph {
        ref_item: ItemId(main_id),
        apps: vec![].leak(),
    };
    m.solve_monomorph(&init_monomorph);
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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Monomorph {
    ref_item: ItemId,
    apps: &'static [TyApp],
    // output: IR,
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
    cache: FxHashMap<Monomorph, IR>,
    cache_ref: FxHashMap<Monomorph, IR>,
    final_ir: FxHashMap<ItemId, IR>,
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

    fn solve_monomorph(&mut self, mono: &Monomorph) {
        let ir = self.ref_ir.get(&mono.ref_item).expect("IR should exist");
        let mut sub_morphs = FxHashSet::default();
        self.collect_types(ir, &mut sub_morphs);
        dbg!(sub_morphs);

        // todo!()
    }

    fn collect_types(&self, ir: &IR, types: &mut FxHashSet<Monomorph>) {
        // dbg!(ir.who_do_i_call());
        match ir {
            IR::App(func, body) => {
                self.collect_types(func, types);
                self.collect_types(body, types);
            }
            IR::TyFun(_, ir) => self.collect_types(ir, types),
            IR::TyApp(body, app) => {
                if let Some(monomorph) = self.probe_item(body, vec![app.clone()]) {
                    // println!("monomorph added: {monomorph:?}");

                    types.insert(monomorph);
                } else {
                    unreachable!("Could not generate monomorph for {ir}")
                }

                // self.collect_types(body, types);
            }
            IR::Local(_, def, body) => {
                self.collect_types(def, types);
                self.collect_types(body, types);
            }
            IR::Tuple(irs) => {
                for elem in irs {
                    self.collect_types(elem, types);
                }
            }
            IR::Case(t, ir, branches) => {
                self.collect_types(ir, types);
                for b in branches {
                    self.collect_types(&b.body, types);
                }
            }
            IR::If(c, t, o) => {
                self.collect_types(c, types);
                self.collect_types(t, types);
                self.collect_types(o, types);
            }
            IR::Bin(l, _, r) => {
                self.collect_types(l, types);
                self.collect_types(r, types);
            }
            IR::Var(_) | IR::Num(_) | IR::Str(_) | IR::Bool(_) | IR::Particle(_) | IR::Unit => (),

            IR::Comment(_, ir) => self.collect_types(ir, types),
            IR::Fun(var, ir) => self.collect_types(ir, types),
            IR::Field(ir, _) => self.collect_types(ir, types),
            IR::Tag(_, _, ir) => self.collect_types(ir, types),
            IR::Item(_, _) | IR::Extern(_, _) => (),
        }
    }

    fn probe_item(&self, ir: &IR, mut app_accum: Vec<TyApp>) -> Option<Monomorph> {
        // dbg!(ir);
        match ir {
            IR::TyApp(ir, t) => self.probe_item(ir, [app_accum, vec![t.clone()]].concat()),
            IR::Item(_, item_id) => Some(Monomorph {
                ref_item: *item_id,
                apps: app_accum.leak(),
            }),
            // IR::Local(_, ref d, ref b) => self.probe_item(d).or_else(|| self.probe_item(b)),
            _ => None,
        }
    }

    fn instantiate_ir(&self, ir: IR, types: &[TyApp]) -> IR {
        // dbg!();
        match types {
            [] => ir,

            [ty, rest_types @ ..] => match ir {
                IR::TyFun(k, body) => {
                    let body = self.instantiate_ir(*body, rest_types);

                    match (k, ty) {
                        (Kind::Type, TyApp::Ty(t)) => simplify::subst_ty(body, t.clone()),
                        (Kind::Row, TyApp::Row(row)) => simplify::subst_row(body, row.clone()),
                        (_, _) => unreachable!("Invalid substitution: {:?} {:?}", k, ty),
                    }
                }
                IR::TyApp(body, t) => {
                    // *body
                    self.instantiate_ir(*body, rest_types)
                    // simplify::subst_ty(body, ty.clone())
                }
                IR::Local(v, d, b) => {
                    // let v = v.map_ty(|t| t.subst_app(ty.clone()));
                    let defn = self.instantiate_ir(*d, types);
                    // let v = v.map_ty(|t| defn.type_of());

                    let body = self.instantiate_ir(*b, types);
                    IR::local(v, defn, body)
                }
                IR::App(l, r) => IR::app(
                    self.instantiate_ir(*l, types),
                    self.instantiate_ir(*r, types),
                ),
                IR::Fun(v, ir) => IR::Fun(v, Box::new(self.instantiate_ir(*ir, types))),
                IR::Tuple(v) => IR::Tuple(
                    v.into_iter()
                        .map(|ir| self.instantiate_ir(ir, types))
                        .collect(),
                ),
                IR::Case(t, ir, b) => {
                    // dbg!(&ty);

                    let t = types
                        .iter()
                        .fold(t, |ty, tyapp| ty.subst_app(tyapp.clone()));
                    IR::case(
                        t,
                        // t,
                        self.instantiate_ir(*ir, types),
                        b.into_iter().map(|b| Branch {
                            param: b.param.map_ty(|t| t.subst_app(ty.clone())),
                            body: self.instantiate_ir(b.body, types),
                        }),
                    )
                }
                IR::Field(ir, u) => IR::field(self.instantiate_ir(*ir, types), u),
                IR::Tag(t, u, ir) => IR::tag(t, u, self.instantiate_ir(*ir, types)),
                // _ => self.instantiate(ir, rest_types),
                _ => ir,
            },
        }
    }

    fn instantiate_replacements(&self, ir: IR, replacement: Replacement) -> IR {
        match ir {
            IR::TyFun(k, body) => {
                let [t, rest @ ..] = replacement.apps else {
                    unreachable!("Not enough types in replacement")
                };
                let new_rep = Replacement {
                    apps: rest,
                    ..replacement
                };
                let body = self.instantiate_replacements(*body, new_rep);
                match (k, t) {
                    (Kind::Type, TyApp::Ty(t)) => simplify::subst_ty(body, t.clone()),
                    (Kind::Row, TyApp::Row(row)) => simplify::subst_row(body, row.clone()),
                    (_, _) => unreachable!("Invalid substitution: {:?} {:?}", k, t),
                }
            }
            IR::TyApp(body, t) => {
                let body = match t {
                    TyApp::Ty(ref t) => simplify::subst_ty(*body, t.clone()),
                    TyApp::Row(ref row) => simplify::subst_row(*body, row.clone()),
                };
                if let Some(monomorph) = self.probe_item(&body, vec![t.clone()]) {
                    dbg!(&monomorph, replacement);
                    if replacement.apps == monomorph.apps
                        && replacement.ref_item == monomorph.ref_item
                    {
                        match t {
                            TyApp::Ty(t) => IR::Item(t.clone(), replacement.replacement),

                            TyApp::Row(ref row) => todo!(),
                        }
                    } else {
                        body
                    }
                } else {
                    panic!("mono not found")
                    // self.instantiate_ir(*body, types)
                    // let body = self.instantiate_replacements(*body, replacement);
                    // simplify::subst_ty(body, ty.clone())
                }
            }
            IR::Local(v, d, b) => {
                // dbg!(&v);

                // let v = v;
                let defn = self.instantiate_replacements(*d, replacement);
                // let v = v.map_ty(|t| defn.type_of());

                let body = self.instantiate_replacements(*b, replacement);
                IR::local(v, defn, body)
            }
            IR::App(l, r) => IR::app(
                self.instantiate_replacements(*l, replacement),
                self.instantiate_replacements(*r, replacement),
            ),
            IR::Fun(v, ir) => IR::Fun(v, Box::new(self.instantiate_replacements(*ir, replacement))),
            IR::Tuple(v) => IR::Tuple(
                v.into_iter()
                    .map(|ir| self.instantiate_replacements(ir, replacement))
                    .collect(),
            ),
            IR::Case(t, ir, b) => {
                // dbg!(&ty);

                // let t = types
                // .iter()
                // .fold(t, |ty, tyapp| ty.subst_app(tyapp.clone()));
                IR::case(
                    // t.subst_app(ty.clone()),
                    t,
                    self.instantiate_replacements(*ir, replacement),
                    b.into_iter().map(|b| Branch {
                        param: b.param, //b.param.map_ty(|t| t.subst_app(ty.clone())),
                        body: self.instantiate_replacements(b.body, replacement),
                    }),
                )
            }
            IR::Field(ir, u) => IR::field(self.instantiate_replacements(*ir, replacement), u),
            IR::Tag(t, u, ir) => IR::tag(t, u, self.instantiate_replacements(*ir, replacement)),
            // _ => self.instantiate(ir, rest_types),
            _ => ir,
        }
    }
}
