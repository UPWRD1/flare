use rustc_hash::{FxHashMap, FxHashSet};

use crate::passes::backend::{
    lowering::ir::{Branch, IR, ItemId, TyApp, Type},
    simplify,
};

pub fn monomorph(the_ir: Vec<IR>) -> Vec<IR> {
    // the_ir
    let mut m = Monomorpher::new(the_ir.clone());
    for (dist, ir) in the_ir.into_iter().enumerate() {
        let mut types = vec![];
        m.collect_types(&ir, &mut types);

        for morph in &m.monomorph_set {
            let new_ir = m.instantiate(morph);

            let t = new_ir.type_of();
            m.new_ir.push(new_ir);
            let magic_id = m.new_ir.len() - m.monomorph_set.len() + dist;
            let id = ItemId(magic_id as u32);
            // dbg!(magic_id);
            m.replacement_set.insert(morph.clone(), (id, t));
        }

        let ir = m.instantiate_ir(ir, &types);
        m.new_ir.push(ir);
        // }
    }

    m.new_ir
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Monomorph {
    ref_item: ItemId,
    apps: Vec<TyApp>,
    // output: IR,
}

struct Monomorpher {
    ref_ir: Vec<IR>,
    new_ir: Vec<IR>,
    monomorph_set: FxHashSet<Monomorph>,
    replacement_set: FxHashMap<Monomorph, (ItemId, Type)>,
}

impl Monomorpher {
    fn new(ref_ir: Vec<IR>) -> Self {
        Self {
            ref_ir,
            new_ir: vec![],
            monomorph_set: FxHashSet::default(), // saturated_fun_count: 0,
            replacement_set: FxHashMap::default(),
        }
    }

    fn collect_types(&mut self, ir: &IR, types: &mut Vec<TyApp>) {
        // dbg!(ir);
        match ir {
            IR::App(func, body) => {
                self.collect_types(func, types);
                self.collect_types(body, types);
            }
            IR::TyFun(_, ir) => self.collect_types(ir, types),
            IR::TyApp(body, app) => {
                if let Some(monomorph) = self.probe_item(body, vec![app.clone()]) {
                    // println!("monomorph added: {monomorph:?}");
                    self.monomorph_set.insert(monomorph);
                    types.push(app.clone());
                }
                // else {
                // types.push(app.clone());
                // self.collect_types(body, types);
                // }

                types.push(app.clone());
                self.collect_types(body, types);
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
                apps: app_accum,
            }),
            // IR::Local(_, ref d, ref b) => self.probe_item(d).or_else(|| self.probe_item(b)),
            _ => None,
        }
    }

    fn instantiate(
        &self,
        // ir: IR,
        morph: &Monomorph,
    ) -> IR {
        let ir = self.ref_ir[morph.ref_item.0 as usize].clone();
        self.instantiate_ir(ir.clone(), &morph.apps)
    }

    fn instantiate_ir(&self, ir: IR, types: &[TyApp]) -> IR {
        // dbg!();
        match types {
            [] => ir,

            [ty, rest_types @ ..] => match ir {
                IR::TyFun(_, body) => {
                    let body = self.instantiate_ir(*body, rest_types);
                    simplify::subst_ty(body, ty.clone())
                }
                IR::TyApp(body, t) => {
                    if let Some(monomorph) = self.probe_item(&body, vec![t.clone()]) {
                        // dbg!(&monomorph);
                        let (id, t) = self.replacement_set.get(&monomorph).unwrap_or_else(|| {
                            panic!(
                                "{:?} was not found in replacement set {:?}",
                                monomorph, self.replacement_set
                            )
                        });
                        IR::Item(t.clone(), *id)
                    } else {
                        // *body
                        let body = self.instantiate_ir(*body, rest_types);
                        simplify::subst_ty(body, ty.clone())
                    }
                }
                IR::Local(v, d, b) => {
                    // dbg!(&v);

                    // let v = v.map_ty(|t| t.subst_app(ty.clone()));
                    let defn = self.instantiate_ir(*d, types);
                    let v = v.map_ty(|t| defn.type_of());

                    let body = self.instantiate_ir(*b, types);
                    IR::local(v, defn, body)
                }
                // IR::Item(t, id) => {
                //     // let id = self.instance_item(id, types);
                //     IR::Item(t.subst_app(ty.clone()), id)
                // }
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

        // dbg!(&types);
        // types.into_iter().fold(ir, |ir, ty| match ir {
        //     IR::TyFun(_, body) => match ty {
        //         TyApp::Ty(ty) => simplify::subst_ty(*body, ty),
        //         TyApp::Row(r) => todo!(),
        //     },
        //     IR::TyApp(body, a) => {
        //         dbg!(a, &ty);
        //         match ty {
        //             TyApp::Ty(ty) => simplify::subst_ty(*body, ty),
        //             TyApp::Row(r) => todo!(),
        //         }
        //     }
        //     _ => ir,
        // })
        // dbg!(t)
    }
}
