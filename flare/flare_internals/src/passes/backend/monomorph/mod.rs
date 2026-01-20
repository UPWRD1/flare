use crate::passes::backend::{
    lowering::ir::{IR, ItemId, TyApp},
    simplify,
};

pub fn monomorph(the_ir: Vec<IR>) -> Vec<IR> {
    let mut m = Monomorpher::new(the_ir.clone());
    for ir in the_ir {
        let mut types = vec![];
        collect_types(&ir, &mut types);
        let new_ir = m.instantiate(ir.clone(), &types);
        // println!("{}", &new_ir);
        m.new_ir.push(new_ir);
    }
    // m.new_ir
    //     .iter()
    //     .enumerate()
    //     .for_each(|(i, x)| println!("item #{i}:\n{x}"));
    // println!("{}", m.new_ir.last().unwrap());
    m.new_ir
}

fn collect_types(ir: &IR, types: &mut Vec<TyApp>) {
    // dbg!(ir);
    match ir {
        IR::App(func, body) => {
            collect_types(func, types);
            collect_types(body, types);
        }
        IR::TyFun(_, ir) => collect_types(ir, types),
        IR::TyApp(ir, ty_app) => {
            types.push(ty_app.clone());
            collect_types(ir, types);
        }
        IR::Local(_, def, body) => {
            collect_types(def, types);
            collect_types(body, types);
        }
        IR::Tuple(irs) => {
            for elem in irs {
                collect_types(elem, types);
            }
        }
        IR::Case(_, ir, branches) => {
            collect_types(ir, types);
            for b in branches {
                collect_types(&b.body, types);
            }
        }
        IR::If(c, t, o) => {
            collect_types(c, types);
            collect_types(t, types);
            collect_types(o, types);
        }
        IR::Bin(l, _, r) => {
            collect_types(l, types);
            collect_types(r, types);
        }

        // IR::Var(v) => types.push(TyApp::Ty(v.ty.clone())),

        // IR::Num(_) => types.push(TyApp::Ty(Type::Num)),
        // IR::Str(intern) => types.push(TyApp::Ty(Type::Str)),
        // IR::Bool(_) => todo!(),
        // IR::Unit => todo!(),
        // IR::Particle(intern) => todo!(),
        // IR::Fun(var, ir) => todo!(),
        // IR::If(ir, ir1, ir2) => todo!(),
        // IR::Bin(ir, bin_op, ir1) => todo!(),
        // IR::Field(ir, _) => todo!(),
        // IR::Tag(_, _, ir) => todo!(),
        // IR::Item(t, _) => types.push(TyApp::Ty(t.clone())),
        // IR::Extern(_, _) => todo!(),
        // _ => types.push(TyApp::Ty(ir.type_of())),
        _ => (),
    }
}

struct Monomorpher {
    ref_ir: Vec<IR>,
    new_ir: Vec<IR>,
    // saturated_fun_count: usize,
}
impl Monomorpher {
    const fn new(ref_ir: Vec<IR>) -> Self {
        Self {
            ref_ir,
            new_ir: vec![],
            // saturated_fun_count: 0,
        }
    }
    // fn did_work(&self) -> bool {
    //     self.saturated_fun_count > 0
    // }

    fn probe(&self, ir: &IR) -> Option<IR> {
        match *ir {
            IR::TyFun(_, _) => Some(ir.clone()),
            IR::TyApp(ref ir, _) => Some(*ir.clone()),
            IR::Item(_, item_id) => self
                .ref_ir
                .get(item_id.0 as usize)
                .and_then(|ir| self.probe(ir)),
            IR::Local(_, ref d, ref b) => self.probe(d).or_else(|| self.probe(b)),
            _ => None,
        }
    }

    fn instantiate(&mut self, ir: IR, types: &[TyApp]) -> IR {
        match types {
            [] => ir,

            [ty, rest_types @ ..] => match ir {
                IR::TyFun(_, body) => {
                    let body = self.instantiate(*body, rest_types);
                    simplify::subst_ty(body, ty.clone())
                }
                IR::TyApp(body, _) => {
                    // dbg!(a, &ty);
                    self.probe(&body).map_or_else(
                        || {
                            let body = self.instantiate(*body, rest_types);
                            simplify::subst_ty(body, ty.clone())
                        },
                        |ir| simplify::subst_ty(ir, ty.clone()),
                    )
                }
                IR::Local(v, d, b) => {
                    let defn = self.instantiate(*d, types);
                    let body = self.instantiate(*b, types);
                    IR::local(v, defn, body)
                }
                IR::Item(t, id) => {
                    let item_ir = self.ref_ir[id.0 as usize].clone();
                    let monomorphed_ir = self.instantiate(item_ir, types);
                    let t = t.subst_app(ty.clone());
                    self.new_ir.push(monomorphed_ir);
                    IR::Item(t, ItemId(self.new_ir.len() as u32))
                }
                IR::App(l, r) => IR::app(self.instantiate(*l, types), self.instantiate(*r, types)),
                // IR::Tuple()
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
