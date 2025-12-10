use crate::passes::backend::{
    lowering::{
        ir::{IR, TyApp, Type},
        subst::Subst,
    },
    simplify,
};

pub fn monomorph(ir: Vec<(IR, Type)>) -> Vec<(IR, Type)> {
    ir.into_iter()
        .map(|(ir, t)| {
            let mut types = vec![];
            collect_types(&ir, &mut types);
            (instantiate(ir, &types), t)
        })
        .collect()
}

fn collect_types(ir: &IR, types: &mut Vec<TyApp>) {
    // dbg!(ir);
    match ir {
        IR::App(func, body) => {
            collect_types(func, types);
            collect_types(body, types);
        }
        IR::TyFun(kind, ir) => collect_types(ir, types),
        IR::TyApp(ir, ty_app) => {
            types.push(ty_app.clone());
            collect_types(ir, types);
        }
        IR::Local(var, def, body) => {
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
        _ => (),
    }
}

fn instantiate(ir: IR, types: &[TyApp]) -> IR {
    match types {
        [] => ir,

        [ty, rest_types @ ..] => match ir {
            IR::TyFun(_, body) => {
                let body = instantiate(*body, rest_types);
                match ty {
                    TyApp::Ty(ty) => simplify::subst_ty(body, ty.clone()),
                    TyApp::Row(r) => todo!(),
                }
            }
            IR::TyApp(body, a) => {
                // dbg!(a, &ty);
                let body = instantiate(*body, rest_types);
                match ty {
                    TyApp::Ty(ty) => simplify::subst_ty(body, ty.clone()),
                    TyApp::Row(r) => todo!(),
                }
            }
            IR::Local(v, d, b) => IR::local(v, instantiate(*d, types), instantiate(*b, types)),

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
