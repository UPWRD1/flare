use std::collections::{BTreeMap, VecDeque};

use internment::Intern;
use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::resource::rep::{
    backend::{
        lir::{Item, LIR, Var},
        types::LIRType,
    },
    midend::{
        ir::ItemId,
        irtype::{self, IRType},
    },
};

use crate::{
    passes::midend::lowering::lower_ast::{ItemSupply, VarSupply},
    resource::rep::midend::ir::{self, Param},
};

#[derive(Debug, Clone)]
pub struct ClosureConvertOut {
    pub item: Item,
    pub closure_items: BTreeMap<ir::ItemId, Item>,
}

struct ClosureConvert {
    var_supply: VarSupply<ir::VarId>,
    item_supply: ItemSupply<ir::ItemId>,
    items: BTreeMap<ir::ItemId, Item>,
    is_in_app: bool,
}

impl ClosureConvert {
    fn convert(&mut self, ir: ir::IR, env: &im::HashMap<ir::Var, Var, FxBuildHasher>) -> LIR {
        match ir {
            ir::IR::Num(n) => {
                // if n.fract() == 0.0 {
                //     let n = n.0 as i32;
                //     LIR::Int(n)
                // } else {
                LIR::Float(n)
                // }
            }
            ir::IR::Str(s) => LIR::Str(s),
            ir::IR::Unit => LIR::Unit,
            ir::IR::Var(var) => LIR::Var(env[&var]),
            ir::IR::Particle(p) => LIR::Str(p),

            ir::IR::Local(var, defn, body) => {
                // dbg!(&var, env);
                let defn = self.convert(*defn, env);
                let v = Var {
                    id: self.var_supply.supply_for(var.id),
                    // id: self.var_supply.supply(),
                    ty: lower_ty(&var.ty),
                };
                let body = self.convert(*body, &env.update(var, v));
                LIR::local(v, defn, body)
            }
            ir::IR::Fun(ref v, ref body) => {
                // dbg!(v);
                let mut env = env.clone();
                let mut rec_body = ir.clone();
                let mut vars = vec![];
                while let ir::IR::Fun(fun_var, b) = rec_body {
                    let var = Var {
                        id: self.var_supply.supply_for(fun_var.id),
                        ty: lower_ty(&fun_var.ty),
                    };
                    vars.push(var);
                    env = env.update(fun_var, var);
                    rec_body = *b;
                }
                // dbg!(&vars);
                self.make_closure(&vars, rec_body, &env)
            }
            ir::IR::App(fun, arg) => {
                self.is_in_app = true;
                let lir = if let ir::IR::App(_, _) = *fun {
                    let mut b = fun;
                    let arg = self.convert(*arg, env);
                    let mut args: VecDeque<_> = VecDeque::from([arg]);
                    while let ir::IR::App(f, arg) = *b {
                        args.push_front(self.convert(*arg, env));
                        b = f;
                    }

                    let closure = self.convert(*b, env);

                    LIR::BulkApply(Box::new(closure), args.into())
                } else {
                    let closure = self.convert(*fun, env);
                    // dbg!(&closure);

                    let arg = self.convert(*arg, env);
                    LIR::apply_lir(closure, arg)
                };
                self.is_in_app = false;
                lir
            }

            ir::IR::TyFun(..) | ir::IR::TyApp(..) => {
                unreachable!("Generics appeared after monomorphization: {ir}")
            }
            ir::IR::Tuple(v) => LIR::Struct(v.into_iter().map(|v| self.convert(v, env)).collect()),
            ir::IR::Tag(t, idx, ir) => LIR::tag(lower_ty(&t), idx, self.convert(*ir, env)), //TODO: special tag for unions
            ir::IR::Field(ir, u) => LIR::index(self.convert(*ir, env), u),
            ir::IR::Bin(l, op, r) => LIR::binop(self.convert(*l, env), op, self.convert(*r, env)),
            ir::IR::Case(ty, scrutinee, branches) => LIR::case(
                lower_ty(&ty),
                self.convert(*scrutinee, env),
                branches
                    .into_iter()
                    .map(|b| b.as_fun())
                    .map(|ir| self.convert(ir, env)),
            ),

            ir::IR::Item(t, d) => {
                let id = self.item_supply.supply_for(d);
                let ty = lower_ty(&t);
                let item = LIR::Item(id, ty);
                match ty {
                    LIRType::Closure(args, _) if !args.is_empty() => item,

                    LIRType::ClosureEnv(f, e)
                        if matches!(*f, LIRType::Closure(args, _) if !args.is_empty())
                            && e.is_empty() =>
                    {
                        item
                    }
                    _ => {
                        let new_ty = LIRType::closure(&[], ty);
                        let item = LIR::Item(d, new_ty);
                        LIR::BulkApply(Box::new(item), vec![])
                    }
                }

                // LIR::ClosureBuild(ty, d, vec![])
                // LIR::FuncRef(AppType::Item(self.item_supply.supply_for(d), lower_ty(&t)))

                // let item_ref =
                // LIR::FuncRef(AppType::Item(self.item_supply.supply_for(d), lower_ty(&t)));
                // if self.is_in_app {
                //     item_ref
                // } else {
                //     LIR::BulkApply(Box::new(item_ref), vec![])
                // }
            }
            ir::IR::Extern(n, t) => {
                LIR::Extern(n, lower_ty(&t))
                // LIR::FuncRef(AppType::Extern(n, lower_ty(&t)))
                // let extern_ref = LIR::FuncRef(AppType::Extern(n, lower_ty(&t)));
                // if self.is_in_app {
                //     extern_ref
                // } else {
                //     LIR::BulkApply(Box::new(extern_ref), vec![])
                // }
            }
            _ => todo!("{ir:?}"),
        }
    }

    fn make_closure(
        &mut self,
        vars: &[Var],
        body: ir::IR,
        env: &im::HashMap<ir::Var, Var, FxBuildHasher>,
    ) -> LIR {
        // dbg!(&body);
        let ret = lower_ty(&body.type_of());
        let mut body = self.convert(body, env);
        // dbg!(&body);
        // dbg!(vars);
        let mut free_vars_set = body.free_vars();
        for var in vars {
            free_vars_set.remove(var);
        }

        if free_vars_set.is_empty() {
            let var_tys: Vec<_> = vars.iter().map(|v| v.ty).collect();
            let fn_ty = LIRType::closure(&var_tys, ret);
            let params = vars.to_vec();
            let item = self.item_supply.supply();
            self.items.insert(
                item,
                Item {
                    id: item,
                    params,
                    ret_ty: ret,
                    body,
                },
            );
            LIR::Item(item, fn_ty)
        } else {
            // dbg!(&free_vars_set);

            let free_vars: Vec<Var> = free_vars_set.iter().copied().collect();
            let var_tys: Vec<_> = vars.iter().map(|v| v.ty).collect();
            let closure_ty = LIRType::closure(&var_tys, ret);
            let env_var = Var {
                id: self.var_supply.supply(),
                ty: LIRType::closure_env(closure_ty, free_vars.iter().map(|var| var.ty).collect()),
            };

            let subst = free_vars_set
                .into_iter()
                .enumerate()
                .map(|(i, var)| {
                    let id = self.var_supply.supply();
                    let new_var = Var { id, ty: var.ty };
                    body = LIR::local(new_var, LIR::access(LIR::Var(env_var), i + 1), body.clone());
                    (var, new_var)
                })
                .collect::<FxHashMap<_, _>>();
            body.rename(&subst);

            let params = [&[env_var], vars].concat();
            let item = self.item_supply.supply();
            self.items.insert(
                item,
                Item {
                    id: item,
                    params,
                    ret_ty: ret,
                    body,
                },
            );
            LIR::ClosureBuild(closure_ty, item, free_vars)
        }
    }
}

pub fn closure_convert(ir: Vec<ir::IR>) -> Vec<ClosureConvertOut> {
    let var_supply = VarSupply::default();
    // let mut env = im::HashMap::with_hasher(FxBuildHasher);
    let mut converter = ClosureConvert {
        var_supply,
        item_supply: ItemSupply::new(),
        items: BTreeMap::default(),
        is_in_app: false,
    };
    ir.into_iter()
        .enumerate()
        .map(|(idx, ir)| {
            let id = ItemId(idx as u32);
            convert(ir, id, &mut converter)
        })
        .collect()
}

fn convert(ir: ir::IR, id: ItemId, conversion: &mut ClosureConvert) -> ClosureConvertOut {
    let (params, ir) = ir.split_funs();
    let mut var_supply: VarSupply<ir::VarId> = VarSupply::default();
    let mut env = im::HashMap::with_hasher(FxBuildHasher);

    let id = conversion.item_supply.supply_for(id);
    let params: Vec<_> = params
        .into_iter()
        .map(|param| match param {
            Param::Ty(_) => unreachable!("Type function encountered after monomorphization"),
            Param::Val(lower_var) => {
                let id = var_supply.supply_for(lower_var.id);
                // let id = var_supply.supply();
                let var = Var {
                    id,
                    ty: lower_ty(&lower_var.ty),
                };
                env.insert(lower_var, var);
                var
            }
        })
        .collect();
    let ret_ty = lower_ty(&ir.type_of());
    // dbg!(ret_ty);
    let body = conversion.convert(ir, &env);
    // let ret_ty = body.type_of();
    ClosureConvertOut {
        item: Item {
            id,
            params,
            ret_ty,
            body,
        },
        closure_items: std::mem::take(&mut conversion.items),
    }
}

fn lower_ty(ty: &IRType) -> LIRType {
    match ty {
        IRType::Num => LIRType::Float,
        IRType::Str => LIRType::String,
        IRType::Unit => LIRType::Unit,
        IRType::Fun(arg, ret) => {
            // dbg!(ty);
            // todo!()
            let mut args = vec![*arg.clone()];
            let mut marg = *ret.clone();
            while let IRType::Fun(a, r) = marg {
                args.push(*a);
                marg = *r;
            }
            let ret = marg;
            // dbg!(&args, &ret);
            LIRType::closure(
                &args.iter().map(lower_ty).collect::<Vec<_>>(),
                lower_ty(&ret),
            )
        }
        IRType::Var(_) | IRType::TyFun(..) => {
            unreachable!("Type function or variable appeared after monomorphization: {ty:?}")
        }
        IRType::Prod(r) => LIRType::Struct(lower_row(r)),
        IRType::Sum(r) => LIRType::Union(lower_row(r)),
        IRType::Particle(_) => LIRType::String,
        _ => todo!("{ty:?}"),
    }
}

fn lower_row(row: &irtype::Row) -> Intern<[LIRType]> {
    match row {
        irtype::Row::Open(_type_var) => todo!(),
        irtype::Row::Closed(items) => items
            .iter()
            .map(lower_ty)
            .collect::<Vec<_>>()
            .as_slice()
            .into(),
    }
}
