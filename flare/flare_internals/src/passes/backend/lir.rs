use std::collections::BTreeMap;

use internment::Intern;
use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::resource::rep::{
    backend::{
        lir::{AppType, Item, LIR, Var},
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
    fn convert(&mut self, ir: ir::IR, env: im::HashMap<ir::Var, Var, FxBuildHasher>) -> LIR {
        match ir {
            ir::IR::Num(n) => {
                if n.fract() == 0.0 {
                    let n = n.0 as i32;
                    LIR::Int(n)
                } else {
                    LIR::Float(n)
                }
            }
            ir::IR::Str(s) => LIR::Str(s),
            ir::IR::Unit => LIR::Unit,
            ir::IR::Var(var) => LIR::Var(env[&var]),
            ir::IR::Particle(p) => LIR::Str(p),

            ir::IR::Local(var, defn, body) => {
                let defn = self.convert(*defn, env.clone());
                let v = Var {
                    id: self.var_supply.supply_for(var.id),
                    ty: lower_ty(&var.ty),
                };
                let body = self.convert(*body, env.update(var, v));
                LIR::local(v, defn, body)
            }
            ir::IR::Fun(fun_var, body) => {
                let var = Var {
                    id: self.var_supply.supply_for(fun_var.id),
                    ty: lower_ty(&fun_var.ty),
                };
                self.make_closure(var, *body, env.update(fun_var, var))
            }
            ir::IR::App(fun, arg) => {
                self.is_in_app = true;
                let ir = if let ir::IR::App(_, _) = *fun {
                    let mut b = fun;
                    let arg = self.convert(*arg, env.clone());
                    let mut args: Vec<_> = vec![arg];
                    while let ir::IR::App(f, arg) = *b {
                        args.push(self.convert(*arg, env.clone()));
                        b = f;
                    }
                    args.reverse();

                    let closure = self.convert(*b, env.clone());

                    LIR::BulkApply(Box::new(closure), args)
                } else {
                    let closure = self.convert(*fun, env.clone());
                    // dbg!(&closure);
                    let arg = self.convert(*arg, env);
                    LIR::apply_lir(closure, arg)
                };
                self.is_in_app = false;
                ir
            }

            ir::IR::TyFun(..) | ir::IR::TyApp(..) => {
                unreachable!("Generics appeared after monomorphization: {ir}")
            }
            ir::IR::Tuple(v) => LIR::Struct(
                v.into_iter()
                    .map(|v| self.convert(v, env.clone()))
                    .collect(),
            ),

            ir::IR::Tag(t, idx, ir) => LIR::tag(lower_ty(&t), idx, self.convert(*ir, env)), //TODO: special tag for unions
            // ir::IR::Item(t, d) => LIR::Item(d, lower_ty(&t)),
            ir::IR::Field(ir, u) => LIR::index(self.convert(*ir, env), u),
            ir::IR::Bin(l, op, r) => {
                LIR::binop(self.convert(*l, env.clone()), op, self.convert(*r, env))
            }
            ir::IR::Case(ty, scrutinee, branches) => LIR::case(
                lower_ty(&ty),
                self.convert(*scrutinee, env.clone()),
                branches
                    .into_iter()
                    .map(|b| ir::IR::fun(b.param, b.body))
                    .map(|ir| self.convert(ir, env.clone())),
            ),

            ir::IR::Item(t, d) => {
                // if self.is_in_app {
                LIR::FuncRef(AppType::Item(self.item_supply.supply_for(d), lower_ty(&t)))
                // } else {
                //     LIR::BulkApply(Box::new(item_ref), vec![])
                // }
            }
            ir::IR::Extern(n, t) => {
                // if self.is_in_app {
                LIR::FuncRef(AppType::Extern(n, lower_ty(&t)))
                // } else {
                //     LIR::BulkApply(Box::new(extern_ref), vec![])
                // }
            }
            _ => todo!("{ir:?}"),
        }
    }

    fn make_closure(
        &mut self,
        var: Var,
        body: ir::IR,
        env: im::HashMap<ir::Var, Var, FxBuildHasher>,
    ) -> LIR {
        let ret = lower_ty(&body.type_of());
        let mut body = self.convert(body, env);
        let mut free_vars = body.free_vars();
        free_vars.remove(&var);

        let vars: Vec<Var> = free_vars.iter().copied().collect();
        let closure_ty = LIRType::closure(var.ty, ret);
        let env_var = Var {
            id: self.var_supply.supply(),
            ty: LIRType::closure_env(closure_ty, vars.iter().map(|var| var.ty).collect()),
        };

        let subst = free_vars
            .into_iter()
            .enumerate()
            .map(|(i, var)| {
                let id = self.var_supply.supply();
                let new_var = Var { id, ty: var.ty };
                body = LIR::local(new_var, LIR::access(LIR::Var(env_var), i), body.clone());
                (var, new_var)
            })
            .collect::<FxHashMap<_, _>>();
        body.rename(&subst);

        let params = vec![env_var, var];
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
        LIR::ClosureBuild(closure_ty, item, vars)
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
    let mut var_supply = VarSupply::default();
    let mut env = im::HashMap::with_hasher(FxBuildHasher);

    let id = conversion.item_supply.supply_for(id);
    let params: Vec<_> = params
        .into_iter()
        .map(|param| match param {
            Param::Ty(_) => unreachable!("Type function encountered after monomorphization"),
            Param::Val(lower_var) => {
                let id = var_supply.supply_for(lower_var.id);
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
    let body = conversion.convert(ir, env);
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
        IRType::Fun(arg, ret) => LIRType::closure(lower_ty(arg), lower_ty(ret)),
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
