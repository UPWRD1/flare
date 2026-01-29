use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
};

use internment::Intern;
use itertools::Itertools;
use ordered_float::OrderedFloat;
use rustc_hash::{FxBuildHasher, FxHashMap};
use tiny_pretty::Doc;

use crate::{
    passes::backend::lowering::ir::ItemId,
    resource::pretty::{DocExt, Render},
};
use crate::{
    passes::backend::{
        lowering::{
            ir::{self, Param},
            lower_ast::{ItemSupply, VarSupply},
        },
        target::Target,
    },
    resource::rep::ast::BinOp,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Float,
    String,
    Unit,
    Array(Vec<Self>),
    Union(Vec<Self>),
    Closure(Box<Self>, Box<Self>),
    ClosureEnv(Box<Self>, Vec<Self>),
}

impl Render for Type {
    fn render(self) -> Doc<'static> {
        // dbg!(&self);
        match self {
            Self::Int => Doc::text("i32"),
            Self::Float => Doc::text("f64"),
            Self::String => Doc::text("str"),
            Self::Unit => Doc::text("unit"),
            Self::Closure(l, r) => l.render().space().text("->").space().render(*r).brackets(),
            Self::ClosureEnv(c, params) => Doc::text("code:")
                .space()
                .hard_line()
                .render(*c)
                .nest(2)
                .hard_line()
                .text("env:")
                .space()
                .hard_line()
                .append(
                    Doc::list(
                        params
                            .into_iter()
                            .map(Render::render)
                            .intersperse(Doc::text(", "))
                            .collect(),
                    )
                    .brackets()
                    .nest(2),
                ),
            Self::Array(v) => Doc::list(
                v.into_iter()
                    .map(Render::render)
                    .intersperse(Doc::text(","))
                    .collect(),
            )
            .braces(),

            Self::Union(v) => Doc::list(
                v.into_iter()
                    .map(Render::render)
                    .intersperse(Doc::text("|"))
                    .collect(),
            )
            .braces(),
        }
    }
}

impl Type {
    fn closure(l: Self, r: Self) -> Self {
        Self::Closure(Box::new(l), Box::new(r))
    }

    fn closure_env(l: Self, r: Vec<Self>) -> Self {
        Self::ClosureEnv(Box::new(l), r)
    }
}

#[derive(Clone, Copy)]
pub struct LIRTarget;

impl Display for LIR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let doc = self.clone().render();
        write!(
            f,
            "{}",
            tiny_pretty::print(
                &doc,
                &tiny_pretty::PrintOptions {
                    width: 80,
                    ..Default::default()
                }
            )
        )
    }
}

impl LIRTarget {
    fn render_item(&self, item: Item) -> String {
        format!(
            "({}) {{\n\t{}\n}}\n",
            item.params
                .iter()
                .map(|x| format!(
                    "${}: {}",
                    x.id.0,
                    tiny_pretty::print(
                        &x.ty.clone().render(),
                        &tiny_pretty::PrintOptions {
                            width: 80,
                            ..Default::default()
                        }
                    )
                ))
                .join(","),
            tiny_pretty::print(
                &item.body.render(),
                &tiny_pretty::PrintOptions {
                    width: 80,
                    ..Default::default()
                }
            ),
        )
    }

    fn render_closures(&self, closures: BTreeMap<ItemId, Item>) -> String {
        closures
            .into_iter()
            .map(|(i, x)| {
                let item_body = self.render_item(x);
                format!("fn {}{item_body}", i.0)
            })
            .join("n")
    }
}

impl Target for LIRTarget {
    type Partial = String;

    type Output = String;
    type Input = ClosureConvertOut;

    fn generate(&mut self, ir: Self::Input) -> Self::Partial {
        let closures = self.render_closures(ir.closure_items);
        let main_body = self.render_item(ir.item);

        format!("closures = {closures}\n{main_body}")
    }

    fn finish(&self, p: Vec<Self::Partial>) -> Self::Output {
        p.into_iter()
            // .enumerate()
            // .map(|(i, x)| format!("{x}"))
            // .collect::<Vec<String>>()
            .join("\n\n")
    }
    fn ext(&self) -> &str {
        "lir"
    }

    fn convert(&self, ir: Vec<ir::IR>) -> Vec<Self::Input> {
        // let ir = ;
        closure_convert(ir)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum LIR {
    Var(Var),
    Int(i32),
    Str(Intern<String>),
    #[default]
    Unit,
    Float(OrderedFloat<f64>),
    Closure(Type, ir::ItemId, Vec<Var>),
    Apply(Box<Self>, Box<Self>),
    Local(Var, Box<Self>, Box<Self>),
    Access(Box<Self>, usize),
    Array(Vec<Self>),
    Index(Box<Self>, usize),
    Item(ir::ItemId),
    BinOp(Box<Self>, BinOp, Box<Self>),
}

impl LIR {
    fn local(v: Var, defn: Self, body: Self) -> Self {
        Self::Local(v, Box::new(defn), Box::new(body))
    }

    fn apply(l: Self, r: Self) -> Self {
        Self::Apply(Box::new(l), Box::new(r))
    }

    fn access(v: Self, u: usize) -> Self {
        Self::Access(Box::new(v), u)
    }

    fn index(v: Self, u: usize) -> Self {
        Self::Index(Box::new(v), u)
    }
    fn binop(l: Self, op: BinOp, r: Self) -> Self {
        Self::BinOp(Box::new(l), op, Box::new(r))
    }

    fn free_vars_aux(&self, free: &mut BTreeSet<Var>) {
        match self {
            Self::Var(var) => {
                free.insert(var.clone());
            }
            Self::Int(_) | Self::Float(_) | Self::Unit | Self::Str(_) => {}
            Self::Closure(_, _, vars) => {
                for var in vars {
                    free.insert(var.clone());
                }
            }
            Self::Apply(fun, arg) => {
                fun.free_vars_aux(free);
                arg.free_vars_aux(free);
            }
            Self::Local(var, defn, body) => {
                body.free_vars_aux(free);
                defn.free_vars_aux(free);
                free.remove(var);
            }
            Self::Access(ir, _) | Self::Index(ir, _) => ir.free_vars_aux(free),
            Self::Array(v) => v.iter().for_each(|ir| ir.free_vars_aux(free)),
            Self::Item(d) => {}
            Self::BinOp(l, _, r) => {
                l.free_vars_aux(free);
                r.free_vars_aux(free);
            }
        }
    }

    fn free_vars(&self) -> BTreeSet<Var> {
        let mut free = BTreeSet::default();
        self.free_vars_aux(&mut free);
        free
    }

    fn rename(&mut self, subst: &FxHashMap<Var, Var>) {
        match self {
            Self::Var(var) => {
                if let Some(new_var) = subst.get(var) {
                    *var = new_var.clone();
                }
            }
            Self::Int(_) | Self::Float(_) | Self::Unit | Self::Str(_) | Self::Item(_) => {}
            Self::Closure(_, _, vars) => {
                for var in vars.iter_mut() {
                    if let Some(new_var) = subst.get(var) {
                        *var = new_var.clone();
                    }
                }
            }
            Self::Apply(fun, arg) => {
                fun.rename(subst);
                arg.rename(subst);
            }
            Self::Local(_, defn, body) => {
                defn.rename(subst);
                body.rename(subst);
            }
            Self::Access(body, _) | Self::Index(body, _) => body.rename(subst),
            Self::Array(v) => v.iter_mut().for_each(|ir| ir.rename(subst)),
            Self::BinOp(l, _, r) => {
                l.rename(subst);
                r.rename(subst);
            }
        }
    }
}

impl Render for LIR {
    fn render(self) -> Doc<'static> {
        match self {
            Self::Var(var) => Doc::text(format!("${}", var.id.0)),
            Self::Int(i) => Doc::text(format!("{i}i")),
            Self::Str(s) => Doc::text(format!("{s}")),
            Self::Unit => Doc::text("unit".to_string()),
            Self::Float(f) => Doc::text(format!("{f}f")),
            Self::Closure(t, item_id, vars) => Doc::text("closure")
                .space()
                .text(format!("#{}", item_id.0))
                .append(
                    Doc::list(
                        vars.iter()
                            .map(|x| {
                                Doc::text(format!("${}: ", x.id.0,)).append(x.ty.clone().render())
                            })
                            .intersperse(Doc::text(", "))
                            .collect(),
                    )
                    .brackets(),
                ),
            Self::Apply(ir, ir1) => ir.render().append(ir1.render().parens()),
            Self::Local(var, d, b) => Doc::text("let")
                .space()
                .render(var)
                .space()
                .text("=")
                .space()
                .render(*d)
                .text(";")
                .hard_line()
                .render(*b),
            Self::Access(ir, i) => ir.render().text(format!("^{i}")),
            Self::Array(v) => Doc::list(
                v.into_iter()
                    .map(Render::render)
                    .intersperse(Doc::text(","))
                    .collect(),
            )
            .braces(),
            Self::Index(ir, u) => ir.render().append(Doc::text(u.to_string()).brackets()),
            Self::Item(id) => Doc::text(format!("#{}", id.0)),
            Self::BinOp(l, op, r) => l.render().space().text(format!("{op}")).space().render(*r),
        }
    }
}
#[derive(Debug)]
pub struct Item {
    pub params: Vec<Var>,
    pub ret_ty: Type,
    pub body: LIR,
}

#[derive(Debug)]
pub struct ClosureConvertOut {
    pub item: Item,
    pub closure_items: BTreeMap<ir::ItemId, Item>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub id: ir::VarId,
    pub ty: Type,
}

impl Render for Var {
    fn render(self) -> Doc<'static> {
        Doc::text(format!("${}", self.id.0))
    }
}

impl PartialOrd for Var {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Var {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

struct ClosureConvert {
    var_supply: VarSupply<ir::VarId>,
    item_supply: ItemSupply,
    items: BTreeMap<ir::ItemId, Item>,
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
            ir::IR::Var(var) => LIR::Var(env[&var].clone()),

            ir::IR::Local(var, defn, body) => {
                let defn = self.convert(*defn, env.clone());
                let v = Var {
                    id: self.var_supply.supply_for(var.id),
                    ty: lower_ty(&var.ty),
                };
                let body = self.convert(*body, env.update(var, v.clone()));
                LIR::local(v, defn, body)
            }
            ir::IR::Fun(fun_var, body) => {
                let var = Var {
                    id: self.var_supply.supply_for(fun_var.id),
                    ty: lower_ty(&fun_var.ty),
                };

                self.make_closure(var.clone(), *body, env.update(fun_var, var))
            }
            ir::IR::App(fun, arg) => {
                let closure = self.convert(*fun, env.clone());
                let arg = self.convert(*arg, env);
                LIR::apply(closure, arg)
            }

            ir::IR::TyFun(..) | ir::IR::TyApp(..) => {
                unreachable!("Generics appeared after monomorphization: {ir}")
            }
            ir::IR::Tuple(v) => LIR::Array(
                v.into_iter()
                    .map(|v| self.convert(v, env.clone()))
                    .collect(),
            ),

            ir::IR::Tag(t, u, ir) => self.convert(*ir, env),
            ir::IR::Item(t, d) => LIR::Item(d),
            ir::IR::Field(ir, u) => LIR::index(self.convert(*ir, env), u),
            ir::IR::Bin(l, op, r) => {
                LIR::binop(self.convert(*l, env.clone()), op, self.convert(*r, env))
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

        let vars: Vec<Var> = free_vars.iter().cloned().collect();
        let closure_ty = Type::closure(var.ty.clone(), ret.clone());
        let env_var = Var {
            id: self.var_supply.supply(),
            ty: Type::closure_env(
                closure_ty.clone(),
                vars.iter().map(|var| var.ty.clone()).collect(),
            ),
        };

        let subst = free_vars
            .into_iter()
            .enumerate()
            .map(|(i, var)| {
                let id = self.var_supply.supply();
                let new_var = Var {
                    id,
                    ty: var.ty.clone(),
                };
                body = LIR::local(
                    new_var.clone(),
                    LIR::access(LIR::Var(env_var.clone()), i + 1),
                    body.clone(),
                );
                (var, new_var)
            })
            .collect::<FxHashMap<_, _>>();
        body.rename(&subst);

        let params = vec![env_var, var];
        let item = self.item_supply.supply();
        self.items.insert(
            item,
            Item {
                params,
                ret_ty: ret,
                body,
            },
        );
        LIR::Closure(closure_ty, item, vars)
    }
}

pub fn closure_convert(ir: Vec<ir::IR>) -> Vec<ClosureConvertOut> {
    ir.into_iter().map(convert).collect()
}

fn convert(ir: ir::IR) -> ClosureConvertOut {
    let (params, ir) = ir.split_funs();
    let mut var_supply = VarSupply::default();
    let mut env = im::HashMap::with_hasher(FxBuildHasher);

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
                env.insert(lower_var, var.clone());
                var
            }
        })
        .collect();
    let ret_ty = lower_ty(&ir.type_of());

    let mut conversion = ClosureConvert {
        var_supply,
        item_supply: ItemSupply::default(),
        items: BTreeMap::default(),
    };

    let body = conversion.convert(ir, env);
    ClosureConvertOut {
        item: Item {
            params,
            ret_ty,
            body,
        },
        closure_items: conversion.items,
    }
}

fn lower_ty(ty: &ir::Type) -> Type {
    match ty {
        ir::Type::Num => Type::Float,
        &ir::Type::Str => Type::String,
        ir::Type::Unit => Type::Unit,
        ir::Type::Fun(arg, ret) => Type::closure(lower_ty(arg), lower_ty(ret)),
        ir::Type::Var(_) | ir::Type::TyFun(..) => {
            unreachable!("Type function or variable appeared after monomorphization: {ty:?}")
        }
        ir::Type::Prod(r) => Type::Array(lower_row(r)),
        ir::Type::Sum(r) => Type::Union(lower_row(r)),
        _ => todo!("{ty:?}"),
    }
}

fn lower_row(row: &ir::Row) -> Vec<Type> {
    match row {
        ir::Row::Open(type_var) => todo!(),
        ir::Row::Closed(items) => items.iter().map(lower_ty).collect(),
    }
}
