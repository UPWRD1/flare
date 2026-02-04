use std::collections::BTreeSet;

use internment::Intern;
use ordered_float::OrderedFloat;
use rustc_hash::FxHashMap;

use crate::resource::rep::{
    backend::types::LIRType,
    frontend::ast::BinOp,
    midend::ir::{self, ItemId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var {
    pub id: ir::VarId,
    pub ty: LIRType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum LIR {
    Var(Var),
    Int(i32),
    Str(Intern<String>),
    #[default]
    Unit,
    Float(OrderedFloat<f32>),
    ClosureBuild(LIRType, ir::ItemId, Vec<Var>),
    Apply(Box<Self>, Box<Self>),
    BulkApply(Box<Self>, Vec<Self>),
    Local(Var, Box<Self>, Box<Self>),
    Access(Box<Self>, usize),
    Struct(Vec<Self>),
    Field(Box<Self>, usize),
    Case(Box<Self>, Vec<Self>),
    Item(ir::ItemId, LIRType),
    Extern(Intern<String>, LIRType),
    BinOp(Box<Self>, BinOp, Box<Self>),
}

impl LIR {
    pub fn local(v: Var, defn: Self, body: Self) -> Self {
        Self::Local(v, Box::new(defn), Box::new(body))
    }

    pub fn apply(l: Self, r: Self) -> Self {
        Self::Apply(Box::new(l), Box::new(r))
    }

    pub fn access(v: Self, u: usize) -> Self {
        Self::Access(Box::new(v), u)
    }

    pub fn index(v: Self, u: usize) -> Self {
        Self::Field(Box::new(v), u)
    }

    pub fn binop(l: Self, op: BinOp, r: Self) -> Self {
        Self::BinOp(Box::new(l), op, Box::new(r))
    }

    pub fn case(scrutinee: Self, branches: impl IntoIterator<Item = Self>) -> Self {
        Self::Case(Box::new(scrutinee), branches.into_iter().collect())
    }

    fn free_vars_aux(&self, free: &mut BTreeSet<Var>) {
        match self {
            Self::Var(var) => {
                free.insert(*var);
            }
            Self::Int(_) | Self::Float(_) | Self::Unit | Self::Str(_) => {}
            Self::ClosureBuild(_, _, vars) => {
                for var in vars {
                    free.insert(*var);
                }
            }
            Self::Apply(fun, arg) => {
                fun.free_vars_aux(free);
                arg.free_vars_aux(free);
            }
            Self::BulkApply(fun, args) => {
                fun.free_vars_aux(free);
                args.iter().for_each(|arg| arg.free_vars_aux(free));
            }
            Self::Local(var, defn, body) => {
                body.free_vars_aux(free);
                defn.free_vars_aux(free);
                free.remove(var);
            }
            Self::Access(ir, _) | Self::Field(ir, _) => ir.free_vars_aux(free),
            Self::Struct(v) => v.iter().for_each(|ir| ir.free_vars_aux(free)),
            Self::Item(..) | Self::Extern(..) => {}

            Self::BinOp(l, _, r) => {
                l.free_vars_aux(free);
                r.free_vars_aux(free);
            }
            Self::Case(scrutinee, branches) => {
                scrutinee.free_vars_aux(free);
                branches
                    .iter()
                    .for_each(|branch| branch.free_vars_aux(free));
            }
        }
    }

    pub fn free_vars(&self) -> BTreeSet<Var> {
        let mut free = BTreeSet::default();
        self.free_vars_aux(&mut free);
        free
    }

    pub fn rename(&mut self, subst: &FxHashMap<Var, Var>) {
        match self {
            Self::Var(var) => {
                if let Some(new_var) = subst.get(var) {
                    *var = *new_var;
                }
            }
            Self::Int(_)
            | Self::Float(_)
            | Self::Unit
            | Self::Str(_)
            | Self::Item(..)
            | Self::Extern(..) => {}
            Self::ClosureBuild(_, _, vars) => {
                for var in vars.iter_mut() {
                    if let Some(new_var) = subst.get(var) {
                        *var = *new_var;
                    }
                }
            }
            Self::Apply(fun, arg) => {
                fun.rename(subst);
                arg.rename(subst);
            }
            Self::BulkApply(fun, args) => {
                fun.rename(subst);
                args.iter_mut().for_each(|arg| arg.rename(subst));
            }
            Self::Local(_, defn, body) => {
                defn.rename(subst);
                body.rename(subst);
            }
            Self::Access(body, _) | Self::Field(body, _) => body.rename(subst),
            Self::Struct(v) => v.iter_mut().for_each(|ir| ir.rename(subst)),
            Self::BinOp(l, _, r) => {
                l.rename(subst);
                r.rename(subst);
            }
            Self::Case(scrutinee, branches) => {
                scrutinee.rename(subst);
                branches.iter_mut().for_each(|branch| branch.rename(subst));
            }
        }
    }

    pub fn type_of(&self) -> LIRType {
        match self {
            LIR::Var(var) => var.ty,
            LIR::Int(_) => LIRType::Int,
            LIR::Str(_) => LIRType::String,
            LIR::Unit => LIRType::Unit,
            LIR::Float(_) => LIRType::Float,
            LIR::ClosureBuild(t, _, _) => *t,
            LIR::Apply(func, arg) => func.type_of(),
            LIR::BulkApply(func, _) => func.type_of(),
            LIR::Local(.., body) => body.type_of(),
            LIR::Access(closure, t) => {
                if let LIRType::ClosureEnv(_, env) = closure.type_of() {
                    env[*t]
                } else {
                    panic!("Not a closure")
                }
            }
            LIR::Struct(lirs) => LIRType::Struct(
                lirs.iter()
                    .map(|lir| lir.type_of())
                    .collect::<Vec<_>>()
                    .as_slice()
                    .into(),
            ),
            LIR::Field(lir, idx) => {
                if let LIRType::Struct(ref fields) = lir.type_of() {
                    fields[*idx]
                } else {
                    panic!("Field expression is on non-struct element: {lir}")
                }
            }
            LIR::Case(lir, lirs) => todo!(),
            LIR::Item(_, t) => *t,
            LIR::Extern(_, t) => *t,
            LIR::BinOp(left, ..) => left.type_of(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Item {
    pub id: ItemId,
    pub params: Vec<Var>,
    pub ret_ty: LIRType,
    pub body: LIR,
}
