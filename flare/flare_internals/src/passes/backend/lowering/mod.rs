use std::collections::BTreeMap;

use rustc_hash::FxHashMap;

use crate::{
    passes::{
        backend::lowering::{
            ir::{Kind, Var, IR},
            lower_ast::{LowerAst, VarSupply},
            lower_types::{AstTypeVar, LowerTypes},
        },
        midend::typing::{self, Evidence, TypeScheme, Typed},
    },
    resource::rep::{
        ast::{Expr, NodeId},
        entry::ItemKind,
    },
};

pub mod ir;
pub mod lower_ast;
pub mod lower_types;
pub mod subst;

use ir::{Type, TypeVar};

struct LoweredTyScheme {
    scheme: Type,
    lower_types: LowerTypes,
    kinds: Vec<Kind>,
    ev_to_ty: BTreeMap<Evidence, Type>,
}

fn lower_ty_scheme(scheme: typing::TypeScheme) -> LoweredTyScheme {
    let mut kinds = vec![Kind::Type; scheme.unbound_types.len() + scheme.unbound_rows.len()];
    let ty_env = scheme
        .unbound_types
        .into_iter()
        .map(AstTypeVar::Ty)
        .chain(scheme.unbound_rows.into_iter().map(AstTypeVar::Row))
        .rev()
        .enumerate()
        .map(|(i, tyvar)| {
            kinds[i] = tyvar.kind();
            (tyvar, TypeVar(i))
        })
        .collect();

    let lower_types = LowerTypes { env: ty_env };
    let lower_ty = lower_types.lower_ty(*scheme.ty);
    let mut ev_to_ty = BTreeMap::new();
    let ev_tys = scheme
        .evidence
        .into_iter()
        .map(|ev| {
            let ty = lower_types.lower_ev_ty(ev.clone());
            ev_to_ty.insert(ev, ty.clone());
            ty
        })
        .collect::<Vec<_>>();
    let evident_lower_ty = Type::funs(ev_tys, lower_ty);

    let bound_lower_ty = kinds
        .iter()
        .fold(evident_lower_ty, |ty, kind| Type::ty_fun(*kind, ty));
    LoweredTyScheme {
        scheme: bound_lower_ty,
        lower_types,
        kinds,
        ev_to_ty,
    }
}

pub struct TypesOutput {
    typed_ast: Expr<Typed>,
    scheme: TypeScheme,
    row_to_ev: FxHashMap<NodeId, Evidence>,
    branch_to_ret_ty: FxHashMap<NodeId, typing::Type>,
}

pub struct Lowerer {
    items: Vec<ItemKind<Typed>>,
}

impl Lowerer {
    fn new(items: Vec<ItemKind<Typed>>) -> Self {
        Self { items }
    }

    fn lower(&mut self) -> Vec<IR> {
        for item in &self.items {}
        todo!()
    }

    fn lower_logic(out: TypesOutput) -> (IR, Type) {
        let lowered_scheme = lower_ty_scheme(out.scheme);

        let mut supply = VarSupply::default();
        let mut params = vec![];
        let ev_to_var = lowered_scheme
            .ev_to_ty
            .into_iter()
            .map(|(ev, ty)| {
                let param = supply.supply();
                let var = Var::new(param, ty);
                params.push(var.clone());
                (ev, var)
            })
            .collect();

        let mut lower_ast = LowerAst::new(
            supply,
            lowered_scheme.lower_types,
            ev_to_var,
            out.row_to_ev,
            out.branch_to_ret_ty,
        );
        let ir = lower_ast.lower_ast(out.typed_ast);
        todo!()
    }
}
