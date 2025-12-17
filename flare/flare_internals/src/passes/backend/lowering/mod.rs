use std::collections::BTreeMap;

use rustc_hash::FxHashMap;

use crate::{
    passes::{
        backend::lowering::{
            ir::{IR, Kind, Var},
            lower_ast::{ItemSupply, LowerAst, VarSupply},
            lower_types::{AstTypeVar, LowerTypes, LoweredTyScheme},
        },
        midend::typing::{self, TypesOutput},
    },
    resource::rep::ast::{self, ItemId},
};

pub mod ir;
pub mod lower_ast;
pub mod lower_types;
pub mod subst;

use ir::{Type, TypeVar};

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
    // dbg!(scheme.ty);
    let lower_ty = lower_types.lower_ty(*scheme.ty.0);
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
#[derive(Debug)]
pub struct ItemSource {
    items: FxHashMap<ast::ItemId, Type>,
}

impl ItemSource {
    pub fn lookup_item(&self, item: ast::ItemId) -> Type {
        self.items[&item].clone()
    }
}
#[derive(Default)]
pub struct Lowerer {
    // items: Vec<TypesOutput>,
    // source: typing::ItemSource,
    item_supply: ItemSupply,
}
impl Lowerer {
    pub fn new() -> Self {
        Self {
            // items,
            item_supply: ItemSupply::default(),
        }
    }

    pub fn lower(
        mut self,
        source: typing::ItemSource,
        items: Vec<(ast::ItemId, TypesOutput)>,
    ) -> Vec<(IR, Type)> {
        // dbg!(&source);
        let source = Self::lower_item_source(source);
        items
            .iter()
            .map(|(idx, item)| self.lower_logic(&source, item, idx))
            .collect()
    }

    fn lower_item_source(items: typing::ItemSource) -> ItemSource {
        ItemSource {
            items: items
                .types
                .into_iter()
                .map(|(item_id, ty_scheme)| {
                    let LoweredTyScheme { scheme, .. } = lower_ty_scheme(ty_scheme);
                    (item_id, scheme)
                })
                .collect(),
        }
    }

    fn lower_logic(
        &mut self,
        item_source: &ItemSource,
        out: &TypesOutput,
        item_id: &ItemId,
    ) -> (IR, Type) {
        let lowered_scheme = lower_ty_scheme(out.scheme.clone());

        let mut var_supply = VarSupply::default();
        let mut params = vec![];
        let ev_to_var = lowered_scheme
            .ev_to_ty
            .into_iter()
            .map(|(ev, ty)| {
                let param = var_supply.supply();
                let var = Var::new(param, ty);
                params.push(var.clone());
                (ev, var)
            })
            .collect();
        self.item_supply.supply_for(*item_id);
        let mut lower_ast = LowerAst::new(
            var_supply,
            lowered_scheme.lower_types,
            ev_to_var,
            &out.row_to_ev,
            &out.branch_to_ret_ty,
            &out.item_wrappers,
            item_source,
            &mut self.item_supply,
        );
        let ir = lower_ast.lower_ast(out.typed_ast);
        let solved_ir = lower_ast
            .solved
            .into_iter()
            .fold(ir, |ir, (var, solved)| IR::local(var, solved, ir));
        let param_ir = params
            .into_iter()
            .rfold(solved_ir, |ir, var| IR::fun(var, ir));

        let bound_ir = lowered_scheme
            .kinds
            .into_iter()
            .fold(param_ir, |ir, kind| IR::ty_fun(kind, ir));
        (bound_ir, lowered_scheme.scheme)
    }
}
