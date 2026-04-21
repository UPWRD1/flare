use rustc_hash::{FxHashMap, FxHashSet};

use crate::resource::rep::midend::{
    ir::{Branch, IR, ItemId},
    irtype::{IRType, Row},
};

fn track_seen(ir: &[IR]) -> FxHashSet<ItemId> {
    ir.iter().rev().flat_map(IR::who_do_i_call).collect()
}
/// DANGER!
// / currently, this is an invalid transformation because it does not update item indexes afterwards. Probably. Need to review
pub fn reduce(irs: Vec<IR>) -> Vec<IR> {
    let mut seen = track_seen(&irs);
    seen.insert(ItemId(irs.len()));
    let mut new_counter = 0;
    let mut map: FxHashMap<ItemId, ItemId> = FxHashMap::default();
    let len = irs.len() - 1;
    let ir: Vec<IR> = irs
        .into_iter()
        .enumerate()
        .filter_map(|(index_counter, ir)| {
            // dbg!(&ir);
            let id = ItemId(index_counter);
            let nid = ItemId(new_counter);
            let retain_item = seen.contains(&id) || id.0 == len || matches!(ir, IR::Extern(_, _));
            // index_counter += 1;
            if retain_item {
                new_counter += 1;
                map.insert(id, nid);
                Some(ir)
            } else {
                None
            }
        })
        .collect();
    ir.into_iter().map(|ir| reduce_ir(ir, &map)).collect()
}

fn strip_ty(ty: IRType) -> IRType {
    // dbg!(&ty);
    match ty {
        IRType::Var(type_var) => panic!("Invalid"),
        IRType::Fun(l, r) => IRType::fun(strip_ty(*l), strip_ty(*r)),
        IRType::TyFun(kind, irtype) => strip_ty(*irtype),
        IRType::Prod(row) => IRType::Prod(match row {
            Row::Open(type_var) => todo!(),
            Row::Closed(irtypes) => Row::Closed(irtypes.into_iter().map(strip_ty).collect()),
        }),
        IRType::Sum(row) => IRType::Sum(match row {
            Row::Open(type_var) => todo!(),
            Row::Closed(irtypes) => Row::Closed(irtypes.into_iter().map(strip_ty).collect()),
        }),
        IRType::Volatile(irtype) => IRType::volatile(strip_ty(*irtype)),
        _ => ty,
    }
}

fn reduce_ir(ir: IR, map: &FxHashMap<ItemId, ItemId>) -> IR {
    match ir {
        // IR::Comment(s, ir) => IR::Comment(s, Box::new(reduce_ir(*ir, map))),
        IR::Fun(var, ir) => IR::fun(var.map_ty(strip_ty), reduce_ir(*ir, map)),
        IR::App(l, r) => IR::app(reduce_ir(*l, map), reduce_ir(*r, map)),
        IR::TyFun(..)| //IR::ty_fun(kind, reduce_ir(*ir, map)),
        IR::TyApp(..) => panic!("Invalid"), // IR::ty_app(reduce_ir(*ir, map), ty_app),
        IR::Local(var, defn, body) => IR::local(var.map_ty(strip_ty), reduce_ir(*defn, map), reduce_ir(*body, map)),
        IR::If(ir, t, o) => IR::r#if(reduce_ir(*ir, map), reduce_ir(*t, map), reduce_ir(*o, map)),
        IR::Bin(l, op, r) => IR::bin(reduce_ir(*l, map), op, reduce_ir(*r, map)),
        IR::Tuple(irs) => IR::tuple(irs.into_iter().map(|ir| reduce_ir(ir, map))),
        IR::Field(ir, i) => IR::field(reduce_ir(*ir, map), i),
        IR::Tag(ty, tag, body) => IR::tag(strip_ty(ty), tag, reduce_ir(*body, map)),
        IR::Case(t, ir, branchs) => IR::case(
            strip_ty(t),
            reduce_ir(*ir, map),
            branchs.into_iter().map(|b| Branch {
                body: reduce_ir(b.body, map),
                ..b
            }),
        ),
        IR::Item(t, item_id) => IR::Item(
            strip_ty(t),
            *map.get(&item_id)
                .unwrap_or_else(|| panic!("cannot find {item_id:?}")),
        ),
        _ => ir,
    }
}
