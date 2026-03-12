use rustc_hash::FxHashMap;

use crate::resource::rep::midend::{
    ir::{IR, ItemId},
    irtype::IRType,
};

/// DANGER!
/// currently, this is an invalid transformation because it does not update item indexes afterwards.
pub fn reduce(irs: Vec<IR>) -> Vec<IR> {
    loop {}
}

fn strip_ty(ty: IRType) -> IRType {
    loop {}
}

fn reduce_ir(ir: IR, map: &FxHashMap<ItemId, ItemId>) -> IR {
    loop {}
}
