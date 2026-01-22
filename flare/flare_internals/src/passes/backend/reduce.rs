use chumsky::input::Input;
use im::HashSet;
use rustc_hash::{FxBuildHasher, FxHashSet};

use crate::passes::backend::lowering::ir::{IR, ItemId};

fn find_used(ir: &IR) -> FxHashSet<ItemId> {
    match ir {
        IR::Fun(_, ir) | IR::TyFun(_, ir) | IR::TyApp(ir, _) => find_used(ir),

        IR::If(c, t, o) => find_used(c)
            .into_iter()
            .chain(find_used(t))
            .chain(find_used(o))
            .collect(),

        IR::Local(_, l, r) | IR::App(l, r) | IR::Bin(l, _, r) => {
            find_used(l).into_iter().chain(find_used(r)).collect()
        }
        IR::Tuple(irs) => irs.iter().flat_map(find_used).collect(),
        IR::Field(ir, _) => find_used(ir),
        IR::Tag(_, _, ir) => find_used(ir),
        IR::Case(_, scrutinee, branches) => find_used(scrutinee)
            .into_iter()
            .chain(branches.iter().flat_map(|b| find_used(&b.body)))
            .collect(),
        IR::Item(_, item_id) => FxHashSet::from_iter(vec![*item_id]),
        IR::Extern(intern, _) => todo!(),
        _ => FxHashSet::with_capacity_and_hasher(0, FxBuildHasher),
    }
}

fn track_seen(ir: &[IR]) -> FxHashSet<ItemId> {
    ir.iter().rev().flat_map(find_used).collect()
}
/// DANGER!
/// currently, this is an invalid transformation because it does not update item indexes afterwards.
pub fn reduce(mut ir: Vec<IR>) -> Vec<IR> {
    // dbg!(ir.len());
    // let mut seen = track_seen(&ir);
    // seen.insert(ItemId(ir.len() as u32));
    // dbg!(&seen);

    // let mut index_counter = 0;
    // ir.retain(|_| {
    //     let retain_item = !seen.contains(&ItemId(index_counter));
    //     index_counter += 1;
    //     retain_item
    // });
    ir
}
