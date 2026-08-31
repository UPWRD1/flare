#[derive(Default)]
pub struct AstSizeNoLet;
use std::cmp::Ordering;

use slotted_egraphs::*;

use super::Rise;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum MyCost {
    Finite(u32),
    Infinite,
}

impl MyCost {
    pub fn add(&self, other: &MyCost) -> MyCost {
        use MyCost::*;
        match (self, other) {
            (Infinite, _) => Infinite,
            (_, Infinite) => Infinite,
            (Finite(x), Finite(y)) => Finite(*x + *y),
        }
    }
}
impl PartialOrd for MyCost {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use MyCost::*;
        let out = match (self, other) {
            (Finite(x), Finite(y)) => x.cmp(y),
            (Infinite, Infinite) => Ordering::Equal,
            (Infinite, Finite(_)) => Ordering::Greater,
            (Finite(_), Infinite) => Ordering::Less,
        };

        Some(out)
    }
}

impl Ord for MyCost {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}
impl CostFunction<Rise> for AstSizeNoLet {
    type Cost = MyCost;

    fn cost<C>(&self, enode: &Rise, costs: C) -> MyCost
    where
        C: Fn(Id) -> MyCost,
    {
        if let Rise::Let(..) = enode {
            MyCost::Infinite
        } else {
            let mut s = MyCost::Finite(1);
            for x in enode.applied_id_occurrences() {
                s = s.add(&costs(x.id));
            }
            s
        }
    }
}
