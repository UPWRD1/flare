use std::collections::BTreeSet;

use internment::Intern;
use petgraph::{graph::NodeIndex, Direction::Incoming};
use rustc_hash::FxHashMap;

use crate::{
    passes::midend::{
        environment::Environment,
        typing::{ItemSource, Solver, Type, TypeScheme},
    },
    resource::{
        errors::CompResult,
        rep::{
            ast::{ItemId, Untyped},
            entry::FunctionItem,
        },
    },
};

pub struct Typechecker {
    item_order: &'static [NodeIndex],
    context: FxHashMap<ItemId, TypeScheme>,
    env: Environment,
}

impl Typechecker {
    pub fn new(item_order: &'static [NodeIndex], env: Environment) -> Self {
        Self {
            item_order,
            env,
            context: FxHashMap::default(),
        }
    }

    pub fn check(&mut self) -> CompResult<()> {
        for item_idx in self.item_order {
            let item = self.env.value(*item_idx)?;
            match item.kind {
                crate::resource::rep::entry::ItemKind::Function(f) => {
                    self.check_function(*item_idx, f)?
                }
                crate::resource::rep::entry::ItemKind::Type(t, _) => self.check_type(*item_idx, t),
                crate::resource::rep::entry::ItemKind::Extern { name, sig } => todo!(),
                crate::resource::rep::entry::ItemKind::Field { name, value } => {}
                _ => unreachable!(),
            }
        }
        Ok(())
    }

    fn check_type(&mut self, item_idx: NodeIndex, t: Intern<Type>) {
        // TODO: Add support for rows and generics
        let scheme = TypeScheme {
            unbound_types: BTreeSet::new(),
            unbound_rows: BTreeSet::new(),
            evidence: Vec::new(),
            ty: t,
        };
        self.context.insert(ItemId(item_idx.index()), scheme);
    }

    fn check_function(&mut self, item_idx: NodeIndex, f: FunctionItem<Untyped>) -> CompResult<()> {
        let scheme = TypeScheme {
            unbound_types: BTreeSet::new(),
            unbound_rows: BTreeSet::new(),
            evidence: Vec::new(),
            ty: f.sig.0,
        };
        self.context.insert(ItemId(item_idx.index()), scheme);
        let source = ItemSource::new(self.context.clone());

        let infer = Solver::type_infer_with_items(source, f.body)?;
        dbg!(infer.scheme);
        Ok(())
    }

    pub fn finish(self) -> Environment {
        self.env
    }
}
