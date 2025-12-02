use std::collections::BTreeSet;

use internment::Intern;
use log::info;
use petgraph::graph::NodeIndex;
use rustc_hash::FxHashMap;

use crate::{
    passes::midend::{
        environment::Environment,
        typing::{ItemSource, Solver, Type, TypeScheme, TypeVar},
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
                crate::resource::rep::entry::ItemKind::Type(_, t, _) => {
                    self.check_type(*item_idx, t)
                }
                crate::resource::rep::entry::ItemKind::Extern { name, sig } => {
                    let unbound_types = self.extract_generics(sig.0);
                    let scheme = TypeScheme {
                        unbound_types,
                        unbound_rows: BTreeSet::new(),
                        evidence: Vec::new(),
                        ty: sig.0,
                    };
                    self.context
                        .insert(ItemId(item_idx.index()), scheme.clone());
                }
                crate::resource::rep::entry::ItemKind::Field { name, value } => {
                    self.check_type(*item_idx, value)
                }

                _ => unreachable!(),
            }
        }
        Ok(())
    }

    fn check_type(&mut self, item_idx: NodeIndex, t: Intern<Type>) {
        let unbound_types = self.extract_generics(t);
        // TODO: Add support for rows and generics
        let scheme = TypeScheme {
            unbound_types,
            unbound_rows: BTreeSet::new(),
            evidence: Vec::new(),
            ty: t,
        };
        self.context.insert(ItemId(item_idx.index()), scheme);
    }

    fn extract_generics(&mut self, t: Intern<Type>) -> BTreeSet<TypeVar> {
        let mut accum = BTreeSet::new();
        fn helper(t: Intern<Type>, accum: &mut BTreeSet<TypeVar>) {
            match *t {
                Type::Var(type_var) => {
                    accum.insert(type_var);
                }

                Type::Func(l, r) => {
                    helper(l, accum);
                    helper(r, accum);
                }
                Type::Package(spanned) => todo!(),

                Type::Prod(row) => match row {
                    crate::passes::midend::typing::Row::Closed(closed_row) => {
                        for t in closed_row.values {
                            helper(*t, accum);
                        }
                    }
                    _ => panic!("Should be closed? todo"),
                },
                Type::Sum(row) => todo!(),
                Type::Label(label, intern) => todo!(),
                _ => (),
            };
        }
        helper(t, &mut accum);
        accum
    }

    fn check_function(&mut self, item_idx: NodeIndex, f: FunctionItem<Untyped>) -> CompResult<()> {
        let unbound_types = self.extract_generics(f.sig.0);
        let scheme = TypeScheme {
            unbound_types,
            unbound_rows: BTreeSet::new(),
            evidence: Vec::new(),
            ty: f.sig.0,
        };
        self.context
            .insert(ItemId(item_idx.index()), scheme.clone());
        let source = ItemSource::new(self.context.clone());

        let check = Solver::check_with_items(source, f.body, scheme)?;
        // let infer = Solver::type_infer_with_items(source, f.body)?;
        info!("{:?}", check);
        // info!(infer.scheme);
        Ok(())
    }

    pub fn finish(self) -> Environment {
        self.env
    }
}
