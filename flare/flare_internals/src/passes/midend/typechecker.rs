use std::collections::BTreeSet;

use internment::Intern;
use log::info;
use petgraph::graph::NodeIndex;
use rustc_hash::FxHashMap;

use crate::{
    passes::midend::{
        environment::Environment,
        typing::{ItemSource, Solver, Type, TypeScheme, TypeVar, Typed},
    },
    resource::{
        errors::CompResult,
        rep::{
            ast::{Expr, ItemId, Untyped},
            entry::{FunctionItem, ItemKind},
            Spanned,
        },
    },
};

pub struct Typechecker {
    item_order: &'static [NodeIndex],
    context: ItemSource,
    env: Environment,
}

impl Typechecker {
    pub fn new(item_order: &'static [NodeIndex], env: Environment) -> Self {
        Self {
            item_order,
            env,
            context: ItemSource::new(FxHashMap::default()),
        }
    }

    pub fn check(&mut self) -> CompResult<Vec<ItemKind<Typed>>> {
        let mut accum: Vec<ItemKind<Typed>> = vec![];
        for item_idx in self.item_order {
            let item = self.env.value(*item_idx)?;
            let new_item = match item.kind {
                ItemKind::Function(f) => {
                    let (ty, ast) = self.check_function(*item_idx, f)?;
                    let name = Typed(f.name, ty);
                    ItemKind::Function(FunctionItem {
                        name,
                        sig: f.sig.update(ty),
                        body: ast,
                    })
                }
                ItemKind::Type(n, t, s) => {
                    self.check_type(*item_idx, t)?;
                    ItemKind::Type(n, t, s)
                }
                ItemKind::Extern { name, sig } => {
                    let unbound_types = self.extract_generics(sig.0);
                    let scheme = TypeScheme {
                        unbound_types,
                        unbound_rows: BTreeSet::new(),
                        evidence: Vec::new(),
                        ty: sig.0,
                    };
                    self.context
                        .insert(ItemId(item_idx.index()), scheme.clone());
                    ItemKind::Extern { name, sig }
                }
                ItemKind::Field { name, value } => {
                    self.check_type(*item_idx, value)?;
                    ItemKind::Field { name, value }
                }

                _ => unreachable!(),
            };
            accum.push(new_item)
        }
        Ok(accum)
    }

    fn check_type(&mut self, item_idx: NodeIndex, t: Intern<Type>) -> CompResult<()> {
        let unbound_types = self.extract_generics(t);
        // TODO: Add support for rows and generics
        let scheme = TypeScheme {
            unbound_types,
            unbound_rows: BTreeSet::new(),
            evidence: Vec::new(),
            ty: t,
        };
        self.context.insert(ItemId(item_idx.index()), scheme);
        Ok(())
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

    fn check_function(
        &mut self,
        item_idx: NodeIndex,
        f: FunctionItem<Untyped>,
    ) -> CompResult<(Intern<Type>, Spanned<Intern<Expr<Typed>>>)> {
        let unbound_types = self.extract_generics(f.sig.0);
        let scheme = TypeScheme {
            unbound_types,
            unbound_rows: BTreeSet::new(),
            evidence: Vec::new(),
            ty: f.sig.0,
        };
        self.context
            .insert(ItemId(item_idx.index()), scheme.clone());

        let checked_ast = Solver::check_with_items(&self.context, f.body, scheme)?;
        let infer = Solver::type_infer_with_items(&self.context, f.body)?;
        // let name = Typed(f.name, infer.scheme.ty);
        Ok((infer.scheme.ty, checked_ast))
        // Ok(ItemKind::Function( FunctionItem {
        //     name,
        //     sig: f.sig,
        //     body: checked_ast,
        // }))
        // let infer = Solver::type_infer_with_items(&self.context, f.body)?;
        // info!("{}\n", check);
        // info!("{:#?}", infer.scheme);
    }

    pub fn finish(self) -> Environment {
        self.env
    }
}
