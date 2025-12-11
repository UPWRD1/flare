use std::collections::BTreeSet;

use internment::Intern;

use petgraph::graph::NodeIndex;
use rustc_hash::FxHashMap;

use crate::{
    passes::midend::{
        environment::Environment,
        typing::{ItemSource, RowVar, Solver, Type, TypeScheme, TypeVar, TypesOutput},
    },
    resource::{
        errors::{CompResult, DynamicErr, ErrorCollection},
        rep::{
            Spanned,
            ast::{ItemId, Untyped},
            common::Ident,
            entry::{FunctionItem, ItemKind},
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

    pub fn check(mut self) -> CompResult<(Vec<TypesOutput>, ItemSource)> {
        let mut accum: Vec<TypesOutput> = vec![];
        for item_idx in self.item_order {
            let item = self.env.value(*item_idx)?;
            match item.kind {
                ItemKind::Function(f) => {
                    let o = self.check_function(*item_idx, f)?;
                    accum.push(o);
                }
                ItemKind::Type(_n, t) => {
                    self.check_type(*item_idx, t)?;
                }
                ItemKind::Extern { name: _, sig } => {
                    let (unbound_types, unbound_rows) = self.extract_generics(sig);
                    let scheme = TypeScheme {
                        unbound_types,
                        unbound_rows,
                        evidence: Vec::new(),
                        ty: sig,
                    };
                    self.context
                        .insert(ItemId(item_idx.index()), scheme.clone());
                }
                ItemKind::Field { name: _, value } => {
                    self.check_type(*item_idx, value)?;
                }

                _ => unreachable!(),
            };
        }

        Ok((accum, self.context))
    }

    fn check_type(&mut self, item_idx: NodeIndex, t: Spanned<Intern<Type>>) -> CompResult<()> {
        let (unbound_types, unbound_rows) = self.extract_generics(t);
        // TODO: Add support for rows and generics
        let scheme = TypeScheme {
            unbound_types,
            unbound_rows,
            evidence: Vec::new(),
            ty: t,
        };
        self.context.insert(ItemId(item_idx.index()), scheme);
        Ok(())
    }

    pub fn extract_generics(
        &mut self,
        t: Spanned<Intern<Type>>,
    ) -> (BTreeSet<TypeVar>, BTreeSet<RowVar>) {
        // let mut rowcount = 0;
        let mut accum = BTreeSet::new();
        let mut row_accum = BTreeSet::new();
        // let row_accum = BTreeSet::new();
        fn helper(
            t: Spanned<Intern<Type>>,
            accum: &mut BTreeSet<TypeVar>,
            row_accum: &mut BTreeSet<RowVar>,
        ) {
            match *t.0 {
                Type::Var(type_var) => {
                    accum.insert(type_var);
                }

                Type::Func(l, r) => {
                    helper(l, accum, row_accum);
                    helper(r, accum, row_accum);
                }

                Type::Prod(row) | Type::Sum(row) => match row {
                    crate::passes::midend::typing::Row::Closed(closed_row) => {
                        for t in closed_row.values {
                            helper(*t, accum, row_accum);
                        }
                    }
                    crate::passes::midend::typing::Row::Open(o) => {
                        row_accum.insert(o);
                    }

                    _ => todo!("Should be closed? todo"),
                },
                // Type::Sum(row) => todo!(),
                Type::User(t) => {
                    unreachable!("Encountered user type {t} after resolution")
                }
                _ => (),
            };
        }
        helper(t, &mut accum, &mut row_accum);
        (accum, row_accum)
    }

    fn check_function(
        &mut self,
        item_idx: NodeIndex,
        f: FunctionItem<Untyped>,
    ) -> CompResult<TypesOutput> {
        // let unbound_types = BTreeSet::new();
        let evidence = vec![];
        // let unbound_rows = (*f.unbound_rows).clone();
        let (unbound_types, unbound_rows) = self.extract_generics(f.sig);
        let scheme = TypeScheme {
            unbound_types,
            unbound_rows,
            evidence,
            ty: f.sig,
        };
        // dbg!(&scheme);
        self.context
            .insert(ItemId(item_idx.index()), scheme.clone());

        let solved = if matches!(*f.sig.0, Type::Infer) {
            Solver::type_infer_with_items(&self.context, f.body)
                .unwrap()
                .to_typesoutput()
        } else {
            Solver::check_with_items(&self.context, f.body, scheme).map_err(|e| {
                if let Some(e) = e.downcast_ref::<DynamicErr>() {
                    e.clone()
                        .label("in this let-definition", f.name.ident().unwrap().1)
                        .into()
                } else {
                    e
                }
            })?
        };

        // let infer = Solver::type_infer_with_items(&self.context, f.body).unwrap();
        // dbg!(infer.scheme);

        if !solved.errors.is_empty() {
            Err(ErrorCollection::new(
                solved
                    .errors
                    .into_values()
                    .map(|e| {
                        if let Some(e) = e.downcast_ref::<DynamicErr>() {
                            e.clone()
                                .extra("in this let-definition", f.name.ident().unwrap().1)
                                .into()
                        } else {
                            e
                        }
                    })
                    .collect(),
            )
            .into())
        } else {
            self.context
                .insert(ItemId(item_idx.index()), solved.scheme.clone());
            Ok(solved)
        }
        // dbg!(&checked);

        // Ok(infer.scheme)
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
