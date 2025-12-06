use std::collections::BTreeSet;

use internment::Intern;
use log::info;
use petgraph::graph::NodeIndex;
use rustc_hash::FxHashMap;

use crate::{
    passes::midend::{
        environment::Environment,
        typing::{ItemSource, RowVar, Solver, Type, TypeScheme, TypeVar, Typed, TypesOutput},
    },
    resource::{
        errors::{CompResult, CompilerErr, DynamicErr, ErrorCollection},
        rep::{
            ast::{Expr, ItemId, Untyped},
            common::Ident,
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

    pub fn check(mut self) -> CompResult<(Vec<TypesOutput>, ItemSource)> {
        let mut accum: Vec<TypesOutput> = vec![];
        for item_idx in self.item_order {
            let item = self.env.value(*item_idx)?;
            match item.kind {
                ItemKind::Function(f) => {
                    let o = self.check_function(*item_idx, f)?;
                    accum.push(o);
                }
                ItemKind::Type(n, t, s) => {
                    self.check_type(*item_idx, t)?;
                }
                ItemKind::Extern { name, sig } => {
                    let unbound_types = Self::extract_generics(sig.0);
                    let unbound_rows = BTreeSet::new();
                    let scheme = TypeScheme {
                        unbound_types,
                        unbound_rows,
                        evidence: Vec::new(),
                        ty: sig.0,
                    };
                    self.context
                        .insert(ItemId(item_idx.index()), scheme.clone());
                }
                ItemKind::Field { name, value } => {
                    self.check_type(*item_idx, value)?;
                }

                _ => unreachable!(),
            };
        }

        Ok((accum, self.context))
    }

    fn check_type(&mut self, item_idx: NodeIndex, t: Intern<Type>) -> CompResult<()> {
        let unbound_types = Self::extract_generics(t);
        let unbound_rows = BTreeSet::new();
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

    pub fn extract_generics(t: Intern<Type>) -> BTreeSet<TypeVar> {
        // let mut rowcount = 0;
        let mut accum = BTreeSet::new();
        // let row_accum = BTreeSet::new();
        fn helper(
            t: Intern<Type>,
            accum: &mut BTreeSet<TypeVar>,
            // _rowaccum: &mut BTreeSet<RowVar>,
            // row_count: &mut u32,
        ) {
            match *t {
                Type::Var(type_var) => {
                    accum.insert(type_var);
                }

                Type::Func(l, r) => {
                    helper(l, accum);
                    helper(r, accum);
                }

                Type::Prod(row) | Type::Sum(row) => match row {
                    crate::passes::midend::typing::Row::Closed(closed_row) => {
                        for t in closed_row.values {
                            helper(*t, accum);
                        }
                    }
                    // crate::passes::midend::typing::Row::Open(o) => {
                    // row_accum.insert(o);
                    // *row_count += 1;
                    // }
                    _ => panic!("Should be closed? todo"),
                },
                // Type::Sum(row) => todo!(),
                Type::Label(label, intern) => todo!(),
                Type::User(t) => {
                    panic!("{t}")
                }
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
    ) -> CompResult<TypesOutput> {
        // let unbound_types = Intern::as_ref(f.unbound_types).clone();
        // let unbound_rows = Intern::as_ref(f.unbound_rows).clone();
        // let evidence = Intern::as_ref(f.evidence).to_vec();
        let unbound_types = BTreeSet::new();
        let unbound_rows = BTreeSet::new();
        let evidence = Vec::new();
        let scheme = TypeScheme {
            unbound_types,
            unbound_rows,
            evidence,
            ty: f.sig.0,
        };
        // dbg!(&scheme);
        self.context
            .insert(ItemId(item_idx.index()), scheme.clone());

        let infer = Solver::type_infer_with_items(&self.context, f.body)?;
        dbg!(infer.scheme);

        let checked = Solver::check_with_items(&self.context, f.body, scheme)?;

        if !checked.errors.is_empty() {
            Err(ErrorCollection::new(
                checked
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
                .insert(ItemId(item_idx.index()), checked.scheme.clone());
            Ok(checked)
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
