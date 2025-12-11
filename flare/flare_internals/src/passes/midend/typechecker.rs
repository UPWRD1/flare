use std::collections::BTreeSet;

use internment::Intern;

use petgraph::graph::NodeIndex;
use rustc_hash::FxHashMap;

use crate::{
    passes::midend::{
        environment::Environment,
        typing::{
            ClosedRow, Evidence, ItemSource, Row, RowVar, Solver, Type, TypeScheme, TypeVar,
            TypesOutput,
        },
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
    type_var_count: usize,
    row_var_count: usize,
}

impl Typechecker {
    pub fn new(item_order: &'static [NodeIndex], env: Environment) -> Self {
        Self {
            item_order,
            env,

            type_var_count: 0,
            row_var_count: 0,
            context: ItemSource::new(FxHashMap::default()),
        }
    }

    pub fn check(mut self) -> CompResult<(Vec<TypesOutput>, ItemSource)> {
        let mut accum: Vec<TypesOutput> = vec![];
        // self.env.debug();
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
                    let (unbound_types, unbound_rows, types_to_name, ty, _) =
                        self.extract_generics(sig);
                    let scheme = TypeScheme {
                        unbound_types,
                        unbound_rows,
                        evidence: Vec::new(),
                        ty,
                        types_to_name,
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
        let (unbound_types, unbound_rows, types_to_name, ty, _) = self.extract_generics(t);
        // TODO: Add support for rows and generics
        let scheme = TypeScheme {
            unbound_types,
            unbound_rows,
            evidence: Vec::new(),
            types_to_name,
            ty,
        };
        self.context.insert(ItemId(item_idx.index()), scheme);
        Ok(())
    }

    fn new_type_var(&mut self) -> TypeVar {
        let v = TypeVar(self.type_var_count);
        self.type_var_count += 1;
        v
    }

    fn new_row_var(&mut self) -> RowVar {
        let v = RowVar(self.row_var_count.to_string().into());
        self.row_var_count += 1;
        v
    }

    fn helper(
        &mut self,
        t: Spanned<Intern<Type>>,
        accum: &mut BTreeSet<TypeVar>,
        row_accum: &mut BTreeSet<RowVar>,
        names: &mut FxHashMap<TypeVar, Spanned<Intern<String>>>,
        evidence: &mut Vec<Evidence>,
    ) -> Spanned<Intern<Type>> {
        match *t.0 {
            Type::Var(type_var) => {
                accum.insert(type_var);
                t
            }
            Type::Subtable(sub) => {
                let sub = self.helper(sub, accum, row_accum, names, evidence);
                let v = self.new_row_var();
                // dbg!(&sub);
                let g = self.new_row_var();
                evidence.push(Evidence::RowEquation {
                    left: sub.0.to_row(),
                    right: Row::Open(v),
                    goal: sub.0.to_row(),
                });
                row_accum.insert(v);
                // row_accum.insert(g);
                sub
            }
            Type::Generic(n) => {
                let v = if let Some((v, _)) = names.iter().find(|(v, x)| x.0 == n.0) {
                    *v
                } else {
                    self.new_type_var()
                };

                accum.insert(v);
                names.insert(v, n);
                t.modify(Type::Var(v))
            }

            Type::Func(l, r) => {
                let l = self.helper(l, accum, row_accum, names, evidence);
                let r = self.helper(r, accum, row_accum, names, evidence);
                t.modify(Type::Func(l, r))
            }

            Type::Prod(row) => t.modify(Type::Prod(match row {
                Row::Closed(closed_row) => Row::Closed(ClosedRow {
                    fields: closed_row.fields,
                    values: closed_row
                        .values
                        .iter()
                        .map(|t| self.helper(*t, accum, row_accum, names, evidence))
                        .collect::<Vec<_>>()
                        .leak(),
                }),

                Row::Open(o) => {
                    row_accum.insert(o);
                    row
                }

                _ => todo!("Should be closed? todo"),
            })),
            Type::Sum(row) => todo!(),
            // Type::Sum(row) => todo!(),
            Type::User(t) => {
                unreachable!("Encountered user type {t} after resolution")
            }
            _ => t,
        }
    }

    pub fn extract_generics(
        &mut self,
        t: Spanned<Intern<Type>>,
    ) -> (
        BTreeSet<TypeVar>,
        BTreeSet<RowVar>,
        FxHashMap<TypeVar, Spanned<Intern<String>>>,
        Spanned<Intern<Type>>,
        Vec<Evidence>,
    ) {
        // let mut rowcount = 0;
        let mut accum = BTreeSet::new();
        let mut row_accum = BTreeSet::new();
        let mut names = FxHashMap::default();
        let mut evidence = vec![];
        // let row_accum = BTreeSet::new();
        let t = self.helper(t, &mut accum, &mut row_accum, &mut names, &mut evidence);
        (accum, row_accum, names, t, evidence)
    }

    fn check_function(
        &mut self,
        item_idx: NodeIndex,
        f: FunctionItem<Untyped>,
    ) -> CompResult<TypesOutput> {
        // let unbound_types = BTreeSet::new();
        // let evidence = vec![];
        // let unbound_rows = (*f.unbound_rows).clone();
        let (unbound_types, unbound_rows, types_to_name, ty, evidence) =
            self.extract_generics(f.sig);
        let scheme = TypeScheme {
            unbound_types,
            unbound_rows,
            evidence,
            ty,
            types_to_name,
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
