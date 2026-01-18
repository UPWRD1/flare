use std::collections::{BTreeMap, BTreeSet};

use internment::Intern;

use petgraph::graph::NodeIndex;
use rustc_hash::FxHashMap;

use crate::{
    passes::{
        backend::lowering::subst::{self, Subst},
        midend::{
            environment::Environment,
            resolution::subst_generic_type,
            typing::{
                ClosedRow, Evidence, ItemSource, Row, RowVar, Solver, Type, TypeScheme, TypeVar,
                TypesOutput,
            },
        },
    },
    resource::{
        errors::{CompResult, DynamicErr, ErrorCollection},
        rep::{
            Spanned,
            ast::{Expr, ItemId, Kind, LambdaInfo, NodeId, Untyped},
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

#[derive(Default, Debug)]
enum TyappState {
    #[default]
    Outside,
    Arg,
    Inside,
}

#[derive(Default, Debug)]
struct TypeFixer {
    unbound_types: BTreeSet<TypeVar>,
    unbound_rows: BTreeSet<RowVar>,
    pub types_to_name: FxHashMap<TypeVar, Intern<String>>,
    evidence: Vec<Evidence>,
    seen_row_vars: FxHashMap<NodeId, RowVar>,
    inside_type_fun: TyappState,
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

    pub fn check(mut self) -> CompResult<(Vec<(ItemId, TypesOutput)>, ItemSource)> {
        for item_idx in self.item_order {
            let item = self.env.value(*item_idx)?;
            match item.kind {
                ItemKind::Function(f) => {
                    // dbg!(f.sig);
                    self.register_function(*item_idx, f);
                }
                ItemKind::Type(_n, _, t) => {
                    // dbg!(t);
                    self.register_type(*item_idx, t);
                }
                ItemKind::Extern { name, sig, .. } => {
                    let (
                        TypeFixer {
                            unbound_types,
                            unbound_rows,
                            types_to_name,
                            evidence,
                            ..
                        },
                        ty,
                    ) = self.extract_generics(sig);
                    let scheme = TypeScheme {
                        unbound_types,
                        unbound_rows,
                        evidence: Vec::new(),
                        ty,
                        types_to_name,
                        kind: Kind::Extern(name.0),
                    };
                    self.context.insert(ItemId(item_idx.index()), scheme);
                }
                ItemKind::Package(_) => {
                    let scheme = TypeScheme::default();
                    self.context.insert(ItemId(item_idx.index()), scheme);
                }
                _ => unreachable!("{:?}", item.kind),
            };
        }

        let out = self.check_items()?;

        Ok((out, self.context))
    }

    fn register_type(&mut self, item_idx: NodeIndex, ty: Spanned<Intern<Type>>) {
        // let (
        //     TypeFixer {
        //         unbound_types,
        //         unbound_rows,
        //         types_to_name,
        //         evidence,
        //         ..
        //     },
        //     ty,
        // ) = self.extract_generics(t);
        // dbg!(ty);
        // dbg!(&unbound_rows);
        // // TODO: Add support for  Type::Generic(_) =>rows and generics
        let scheme = TypeScheme {
            ty,
            kind: Kind::Ty,
            ..Default::default()
        };
        self.context.insert(ItemId(item_idx.index()), scheme);
    }

    fn new_type_var(&mut self) -> TypeVar {
        let v = TypeVar(self.type_var_count);
        // let v = TypeVar(n);
        self.type_var_count += 1;
        v
    }

    fn new_row_var(&mut self) -> RowVar {
        let v = RowVar(self.row_var_count.to_string().into());
        self.row_var_count += 1;
        v
    }

    fn helper(&mut self, t: Spanned<Intern<Type>>, f: &mut TypeFixer) -> Spanned<Intern<Type>> {
        // dbg!(t);
        match *t.0 {
            Type::Var(type_var) => {
                f.unbound_types.insert(type_var);
                t
            }
            // Type::Subtable(sub, id) => {
            //     let sub = self.helper(sub, f);

            //     let v = if let Some(v) = f.seen_row_vars.get(&id) {
            //         *v
            //     } else {
            //         let v = self.new_row_var();
            //         f.seen_row_vars.insert(id, v);
            //         v
            //     };

            //     // dbg!(&sub);
            //     // let g = self.new_row_var();
            //     f.evidence.push(Evidence::RowEquation {
            //         left: sub.map(|sub| sub.to_row().into()),
            //         right: sub.map(|_| Intern::from(Row::Open(v))),
            //         goal: sub.map(|sub| sub.to_row().into()),
            //     });
            //     f.unbound_rows.insert(v);
            //     // row_accum.insert(g);
            //     // sub.modify(Type::Prod(Row::Open(v).into()))
            //     sub
            // }
            Type::Generic(n) => {
                println!("Generic {}", n.0);
                let v = if let Some((v, _)) =
                    f.types_to_name.iter().find(|(_, name)| ***name == *n.0)
                {
                    println!("Loaded {v:?}");
                    *v
                } else {
                    let v = self.new_type_var();
                    f.types_to_name.insert(v, n.0);
                    f.unbound_types.insert(v);
                    println!("Created {v:?}");
                    v
                };

                t.modify(Type::Var(v))
            }

            Type::Func(l, r) => {
                let l = self.helper(l, f);
                let r = self.helper(r, f);
                t.modify(Type::Func(l, r))
            }

            Type::Prod(row) => t.modify(Type::Prod(row.map(|row| {
                match *row {
                    Row::Closed(closed_row) => Row::Closed(ClosedRow {
                        fields: closed_row.fields,
                        values: closed_row
                            .values
                            .iter()
                            .map(|value_ty| self.helper(*value_ty, f))
                            .collect::<Vec<_>>()
                            .leak(),
                    })
                    .into(),

                    // Row::Open(o) => {
                    //     f.unbound_rows.insert(o);
                    //     row
                    // }
                    _ => todo!("Should be closed? todo"),
                }
            }))),
            Type::Sum(row) => t.modify(Type::Sum(row.map(|row| {
                match *row {
                    Row::Closed(closed_row) => Row::Closed(ClosedRow {
                        fields: closed_row.fields,
                        values: closed_row
                            .values
                            .iter()
                            .map(|value_ty| self.helper(*value_ty, f))
                            .collect::<Vec<_>>()
                            .leak(),
                    })
                    .into(),

                    // Row::Open(o) => {
                    //     f.unbound_rows.insert(o);
                    //     row
                    // }
                    _ => todo!("Should be closed? todo"),
                }
            }))),
            Type::Label(l, t) => t.modify(Type::Label(l, self.helper(t, f))),
            Type::User(t, g) => {
                unreachable!("Encountered user type {t}[{g:?}] after resolution")
            }
            Type::TypeApp(l, r) => {
                if let Type::TypeFun(g, t) = *l.0 {
                    // let t = self.helper(t, f);
                    // dbg!(l, r, g, t);
                    self.helper(subst_generic_type(t, g.0, r.0), f)
                } else {
                    let l = self.helper(l, f);
                    self.helper(t.modify(Type::TypeApp(l, r)), f)
                }
            }
            _ => t,
        }
        // dbg!(o)
    }

    fn extract_generics(&mut self, t: Spanned<Intern<Type>>) -> (TypeFixer, Spanned<Intern<Type>>) {
        let mut f = TypeFixer::default();
        let t = self.helper(t, &mut f);
        // dbg!(t);
        (f, t)
    }

    fn register_function(&mut self, item_idx: NodeIndex, f: FunctionItem<Untyped>) {
        // let unbound_types = BTreeSet::new();
        // let evidence = vec![];
        // let unbound_rows = (*f.unbound_rows).clone();
        let (
            TypeFixer {
                unbound_types,
                unbound_rows,
                types_to_name,
                evidence,
                ..
            },
            ty,
        ) = self.extract_generics(f.sig);
        // dbg!(&ty);

        let scheme = TypeScheme {
            unbound_types,
            unbound_rows,
            evidence,
            ty,
            types_to_name,
            kind: Kind::Func,
        };
        println!("Registered {}: {}", f.name, ty.0);
        self.context.insert(ItemId(item_idx.index()), scheme);
    }

    fn check_items(&mut self) -> CompResult<Vec<(ItemId, TypesOutput)>> {
        // dbg!(&self.context);
        let to_check: Vec<_> = self
            .item_order
            .iter()
            .filter_map(|idx| {
                let id = ItemId(idx.index());
                let scheme = self
                    .context
                    .types
                    .get(&id)
                    .expect("Context should be loaded")
                    .clone();

                let item = self.env.value(*idx).expect("Item should exist");

                match item.kind {
                    ItemKind::Function(_) | ItemKind::Extern { .. } => Some((id, scheme, item)),
                    _ => None,
                }
            })
            .collect();
        to_check
            .into_iter()
            .map(|(id, scheme, item)| {
                let solved = match item.kind {
                    ItemKind::Function(f) => {
                        // println!("Checking {} : {}", f.name.0, scheme.ty.0);
                        Solver::check_with_items(&self.context, f.body, scheme)
                            .map_err(|e| {
                                if let Some(e) = e.downcast_ref::<DynamicErr>() {
                                    e.clone()
                                        .label(
                                            "in this let-definition",
                                            f.name
                                                .ident()
                                                .expect("Function should have a valid name")
                                                .1,
                                        )
                                        .into()
                                } else {
                                    e
                                }
                            })
                            .inspect(|types_output| {
                                println!("Checked {} : {}", f.name.0, types_output.scheme.ty.0)
                            })?
                    }

                    ItemKind::Extern { name, args, sig: _ } => {
                        let ex = name.convert(Expr::Item(
                            id,
                            crate::resource::rep::ast::Kind::Extern(name.0),
                        ));

                        let funcs = args.iter().fold(ex, |prev, arg| {
                            prev.convert(Expr::Lambda(
                                *arg,
                                prev.convert(Expr::Call(prev, arg.0.convert(Expr::Ident(*arg)))),
                                LambdaInfo::Curried,
                            ))
                        });

                        Solver::check_with_items(&self.context, funcs, scheme)
                            .expect("Inference should succeed")
                    }

                    _ => unreachable!(),
                };

                if !solved.errors.is_empty() {
                    Err(ErrorCollection::new(solved.errors.into_values().collect()).into())
                } else {
                    self.context.insert(id, solved.scheme.clone());
                    Ok((id, solved))
                }
            })
            .collect()
    }

    pub fn finish(self) -> Environment {
        self.env
    }
}
