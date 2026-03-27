use petgraph::graph::NodeIndex;
use rustc_hash::FxHashMap;

use crate::{
    passes::frontend::{
        environment::Environment,
        typing::{ItemSource, Solver, TypeScheme, TypesOutput},
    },
    resource::{
        errors::{CompResult, DynamicErr, ErrorCollection},
        rep::frontend::{
            ast::{Expr, ItemId, UntypedAst},
            entry::{FunctionItem, ItemKind},
        },
    },
};

pub struct Typechecker {
    item_order: &'static [NodeIndex],
    context: ItemSource,
    env: Environment<UntypedAst>,
}

impl Typechecker {
    pub fn new(item_order: &'static [NodeIndex], env: Environment<UntypedAst>) -> Self {
        Self {
            item_order,
            env,
            // row_var_count: 0,
            context: ItemSource::new(FxHashMap::default()),
        }
    }

    pub fn check(mut self) -> CompResult<(Vec<(ItemId, TypesOutput)>, ItemSource)> {
        for item_idx in self.item_order {
            let item = self
                .env
                .value(*item_idx)
                .expect("Item should exist")
                .clone();
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
                    self.context.insert(ItemId(item_idx.index()), sig.clone());
                }
                _ => unreachable!("{:?}", item.kind),
            }
        }

        let out = self.check_items()?;

        Ok((out, self.context))
    }

    fn register_type(&mut self, item_idx: NodeIndex, scheme: TypeScheme) {
        // TODO: Add support for  Type::Generic(_) =>rows and generics
        self.context.insert(ItemId(item_idx.index()), scheme);
    }

    fn register_function(&mut self, item_idx: NodeIndex, f: FunctionItem<UntypedAst>) {
        // println!("Registered {}: {}", f.name, ty.0);
        self.context.insert(ItemId(item_idx.index()), f.sig);
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
                let solved = match &item.kind {
                    ItemKind::Function(f) => {
                        // println!("Checking {} : {}", f.name.0, scheme.ty.0);
                        Solver::check_with_items(&self.context, f.body, scheme).map_err(|e| {
                            if let Some(e) = e.downcast_ref::<DynamicErr>() {
                                e.clone().label("in this let-definition", f.name.1).into()
                            } else {
                                e
                            }
                        })?
                        // .inspect(|types_output| {
                        //     println!("Checked {} : {}", f.name.0, types_output.scheme.ty.0)
                        // })?
                    }

                    ItemKind::Extern { name, args, sig: _ } => {
                        let ex = name.convert(Expr::Item(
                            id,
                            crate::resource::rep::frontend::ast::Kind::Extern(name.0),
                        ));

                        let funcs = args.iter().fold(ex, |prev, arg| {
                            prev.convert(Expr::Lambda(
                                *arg,
                                prev.convert(Expr::Call(prev, arg.0.convert(Expr::Ident(*arg)))),
                            ))
                        });

                        Solver::check_with_items(&self.context, funcs, scheme)
                            .expect("Inference should succeed")
                    }

                    _ => unreachable!(),
                };

                if solved.errors.is_empty() {
                    self.context.insert(id, solved.scheme.clone());
                    Ok((id, solved))
                } else {
                    Err(ErrorCollection::new(solved.errors.into_values().collect()).into())
                }
            })
            .collect()
    }

    pub fn finish(self) -> Environment<UntypedAst> {
        self.env
    }
}
