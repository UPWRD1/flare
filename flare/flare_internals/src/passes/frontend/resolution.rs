use std::collections::BTreeSet;

use internment::Intern;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    passes::frontend::{
        environment::Environment,
        typing::{ClosedRow, Evidence, Row, RowVar, Type, TypeScheme, TypeVar},
    },
    resource::{
        errors::{CompResult, CompilerErr, DynamicErr, ErrorCollection},
        rep::{
            common::{FlareSpan, Spanned, Syntax},
            frontend::{
                ast::{Expr, ItemId, Kind, UntypedAst},
                cst::{CstExpr, UntypedCst},
                csttypes::{CstClosedRow, CstType},
                entry::{FunctionItem, Item, ItemKind},
            },
        },
    },
};

/// The name resolution engine.
// Once the environment is built from the parser, the `Resolver` takes ownership
/// of the graph.
///
/// The engine works in a two-pass system. First, each `Item` is analyzed
/// for names that need resolving. This information becomes a dependency
/// graph / DAG that is used to determine the order in which the items should
/// be typechecked.
///
/// Finally the AST in each item is modified to reference the `Expr::Item`
/// instead of plain Identifiers.
/// In theory, this whole process could be built into the intial graph building.
/// However, I'd rather maintain a separation of concerns,
/// even if it would be more efficient to use a single pass over the environment.
#[derive(Default)]
pub struct Resolver {
    env: Environment<UntypedCst>,
    errors: Vec<CompilerErr>,
    pub seen: FxHashSet<ItemId>,
}

#[derive(Default, Debug)]
pub struct TypeFixer {
    unbound_types: BTreeSet<TypeVar>,
    unbound_rows: BTreeSet<RowVar>,
    pub types_to_name: Vec<(TypeVar, Intern<String>)>,
    evidence: Vec<Evidence>,
    type_var_count: usize,
}

impl TypeFixer {
    fn new_type_var(&mut self) -> TypeVar {
        let v = TypeVar(self.type_var_count);
        // let v = TypeVar(n);
        self.type_var_count += 1;
        v
    }

    fn helper(&mut self, t: Spanned<Intern<CstType>>) -> Spanned<Intern<Type>> {
        match *t.0 {
            CstType::Generic(n) => {
                // println!("Generic {}", n.0);
                let v = if let Some((v, _)) =
                    self.types_to_name.iter().find(|(_, name)| ***name == *n.0)
                {
                    // println!("Loaded {v:?}");
                    *v
                } else {
                    let v = self.new_type_var();
                    self.types_to_name.push((v, n.0));
                    self.unbound_types.insert(v);
                    // println!("Created {v:?}");
                    v
                };

                t.convert(Type::Var(v))
            }

            CstType::Func(l, r) => {
                let l = self.helper(l);
                let r = self.helper(r);
                t.convert(Type::Func(l, r))
            }

            CstType::Prod(row) => t.convert(Type::Prod(
                t.convert(Row::Closed(ClosedRow {
                    fields: row.fields,
                    values: row
                        .values
                        .iter()
                        .map(|value_ty| self.helper(*value_ty))
                        .collect::<Vec<_>>()
                        .leak(),
                })),
            )),
            CstType::Sum(row) => t.convert(Type::Sum(
                t.convert(Row::Closed(ClosedRow {
                    fields: row.fields,
                    values: row
                        .values
                        .iter()
                        .map(|value_ty| self.helper(*value_ty))
                        .collect::<Vec<_>>()
                        .leak(),
                })),
            )),
            CstType::Label(l, t) => t.convert(Type::Label(l, self.helper(t))),
            CstType::User(t) => {
                unreachable!("Encountered user type {} after environment", t.0)
            }

            CstType::GenericApp(l, r) => {
                // Should have been reduced by analyze_type; attempt recovery
                // let reduced = self.analyze_type(t); // re-run beta reduction
                // self.helper(reduced, f)
                panic!()
            }
            CstType::GenericFun(l, r) => {
                panic!("Should have been resolved");
                // let l = self.helper(l);
                // self.helper(r)
            }
            CstType::ForAll(t, within) => {
                panic!("Should have been resolved");
            }
            CstType::Primitive(p) => t.convert(Type::Primitive(p)),
            CstType::Hole => t.convert(Type::Hole),
        }
    }
}

impl Resolver {
    pub fn new(env: Environment<UntypedCst>) -> Self {
        Self {
            env,
            errors: Vec::new(),
            seen: FxHashSet::default(),
        }
    }

    pub fn analyze(mut self) -> CompResult<Environment<UntypedAst>> {
        let err_no_main = DynamicErr::new("Could not find a main function")
            .label("not found in any packages", FlareSpan::default());

        let mut converted: FxHashMap<_, _> = std::mem::take(&mut self.env)
            .into_iter()
            .map(|(idx, item)| (idx, self.convert(&item)))
            .collect();

        converted.retain(|k, v| self.seen.contains(&ItemId(k.index())) || v.ident() == "Main");

        if self.errors.is_empty() {
            Ok(converted)
        } else {
            Err(ErrorCollection::new(self.errors).into())
        }
    }

    fn convert_func(&mut self, the_func: FunctionItem<UntypedCst>) -> FunctionItem<UntypedAst> {
        let sig = self.convert_type(the_func.sig, Kind::Func).into();
        let body = self.desugar_cstexpr(the_func.body);
        FunctionItem {
            sig,
            body,
            name: the_func.name,
        }
    }

    fn extract_generics(&self, t: Spanned<Intern<CstType>>, kind: Kind) -> TypeScheme {
        let mut f = TypeFixer::default();
        let ty = f.helper(t);
        TypeScheme {
            unbound_types: f.unbound_types,
            unbound_rows: f.unbound_rows,
            evidence: f.evidence,
            ty,
            types_to_name: f.types_to_name,
            kind,
        }
    }

    fn convert_type(&mut self, t: Spanned<Intern<CstType>>, kind: Kind) -> TypeScheme {
        let t = self.analyze_type(t);
        // dbg!(t);
        self.extract_generics(t, kind)
    }
    fn analyze_type(&mut self, t: Spanned<Intern<CstType>>) -> Spanned<Intern<CstType>> {
        match *t.0 {
            CstType::Func(l, r) => {
                let l = self.analyze_type(l);
                let r = self.analyze_type(r);

                t.modify(CstType::Func(l, r))
            }
            CstType::Label(l, the_r) => {
                let new_t = self.analyze_type(the_r);
                // dbg!(new_t);
                t.modify(CstType::Label(l, new_t))
            }
            CstType::Prod(r) => {
                let new_r = CstClosedRow {
                    values: r
                        .values
                        .iter()
                        .map(|t| -> Spanned<Intern<CstType>> {
                            // let t = self.resolve_name_generic(&n.0)?.get_ty()?.0;
                            self.analyze_type(*t)
                        })
                        .collect::<Vec<_>>()
                        .leak(),
                    fields: r.fields,
                };

                t.convert(CstType::Prod(new_r))
            }
            CstType::Sum(r) => {
                let new_r = CstClosedRow {
                    values: r
                        .values
                        .iter()
                        .map(|t| -> Spanned<Intern<CstType>> {
                            // let t = self.resolve_name_generic(&n.0)?.get_ty()?.0;
                            self.analyze_type(*t)
                        })
                        .collect::<Vec<_>>()
                        .leak(),
                    fields: r.fields,
                };
                t.convert(CstType::Sum(new_r))
            }
            CstType::GenericApp(l, r) => {
                let l = self.analyze_type(l);

                if let CstType::GenericFun(param, body) = *l.0 {
                    let subst = subst_generic_type(body, param.0, r.0);
                    // dbg!(subst);
                    self.analyze_type(subst)
                } else {
                    panic!("Not a generic function")
                    // t.modify(Type::TypeApp(l, r))
                }
            }
            CstType::GenericFun(l, r) => {
                let r = self.analyze_type(r);

                t.convert(CstType::GenericFun(l, r))
            }
            CstType::ForAll(t, within) => {
                todo!()
            }
            CstType::Generic(_) | CstType::Primitive(_) | CstType::Hole => t,
            CstType::User(..) => panic!("Found User type after environment"),
        }
    }

    fn convert(&mut self, item: &Item<UntypedCst>) -> Item<UntypedAst> {
        match item.kind {
            ItemKind::Function(f) => {
                let f = self.convert_func(f);

                Item::new(ItemKind::Function(f))
            }
            ItemKind::Type(n, g, t) => {
                let scheme = self.convert_type(t, Kind::Ty).into();

                Item::new(ItemKind::Type(n, vec![].leak(), scheme))
            }
            ItemKind::Extern { name, args, sig } => {
                let sig = self.convert_type(sig, Kind::Extern(name.0)).into();

                Item::new(ItemKind::Extern { name, args, sig })
            }
            ItemKind::Root => Item::new(ItemKind::Root),
            ItemKind::Filename(f) => Item::new(ItemKind::Filename(f)),
            ItemKind::Dummy(d) => Item::new(ItemKind::Dummy(d)),
        }
    }

    #[allow(unused_variables)]
    pub fn desugar_cstexpr(
        &mut self,
        expr: Spanned<Intern<CstExpr<UntypedCst>>>,
    ) -> Spanned<Intern<Expr<<UntypedCst as Syntax>::Variable>>> {
        todo!()
    }
}
pub fn subst_generic_type(
    subject: Spanned<Intern<CstType>>,
    target: Intern<CstType>,
    replacement: Intern<CstType>,
) -> Spanned<Intern<CstType>> {
    let mut accum: Spanned<Intern<CstType>> = subject;
    // dbg!(t);
    match (*subject.0, *target) {
        // t if t == *target => accum = accum.modify(replacement),
        (CstType::Generic(subj_name), CstType::Generic(target_name))
            if subj_name == target_name =>
        {
            accum = subject.modify(replacement);
        }

        (CstType::Func(l, r), _) => {
            accum = accum.modify(CstType::Func(
                subst_generic_type(l, target, replacement),
                subst_generic_type(r, target, replacement),
            ));
        }
        (CstType::Prod(r), _) => {
            let r = {
                let values: Vec<_> = r
                    .values
                    .iter()
                    .map(|x| subst_generic_type(*x, target, replacement))
                    .collect();

                CstClosedRow {
                    values: values.leak(),
                    ..r
                }
            };
            accum = accum.modify(CstType::Prod(r));
        }

        (CstType::Sum(r), _) => {
            let r = {
                let values: Vec<_> = r
                    .values
                    .iter()
                    .map(|x| subst_generic_type(*x, target, replacement))
                    .collect();

                CstClosedRow {
                    values: values.leak(),
                    ..r
                }
            };
            accum = accum.modify(CstType::Sum(r));
        }

        (CstType::Label(label, value), _) => {
            accum = accum.modify(CstType::Label(
                label,
                subst_generic_type(value, target, replacement),
            ));
        }
        // GENERATED: Claude
        (CstType::GenericFun(param, body), _) => {
            if *param.0 == *target {
                // The parameter shadows the target, don't substitute in body
                accum = subject;
            } else {
                // Safe to substitute in body
                let new_body = subst_generic_type(body, target, replacement);
                accum = accum.modify(CstType::GenericFun(param, new_body));
            }
        }
        _ => accum = subject,
    }

    accum
}
