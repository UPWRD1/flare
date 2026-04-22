use std::collections::BTreeSet;

use internment::Intern;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    passes::frontend::{
        environment::Environment,
        matchmatrix::{self, DecisionTree, Occ, SigElem},
        typing::{ClosedRow, Evidence, Row, RowVar, Type, TypeScheme, TypeVar},
    },
    resource::{
        errors::{CompResult, CompilerErr, DynamicErr, ErrorCollection},
        rep::{
            common::{FlareSpan, Spanned, Syntax},
            frontend::{
                ast::{BinOp, Direction, Expr, ItemId, Kind, Label, Untyped, UntypedAst},
                cst::{CstExpr, Field, UntypedCst},
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
            CstType::User(t, g) => {
                unreachable!("Encountered user type {}[{g:?}] after environment", t.0)
            }

            CstType::Item(t, g) => {
                unreachable!("Encountered item type {}[{g:?}] after resolution", t.0)
            }

            CstType::GenericApp(l, r) => {
                // Should have been reduced by analyze_type; attempt recovery
                // let reduced = self.analyze_type(t); // re-run beta reduction
                // self.helper(reduced, f)
                panic!()
            }
            CstType::GenericFun(l, r) => {
                // panic!("Should have been resolved");
                // let l = self.helper(l);
                self.helper(r)
            }
            CstType::Particle(p) => t.convert(Type::Particle(p)),
            CstType::Unit => t.convert(Type::Unit),
            CstType::Num => t.convert(Type::Num),
            CstType::Bool => t.convert(Type::Bool),
            CstType::String => t.convert(Type::String),
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

        converted.retain(|k, v| self.seen.contains(&ItemId(k.index())) || v.ident() == "main");

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
            CstType::Item(id, instanced_generics) => {
                todo!()
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
                // let r = self.analyze_type(r);
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
                let l = self.analyze_type(l);
                let r = self.analyze_type(r);

                t.convert(CstType::GenericFun(l, r))
            }
            CstType::Generic(_)
            | CstType::Particle(_)
            | CstType::Unit
            | CstType::Num
            | CstType::Bool
            | CstType::String
            | CstType::Hole => t,
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
        match *expr.0 {
            CstExpr::Ident(u) => expr.convert(Expr::Ident(u)),
            CstExpr::ProductConstructor { fields } => fields
                .iter()
                .map(|field| match *field {
                    Field::Def(field) => {
                        let val = self.desugar_cstexpr(field.value);
                        expr.convert(Expr::Label(Label(field.name), val))
                    }
                    Field::Macro(field_macro) => todo!(),
                })
                .reduce(|l, r| expr.convert(Expr::Concat(l, r)))
                .unwrap(),
            CstExpr::VariantConstructor { name, value } => {
                let value = if let Some(value) = value {
                    self.desugar_cstexpr(value)
                } else {
                    name.convert(Expr::Unit)
                };

                let label = expr.convert(Expr::Label(Label(name), value));
                expr.convert(Expr::Inject(Direction::Right, label))
            }
            CstExpr::Bin(l, op, r) => {
                let l = self.desugar_cstexpr(l);
                let r = self.desugar_cstexpr(r);
                expr.convert(Expr::Bin(l, op, r))
            }
            CstExpr::Call(func, arg) => {
                let func = self.desugar_cstexpr(func);

                let arg = self.desugar_cstexpr(arg);
                if let Expr::Item(id) = *func.0 {
                    todo!(
                        "This would be a type constructor, but it lowk isn't being used right now"
                    )
                }
                expr.convert(Expr::Call(func, arg))
            }
            CstExpr::FieldAccess(l, r) => {
                let l = self.desugar_cstexpr(l);
                expr.convert(Expr::Access(l, r))
            }
            CstExpr::Match(matchee, branches) => {
                let (patterns, actions): (Vec<_>, Vec<_>) =
                    branches.iter().map(|b| (b.pat, b.body)).unzip();
                let patterns: Vec<_> = patterns.iter().map(|p| *p.0).collect();
                let decision_tree = matchmatrix::compile(&patterns);
                decision_tree.print(0);
                let t = self.translate_decision_tree(matchee, decision_tree, &actions);
                println!("match-tree: {t}");
                t
            }
            CstExpr::Lambda(arg, body) => {
                let body = self.desugar_cstexpr(body);
                expr.convert(Expr::Lambda(arg, body))
            }
            CstExpr::Let(pat, body, and_in) => {
                let (patterns, actions) = (vec![pat], vec![and_in]);
                let patterns: Vec<_> = patterns.iter().map(|p| *p.0).collect();

                let decision_tree = matchmatrix::compile(&patterns);
                decision_tree.print(0);
                let t = self.translate_decision_tree(body, decision_tree, &actions);

                println!("let-tree {t}");
                t
            }
            CstExpr::Number(n) => expr.convert(Expr::Number(n)),
            CstExpr::String(s) => expr.convert(Expr::String(s)),
            CstExpr::Bool(b) => expr.convert(Expr::Bool(b)),
            CstExpr::Unit => expr.convert(Expr::Unit),
            CstExpr::Particle(p) => expr.convert(Expr::Particle(p)),
            CstExpr::Hole(v) => expr.convert(Expr::Hole(v)),
            CstExpr::Item(item_id) => {
                self.seen.insert(item_id);
                expr.convert(Expr::Item(item_id))
            } // CstExpr::Myself => todo!(),
              // CstExpr::MethodAccess { obj, prop, method } => todo!(),
        }
    }

    fn translate_dtree_occ(
        &mut self,
        occ: Occ,
        matchee: Spanned<Intern<CstExpr<UntypedCst>>>,
    ) -> Spanned<Intern<Expr<Untyped>>> {
        match occ {
            Occ::Base => self.desugar_cstexpr(matchee),
            Occ::Proj(occ, l) => {
                let subtree = self.translate_dtree_occ(*occ, matchee);
                matchee.convert(Expr::Access(subtree, l))
            }
            Occ::Unwrap(occ, l) => {
                let subtree = self.translate_dtree_occ(*occ, matchee);
                matchee.convert(Expr::Unlabel(subtree, l))
            }
        }
    }

    fn translate_sigelem(
        &self,
        sigelem: SigElem,
        matchee_span: FlareSpan,
    ) -> Spanned<Intern<Expr<Untyped>>> {
        match sigelem {
            SigElem::Label(label) => unimplemented!("use translate_sigelem_pat"),
            SigElem::Num(f) => Spanned(Expr::Number(f).into(), matchee_span),
            SigElem::String(intern) => todo!(),
        }
    }

    fn translate_decision_tree(
        &mut self,
        matchee: Spanned<Intern<CstExpr<UntypedCst>>>,
        tree: DecisionTree,

        actions: &Vec<Spanned<Intern<CstExpr<UntypedCst>>>>,
    ) -> Spanned<Intern<Expr<Untyped>>> {
        match tree {
            DecisionTree::Fail => todo!(),
            DecisionTree::Leaf(i) => self.desugar_cstexpr(actions[i]),
            DecisionTree::Switch {
                occ,
                cases,
                default,
            } => {
                let case_lambdas = cases
                    .into_iter()
                    .enumerate()
                    .map(|(i, (label, subtree))| {
                        let body = self.translate_decision_tree(matchee, subtree, actions);
                        let param = Untyped(matchee.convert(i.to_string()));
                        let arg = matchee.convert(Expr::Ident(param));

                        let unlabeling: Spanned<Intern<Expr<Untyped>>> =
                            matchee.convert(Expr::Unlabel(arg, label));
                        body
                    })
                    .collect_vec();

                let branches = case_lambdas
                    .into_iter()
                    .reduce(|l, r| Spanned(Expr::Branch(l, r).into(), l.1.union(r.1)))
                    .expect("Branches was empty; match has no arms");

                let matchee = self.translate_dtree_occ(occ, matchee);
                matchee.convert(Expr::Call(branches, matchee))
            }
            DecisionTree::IfEq {
                occ,
                lit,
                then,
                else_,
            } => {
                let occ = self.translate_dtree_occ(occ, matchee);
                let lit = self.translate_sigelem(lit, matchee.1);
                let then = self.translate_decision_tree(matchee, *then, actions);
                let else_ = self.translate_decision_tree(matchee, *else_, actions);
                matchee.convert(Expr::If(
                    matchee.convert(Expr::Bin(occ, BinOp::Eq, lit)),
                    then,
                    else_,
                ))
            }
        }
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
