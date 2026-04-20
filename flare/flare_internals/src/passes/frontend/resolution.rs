use std::collections::BTreeSet;

use internment::Intern;
use itertools::Itertools;
use petgraph::{
    algo::toposort,
    dot::Config,
    graph::NodeIndex,
    visit::{Dfs, IntoNodeReferences, Walker},
};
use rustc_hash::FxHashSet;

type DiGraph<N, E> = petgraph::graph::DiGraph<N, E>;
use crate::{
    passes::frontend::{
        environment::EnvironmentBuilder,
        matchmatrix::{self, DecisionTree, Occ, SigElem},
        typing::{ClosedRow, Evidence, Row, RowVar, Type, TypeScheme, TypeVar},
    },
    resource::{
        errors::{self, CompResult, CompilerErr, DynamicErr, ErrorCollection},
        rep::{
            common::{FlareSpan, Spanned, Syntax},
            frontend::{
                ast::{BinOp, Expr, ItemId, Kind, Label, Untyped, UntypedAst},
                cst::{CstExpr, FieldDef, MatchArm, Pattern, UntypedCst},
                csttypes::{CstClosedRow, CstType},
                entry::{FunctionItem, Item, ItemKind},
                quantifier::QualifierFragment,
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
    env: EnvironmentBuilder<UntypedCst>,
    // new_env: Environment<UntypedAst>
    current_parent: QualifierFragment,
    current_dag_node: Option<NodeIndex>,
    pub dag: DiGraph<DagIdx, ()>,
    main_dag_idx: Option<NodeIndex>,
    errors: Vec<CompilerErr>,
}

#[derive(Default, Debug)]
pub struct TypeFixer {
    unbound_types: BTreeSet<TypeVar>,
    unbound_rows: BTreeSet<RowVar>,
    pub types_to_name: Vec<(TypeVar, Intern<String>)>,
    evidence: Vec<Evidence>,
    type_var_count: usize,
    // seen_row_vars: FxHashMap<FlareSpan, RowVar>,
    // inside_type_fun: TyappState,
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
                unreachable!("Encountered user type {}[{g:?}] after resolution", t.0)
            }

            CstType::GenericApp(l, r) => {
                // Should have been reduced by analyze_type; attempt recovery
                // let reduced = self.analyze_type(t); // re-run beta reduction
                // self.helper(reduced, f)
                panic!()
                // let r = self.convert_type(r);

                // t.convert(Type::TypeApp(l, r))
            }
            CstType::GenericFun(l, r) => {
                // panic!("Should have been resolved");
                // let l = self.helper(l);
                self.helper(r)

                // t.convert(Type::TypeFun(l, r))
            }
            CstType::Particle(p) => t.convert(Type::Particle(p)),
            CstType::Unit => t.convert(Type::Unit),
            CstType::Num => t.convert(Type::Num),
            CstType::Bool => t.convert(Type::Bool),
            CstType::String => t.convert(Type::String),
            CstType::Hole => t.convert(Type::Hole),
        }
        // dbg!(o)
    }
}

type DagIdx = usize;

/// A single destructuring step produced by pattern compilation.
#[derive(Debug)]
struct Binding {
    binder: Untyped,
    /// The RHS: some destructuring of the scrutinee (e.g. Unlabel, or the scrutinee itself)
    value: Spanned<Intern<CstExpr<UntypedCst>>>,
    /// Whether this binding is user-visible (goes into `vars` for name resolution)
    /// or just exists to constrain the type (e.g. inaccessible for nullary ctors)
    user_visible: bool,
}
impl Resolver {
    pub fn new(env: EnvironmentBuilder<UntypedCst>) -> Self {
        Self {
            env,
            current_parent: QualifierFragment::Root,
            current_dag_node: None,
            dag: DiGraph::new(),
            main_dag_idx: None,
            errors: Vec::new(),
        }
    }

    pub fn analyze(mut self) -> CompResult<(EnvironmentBuilder<UntypedAst>, Vec<NodeIndex>)> {
        todo!()
        // let err_no_main = DynamicErr::new("Could not find a main function")
        //     .label("not found in any packages", FlareSpan::default());

        // let g = self.env.graph.clone();
        // let stable_g =
        //     StableDiGraph::from(g).map_owned(|idx, item| self.analyze_item(idx, &item), |idx, e| e);

        // let reachable: FxHashSet<NodeIndex> =
        //     Dfs::new(&self.dag.clone(), self.main_dag_idx.ok_or(err_no_main)?)
        //         .iter(&self.dag)
        //         .collect();

        // let mut sorted: Vec<NodeIndex> = toposort(&self.dag, None)
        //     .into_iter()
        //     .flatten()
        //     .filter(|x| reachable.contains(x))
        //     .map(|x| NodeIndex::new(*self.dag.node_weight(x).expect("Node should exist")))
        //     .collect();

        // let g = stable_g.filter_map_owned(
        //     |idx, item| {
        //         if reachable.contains(&idx) {
        //             Some(self.convert(&item))
        //         } else {
        //             None
        //         }
        //     },
        //     |_, e| Some(e),
        // );
        // // self.debug();
        // // self.env.debug();
        // dbg!(&sorted);
        // sorted.reverse();

        // if self.errors.is_empty() {
        //     let env = Environment::from_graph_and_root(g, self.env.root);
        //     // env.debug();
        //     Ok((env, sorted))
        // } else {
        //     Err(ErrorCollection::new(self.errors).into())
        // }
    }

    // fn analyze_item(&mut self, node_idx: NodeIndex, item: &Item<UntypedCst>) -> Item<UntypedCst> {
    //     let dag_idx = if let Some((node_idx, _)) = self
    //         .dag
    //         .node_references()
    //         .find(|(_, x)| **x == node_idx.index())
    //     {
    //         // dbg!(node_idx);
    //         node_idx
    //     } else {
    //         self.dag
    //             .try_add_node(node_idx.index())
    //             .unwrap_or_else(|_| unreachable!("Graph overflow in resolution"))
    //     };

    //     self.current_dag_node = Some(dag_idx);
    //     self.current_parent = self
    //         .env
    //         .get_parent(node_idx)
    //         .unwrap_or(QualifierFragment::Root);
    //     match item.kind {
    //         ItemKind::Function(f) => {
    //             // dbg!(f.sig);
    //             if *f.name.0 == "main" {
    //                 self.main_dag_idx = Some(dag_idx);
    //             }
    //             let f = self.analyze_func(f, dag_idx);

    //             Item::new(ItemKind::Function(f))
    //         }
    //         ItemKind::Type(n, g, t) => Item::new(ItemKind::Type(n, vec![].leak(), t)),
    //         ItemKind::Extern { name, args, sig } => Item::new(ItemKind::Extern { name, args, sig }),
    //         ItemKind::Root => Item::new(ItemKind::Root),
    //         ItemKind::Filename(f) => Item::new(ItemKind::Filename(f)),
    //         ItemKind::Dummy(d) => Item::new(ItemKind::Dummy(d)),
    //     }
    // }

    // fn in_context<T>(&mut self, mut f: impl FnMut(&mut Self) -> T, dag_idx: NodeIndex) -> T {
    //     let old = self.current_dag_node;
    //     self.current_dag_node = Some(dag_idx);

    //     let out = f(self);
    //     self.current_dag_node = old;
    //     out
    // }

    // fn analyze_func(
    //     &mut self,
    //     the_func: FunctionItem<UntypedCst>,
    //     idx: NodeIndex,
    // ) -> FunctionItem<UntypedCst> {
    //     self.in_context(
    //         |me| {
    //             // me.generic_scope.clear()
    //             // dbg!(the_func.sig);
    //             // dbg!(sig.ty);
    //             let body = me.analyze_expr(the_func.body, &[]);
    //             // me.generic_scope.clear();
    //             FunctionItem {
    //                 body,
    //                 name: the_func.name,
    //                 sig: the_func.sig,
    //             }
    //         },
    //         idx,
    //     )
    // }

    // fn convert_func(&mut self, the_func: FunctionItem<UntypedCst>) -> FunctionItem<UntypedAst> {
    //     let sig = self.convert_type(the_func.sig, Kind::Func);
    //     let body = self.convert_expr(the_func.body);
    //     FunctionItem {
    //         sig,
    //         body,
    //         name: the_func.name,
    //     }
    // }

    // fn extract_generics(&self, t: Spanned<Intern<CstType>>, kind: Kind) -> TypeScheme {
    //     let mut f = TypeFixer::default();
    //     let ty = f.helper(t);
    //     TypeScheme {
    //         unbound_types: f.unbound_types,
    //         unbound_rows: f.unbound_rows,
    //         evidence: f.evidence,
    //         ty,
    //         types_to_name: f.types_to_name,
    //         kind,
    //     }
    // }

    // fn convert_type(&mut self, t: Spanned<Intern<CstType>>, kind: Kind) -> TypeScheme {
    //     let t = self.analyze_type(t);
    //     // dbg!(t);
    //     self.extract_generics(t, kind)
    // }
    // fn analyze_type(&mut self, t: Spanned<Intern<CstType>>) -> Spanned<Intern<CstType>> {
    //     match *t.0 {
    //         CstType::Func(l, r) => {
    //             let l = self.analyze_type(l);
    //             let r = self.analyze_type(r);

    //             t.modify(CstType::Func(l, r))
    //         }
    //         CstType::Label(l, the_r) => {
    //             let new_t = self.analyze_type(the_r);
    //             // dbg!(new_t);
    //             t.modify(CstType::Label(l, new_t))
    //         }
    //         CstType::User(name, instanced_generics) => {
    //             let the_item = self.resolve_name_type((name.0).to_string());

    //             if let Some(item_id) = the_item {
    //                 // self.dag_add(item_id);
    //                 let the_item = self
    //                     .env
    //                     .value(NodeIndex::new(item_id.0))
    //                     .expect("Item has not been defined in the environment");

    //                 if let ItemKind::Type(_, generics, new_t) = the_item.kind {
    //                     // dbg!(new_t);
    //                     // if !instanced_generics.is_empty() {
    //                     let analyzed_instances: Vec<_> = instanced_generics
    //                         .iter()
    //                         .map(|ty| self.analyze_type(*ty))
    //                         .collect();

    //                     let new_t = self.analyze_type(new_t);

    //                     let mut final_t = analyzed_instances
    //                         .into_iter()
    //                         .fold(new_t, |x, y| Spanned(CstType::GenericApp(x, y).into(), x.1));
    //                     final_t.1 = t.1;

    //                     self.analyze_type(final_t)
    //                 } else {
    //                     panic!("not a type")
    //                 }
    //             } else {
    //                 let err = errors::not_defined(name);
    //                 self.errors.push(err);
    //                 name.convert(CstType::Hole)
    //             }
    //         }
    //         CstType::Prod(r) => {
    //             let new_r = CstClosedRow {
    //                 values: r
    //                     .values
    //                     .iter()
    //                     .map(|t| -> Spanned<Intern<CstType>> {
    //                         // let t = self.resolve_name_generic(&n.0)?.get_ty()?.0;
    //                         self.analyze_type(*t)
    //                     })
    //                     .collect::<Vec<_>>()
    //                     .leak(),
    //                 fields: r.fields,
    //             };

    //             t.convert(CstType::Prod(new_r))
    //         }
    //         CstType::Sum(r) => {
    //             let new_r = CstClosedRow {
    //                 values: r
    //                     .values
    //                     .iter()
    //                     .map(|t| -> Spanned<Intern<CstType>> {
    //                         // let t = self.resolve_name_generic(&n.0)?.get_ty()?.0;
    //                         self.analyze_type(*t)
    //                     })
    //                     .collect::<Vec<_>>()
    //                     .leak(),
    //                 fields: r.fields,
    //             };
    //             t.convert(CstType::Sum(new_r))
    //         }
    //         CstType::GenericApp(l, r) => {
    //             let l = self.analyze_type(l);
    //             // let r = self.analyze_type(r);
    //             if let CstType::GenericFun(param, body) = *l.0 {
    //                 let subst = subst_generic_type(body, param.0, r.0);
    //                 // dbg!(subst);
    //                 self.analyze_type(subst)
    //             } else {
    //                 panic!("Not a generic function")
    //                 // t.modify(Type::TypeApp(l, r))
    //             }
    //         }
    //         CstType::GenericFun(l, r) => {
    //             let l = self.analyze_type(l);
    //             let r = self.analyze_type(r);

    //             t.convert(CstType::GenericFun(l, r))
    //         }
    //         CstType::Generic(_)
    //         | CstType::Particle(_)
    //         | CstType::Unit
    //         | CstType::Num
    //         | CstType::Bool
    //         | CstType::String
    //         | CstType::Hole => t,
    //     }
    // }

    // #[allow(unused_variables)]
    // pub fn analyze_expr(
    //     &mut self,
    //     expr: Spanned<Intern<CstExpr<UntypedCst>>>,
    //     // vars: &[(Intern<String>, Spanned<Intern<CstExpr<UntypedCst>>>)],
    //     vars: &[Intern<String>],
    // ) -> Spanned<Intern<CstExpr<UntypedCst>>> {
    //     // dbg!(&expr);

    //     match *expr.0 {
    //         CstExpr::Ident(u) => {
    //             if vars.iter().rev().find(|n| **n == u.0.0).is_some() {
    //                 expr
    //             } else {
    //                 // dbg!(expr);
    //                 self.resolve_name(u.0)
    //             }
    //         }
    //         CstExpr::ProductConstructor { fields } => {
    //             let mut new_vars = vars.to_vec();
    //             for field in fields.iter() {
    //                 new_vars.push(field.name.0);
    //             }
    //             let fields: Vec<FieldDef<_>> = fields
    //                 .iter()
    //                 .map(|field| FieldDef {
    //                     value: self.analyze_expr(field.value, &new_vars),
    //                     ty: field.ty.map(|ty| self.analyze_type(ty)),
    //                     ..*field
    //                 })
    //                 .collect();
    //             expr.modify(CstExpr::ProductConstructor {
    //                 fields: fields.as_slice().into(),
    //             })
    //         }
    //         CstExpr::Label(l, v) => {
    //             let v = self.analyze_expr(v, vars);
    //             expr.modify(CstExpr::Label(l, v))
    //         }
    //         CstExpr::Unlabel(v, l) => {
    //             let v = self.analyze_expr(v, vars);
    //             expr.modify(CstExpr::Unlabel(v, l))
    //         }
    //         CstExpr::Pat(spanned) => todo!(),
    //         CstExpr::Mul(l, r) => {
    //             let l = self.analyze_expr(l, vars);
    //             let r = self.analyze_expr(r, vars);
    //             expr.modify(CstExpr::Mul(l, r))
    //         }
    //         CstExpr::Div(l, r) => {
    //             let l = self.analyze_expr(l, vars);
    //             let r = self.analyze_expr(r, vars);
    //             expr.modify(CstExpr::Div(l, r))
    //         }
    //         CstExpr::Add(l, r) => {
    //             let l = self.analyze_expr(l, vars);
    //             let r = self.analyze_expr(r, vars);
    //             expr.modify(CstExpr::Add(l, r))
    //         }
    //         CstExpr::Sub(l, r) => {
    //             let l = self.analyze_expr(l, vars);
    //             let r = self.analyze_expr(r, vars);
    //             expr.modify(CstExpr::Sub(l, r))
    //         }
    //         CstExpr::Comparison(l, comparison_op, r) => {
    //             let l = self.analyze_expr(l, vars);
    //             let r = self.analyze_expr(r, vars);
    //             expr.convert(CstExpr::Comparison(l, comparison_op, r))
    //         }
    //         CstExpr::Call(func, arg) => {
    //             let func = self.analyze_expr(func, vars);

    //             let arg = self.analyze_expr(arg, vars);
    //             if let CstExpr::Item(id, Kind::Ty) = *func.0 {
    //                 todo!(
    //                     "This would be a type constructor, but it lowk isn't being used right now"
    //                 )
    //             };
    //             expr.convert(CstExpr::Call(func, arg))
    //         }
    //         CstExpr::FieldAccess(..) => self.resolve_field_access(expr, vars),
    //         CstExpr::If(cond, then, otherwise) => {
    //             let cond = self.analyze_expr(cond, vars);
    //             let then_vars = vars;
    //             let then = self.analyze_expr(then, then_vars);
    //             let otherwise_vars = vars;
    //             let otherwise = self.analyze_expr(otherwise, otherwise_vars);
    //             expr.modify(CstExpr::If(cond, then, otherwise))
    //         }
    //         CstExpr::Match(matchee, branches) => {
    //             let matchee = self.analyze_expr(matchee, vars);

    //             let branches: Vec<_> = branches
    //                 .iter()
    //                 .map(|b| self.resolve_branch(b.pat, b.body, vars))
    //                 .collect();
    //             assert!(!branches.is_empty());
    //             expr.modify(CstExpr::Match(matchee, branches.leak()))
    //         }
    //         CstExpr::Lambda(arg, body) => self.resolve_lambda(expr, arg, body, vars),
    //         CstExpr::Let(id, body, and_in) => self.resolve_let(expr, id, body, and_in, vars),
    //         CstExpr::Number(n) => expr.convert(CstExpr::Number(n)),
    //         CstExpr::String(s) => expr.convert(CstExpr::String(s)),
    //         CstExpr::Bool(b) => expr.convert(CstExpr::Bool(b)),
    //         CstExpr::Unit => expr.convert(CstExpr::Unit),
    //         CstExpr::Particle(p) => expr.convert(CstExpr::Particle(p)),
    //         CstExpr::Hole(v) => expr.convert(CstExpr::Hole(v)),
    //         CstExpr::Item(item_id, kind) => expr.convert(CstExpr::Item(item_id, kind)),
    //         CstExpr::Myself => todo!(),
    //         CstExpr::MethodAccess { obj, prop, method } => todo!(),
    //     }
    // }

    // fn resolve_branch(
    //     &mut self,
    //     pat: Spanned<Intern<Pattern<UntypedCst>>>,
    //     body: Spanned<Intern<CstExpr<UntypedCst>>>,
    //     vars: &[Intern<String>],
    // ) -> MatchArm<UntypedCst> {
    //     // The branch becomes: λparam. <lets> in body
    //     // `param` is the lambda binder that receives the matchee at the call site.
    //     let param = Untyped(pat.convert("%match_arg%".to_string()));
    //     let branch_arg: Spanned<Intern<CstExpr<UntypedCst>>> = pat.convert(CstExpr::Ident(param));

    //     let bindings = self.compile_pattern(pat, branch_arg);
    //     dbg!(&bindings);
    //     // Extend vars with user-visible bindings so analyze_expr can resolve them.
    //     // Each maps the binder name -> its destructured value expression.
    //     let mut branch_vars: Vec<Intern<String>> = vars.to_vec();
    //     for binding in &bindings {
    //         // dbg!(binding);
    //         if binding.user_visible {
    //             branch_vars.push(binding.binder.0.0);
    //         }
    //     }
    //     // dbg!(&bindings);

    //     let body = self.analyze_expr(body, &branch_vars);
    //     MatchArm { pat, body }
    // }

    // /// Recursively compile a pattern into a flat sequence of let-bindings,
    // /// threading the current scrutinee expression down through nested constructors.
    // fn compile_pattern(
    //     &mut self,
    //     p: Spanned<Intern<Pattern<UntypedCst>>>,
    //     branch_arg: Spanned<Intern<CstExpr<UntypedCst>>>,
    // ) -> Vec<Binding> {
    //     match *p.0 {
    //         Pattern::Hole(_) => vec![],
    //         Pattern::Any => vec![],
    //         Pattern::Var(v) => vec![Binding {
    //             binder: v,
    //             value: branch_arg,
    //             user_visible: true,
    //         }],
    //         Pattern::Unit => vec![],
    //         Pattern::Variant(label, inner_pat) => {
    //             let inner_val: Spanned<Intern<CstExpr<UntypedCst>>> =
    //                 branch_arg.convert(CstExpr::Unlabel(branch_arg, label));
    //             self.compile_pattern(inner_pat, inner_val)
    //         }
    //         Pattern::Number(_) => vec![],
    //         Pattern::String(_) => todo!(),
    //         Pattern::Bool(_) => todo!(),
    //         Pattern::Particle(spanned) => todo!(),
    //         Pattern::Record { fields, open } => todo!(),
    //         Pattern::Tuple(elems) => elems
    //             .iter()
    //             .enumerate()
    //             .flat_map(|(idx, p)| {
    //                 let idx_label = Label(p.convert(idx.to_string()));
    //                 let inner_val = branch_arg.convert(CstExpr::Unlabel(branch_arg, idx_label));
    //                 self.compile_pattern(*p, inner_val)
    //             })
    //             .collect(),
    //         Pattern::At(_, spanned) => todo!(),
    //         Pattern::Or(patterns) => todo!(),
    //         Pattern::Guard(spanned, spanned1) => todo!(),
    //     }
    // }
    // fn convert(&mut self, item: &Item<UntypedCst>) -> Item<UntypedAst> {
    //     match item.kind {
    //         ItemKind::Function(f) => {
    //             let f = self.convert_func(f);

    //             Item::new(ItemKind::Function(f))
    //         }
    //         ItemKind::Type(n, g, t) => {
    //             let scheme = self.convert_type(t, Kind::Ty);

    //             Item::new(ItemKind::Type(n, vec![].leak(), scheme))
    //         }
    //         ItemKind::Extern { name, args, sig } => {
    //             let sig = self.convert_type(sig, Kind::Extern(name.0));

    //             Item::new(ItemKind::Extern { name, args, sig })
    //         }
    //         ItemKind::Root => Item::new(ItemKind::Root),
    //         ItemKind::Filename(f) => Item::new(ItemKind::Filename(f)),
    //         ItemKind::Dummy(d) => Item::new(ItemKind::Dummy(d)),
    //     }
    // }

    // #[allow(unused_variables)]
    // pub fn convert_expr(
    //     &mut self,
    //     expr: Spanned<Intern<CstExpr<UntypedCst>>>,
    // ) -> Spanned<Intern<Expr<<UntypedCst as Syntax>::Variable>>> {
    //     match *expr.0 {
    //         CstExpr::Ident(u) => expr.convert(Expr::Ident(u)),
    //         CstExpr::ProductConstructor { fields } => fields
    //             .iter()
    //             .map(|l| {
    //                 let val = self.convert_expr(l.value);
    //                 expr.convert(Expr::Label(Label(l.name), val))
    //             })
    //             .reduce(|l, r| expr.convert(Expr::Concat(l, r)))
    //             .unwrap(),
    //         CstExpr::Label(l, v) => {
    //             // dbg!(vars);
    //             // dbg!(expr);
    //             let v = self.convert_expr(v);
    //             expr.convert(Expr::Label(l, v))
    //         }
    //         CstExpr::Unlabel(v, l) => {
    //             let v = self.convert_expr(v);
    //             expr.convert(Expr::Unlabel(v, l))
    //         }
    //         CstExpr::Pat(spanned) => todo!(),
    //         CstExpr::Mul(l, r) => {
    //             let l = self.convert_expr(l);
    //             let r = self.convert_expr(r);
    //             expr.convert(Expr::Mul(l, r))
    //         }
    //         CstExpr::Div(l, r) => {
    //             let l = self.convert_expr(l);
    //             let r = self.convert_expr(r);
    //             expr.convert(Expr::Div(l, r))
    //         }
    //         CstExpr::Add(l, r) => {
    //             let l = self.convert_expr(l);
    //             let r = self.convert_expr(r);
    //             expr.convert(Expr::Add(l, r))
    //         }
    //         CstExpr::Sub(l, r) => {
    //             let l = self.convert_expr(l);
    //             let r = self.convert_expr(r);
    //             expr.convert(Expr::Sub(l, r))
    //         }
    //         CstExpr::Comparison(l, comparison_op, r) => {
    //             let l = self.convert_expr(l);
    //             let r = self.convert_expr(r);
    //             expr.convert(Expr::Comparison(l, comparison_op, r))
    //         }
    //         CstExpr::Call(func, arg) => {
    //             let func = self.convert_expr(func);

    //             let arg = self.convert_expr(arg);
    //             if let Expr::Item(id, Kind::Ty) = *func.0 {
    //                 todo!(
    //                     "This would be a type constructor, but it lowk isn't being used right now"
    //                 )
    //             };
    //             expr.convert(Expr::Call(func, arg))
    //         }
    //         CstExpr::FieldAccess(l, r) => {
    //             let l = self.convert_expr(l);
    //             expr.convert(Expr::Access(l, r))
    //         }
    //         CstExpr::If(cond, then, otherwise) => {
    //             let cond = self.convert_expr(cond);
    //             let then = self.convert_expr(then);
    //             let otherwise = self.convert_expr(otherwise);
    //             expr.convert(Expr::If(cond, then, otherwise))
    //         }
    //         CstExpr::Match(matchee, branches) => {
    //             // let matchee = self.convert_expr(matchee, vars);
    //             // let base_expr = matchee.convert(CstExpr::Let(
    //             //     UntypedCst
    //             // (matchee.convert("%matchee".to_string())),
    //             //     matchee,
    //             //     expr,
    //             // ));
    //             let (patterns, actions): (Vec<_>, Vec<_>) =
    //                 branches.iter().map(|b| (b.pat, b.body)).unzip();
    //             let patterns: Vec<_> = patterns.iter().map(|p| *p.0).collect();
    //             // dbg!(&patterns);

    //             let decision_tree = matchmatrix::compile(&patterns);
    //             decision_tree.print(0);
    //             let t = self.translate_decision_tree(matchee, decision_tree, &actions);
    //             println!("match-tree: {t}");
    //             t
    //         }
    //         CstExpr::Lambda(arg, body) => {
    //             let body = self.convert_expr(body);
    //             expr.convert(Expr::Lambda(arg, body))
    //         }
    //         CstExpr::Let(pat, body, and_in) => {
    //             let (patterns, actions) = (vec![pat], vec![and_in]);
    //             let patterns: Vec<_> = patterns.iter().map(|p| *p.0).collect();

    //             let decision_tree = matchmatrix::compile(&patterns);
    //             decision_tree.print(0);
    //             let t = self.translate_decision_tree(body, decision_tree, &actions);

    //             println!("let-tree {t}");
    //             t
    //         }
    //         CstExpr::Number(n) => expr.convert(Expr::Number(n)),
    //         CstExpr::String(s) => expr.convert(Expr::String(s)),
    //         CstExpr::Bool(b) => expr.convert(Expr::Bool(b)),
    //         CstExpr::Unit => expr.convert(Expr::Unit),
    //         CstExpr::Particle(p) => expr.convert(Expr::Particle(p)),
    //         CstExpr::Hole(v) => expr.convert(Expr::Hole(v)),
    //         CstExpr::Item(item_id, kind) => expr.convert(Expr::Item(item_id, kind)),
    //         CstExpr::Myself => todo!(),
    //         CstExpr::MethodAccess { obj, prop, method } => todo!(),
    //     }
    // }

    // fn resolve_let(
    //     &mut self,
    //     expr: Spanned<Intern<CstExpr<UntypedCst>>>,
    //     pat: Spanned<Intern<Pattern<UntypedCst>>>,
    //     def: Spanned<Intern<CstExpr<UntypedCst>>>,
    //     body: Spanned<Intern<CstExpr<UntypedCst>>>,
    //     vars: &[Intern<String>],
    // ) -> Spanned<Intern<CstExpr<UntypedCst>>> {
    //     let def = self.analyze_expr(def, vars);
    //     let MatchArm { pat, body } = self.resolve_branch(pat, body, vars);
    //     expr.modify(CstExpr::Let(pat, def, body))
    // }

    // fn resolve_lambda(
    //     &mut self,
    //     expr: Spanned<Intern<CstExpr<UntypedCst>>>,
    //     arg: Untyped,
    //     body: Spanned<Intern<CstExpr<UntypedCst>>>,
    //     vars: &[Intern<String>],
    // ) -> Spanned<Intern<CstExpr<UntypedCst>>> {
    //     let new_vars = &[vars, &[arg.0.0]].concat();
    //     let body = self.analyze_expr(body, new_vars);
    //     // *vars.iter_mut().find(|x| x.0 == arg.0 .0).unwrap() = (arg.0 .0, body);
    //     expr.convert(CstExpr::Lambda(arg, body))
    // }

    // fn resolve_field_access(
    //     &mut self,
    //     expr: Spanned<Intern<CstExpr<UntypedCst>>>,
    //     vars: &[Intern<String>],
    // ) -> Spanned<Intern<CstExpr<UntypedCst>>> {
    //     let CstExpr::FieldAccess(l, r) = *expr.0 else {
    //         panic!("Not a field access")
    //     };
    //     let l = self.analyze_expr(l, vars);
    //     if let CstExpr::Item(_, _) = *l.0 {
    //         self.resolve_name(r.0)
    //     } else {
    //         expr.convert(CstExpr::FieldAccess(l, r))
    //     }
    // }

    // fn translate_dtree_occ(
    //     &mut self,
    //     occ: Occ,
    //     matchee: Spanned<Intern<CstExpr<UntypedCst>>>,
    // ) -> Spanned<Intern<Expr<Untyped>>> {
    //     match occ {
    //         Occ::Base => self.convert_expr(matchee),
    //         Occ::Proj(occ, l) => {
    //             let subtree = self.translate_dtree_occ(*occ, matchee);
    //             matchee.convert(Expr::Access(subtree, l))
    //         }
    //         Occ::Unwrap(occ, l) => {
    //             let subtree = self.translate_dtree_occ(*occ, matchee);
    //             matchee.convert(Expr::Unlabel(subtree, l))
    //         }
    //     }
    // }

    // fn translate_sigelem(
    //     &self,
    //     sigelem: SigElem,
    //     matchee_span: FlareSpan,
    // ) -> Spanned<Intern<Expr<Untyped>>> {
    //     match sigelem {
    //         SigElem::Label(label) => unimplemented!("use translate_sigelem_pat"),
    //         SigElem::Num(f) => Spanned(Expr::Number(f).into(), matchee_span),
    //         SigElem::String(intern) => todo!(),
    //     }
    // }

    // fn translate_decision_tree(
    //     &mut self,
    //     matchee: Spanned<Intern<CstExpr<UntypedCst>>>,
    //     tree: DecisionTree,

    //     actions: &Vec<Spanned<Intern<CstExpr<UntypedCst>>>>,
    // ) -> Spanned<Intern<Expr<Untyped>>> {
    //     match tree {
    //         DecisionTree::Fail => todo!(),
    //         DecisionTree::Leaf(i) => self.convert_expr(actions[i]),
    //         DecisionTree::Switch {
    //             occ,
    //             cases,
    //             default,
    //         } => {
    //             let case_lambdas = cases
    //                 .into_iter()
    //                 .enumerate()
    //                 .map(|(i, (label, subtree))| {
    //                     let body = self.translate_decision_tree(matchee, subtree, actions);
    //                     let param = Untyped(matchee.convert(i.to_string()));
    //                     let arg = matchee.convert(Expr::Ident(param));

    //                     let unlabeling: Spanned<Intern<Expr<Untyped>>> =
    //                         matchee.convert(Expr::Unlabel(arg, label));
    //                     // let the_let = matchee.convert(Expr::Let(, (), ()));
    //                     // todo!()
    //                     body
    //                 })
    //                 .collect_vec();

    //             let branches = case_lambdas
    //                 .into_iter()
    //                 .reduce(|l, r| Spanned(Expr::Branch(l, r).into(), l.1.union(r.1)))
    //                 .expect("Branches was empty; match has no arms");

    //             let matchee = self.translate_dtree_occ(occ, matchee);
    //             matchee.convert(Expr::Call(branches, matchee))
    //         }
    //         DecisionTree::IfEq {
    //             occ,
    //             lit,
    //             then,
    //             else_,
    //         } => {
    //             let occ = self.translate_dtree_occ(occ, matchee);
    //             let lit = self.translate_sigelem(lit, matchee.1);
    //             let then = self.translate_decision_tree(matchee, *then, actions);
    //             let else_ = self.translate_decision_tree(matchee, *else_, actions);
    //             matchee.convert(Expr::If(
    //                 matchee.convert(Expr::Comparison(occ, BinOp::Eq, lit)),
    //                 then,
    //                 else_,
    //             ))
    //         }
    //     }
    // }

    // #[allow(dead_code, clippy::unwrap_used, clippy::dbg_macro)]
    // // #[deprecated]
    // /// Pretty-print GraphViz for the internal state of the dependancy graph.
    // fn debug(&self) {
    //     let render = |_, (_, v): (_, &usize)| {
    //         format!(
    //             "label = \"{}\"",
    //             self.env.value(NodeIndex::from(*v as u32)).unwrap().ident()
    //         )
    //     };
    //     let dot = petgraph::dot::Dot::with_attr_getters(
    //         &self.dag,
    //         &[
    //             Config::EdgeNoLabel,
    //             Config::NodeNoLabel,
    //             Config::RankDir(petgraph::dot::RankDir::LR),
    //         ],
    //         &|_, _| String::new(),
    //         &render,
    //     );
    //     dbg!(dot);
    // }

    // fn dag_add(&mut self, env_node: usize) {
    //     let n = if let Some((idx, _)) = self.dag.node_references().find(|(_, x)| **x == env_node) {
    //         idx
    //     } else {
    //         self.dag.add_node(env_node)
    //     };

    //     if let Some(current) = self.current_dag_node
    //         && n != current
    //     {
    //         self.dag.update_edge(current, n, ());
    //     }
    // }

    // fn search_masterenv(&mut self, q: &QualifierFragment) -> Option<ItemId> {
    //     let search = self.env.get_from_context(q, &self.current_parent);
    //     search.map(|node| {
    //         // if !matches!(q, QualifierFragment::Type(_)) {
    //         self.dag_add(node.index());
    //         // }
    //         ItemId(node.index())
    //     })
    // }

    // fn resolve_name(
    //     &mut self,
    //     name: Spanned<Intern<String>>,
    // ) -> Spanned<Intern<CstExpr<UntypedCst>>> {
    //     if let Some(e) = self.search_masterenv(&QualifierFragment::Func(name.to_string())) {
    //         name.convert(CstExpr::Item(e, Kind::Func))
    //     } else if let Some(e) = self.search_masterenv(&QualifierFragment::Type(name.to_string())) {
    //         name.convert(CstExpr::Item(e, Kind::Ty))
    //     } else if let Some(e) = self.search_masterenv(&QualifierFragment::Package(name.to_string()))
    //     {
    //         name.convert(CstExpr::Item(e, Kind::Package))
    //     } else {
    //         let err = errors::not_defined(name);
    //         self.errors.push(err);
    //         name.convert(CstExpr::Hole(Untyped(name)))
    //     }
    // }

    // fn resolve_name_type(&mut self, name: String) -> Option<ItemId> {
    //     self.search_masterenv(&QualifierFragment::Type(name))
    // }
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
            accum = accum.modify(CstType::Prod(r))
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
            accum = accum.modify(CstType::Sum(r))
        }

        (CstType::Label(label, value), _) => {
            accum = accum.modify(CstType::Label(
                label,
                subst_generic_type(value, target, replacement),
            ))
        }
        // GENERATED: Claude
        (CstType::GenericFun(param, body), _) => {
            if *param.0 == *target {
                // The parameter shadows the target, don't substitute in body
                accum = subject
            } else {
                // Safe to substitute in body
                let new_body = subst_generic_type(body, target, replacement);
                accum = accum.modify(CstType::GenericFun(param, new_body))
            }
        }
        // (CstType::GenericApp(l, r), _) => {
        //     accum = accum.modify(CstType::GenericApp(
        //         subst_generic_type(l, target, replacement),
        //         subst_generic_type(r, target, replacement),
        //     ));
        // }
        _ => accum = subject,
    }

    accum
}
