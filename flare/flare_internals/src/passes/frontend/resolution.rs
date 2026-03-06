use std::collections::BTreeSet;

use chumsky::span::{SimpleSpan, Span};
use internment::Intern;
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
        environment::Environment,
        typing::{ClosedRow, Evidence, Row, RowVar, Type, TypeScheme, TypeVar},
    },
    resource::{
        errors::{self, CompResult, CompilerErr, DynamicErr, ErrorCollection},
        rep::{
            common::{Ident, Spanned},
            frontend::{
                ast::{Expr, ItemId, Kind, Label, Untyped, UntypedAst},
                cst::{CstExpr, MatchArm, Pattern, UntypedCst},
                csttypes::{CstClosedRow, CstType},
                entry::{FunctionItem, Item, ItemKind, PackageEntry},
                quantifier::QualifierFragment,
            },
        },
    },
};

/// The name resolution engine.
/// Once the environment is built from the parser, the `Resolver` takes ownership
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
pub struct Resolver {
    env: Environment<UntypedCst>,
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
    // seen_row_vars: FxHashMap<NodeId, RowVar>,
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
    value: Spanned<Intern<Expr<Untyped>>>,
    /// Whether this binding is user-visible (goes into `vars` for name resolution)
    /// or just exists to constrain the type (e.g. inaccessible for nullary ctors)
    user_visible: bool,
}
impl Resolver {
    pub fn new(env: Environment<UntypedCst>) -> Self {
        Self {
            env,
            current_parent: QualifierFragment::Root,
            current_dag_node: None,
            dag: DiGraph::new(),
            main_dag_idx: None,
            errors: Vec::new(),
        }
    }

    pub fn analyze(mut self) -> CompResult<(Environment<UntypedAst>, Vec<NodeIndex>)> {
        let err_no_main = DynamicErr::new("Could not find a main function")
            .label("not found in any packages", SimpleSpan::default());

        let g = self
            .env
            .graph
            .clone()
            .map(|idx, item| self.analyze_item(idx, item), |idx, e| *e);
        let reachable: FxHashSet<NodeIndex> =
            Dfs::new(&self.dag.clone(), self.main_dag_idx.ok_or(err_no_main)?)
                .iter(&self.dag)
                .collect();
        let mut sorted: Vec<NodeIndex> = toposort(&self.dag, None)
            .into_iter()
            .flatten()
            .filter(|x| reachable.contains(x))
            .map(|x| NodeIndex::new(*self.dag.node_weight(x).expect("Node should exist")))
            .collect();
        sorted.reverse();
        if self.errors.is_empty() {
            let env = Environment::from_graph_and_root(g, self.env.root);
            Ok((env, sorted))
        } else {
            Err(ErrorCollection::new(self.errors).into())
        }
    }

    fn analyze_item(&mut self, node_idx: NodeIndex, item: &Item<UntypedCst>) -> Item<UntypedAst> {
        let dag_idx = if let Some((node_idx, _)) = self
            .dag
            .node_references()
            .find(|(_, x)| **x == node_idx.index())
        {
            // dbg!(node_idx);
            node_idx
        } else {
            self.dag
                .try_add_node(node_idx.index())
                .unwrap_or_else(|_| unreachable!("Graph overflow in resolution"))
        };

        self.current_dag_node = Some(dag_idx);
        self.current_parent = self
            .env
            .get_parent(node_idx)
            .unwrap_or(QualifierFragment::Root);
        match item.kind {
            ItemKind::Package(PackageEntry { name, id }) => {
                Item::new(ItemKind::Package(PackageEntry { name, id }))
            }
            ItemKind::Function(f) => {
                // dbg!(f.sig);
                if *f.name.0 == "main" {
                    self.main_dag_idx = Some(dag_idx);
                }
                let f = self.analyze_func(f, dag_idx);

                Item::new(ItemKind::Function(f))
            }
            ItemKind::Type(n, g, t) => {
                let scheme = self.convert_type(t, Kind::Ty);

                Item::new(ItemKind::Type(n, vec![].leak(), scheme))
            }
            ItemKind::Extern { name, args, sig } => {
                let sig = self.in_context(|me| me.convert_type(sig, Kind::Extern(name.0)), dag_idx);

                Item::new(ItemKind::Extern { name, args, sig })
            }
            ItemKind::Root => Item::new(ItemKind::Root),
            ItemKind::Filename(f) => Item::new(ItemKind::Filename(f)),
            ItemKind::Dummy(d) => Item::new(ItemKind::Dummy(d)),
        }
    }

    fn in_context<T>(&mut self, mut f: impl FnMut(&mut Self) -> T, dag_idx: NodeIndex) -> T {
        let old = self.current_dag_node;
        self.current_dag_node = Some(dag_idx);

        let out = f(self);
        self.current_dag_node = old;
        out
    }

    fn analyze_func(
        &mut self,
        the_func: FunctionItem<UntypedCst>,
        idx: NodeIndex,
    ) -> FunctionItem<UntypedAst> {
        self.in_context(
            |me| {
                // me.generic_scope.clear()
                // dbg!(the_func.sig);
                let sig = me.convert_type(the_func.sig, Kind::Func);
                // dbg!(sig.ty);
                let body = me.analyze_expr(the_func.body, &[]);
                // me.generic_scope.clear();
                FunctionItem {
                    sig,
                    body,
                    name: the_func.name,
                }
            },
            idx,
        )
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
            CstType::User(name, instanced_generics) => {
                let the_item = self.resolve_name_type(&name);

                if let Ok(item_id) = the_item {
                    // self.dag_add(item_id);
                    let the_item = self
                        .env
                        .value(NodeIndex::new(item_id.0))
                        .expect("Item has not been defined in the environment");

                    if let ItemKind::Type(_, generics, new_t) = the_item.kind {
                        // dbg!(new_t);
                        // if !instanced_generics.is_empty() {
                        let analyzed_instances: Vec<_> = instanced_generics
                            .iter()
                            .map(|ty| self.analyze_type(*ty))
                            .collect();

                        let new_t = self.analyze_type(new_t);

                        let mut final_t = analyzed_instances
                            .into_iter()
                            .fold(new_t, |x, y| Spanned(CstType::GenericApp(x, y).into(), x.1));
                        final_t.1 = t.1;

                        self.analyze_type(final_t)
                    } else {
                        panic!("not a type")
                    }
                } else {
                    let err = errors::not_defined(name.0, &name.1);
                    self.errors.push(err);
                    name.convert(CstType::Hole)
                }
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
        }
    }

    #[allow(unused_variables)]
    fn analyze_expr(
        &mut self,
        expr: Spanned<Intern<CstExpr<Untyped>>>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> Spanned<Intern<Expr<Untyped>>> {
        // dbg!(&expr);

        match *expr.0 {
            CstExpr::Ident(u) => {
                if let Some(expr) = vars
                    .iter()
                    .rev()
                    .find(|n| u.ident().is_ok_and(|name| n.0 == name.0))
                {
                    expr.1
                } else {
                    // dbg!(expr);
                    self.resolve_name_expr(expr)
                }
            }
            CstExpr::Concat(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                expr.convert(Expr::Concat(l, r))
            }
            CstExpr::Project(direction, ex) => {
                let ex = self.analyze_expr(ex, vars);
                expr.convert(Expr::Project(direction, ex))
            }
            CstExpr::Inject(direction, ex) => {
                let ex = self.analyze_expr(ex, vars);
                expr.convert(Expr::Inject(direction, ex))
            }
            CstExpr::Branch(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);

                expr.convert(Expr::Branch(l, r))
            }
            CstExpr::Label(l, v) => {
                // dbg!(vars);
                // dbg!(expr);
                let v = self.analyze_expr(v, vars);
                expr.convert(Expr::Label(l, v))
            }
            CstExpr::Unlabel(v, l) => {
                let v = self.analyze_expr(v, vars);
                expr.convert(Expr::Unlabel(v, l))
            }
            CstExpr::Pat(spanned) => todo!(),
            CstExpr::Mul(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                expr.convert(Expr::Mul(l, r))
            }
            CstExpr::Div(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                expr.convert(Expr::Div(l, r))
            }
            CstExpr::Add(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                expr.convert(Expr::Add(l, r))
            }
            CstExpr::Sub(l, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                expr.convert(Expr::Sub(l, r))
            }
            CstExpr::Comparison(l, comparison_op, r) => {
                let l = self.analyze_expr(l, vars);
                let r = self.analyze_expr(r, vars);
                expr.convert(Expr::Comparison(l, comparison_op, r))
            }
            CstExpr::Call(func, arg) => {
                let func = self.analyze_expr(func, vars);

                let arg = self.analyze_expr(arg, vars);
                if let Expr::Item(id, Kind::Ty) = *func.0 {
                    todo!(
                        "This would be a type constructor, but it lowk isn't being used right now"
                    )
                };
                expr.convert(Expr::Call(func, arg))
            }
            CstExpr::FieldAccess(..) => self.resolve_field_access(expr, vars),
            CstExpr::If(cond, then, otherwise) => {
                let cond = self.analyze_expr(cond, vars);
                let then_vars = vars;
                let then = self.analyze_expr(then, then_vars);
                let otherwise_vars = vars;
                let otherwise = self.analyze_expr(otherwise, otherwise_vars);
                expr.convert(Expr::If(cond, then, otherwise))
            }
            CstExpr::Match(matchee, branches) => {
                let matchee = self.analyze_expr(matchee, vars);

                let branches: Vec<_> = branches
                    .iter()
                    .map(|b| self.resolve_branch(*b, vars))
                    .collect();
                assert!(!branches.is_empty());
                let branches = branches
                    .into_iter()
                    .reduce(|l, r| Spanned(Expr::Branch(l, r).into(), l.1.union(r.1)))
                    .expect("Branches was empty; match has no arms");

                expr.convert(Expr::Call(branches, matchee))
            }
            CstExpr::Lambda(arg, body) => self.resolve_lambda(expr, arg, body, vars),
            CstExpr::Let(id, body, and_in) => self.resolve_let(expr, id, body, and_in, vars),
            CstExpr::Number(n) => expr.convert(Expr::Number(n)),
            CstExpr::String(s) => expr.convert(Expr::String(s)),
            CstExpr::Bool(b) => expr.convert(Expr::Bool(b)),
            CstExpr::Unit => expr.convert(Expr::Unit),
            CstExpr::Particle(p) => expr.convert(Expr::Particle(p)),
            CstExpr::Hole(v) => expr.convert(Expr::Hole(v)),
            CstExpr::Item(item_id, kind) => expr.convert(Expr::Item(item_id, kind)),
            CstExpr::Myself => todo!(),
            CstExpr::MethodAccess { obj, prop, method } => todo!(),
        }
    }

    fn resolve_let(
        &mut self,
        expr: Spanned<Intern<CstExpr<Untyped>>>,
        id: Untyped,
        body: Spanned<Intern<CstExpr<Untyped>>>,
        and_in: Spanned<Intern<CstExpr<Untyped>>>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> Spanned<Intern<Expr<Untyped>>> {
        let body = self.analyze_expr(body, vars);

        let new_vars = [vars, &[(id.0.0, body)]].concat();
        let and_in = self.analyze_expr(and_in, &new_vars);
        // let lambda = Spanned(Expr::Lambda(id, and_in, LambdaInfo::Anon).into(), expr.1);
        expr.convert(Expr::Let(id, body, and_in))
    }

    fn resolve_lambda(
        &mut self,
        expr: Spanned<Intern<CstExpr<Untyped>>>,
        arg: Untyped,
        body: Spanned<Intern<CstExpr<Untyped>>>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> Spanned<Intern<Expr<Untyped>>> {
        let new_vars = &[
            vars,
            &[(
                arg.0.0,
                arg.ident()
                    .expect("Expected expression to be namable")
                    .convert(Expr::Ident(arg)),
            )],
        ]
        .concat();
        let body = self.analyze_expr(body, new_vars);
        // *vars.iter_mut().find(|x| x.0 == arg.0 .0).unwrap() = (arg.0 .0, body);
        expr.convert(Expr::Lambda(arg, body))
    }

    fn resolve_field_access(
        &mut self,
        expr: Spanned<Intern<CstExpr<Untyped>>>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> Spanned<Intern<Expr<Untyped>>> {
        let CstExpr::FieldAccess(l, r) = *expr.0 else {
            panic!("Not a field access")
        };
        let l = self.analyze_expr(l, vars);
        if let Expr::Item(_, _) = *l.0 {
            self.resolve_name_expr(r)
        } else {
            let id = r.ident().unwrap();
            expr.convert(Expr::Access(l, Label(id)))
        }
    } // fn resolve_field_access(
    //     &mut self,
    //     expr: Spanned<Intern<CstExpr<Untyped>>>,
    //     vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    // ) -> Spanned<Intern<Expr<Untyped>>> {
    //     let CstExpr::FieldAccess(l, r) = *expr.0 else {
    //         panic!("Not a field access")
    //     };
    //     let l = self.analyze_expr(l, vars);
    //     if let Expr::Item(_, _) = *l.0 {
    //         self.resolve_name_expr(r)
    //     } else if let Expr::Ident(n) = *l.0 {
    //         if let Some((_variable, val)) = vars.iter().find(|x| x.0 == n.0.0) {
    //             let projection: Spanned<Intern<Expr<Untyped>>> = {
    //                 let combo = *val;
    //                 let id = r.ident().expect("Expression should be nameable");
    //                 {
    //                     dbg!(val);
    //                     let ex = combo.convert(Expr::Project(Direction::Right, combo));

    //                     combo.convert(Expr::Unlabel(ex, Label(id)))
    //                 }
    //             };
    //             expr.convert(projection.0)
    //         } else {
    //             todo!()
    //         }
    //     } else if let Expr::Unlabel(_combo, _labell) = *l.0 {
    //         self.resolve_name_expr(r)
    //     } else {
    //         dbg!(l);
    //         let ex = l.convert(Expr::Project(Direction::Right, l));

    //         let id = r.ident().expect("Expression should be nameable");
    //         l.convert(Expr::Unlabel(ex, Label(id)))
    //     }
    // }

    fn resolve_branch(
        &mut self,
        b: MatchArm<Untyped>,
        vars: &[(Intern<String>, Spanned<Intern<Expr<Untyped>>>)],
    ) -> Spanned<Intern<Expr<Untyped>>> {
        // The branch becomes: λparam. <lets> in body
        // `param` is the lambda binder that receives the matchee at the call site.
        let param = Untyped(b.pat.convert("%match_arg%".to_string()));
        let branch_arg: Spanned<Intern<Expr<Untyped>>> = b.pat.convert(Expr::Ident(param));

        let bindings = self.compile_pattern(b.pat, branch_arg);
        // dbg!(&bindings);
        // Extend vars with user-visible bindings so analyze_expr can resolve them.
        // Each maps the binder name -> its destructured value expression.
        let mut branch_vars: Vec<(Intern<String>, Spanned<Intern<Expr<Untyped>>>)> = vars.to_vec();
        for binding in &bindings {
            // dbg!(binding);
            if binding.user_visible {
                branch_vars.push((binding.binder.0.0, binding.value));
            }
        }
        // dbg!(&bindings);

        let body = self.analyze_expr(b.body, &branch_vars);
        // dbg!(body);
        // Fold bindings into nested lets around the body.
        // Reversing means the first binding (outermost destructor) ends up outermost.
        //   bindings = [b0, b1, b2], body = B
        //   → Let(b0, v0, Let(b1, v1, Let(b2, v2, B)))
        let wrapped = bindings.into_iter().rev().fold(body, |acc, binding| {
            let span = acc.1;
            Spanned(
                Intern::new(Expr::Let(binding.binder, binding.value, acc)),
                span,
            )
        });

        let span = b.pat.1;
        let expr = Expr::Lambda(param, wrapped);
        // dbg!(expr);
        Spanned(expr.into(), span)
    }

    /// Recursively compile a pattern into a flat sequence of let-bindings,
    /// threading the current scrutinee expression down through nested constructors.
    fn compile_pattern(
        &mut self,
        p: Spanned<Intern<Pattern<Untyped>>>,
        branch_arg: Spanned<Intern<Expr<Untyped>>>,
    ) -> Vec<Binding> {
        match *p.0 {
            // Wildcard: consume scrutinee, emit nothing.
            // The lambda parameter stays unbound in the body — type is τ → R.
            // Pattern::Wildcard => vec![Binding {
            //     binder: Untyped(p.convert(INACCESSIBLE_IDENTIFIER.to_string())),
            //     value: scrutinee,
            //     user_visible: false,
            // }],
            Pattern::Wildcard => vec![],

            // Var: bind the scrutinee directly under the variable name.
            //   λparam. let v = param in body
            Pattern::Var(v) => vec![Binding {
                binder: v,
                value: branch_arg,
                user_visible: true,
            }],

            // Unit: irrefutable, no value to extract.
            Pattern::Unit => vec![],
            // Pattern::Unit => vec![Binding {
            //     binder: Untyped(p.convert("%inaccessible".to_string())),
            //     value: p.convert(Expr::Unit),
            //     user_visible: false,
            // }],

            // // Nullary constructor Ctor(A, Unit):
            // //   The scrutinee is a sum type with a label; unlabeling constrains
            // //   the type to `{A: ()}` without producing a user-visible binding.
            // //   λparam. let %inaccessible% = unlabel(param, A) in body
            // Pattern::Ctor(label, inner_pat) if *inner_pat.0 == Pattern::Unit => {
            //     // let inner_val = scrutinee.convert::<Expr<Untyped>>(Expr::Unit);
            //     let inner_val = scrutinee.convert(Expr::Unlabel(scrutinee, label));
            //     let inacc = Untyped(p.convert(INACCESSIBLE_IDENTIFIER.to_string()));
            //     // vec![]

            //     vec![Binding {
            //         binder: inacc,
            //         value: inner_val,
            //         user_visible: false,
            //     }]
            // }

            // Unary constructor Ctor(A, inner):
            //   Unlabel the scrutinee to expose the inner value, then recurse.
            //   λparam. let <inner bindings of unlabel(param, A)> in body
            Pattern::Ctor(label, inner_pat) => {
                let inner_val: Spanned<Intern<Expr<Untyped>>> =
                    branch_arg.convert(Expr::Unlabel(branch_arg, label));
                self.compile_pattern(inner_pat, inner_val)
            }

            Pattern::Number(_) => todo!(),
            Pattern::String(_) => todo!(),
            Pattern::Bool(_) => todo!(),
            _ => todo!(),
        }
    }
    #[allow(dead_code, clippy::unwrap_used, clippy::dbg_macro)]
    // #[deprecated]
    /// Pretty-print GraphViz for the internal state of the dependancy graph.
    fn debug(&self) {
        let render = |_, (_, v): (_, &usize)| {
            format!(
                "label = \"{}\"",
                self.env
                    .value(NodeIndex::from(*v as u32))
                    .unwrap()
                    .ident()
                    .unwrap()
                    .0
            )
        };
        let dot = petgraph::dot::Dot::with_attr_getters(
            &self.dag,
            &[
                Config::EdgeNoLabel,
                Config::NodeNoLabel,
                Config::RankDir(petgraph::dot::RankDir::LR),
            ],
            &|_, _| String::new(),
            &render,
        );
        dbg!(dot);
    }

    fn dag_add(&mut self, env_node: usize) {
        let n = if let Some((idx, _)) = self.dag.node_references().find(|(_, x)| **x == env_node) {
            idx
        } else {
            self.dag.add_node(env_node)
        };

        if let Some(current) = self.current_dag_node
            && n != current
        {
            self.dag.update_edge(current, n, ());
        }
    }

    fn search_masterenv(
        &mut self,
        q: &QualifierFragment,
        s: &SimpleSpan<usize, u64>,
    ) -> CompResult<ItemId> {
        let search = self.env.get_from_context(q, &self.current_parent);
        search.map_or_else(
            |_| Err(errors::not_defined(q, s)),
            |node| {
                // if !matches!(q, QualifierFragment::Type(_)) {
                self.dag_add(node.index());
                // }
                Ok(ItemId(node.index()))
            },
        )
    }

    fn resolve_name_expr(
        &mut self,
        expr: Spanned<Intern<CstExpr<Untyped>>>,
    ) -> Spanned<Intern<Expr<Untyped>>> {
        let name = expr.ident().expect("Expression should be nameable");
        // self.env.debug();

        if let Ok(e) = self.search_masterenv(&QualifierFragment::Func(name.0), &expr.1) {
            expr.convert(Expr::Item(e, Kind::Func))
        } else if let Ok(e) = self.search_masterenv(&QualifierFragment::Type(name.0), &expr.1) {
            expr.convert(Expr::Item(e, Kind::Ty))
        } else if let Ok(e) = self.search_masterenv(&QualifierFragment::Package(name.0), &expr.1) {
            expr.convert(Expr::Item(e, Kind::Package))
        } else {
            let err = errors::not_defined(QualifierFragment::Wildcard(name.0), &expr.1);
            self.errors.push(err);
            expr.convert(Expr::Hole(Untyped(name)))
        }
    }

    fn resolve_name_type(&mut self, name: &impl Ident) -> CompResult<ItemId> {
        let name = name.ident()?;

        self.search_masterenv(&QualifierFragment::Type(name.0), &name.1)
            .map_or_else(
                |_| {
                    Err(errors::not_defined(
                        QualifierFragment::Wildcard(name.0),
                        &name.1,
                    ))
                },
                Ok,
            )
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
