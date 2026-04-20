use internment::Intern;
use itertools::Itertools;
// use petgraph::Graph;
use petgraph::{
    dot::Config,
    prelude::StableDiGraph,
    stable_graph::{EdgeReference, NodeIndex},
    visit::EdgeRef as _,
};
use radix_trie::{Trie, TrieKey};
use rustc_hash::{FxHashMap, FxHashSet};

use std::{hash::RandomState, path};

use crate::resource::{
    errors::CompResult,
    rep::{
        // concretetypes::{EnumVariant, Ty},
        common::{Spanned, Syntax},
        frontend::{
            ast::{Label, Untyped},
            cst::{CstExpr, FieldDef, Macro, MatchArm, PackageCollection, Pattern, UntypedCst},
            entry::Item,
            quantifier::QualifierFragment,
        },
    },
};
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Key(Vec<Intern<String>>);

impl Key {
    pub fn from<T>(path: T) -> Self
    where
        T: Into<Vec<Intern<String>>>,
    {
        Self(path.into())
    }
}

impl TrieKey for Key {
    fn encode_bytes(&self) -> Vec<u8> {
        self.0
            .iter()
            .flat_map(|s| s.bytes().collect::<Vec<u8>>())
            .collect()
    }
}

#[derive(Debug)]
/// The main environment graph structure. Holds all the objects produced by
/// the  parser, and the index of the root.
///
/// Obviously, `Environment` does not implement `Copy`, but it also does not
/// implement `Clone`, since it is ridiculously expensive, and there is no
/// real reason to clone the environment.
#[non_exhaustive]
#[derive(Default)]
pub struct Environment<S: Syntax> {
    pub graph: StableDiGraph<Option<S::Expr>, String>,
    pub trie: Trie<Key, Option<S::Expr>>,
    path: Vec<Intern<String>>,
    current_node: NodeIndex,
    vars: Vec<Intern<String>>, // pub root: NodeIndex,
}

impl Environment<UntypedCst> {
    /// Build the environment from a given `Program`
    /// # Errors
    /// - on invalid names,
    ///
    pub fn build(program: PackageCollection<UntypedCst>) -> CompResult<Self> {
        let mut graph: StableDiGraph<Option<<UntypedCst as Syntax>::Expr>, String> =
            StableDiGraph::default();
        let root_node = graph.add_node(None);
        let trie = Trie::new();
        let mut macros: FxHashMap<CstExpr<UntypedCst>, Vec<Macro<UntypedCst>>> =
            FxHashMap::default();
        let mut fields: Vec<FieldDef<UntypedCst>> = vec![];

        program.packages.into_iter().for_each(|(p, _)| {
            macros.extend(p.macros);
            fields.push(p.root_node);
        });
        let root_obj: Spanned<Intern<_>> = Spanned::default_with(
            CstExpr::ProductConstructor {
                fields: fields.as_slice().into(),
            }
            .into(),
        );
        let proj_main_package = Spanned::default_with(
            CstExpr::FieldAccess(
                root_obj,
                Label(Spanned::default_with(String::from("Main").into())),
            )
            .into(),
        );

        let proj_main_func: Spanned<Intern<_>> = Spanned::default_with(
            CstExpr::FieldAccess(
                proj_main_package,
                Label(Spanned::default_with(String::from("main").into())),
            )
            .into(),
        );
        let mut me = Self {
            graph,
            trie,
            current_node: root_node,
            ..Default::default()
        };
        // dbg!(proj_main_func);
        let res = me.enter_context(
            |me| me.analyze_expr(proj_main_func, &[]),
            None,
            Spanned::default_with(String::from("Root").into()),
        );
        me.debug();
        // let mut resolver = crate::passes::frontend::resolution::Resolver::default();
        // let resolved = resolver.analyze_expr(proj_main_func, &[]);
        // let resolved = resolver.convert_expr(resolved);
        // let r = crate::passes::frontend::typing::Solver::infer_with_items(
        //     &crate::passes::frontend::typing::ItemSource::default(),
        //     resolved,
        // );
        // dbg!(r);
        todo!()
        // proj_main_func
    }

    fn resolve_name(
        &self,
        n: <UntypedCst as Syntax>::Name,
    ) -> Option<<UntypedCst as Syntax>::Expr> {
        todo!()
    }

    #[allow(dead_code, clippy::unwrap_used, clippy::dbg_macro)]
    #[deprecated]
    pub fn debug(&self) {
        let render = |_, v: EdgeReference<'_, String>| format!("label = \"{}\"", v.weight());
        let dot = petgraph::dot::Dot::with_attr_getters(
            &self.graph,
            &[
                Config::EdgeNoLabel,
                Config::NodeNoLabel,
                Config::RankDir(petgraph::dot::RankDir::LR),
            ],
            // &|_, _| String::new(),
            &render,
            &|_, _| String::new(),
        );
        dbg!(dot);
    }
    fn add(&mut self, path: &[Intern<String>], value: Option<<UntypedCst as Syntax>::Expr>) {
        self.trie.insert(Key::from(path.clone()), value);
    }

    fn enter_context<T>(
        &mut self,
        mut f: impl FnMut(&mut Self) -> T,
        value: Option<<UntypedCst as Syntax>::Expr>,
        name: <UntypedCst as Syntax>::Name,
    ) -> T {
        let old_node = self.current_node;
        let old_path = self.path.clone();
        self.path.push(name.0);
        self.current_node = self.graph.add_node(value);
        self.graph.add_edge(
            old_node,
            self.current_node,
            self.path.last().unwrap().to_string(),
        );

        self.trie.insert(Key::from(self.path.clone()), value);
        let out = f(self);
        self.current_node = old_node;
        self.path = old_path;

        out
    }

    fn find_nearest_weighted<F>(&self, predicate: F) -> Option<(petgraph::graph::NodeIndex, usize)>
    where
        F: Fn(&Option<<UntypedCst as Syntax>::Expr>) -> bool,
    {
        use petgraph::algo::astar;
        astar(
            &self.graph,
            self.current_node,
            |finish| predicate(&self.graph[finish]),
            |_| 0,
            |_| 0, // no heuristic → Dijkstra behavior
        )
        .map(|(cost, path)| (*path.last().unwrap(), cost))
    }

    fn analyze_expr(
        &mut self,
        expr: Spanned<Intern<CstExpr<UntypedCst>>>,
        vars: &[Intern<String>],
    ) -> Option<<UntypedCst as Syntax>::Expr> {
        // dbg!(&expr);

        match *expr.0 {
            CstExpr::Ident(u) => {
                if vars.iter().rev().find(|n| **n == u.0.0).is_some() {
                    Some(expr)
                } else {
                    // dbg!(expr);
                    self.resolve_name(u.0)
                }
            }
            CstExpr::ProductConstructor { fields } => {
                let mut new_vars = vars.to_vec();
                for field in fields.iter() {
                    new_vars.push(field.name.0);
                }
                fields.iter().for_each(|field| {
                    let value = self.enter_context(
                        |me| me.analyze_expr(field.value, &new_vars),
                        Some(field.value),
                        field.name,
                    );
                });
                None
            }
            CstExpr::Pat(spanned) => todo!(),
            CstExpr::Mul(l, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;
                Some(expr.modify(CstExpr::Mul(l, r)))
            }
            CstExpr::Div(l, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;
                Some(expr.modify(CstExpr::Div(l, r)))
            }
            CstExpr::Add(l, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;
                Some(expr.modify(CstExpr::Add(l, r)))
            }
            CstExpr::Sub(l, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;
                Some(expr.modify(CstExpr::Sub(l, r)))
            }
            CstExpr::Comparison(l, comparison_op, r) => {
                let l = self.analyze_expr(l, vars)?;
                let r = self.analyze_expr(r, vars)?;
                Some(expr.convert(CstExpr::Comparison(l, comparison_op, r)))
            }
            CstExpr::Call(func, arg) => {
                let func = self.analyze_expr(func, vars)?;

                let arg = self.analyze_expr(arg, vars)?;
                Some(expr.convert(CstExpr::Call(func, arg)))
            }
            CstExpr::FieldAccess(..) => self.resolve_field_access(expr, vars),
            CstExpr::Match(matchee, branches) => {
                let matchee = self.analyze_expr(matchee, vars)?;

                let branches: Vec<_> = branches
                    .iter()
                    .map(|b| self.resolve_branch(b.pat, b.body, vars))
                    .collect();
                assert!(!branches.is_empty());
                Some(expr.modify(CstExpr::Match(matchee, branches.leak())))
            }
            CstExpr::Lambda(arg, body) => self.resolve_lambda(expr, arg, body, vars),
            // CstExpr::Let(id, body, and_in) => self.resolve_let(expr, id, body, and_in, vars, path),
            CstExpr::Number(n) => Some(expr.convert(CstExpr::Number(n))),
            CstExpr::String(s) => Some(expr.convert(CstExpr::String(s))),
            CstExpr::Bool(b) => Some(expr.convert(CstExpr::Bool(b))),
            CstExpr::Unit => Some(expr.convert(CstExpr::Unit)),
            CstExpr::Particle(p) => Some(expr.convert(CstExpr::Particle(p))),
            CstExpr::Hole(v) => Some(expr.convert(CstExpr::Hole(v))),
            CstExpr::Item(item_id, kind) => Some(expr.convert(CstExpr::Item(item_id, kind))),
        }
    }

    fn resolve_branch(
        &mut self,
        pat: Spanned<Intern<Pattern<UntypedCst>>>,
        body: Spanned<Intern<CstExpr<UntypedCst>>>,
        vars: &[Intern<String>],
    ) -> MatchArm<UntypedCst> {
        // The branch becomes: λparam. <lets> in body
        // `param` is the lambda binder that receives the matchee at the call site.
        let param = Untyped(pat.convert("%match_arg%".to_string()));
        let branch_arg: Spanned<Intern<CstExpr<UntypedCst>>> = pat.convert(CstExpr::Ident(param));

        let bindings = pat.0.bindings();
        dbg!(&bindings);
        // Extend vars with user-visible bindings so analyze_expr can resolve them.
        // Each maps the binder name -> its destructured value expression.
        let mut branch_vars: Vec<Intern<String>> = vars.to_vec();
        for binding in &bindings {
            branch_vars.push(binding.0.0);
        }
        // dbg!(&bindings);

        let body = self.analyze_expr(body, &branch_vars).unwrap();
        MatchArm { pat, body }
    }

    fn resolve_lambda(
        &mut self,
        expr: Spanned<Intern<CstExpr<UntypedCst>>>,
        arg: Untyped,
        body: Spanned<Intern<CstExpr<UntypedCst>>>,
        vars: &[Intern<String>],
    ) -> Option<Spanned<Intern<CstExpr<UntypedCst>>>> {
        let new_vars = &[vars, &[arg.0.0]].concat();
        let body = self.analyze_expr(body, new_vars)?;
        // *vars.iter_mut().find(|x| x.0 == arg.0 .0).unwrap() = (arg.0 .0, body);
        Some(expr.convert(CstExpr::Lambda(arg, body)))
    }

    fn resolve_field_access(
        &mut self,
        expr: Spanned<Intern<CstExpr<UntypedCst>>>,
        vars: &[Intern<String>],
    ) -> Option<Spanned<Intern<CstExpr<UntypedCst>>>> {
        let CstExpr::FieldAccess(l, r) = *expr.0 else {
            panic!("Not a field access")
        };
        let l = self.analyze_expr(l, vars)?;
        if let CstExpr::Item(_, _) = *l.0 {
            self.resolve_name(r.0)
        } else {
            Some(expr.convert(CstExpr::FieldAccess(l, r)))
        }
    }

    /// Builds an impl definition
    /// # Errors
    /// On invalid names.
    #[allow(dead_code, unused_variables)]
    fn build_impl_def(
        &self,
        package_quant: &QualifierFragment,
        the_ty: <UntypedCst as Syntax>::Name,
        methods: &[(
            <UntypedCst as Syntax>::Name,
            <UntypedCst as Syntax>::Expr,
            <UntypedCst as Syntax>::Type,
        )],
    ) -> CompResult<()> {
        todo!()
    }
}

impl<S: Syntax> Environment<S> {
    pub fn from_graph_and_root(
        graph: &impl Into<StableDiGraph<Option<S::Expr>, QualifierFragment>>,
        root: NodeIndex,
    ) -> Self {
        todo!();
        // let graph = graph.into();
        // Self { graph }
    }
}
