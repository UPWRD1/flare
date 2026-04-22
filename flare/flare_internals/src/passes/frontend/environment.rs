use std::{cell::OnceCell, collections::VecDeque, ops::ControlFlow};

use internment::Intern;

use petgraph::{
    prelude::StableDiGraph,
    stable_graph::{EdgeReference, NodeIndex},
    visit::EdgeRef as _,
};
use radix_trie::{Trie, TrieKey};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::resource::{
    errors::{self, CompResult, CompilerErr},
    rep::{
        common::{Spanned, Syntax},
        frontend::{
            ast::{ItemId, Label, Untyped},
            cst::{CstExpr, Field, FieldDef, MatchArm, PackageCollection, Pattern, UntypedCst},
            entry::{FunctionItem, Item},
            quantifier::QualifierFragment,
        },
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Key(Vec<Intern<String>>);

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

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Element<S: Syntax> {
    name: Spanned<Intern<String>>,
    value: std::cell::OnceCell<S::Expr>,
    ty: Option<S::Type>,
}

#[derive(Debug)]
pub enum Relation {
    Reference,
    Parent,
    Return,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Var {
    name: Intern<String>,
}

impl<S: Syntax> Element<S> {
    pub fn new_with_ty(
        name: Spanned<Intern<String>>,
        value: impl Into<OnceCell<S::Expr>>,
        ty: Option<S::Type>,
    ) -> Self {
        Self {
            name,
            value: value.into(),
            ty,
        }
    }
    pub fn new_with(name: Spanned<Intern<String>>, value: impl Into<OnceCell<S::Expr>>) -> Self {
        Self {
            name,
            value: value.into(),
            ty: None,
        }
    }

    pub fn new(name: Spanned<Intern<String>>) -> Self {
        Self {
            name,
            value: OnceCell::new(),
            ty: None,
        }
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
pub struct EnvironmentBuilder<S: Syntax> {
    pub graph: StableDiGraph<Element<S>, Relation>,
    pub trie: Trie<Key, NodeIndex>,
    path: Vec<Intern<String>>,
    current_node: NodeIndex,
    context: BuilderContext,
    errors: Vec<CompilerErr>,
}

#[derive(Debug, Default, Clone, Copy)]
pub enum BuilderContext {
    #[default]
    /// return autoprojection enabled
    Autoproject,
    /// autoprojection disabled
    Value,
}

pub type Environment<S> = FxHashMap<NodeIndex, Item<S>>;

impl EnvironmentBuilder<UntypedCst> {
    /// Build the environment from a given `PackageCollection`
    /// # Errors
    /// - on invalid names,
    ///
    pub fn build(program: PackageCollection<UntypedCst>) -> CompResult<Environment<UntypedCst>> {
        let mut graph: StableDiGraph<Element<UntypedCst>, Relation> = StableDiGraph::default();
        let root_node = graph.add_node(Element::new(Spanned::default_with("Root".to_string())));
        let trie = Trie::new();
        let mut fields: Vec<Field<UntypedCst>> = vec![];

        program.packages.into_iter().for_each(|(p, _)| {
            fields.push(p.root_node);
        });
        fields.push(Field::Macro(
            crate::resource::rep::frontend::cst::FieldMacro::Ret(Spanned::default_with(
                CstExpr::Ident(Untyped(Spanned::default_with(String::from("Main")))),
            )),
        ));
        let root_obj: Spanned<Intern<_>> = Spanned::default_with(CstExpr::ProductConstructor {
            fields: fields.as_slice().into(),
        });
        let mut me = Self {
            graph,
            trie,
            current_node: root_node,
            context: BuilderContext::Autoproject,
            ..Default::default()
        };
        dbg!(root_obj);
        let res = me
            .enter_context_root(
                |me| me.analyze_expr(root_obj, &[]),
                ControlFlow::Break(root_obj),
                Spanned::default_with(String::from("Root")),
            )
            .into_value();
        dbg!(res);
        me.debug();
        Ok(me.lift(res))
    }

    fn autoproject(
        &self,
        expr: <UntypedCst as Syntax>::Expr,
        index: NodeIndex,
    ) -> <UntypedCst as Syntax>::Expr {
        match self.context {
            BuilderContext::Autoproject => {
                if let Some(edge) = self
                    .graph
                    .edges_directed(index, petgraph::Direction::Outgoing)
                    .find(|edge| matches!(edge.weight(), Relation::Return))
                {
                    expr.convert(CstExpr::FieldAccess(
                        expr,
                        Label(Spanned::default_with(String::from("%return"))),
                    ))
                } else {
                    expr
                }
            }
            BuilderContext::Value => expr,
        }
    }

    fn resolve_name(
        &mut self,
        n: <UntypedCst as Syntax>::Name,
    ) -> ControlFlow<<UntypedCst as Syntax>::Expr, <UntypedCst as Syntax>::Expr> {
        let expr = if let Some(index) = self.find_nearest(|node| node.name.0 == n.0) {
            let stage_1 = n.convert(CstExpr::Item(ItemId(index.index())));
            self.autoproject(stage_1, index)
        } else {
            self.errors.push(errors::not_defined(n));
            n.convert(CstExpr::Hole(Untyped(n)))
        };
        ControlFlow::Continue(expr)
    }

    #[allow(dead_code, clippy::unwrap_used, clippy::dbg_macro)]
    #[deprecated]
    pub fn debug(&self) {
        use petgraph::dot::Config;
        let render = |_, v: EdgeReference<'_, Relation>| format!("label = \"{:?}\"", v.weight());
        let dot = petgraph::dot::Dot::with_attr_getters(
            &self.graph,
            &[
                Config::EdgeNoLabel,
                Config::NodeNoLabel,
                Config::RankDir(petgraph::dot::RankDir::LR),
            ],
            // &|_, _| String::new(),
            &render,
            &|_, (i, e)| format!("label = \"{} = {}\"", i.index(), e.name),
        );
        dbg!(dot);
    }

    fn enter_context<T>(
        &mut self,
        mut f: impl FnMut(&mut Self) -> T,
        value: ControlFlow<<UntypedCst as Syntax>::Expr, <UntypedCst as Syntax>::Expr>,
        name: <UntypedCst as Syntax>::Name,
    ) -> T {
        let value = value
            .continue_value()
            .map_or_else(OnceCell::new, std::convert::Into::into);
        let old_node = self.current_node;
        let old_path = self.path.clone();
        self.path.push(name.0);
        self.current_node = self.graph.add_node(Element::new_with(name, value));
        self.graph
            .add_edge(old_node, self.current_node, Relation::Parent);

        self.trie
            .insert(Key::from(self.path.clone()), self.current_node);
        let out = f(self);
        self.current_node = old_node;
        self.path = old_path;

        out
    }

    fn enter_context_root<T>(
        &mut self,
        mut f: impl FnMut(&mut Self) -> T,
        value: ControlFlow<<UntypedCst as Syntax>::Expr, <UntypedCst as Syntax>::Expr>,
        name: <UntypedCst as Syntax>::Name,
    ) -> T {
        let value = value.continue_value();
        self.path.push(name.0);

        self.trie
            .insert(Key::from(self.path.clone()), self.current_node);

        f(self)
    }

    fn find_nearest<F>(&mut self, predicate: F) -> Option<petgraph::graph::NodeIndex>
    where
        F: Fn(&Element<UntypedCst>) -> bool,
    {
        use petgraph::Direction::{Incoming, Outgoing};
        let mut queue = std::collections::VecDeque::new();
        let mut visited = FxHashSet::default();

        queue.push_back(self.current_node);
        visited.insert(self.current_node);

        while let Some(node) = queue.pop_front() {
            if predicate(&self.graph[node]) {
                self.graph
                    .add_edge(self.current_node, node, Relation::Reference);
                return Some(node);
            }

            let parent = self
                .graph
                .edges_directed(node, Incoming)
                .find(|e| matches!(e.weight(), Relation::Parent))
                .map(|e| e.source());

            if let Some(parent) = parent {
                let siblings = self
                    .graph
                    .edges_directed(parent, Outgoing)
                    .filter(|e| matches!(e.weight(), Relation::Parent))
                    .map(|e| e.target())
                    .filter(|&n| n != node);

                for neighbor in siblings {
                    if visited.insert(neighbor) {
                        queue.push_back(neighbor);
                    }
                }

                if visited.insert(parent) {
                    queue.push_back(parent);
                }
            }
        }

        None
    }

    fn analyze_expr(
        &mut self,
        expr: Spanned<Intern<CstExpr<UntypedCst>>>,
        vars: &[Var],
    ) -> ControlFlow<<UntypedCst as Syntax>::Expr, <UntypedCst as Syntax>::Expr> {
        dbg!(&self.context);
        match *expr.0 {
            CstExpr::Ident(u) => {
                if let Some(var) = vars.iter().rev().find(|n| *n.name == *u.0.0) {
                    ControlFlow::Continue(expr)
                } else {
                    self.resolve_name(u.0)
                }
            }
            CstExpr::ProductConstructor { fields } => {
                // Phase 1: register all sibling nodes before resolving any of them.
                let fields = self.pre_register_fields(fields.to_vec());
                // Phase 2: now resolve expressions, with all siblings visible.
                let resolved_fields = self.resolve_fields(&fields, vars);

                let expr = expr.modify(CstExpr::ProductConstructor {
                    fields: resolved_fields.as_slice().into(),
                });

                ControlFlow::Break(expr)
            }
            CstExpr::VariantConstructor { name, value } => {
                let value = value.map(|value| self.analyze_expr(value, vars).into_value());
                ControlFlow::Continue(expr.modify(CstExpr::VariantConstructor { name, value }))
            }
            CstExpr::Bin(l, op, r) => {
                let l = self.analyze_expr(l, vars).into_value();
                let r = self.analyze_expr(r, vars).into_value();
                ControlFlow::Continue(expr.modify(CstExpr::Bin(l, op, r)))
            }
            CstExpr::Call(func, arg) => {
                let func = self.analyze_expr(func, vars).into_value();

                let arg = self.analyze_expr(arg, vars).into_value();
                ControlFlow::Continue(expr.convert(CstExpr::Call(func, arg)))
            }
            CstExpr::FieldAccess(..) => self.resolve_field_access(expr, vars),
            CstExpr::Match(matchee, branches) => {
                let matchee = self.analyze_expr(matchee, vars).into_value();

                let branches: Vec<_> = branches
                    .iter()
                    .map(|b| self.resolve_branch(b.pat, b.body, vars))
                    .collect();
                assert!(!branches.is_empty());
                ControlFlow::Continue(expr.modify(CstExpr::Match(matchee, branches.leak())))
            }
            CstExpr::Lambda(arg, body) => self.resolve_lambda(expr, arg, body, vars),
            CstExpr::Number(n) => ControlFlow::Continue(expr.convert(CstExpr::Number(n))),
            CstExpr::String(s) => ControlFlow::Continue(expr.convert(CstExpr::String(s))),
            CstExpr::Bool(b) => ControlFlow::Continue(expr.convert(CstExpr::Bool(b))),
            CstExpr::Unit => ControlFlow::Continue(expr.convert(CstExpr::Unit)),
            CstExpr::Particle(p) => ControlFlow::Continue(expr.convert(CstExpr::Particle(p))),
            CstExpr::Hole(v) => ControlFlow::Continue(expr.convert(CstExpr::Hole(v))),
            CstExpr::Item(item_id) => ControlFlow::Continue(expr.convert(CstExpr::Item(item_id))),
            CstExpr::Let(..) => todo!(),
        }
    }

    fn pre_register_fields(&mut self, fields: Vec<Field<UntypedCst>>) -> Vec<Field<UntypedCst>> {
        fields
            .into_iter()
            .filter_map(|field| {
                match field {
                    Field::Def(field) => {
                        let old_node = self.current_node;
                        let old_path = self.path.clone();

                        self.path.push(field.name.0);
                        let key = Key::from(self.path.clone());

                        let new_node = if let Some(&existing) = self.trie.get(&key) {
                            existing
                        } else {
                            let n = self.graph.add_node(Element::new_with_ty(
                                field.name,
                                OnceCell::new(),
                                field.ty,
                            )); // value filled in phase 2
                            self.graph.add_edge(old_node, n, Relation::Parent);
                            self.trie.insert(key, n);
                            n
                        };

                        // Recurse into nested ProductConstructors so deeply-nested fields
                        // are also pre-registered before any resolution happens.
                        self.current_node = new_node;
                        if let CstExpr::ProductConstructor { fields } = *field.value.0 {
                            self.pre_register_fields(fields.to_vec());
                        }

                        self.current_node = old_node;
                        self.path = old_path;
                        Some(Field::Def(field))
                    }

                    Field::Macro(field_macro) => match field_macro {
                        crate::resource::rep::frontend::cst::FieldMacro::Import(_) => todo!(),
                        crate::resource::rep::frontend::cst::FieldMacro::Extend(extend) => {
                            todo!()
                        }
                        crate::resource::rep::frontend::cst::FieldMacro::Ret(v) => {
                            Some(self.add_ret_macro(v))
                        }
                    },
                }
            })
            .collect()
    }

    fn add_ret_macro(&mut self, expr: <UntypedCst as Syntax>::Expr) -> Field<UntypedCst> {
        let name = Spanned::default_with(String::from("%return"));

        let old_node = self.current_node;
        let old_path = self.path.clone();

        self.path.push(name.0);
        let key = Key::from(self.path.clone());

        let new_node = if let Some(&existing) = self.trie.get(&key) {
            existing
        } else {
            let n = self.graph.add_node(Element::new(name)); // value filled in phase 2
            self.graph.add_edge(old_node, n, Relation::Return);
            self.trie.insert(key, n);
            n
        };

        if let CstExpr::ProductConstructor { fields } = *expr.0 {
            self.pre_register_fields(fields.to_vec());
        }

        self.current_node = new_node;

        let new_field_def = FieldDef {
            name,
            ty: None,
            value: expr,
        };

        self.current_node = old_node;
        self.path = old_path;
        Field::Def(new_field_def)
    }

    fn resolve_fields(
        &mut self,
        fields: &[Field<UntypedCst>],
        vars: &[Var],
    ) -> Vec<Field<UntypedCst>> {
        fields
            .iter()
            .map(|field| {
                match field {
                    Field::Def(field) => {
                        // Retrieve the node index that phase 1 already created.
                        self.path.push(field.name.0);
                        let node_index = *self
                            .trie
                            .get(&Key::from(self.path.clone()))
                            .expect("node must exist after pre-registration");

                        let old_node = self.current_node;
                        self.current_node = node_index;
                        let value = self.analyze_expr(field.value, vars).into_value();

                        // Patch the value into the already-existing node.
                        self.graph[node_index].value.set(value);

                        self.current_node = old_node;
                        self.path.pop();

                        Field::Def(FieldDef { value, ..*field })
                    }
                    Field::Macro(field_macro) => {
                        panic!("Should have been inserted in pre-registration")
                    }
                }
            })
            .collect()
    }

    fn resolve_branch(
        &mut self,
        pat: Spanned<Intern<Pattern<UntypedCst>>>,
        body: Spanned<Intern<CstExpr<UntypedCst>>>,
        vars: &[Var],
    ) -> MatchArm<UntypedCst> {
        let bindings = pat.0.bindings();
        // Extend vars with bindings
        let mut branch_vars: Vec<Var> = vars.to_vec();
        branch_vars.extend(bindings.iter().map(|v| Var { name: v.0.0 }));
        let body = self.analyze_expr(body, &branch_vars).into_value();
        MatchArm { pat, body }
    }

    fn resolve_lambda(
        &mut self,
        expr: Spanned<Intern<CstExpr<UntypedCst>>>,
        arg: Untyped,
        body: Spanned<Intern<CstExpr<UntypedCst>>>,
        vars: &[Var],
    ) -> ControlFlow<Spanned<Intern<CstExpr<UntypedCst>>>, Spanned<Intern<CstExpr<UntypedCst>>>>
    {
        let new_vars = &[vars, &[Var { name: arg.0.0 }]].concat();
        let body = self.analyze_expr(body, new_vars).into_value();
        ControlFlow::Continue(expr.convert(CstExpr::Lambda(arg, body)))
    }

    fn resolve_field_access(
        &mut self,
        expr: Spanned<Intern<CstExpr<UntypedCst>>>,
        vars: &[Var],
    ) -> ControlFlow<Spanned<Intern<CstExpr<UntypedCst>>>, Spanned<Intern<CstExpr<UntypedCst>>>>
    {
        let CstExpr::FieldAccess(l, r) = *expr.0 else {
            panic!("Not a field access")
        };
        let old_context = self.context;
        self.context = BuilderContext::Value;
        let l = self.analyze_expr(l, vars).into_value();
        self.context = old_context;
        ControlFlow::Continue(expr.convert(CstExpr::FieldAccess(l, r)))
    }

    fn lift(mut self, main_expr: <UntypedCst as Syntax>::Expr) -> Environment<UntypedCst> {
        let mut map: FxHashMap<NodeIndex, Item<UntypedCst>> = FxHashMap::default();
        let nodes_with_references: Vec<_> = self
            .graph
            .edge_indices()
            .filter_map(|eidx| {
                let edge = self.graph.edge_weight(eidx)?;
                if let Relation::Reference = edge {
                    self.graph.edge_endpoints(eidx).map(|(_, target)| target)
                } else {
                    None
                }
            })
            .collect();

        for node in nodes_with_references {
            let element = &mut self.graph[node];
            let item = Item {
                kind: crate::resource::rep::frontend::entry::ItemKind::Function(FunctionItem {
                    name: element.name,
                    sig: element.ty.expect("Recursive definition requires type"),
                    body: element.value.take().expect("Definition requires body"),
                }),
            };
            map.insert(node, item);
        }

        let main_node = self
            .graph
            .node_indices()
            .find(|n| *self.graph[*n].name.0 == "main")
            .unwrap();
        let element = &mut self.graph[main_node];
        let main_item = Item {
            kind: crate::resource::rep::frontend::entry::ItemKind::Function(FunctionItem {
                name: element.name,
                sig: element.ty.expect("Recursive definition requires type"),
                body: element.value.take().expect("Definition requires body"),
            }),
        };
        map.insert(main_node, main_item);
        map
    }
}

impl<S: Syntax> EnvironmentBuilder<S> {
    pub fn from_graph_and_root(
        graph: &impl Into<StableDiGraph<Option<S::Expr>, QualifierFragment>>,
        root: NodeIndex,
    ) -> Self {
        todo!();
    }
}
