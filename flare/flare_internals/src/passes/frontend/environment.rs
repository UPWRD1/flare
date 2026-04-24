use std::{cell::OnceCell, ops::ControlFlow};

use internment::Intern;

use petgraph::{
    prelude::StableDiGraph,
    stable_graph::{EdgeReference, NodeIndex},
    visit::EdgeRef as _,
};
use radix_trie::{Trie, TrieKey};
use rustc_hash::FxHashMap;

use crate::resource::{
    errors::{self, CompResult, CompilerErr, ErrorCollection},
    rep::{
        common::{Spanned, Syntax},
        frontend::{
            ast::{ItemId, Label, Untyped},
            cst::{CstExpr, Field, FieldDef, MatchArm, PackageCollection, Pattern, UntypedCst},
            csttypes::{CstClosedRow, CstType},
            entry::{FunctionItem, Item},
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Relation {
    Reference,
    Parent,
    PubParent,
    Return,
}

enum LookupMode {
    Unqualified,
    Qualified,
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
    node_stack: Vec<NodeIndex>,
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
            node_stack: vec![root_node],
            context: BuilderContext::Autoproject,
            ..Default::default()
        };

        let res = me
            .enter_context_root(
                |me| me.analyze_expr(root_obj, &[]),
                Spanned::default_with(String::from("Root")),
            )
            .into_value();
        dbg!(res);
        if me.errors.is_empty() {
            let env_map = me.lift(res);
            dbg!(&env_map);
            Ok(env_map)
        } else {
            Err(ErrorCollection::new(me.errors).into())
        }
    }

    fn autoproject_expr(
        &self,
        expr: <UntypedCst as Syntax>::Expr,
        node: NodeIndex,
    ) -> <UntypedCst as Syntax>::Expr {
        if self.should_autoproject(node).is_some() {
            expr.map(|expr| {
                CstExpr::FieldAccess(expr, Label(Spanned::default_with(String::from("%return"))))
            })
        } else {
            expr
        }
    }

    fn should_autoproject(&self, node: NodeIndex) -> Option<NodeIndex> {
        match self.context {
            BuilderContext::Autoproject => self
                .graph
                .edges_directed(node, petgraph::Direction::Outgoing)
                .find(|edge| matches!(edge.weight(), Relation::Return))
                .map(|edge| edge.target()),
            BuilderContext::Value => None,
        }
    }

    fn current_node(&self) -> NodeIndex {
        self.node_stack.last().copied().unwrap()
    }

    fn resolve_name(
        &mut self,
        n: <UntypedCst as Syntax>::Name,
        start: NodeIndex,
        mode: LookupMode,
    ) -> Option<NodeIndex> {
        self.find_nearest(|node| node.name.0 == n.0, start, mode)
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
            &render,
            &|_, (i, e)| {
                format!(
                    "label = \"{} = {}; {}\"",
                    i.index(),
                    e.name,
                    e.value.get().is_some()
                )
            },
        );
        dbg!(dot);
    }
    fn enter_context_root<T>(
        &mut self,
        mut f: impl FnMut(&mut Self) -> T,
        name: <UntypedCst as Syntax>::Name,
    ) -> T {
        self.path.push(name.0);

        self.trie.insert(
            Key::from(self.path.clone()),
            *self.node_stack.last().unwrap(),
        );

        f(self)
    }

    fn find_nearest<F>(
        &mut self,
        predicate: F,
        start: NodeIndex,
        mode: LookupMode,
    ) -> Option<petgraph::graph::NodeIndex>
    where
        F: Fn(&Element<UntypedCst>) -> bool,
    {
        #[derive(Debug, PartialEq, Eq, Hash)]
        enum VisitKind {
            Sibling(NodeIndex),
            Parent(NodeIndex),
            Start(NodeIndex),
        }
        use VisitKind::{Parent, Sibling, Start};
        use petgraph::Direction::{Incoming, Outgoing};
        use std::collections::VecDeque;
        let mut queue: VecDeque<VisitKind> = VecDeque::new();
        queue.push_back(Start(start));

        let parent = self
            .graph
            .edges_directed(start, Incoming)
            .find_map(|e| {
                matches!(
                    e.weight(),
                    Relation::Parent | Relation::PubParent | Relation::Return
                )
                .then(|| e.source())
            })
            .expect("Should not be root node");

        if matches!(mode, LookupMode::Qualified) {
            queue.push_back(Parent(parent));
        }
        // Siblings
        queue.extend(
            self.graph
                .edges_directed(parent, Outgoing)
                .filter(|e| matches!(e.weight(), Relation::PubParent | Relation::Parent))
                .map(|e| e.target())
                .filter(|&n| n != start)
                .map(Sibling),
        );

        while let Some(node) = queue.pop_front() {
            let node_idx = match node {
                Sibling(n) | Parent(n) | Start(n) => n,
            };
            if predicate(&self.graph[node_idx]) {
                let node_idx = self.should_autoproject(node_idx).unwrap_or(node_idx);
                if let Some(edge) = self.graph.find_edge(start, node_idx) {
                    match self.graph.edge_weight(edge) {
                        Some(Relation::Parent | Relation::PubParent) => (),
                        Some(_) => return Some(node_idx),
                        None => unreachable!("Edge must exist to get here"),
                    }
                }
                self.graph.add_edge(start, node_idx, Relation::Reference);
                return Some(node_idx);
            }

            match node {
                Sibling(_) => (),
                Parent(node) => {
                    if let Some(parent_parent) = self
                        .graph
                        .edges_directed(node, Incoming)
                        .filter(|e| matches!(e.weight(), Relation::Parent | Relation::PubParent))
                        .map(|e| e.source())
                        .next()
                    {
                        queue.push_back(Parent(parent_parent));
                    }
                }
                Start(node) => (),
            }
        }
        None
    }

    fn analyze_expr(
        &mut self,
        expr: Spanned<Intern<CstExpr<UntypedCst>>>,
        vars: &[Var],
        mode: LookupMode,
    ) -> ControlFlow<<UntypedCst as Syntax>::Expr, <UntypedCst as Syntax>::Expr> {
        match *expr.0 {
            CstExpr::Ident(name) => {
                if let Some(var) = vars.iter().rev().find(|n| *n.name == *name.0.0) {
                    ControlFlow::Continue(expr)
                } else if let Some(index) =
                    self.resolve_name(name.0, self.node_stack.last().copied().unwrap())
                {
                    ControlFlow::Continue(expr.modify(CstExpr::Item(ItemId(index.index()))))
                } else {
                    self.errors.push(errors::not_defined(name.0));
                    ControlFlow::Continue(expr.modify(CstExpr::Hole(name)))
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

                let expr = self.autoproject_expr(expr, self.node_stack.last().copied().unwrap());
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
            CstExpr::Hole(v) => ControlFlow::Continue(expr.modify(CstExpr::Hole(v))),
            CstExpr::Item(item_id) => ControlFlow::Continue(expr.modify(CstExpr::Item(item_id))),
            CstExpr::Type(ty) => {
                ControlFlow::Continue(expr.modify(CstExpr::Type(self.resolve_type(ty))))
            }
            CstExpr::Let(..) => unimplemented!(),
        }
    }

    fn pre_register_fields(&mut self, fields: Vec<Field<UntypedCst>>) -> Vec<Field<UntypedCst>> {
        fields
            .into_iter()
            .filter_map(|field| {
                match field {
                    Field::Def(field) => {
                        let node_stack_top = self.node_stack.last().copied().unwrap();
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
                            self.graph.add_edge(node_stack_top, n, Relation::Parent);
                            self.trie.insert(key, n);
                            n
                        };

                        // Recurse into nested ProductConstructors so deeply-nested fields
                        // are also pre-registered before any resolution happens.
                        self.node_stack.push(new_node);
                        if let CstExpr::ProductConstructor { fields } = *field.value.0 {
                            self.pre_register_fields(fields.to_vec());
                        }

                        self.node_stack.pop();
                        self.path.pop();
                        Some(Field::Def(field))
                    }

                    Field::PubDef(field) => {
                        let node_stack_top = self.node_stack.last().copied().unwrap();
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
                            self.graph.add_edge(node_stack_top, n, Relation::PubParent);
                            self.trie.insert(key, n);
                            n
                        };

                        // Recurse into nested ProductConstructors so deeply-nested fields
                        // are also pre-registered before any resolution happens.
                        self.node_stack.push(new_node);
                        if let CstExpr::ProductConstructor { fields } = *field.value.0 {
                            self.pre_register_fields(fields.to_vec());
                        }

                        self.node_stack.pop();
                        self.path.pop();
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

        let old_node = self.node_stack.last().copied().unwrap();

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

        self.node_stack.push(new_node);

        let new_field_def = FieldDef {
            name,
            ty: None,
            value: expr,
        };

        self.node_stack.pop();
        self.path.pop();
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
                    Field::Def(field) | Field::PubDef(field) => {
                        // Retrieve the node index that phase 1 already created.
                        self.path.push(field.name.0);
                        let node_index = *self
                            .trie
                            .get(&Key::from(self.path.clone()))
                            .expect("node must exist after pre-registration");

                        let old_node = self.node_stack.last().copied().unwrap();
                        self.node_stack.push(node_index);

                        // Actual transformations
                        let value = self.analyze_expr(field.value, vars).into_value();
                        let f_type = field.ty.map(|ty| self.resolve_type(ty));

                        // Patch the value into the already-existing node.
                        self.graph[node_index].value.set(value).unwrap();

                        self.node_stack.pop();
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
        if let CstExpr::Item(item_id) = *l.0 {
            self.resolve_name(r.0, NodeIndex::from(item_id.0 as u32));
        }
        self.context = old_context;
        ControlFlow::Continue(expr.convert(CstExpr::FieldAccess(l, r)))
    }

    fn resolve_type(&mut self, ty: <UntypedCst as Syntax>::Type) -> <UntypedCst as Syntax>::Type {
        match *ty.0 {
            CstType::Generic(_)
            | CstType::Particle(_)
            | CstType::Unit
            | CstType::Num
            | CstType::Bool
            | CstType::String
            | CstType::Hole => ty,
            CstType::Func(l, r) => {
                let l = self.resolve_type(l);
                let r = self.resolve_type(r);
                ty.modify(CstType::Func(l, r))
            }
            CstType::Item(item_id, spanneds) => todo!(),
            CstType::GenericFun(spanned, spanned1) => todo!(),
            CstType::GenericApp(spanned, spanned1) => todo!(),
            CstType::User(name, generics) => {
                if let Some(index) = self.resolve_name(name, self.current_node()) {
                    let id = ItemId(index.index());
                    ty.modify(CstType::Item(id, generics))
                } else {
                    self.errors.push(errors::not_defined(name));
                    ty.modify(CstType::Hole)
                }
            }
            CstType::Prod(row) => ty.modify(CstType::Prod(CstClosedRow {
                values: row
                    .values
                    .iter()
                    .map(|ty| self.resolve_type(*ty))
                    .collect::<Vec<_>>()
                    .leak(),
                ..row
            })),
            CstType::Sum(row) => ty.modify(CstType::Prod(CstClosedRow {
                values: row
                    .values
                    .iter()
                    .map(|ty| self.resolve_type(*ty))
                    .collect::<Vec<_>>()
                    .leak(),
                ..row
            })),

            CstType::Label(label, inner) => {
                ty.modify(CstType::Label(label, self.resolve_type(inner)))
            }
        }
    }

    fn lift(self, main_expr: <UntypedCst as Syntax>::Expr) -> Environment<UntypedCst> {
        self.debug();
        self.graph
            .node_indices()
            .filter_map(|node| {
                let element = &self.graph[node];
                let ty = element.ty?;
                let body = *element.value.get()?;
                let item = Item {
                    kind: crate::resource::rep::frontend::entry::ItemKind::Function(FunctionItem {
                        name: element.name,
                        sig: ty,
                        body,
                    }),
                };
                Some((node, item))
            })
            .collect()
    }
}
