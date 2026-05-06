use std::{cell::OnceCell, ops::ControlFlow};

use internment::Intern;

use itertools::Itertools;
use petgraph::{
    prelude::StableDiGraph,
    stable_graph::{EdgeReference, NodeIndex},
    visit::EdgeRef as _,
};
use radix_trie::{Trie, TrieKey};
use rustc_hash::FxHashMap;

use crate::{
    passes::frontend::typing::PrimitiveType,
    resource::{
        errors::{self, CompResult, CompilerErr, ErrorCollection},
        rep::{
            common::{FlareSpan, HasSpan, Spanned, Syntax},
            frontend::{
                ast::{ItemId, Untyped},
                cst::{CstExpr, Field, FieldDef, MatchArm, PackageCollection, Pattern, UntypedCst},
                csttypes::{CstClosedRow, CstType},
                entry::{FunctionItem, Item},
            },
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
    is_pub: bool,
    return_expr: Option<S::Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Relation {
    Reference,
    Parent,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Var {
    name: Intern<String>,
}

impl<S: Syntax> Element<S> {
    pub fn new(
        name: Spanned<Intern<String>>,
        value: impl Into<OnceCell<S::Expr>>,
        ty: impl Into<Option<S::Type>>,
        is_pub: bool,
        return_expr: impl Into<Option<S::Expr>>,
    ) -> Self {
        Self {
            name,
            value: value.into(),
            ty: ty.into(),
            is_pub,
            return_expr: return_expr.into(),
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
    errors: Vec<CompilerErr>,
}

#[derive(Clone, Copy)]
#[repr(u8)]
enum Expect {
    Value,
    Struct,
}

pub type Environment<S> = FxHashMap<NodeIndex, Item<S>>;

impl EnvironmentBuilder<UntypedCst> {
    /// Build the environment from a given `PackageCollection`
    /// # Errors
    /// - on invalid names,
    ///
    pub fn build(program: PackageCollection<UntypedCst>) -> CompResult<Environment<UntypedCst>> {
        let mut graph: StableDiGraph<Element<UntypedCst>, Relation> = StableDiGraph::default();
        let root_node = graph.add_node(Element::new(
            Spanned::default_with("Root".to_string()),
            OnceCell::new(),
            Spanned::default_with(CstType::Primitive(PrimitiveType::Num)),
            true,
            Spanned::default_with(CstExpr::Ident(Untyped(Spanned::default_with(
                String::from("Main"),
            )))),
        ));
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
            ..Default::default()
        };

        let res = me
            .enter_context_root(
                |me| me.analyze_expr(root_obj, &[], Expect::Value),
                Spanned::default_with(String::from("Root")),
            )
            .into_value();
        // dbg!(res);
        // me.debug();
        let env_map = me.lift();

        if me.errors.is_empty() {
            dbg!(&env_map);
            Ok(env_map)
        } else {
            Err(ErrorCollection::new(me.errors).into())
        }
    }

    fn current_node(&self) -> NodeIndex {
        self.node_stack.last().copied().unwrap()
    }

    fn resolve_name(
        &mut self,
        n: <UntypedCst as Syntax>::Name,
        start: NodeIndex,
    ) -> Option<NodeIndex> {
        self.lookup_local(n.0, start)
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
                    "label = \"{} = {}; pub: {}; ret: {}; {}\"",
                    i.index(),
                    e.name,
                    e.is_pub,
                    e.return_expr.is_some(),
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

    fn maybe_autoproject(&self, node: NodeIndex, span: FlareSpan) -> <UntypedCst as Syntax>::Expr {
        if let Some(ret) = self.graph[node].return_expr {
            ret
        } else {
            Spanned(CstExpr::Item(ItemId(node.index())).into(), span)
        }
    }
    /// Get the children of a node
    fn children_of(&self, node: NodeIndex) -> impl Iterator<Item = (NodeIndex, Relation)> + '_ {
        self.graph
            .edges_directed(node, petgraph::Direction::Outgoing)
            .map(|e| (e.target(), *e.weight()))
    }

    /// Get the parent of a node
    fn parent_of(&self, node: NodeIndex) -> Option<NodeIndex> {
        self.graph
            .edges_directed(node, petgraph::Direction::Incoming)
            .find(|e| matches!(e.weight(), Relation::Parent))
            .map(|e| e.source())
    }

    fn lookup_local(&mut self, name: Intern<String>, current: NodeIndex) -> Option<NodeIndex> {
        let parent = self.parent_of(current)?;
        // Sibling lookup
        let child = self
            .children_of(parent)
            .find(|(child, _)| self.graph[*child].name.0 == name)
            .map(|(child, _)| child);
        if let Some(child) = child {
            self.graph
                .add_edge(self.current_node(), child, Relation::Reference);
        }
        child
    }

    fn is_same_scope(&self, a: NodeIndex, b: NodeIndex) -> bool {
        self.parent_of(a) == self.parent_of(b)
    }

    fn lookup_in(
        &mut self,
        base: NodeIndex,
        name: Intern<String>,
        // current: NodeIndex,
    ) -> Option<NodeIndex> {
        let current = self.current_node();
        let child = self
            .children_of(base)
            .find(|(child, rel)| {
                let elem = &self.graph[*child];
                if elem.name.0 != name {
                    return false;
                }

                match rel {
                    Relation::Parent => {
                        // only allowed if we're inside base's scope or if pub
                        elem.is_pub || self.is_same_scope(*child, current)
                    }

                    _ => false,
                }
            })
            .map(|(child, _)| child);

        if let Some(child) = child {
            self.graph
                .add_edge(self.current_node(), child, Relation::Reference);
        }
        child
    }

    fn analyze_expr(
        &mut self,
        expr: Spanned<Intern<CstExpr<UntypedCst>>>,
        vars: &[Var],
        expect: Expect,
    ) -> ControlFlow<<UntypedCst as Syntax>::Expr, <UntypedCst as Syntax>::Expr> {
        let _ = expect;
        match *expr.0 {
            CstExpr::Ident(name) => {
                if let Some(var) = vars.iter().rev().find(|n| *n.name == *name.0.0) {
                    ControlFlow::Continue(expr)
                } else if let Some(index) = self.lookup_local(name.0.0, self.current_node()) {
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

                ControlFlow::Break(expr)
            }
            CstExpr::VariantConstructor { name, value } => {
                let value =
                    value.map(|value| self.analyze_expr(value, vars, Expect::Value).into_value());
                ControlFlow::Continue(expr.modify(CstExpr::VariantConstructor { name, value }))
            }

            CstExpr::Call(func, arg) => {
                let func = self.analyze_expr(func, vars, Expect::Value).into_value();

                let arg = self.analyze_expr(arg, vars, Expect::Struct).into_value();
                ControlFlow::Continue(expr.convert(CstExpr::Call(func, arg)))
            }

            CstExpr::Bin(l, op, r) => {
                let l = self.analyze_expr(l, vars, Expect::Value).into_value();
                let r = self.analyze_expr(r, vars, Expect::Value).into_value();
                ControlFlow::Continue(expr.modify(CstExpr::Bin(l, op, r)))
            }
            CstExpr::FieldAccess(..) => self.resolve_field_access(expr, vars, expect),
            CstExpr::Match(matchee, branches) => {
                let matchee = self.analyze_expr(matchee, vars, Expect::Value).into_value();

                let branches: Vec<_> = branches
                    .iter()
                    .map(|b| self.resolve_branch(b.pat, b.body, vars))
                    .collect();
                assert!(!branches.is_empty());
                ControlFlow::Continue(expr.modify(CstExpr::Match(matchee, branches.leak())))
            }
            CstExpr::Lambda(arg, body) => self.resolve_lambda(expr, arg, body, vars),
            CstExpr::Lit(_) | CstExpr::Hole(_) | CstExpr::Item(_) => ControlFlow::Continue(expr),
            CstExpr::Type(ty) => {
                ControlFlow::Continue(expr.modify(CstExpr::Type(self.resolve_type(ty, vars))))
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
                            let n = self.graph.add_node(Element::new(
                                field.name,
                                OnceCell::new(),
                                field.ty,
                                field.is_pub,
                                None,
                            )); // value filled in phase 2
                            self.graph.add_edge(node_stack_top, n, Relation::Parent);
                            self.trie.insert(key, n);
                            n
                        };

                        self.path.pop();
                        Some(Field::Def(field))
                    }

                    Field::Macro(field_macro) => match field_macro {
                        crate::resource::rep::frontend::cst::FieldMacro::Import(_) => todo!(),
                        crate::resource::rep::frontend::cst::FieldMacro::Extend(extend) => {
                            todo!()
                        }
                        crate::resource::rep::frontend::cst::FieldMacro::Ret(v) => {
                            let node_stack_top = self.node_stack.last().unwrap();
                            let weight = self.graph.node_weight_mut(*node_stack_top);
                            dbg!(&weight);
                            let enclosing = weight.map(|node| {
                                // if let Some(spanned) = node.return_expr {
                                //     self.errors
                                //         .push(errors::duplicate_return(v.span(), spanned.span()))
                                // } else {
                                node.return_expr = Some(v)
                                // }
                            });

                            None
                        }
                    },
                }
            })
            .collect()
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

                        let old_node = self.node_stack.last().copied().unwrap();
                        self.node_stack.push(node_index);

                        // Actual transformations
                        let value = self
                            .analyze_expr(field.value, vars, Expect::Value)
                            .into_value();
                        let f_type = field.ty.map(|ty| self.resolve_type(ty, vars));

                        let ret = self.graph[node_index]
                            .return_expr
                            .map(|expr| self.analyze_expr(expr, &[], Expect::Value).into_value());

                        // Patch the value into the already-existing node.
                        self.graph[node_index].value.set(value).unwrap();

                        if let Some(el) = self.graph.node_weight_mut(node_index) {
                            el.return_expr = ret
                        }

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
        let body = self
            .analyze_expr(body, &branch_vars, Expect::Value)
            .into_value();
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
        let body = self
            .analyze_expr(body, new_vars, Expect::Value)
            .into_value();
        ControlFlow::Continue(expr.convert(CstExpr::Lambda(arg, body)))
    }

    fn resolve_field_access(
        &mut self,
        expr: Spanned<Intern<CstExpr<UntypedCst>>>,
        vars: &[Var],
        expect: Expect,
    ) -> ControlFlow<<UntypedCst as Syntax>::Expr, <UntypedCst as Syntax>::Expr> {
        let CstExpr::FieldAccess(l, r) = *expr.0 else {
            unreachable!()
        };

        let base = self.analyze_expr(l, vars, Expect::Struct).into_value();

        if let CstExpr::Item(item_id) = *base.0 {
            let base = NodeIndex::from(item_id.0 as u32);

            if let Some(target) = self.lookup_in(base, r.0.0) {
                return ControlFlow::Continue(match expect {
                    Expect::Value => self.maybe_autoproject(target, expr.span()),
                    Expect::Struct => expr.convert(CstExpr::Item(ItemId(target.index()))),
                });
            } else {
                self.errors.push(errors::not_defined(r.0));
            }
        }

        ControlFlow::Continue(expr.convert(CstExpr::FieldAccess(base, r)))
    }
    fn resolve_type(
        &mut self,
        ty: <UntypedCst as Syntax>::Type,
        vars: &[Var],
    ) -> <UntypedCst as Syntax>::Type {
        match *ty.0 {
            CstType::Generic(_) | CstType::Primitive(_) | CstType::Hole => ty,
            CstType::Func(l, r) => {
                let l = self.resolve_type(l, vars);
                let r = self.resolve_type(r, vars);
                ty.modify(CstType::Func(l, r))
            }
            CstType::Item(item_id) => todo!(),
            CstType::GenericFun(param, body) => {
                let param = self.resolve_type(param, vars);
                let body = self.resolve_type(body, vars);
                ty.modify(CstType::GenericApp(param, body))
            }
            CstType::GenericApp(tyfun, arg) => {
                let tyfun = self.resolve_type(tyfun, vars);
                let arg = self.resolve_type(arg, vars);
                ty.modify(CstType::GenericApp(tyfun, arg))
            }
            CstType::ForAll(t, within) => ty,
            CstType::User(name) => {
                if let Some(var) = vars.iter().rev().find(|n| *n.name == *name.0) {
                    ty
                } else if let Some(index) = self.resolve_name(name, self.current_node()) {
                    let value = self.graph[index].value.get().unwrap();
                    dbg!(value);
                    let id = ItemId(index.index());
                    ty.modify(CstType::Item(id))
                } else {
                    self.errors.push(errors::not_defined(name));
                    ty.modify(CstType::Hole)
                }
            }
            CstType::Prod(row) => ty.modify(CstType::Prod(CstClosedRow {
                values: row
                    .values
                    .iter()
                    .map(|ty| self.resolve_type(*ty, vars))
                    .collect::<Vec<_>>()
                    .leak(),
                ..row
            })),
            CstType::Sum(row) => ty.modify(CstType::Prod(CstClosedRow {
                values: row
                    .values
                    .iter()
                    .map(|ty| self.resolve_type(*ty, vars))
                    .collect::<Vec<_>>()
                    .leak(),
                ..row
            })),

            CstType::Label(label, inner) => {
                ty.modify(CstType::Label(label, self.resolve_type(inner, vars)))
            }
        }
    }

    fn lift(&mut self) -> Environment<UntypedCst> {
        // let dep = self.graph.filter_map(
        //     |_, n| Some(n),
        //     |_, e| {
        //         if let Relation::Reference = e {
        //             Some(e)
        //         } else {
        //             None
        //         }
        //     },
        // );
        let out = petgraph::algo::tarjan_scc(&self.graph);
        dbg!(&out);
        let mut out = out.into_iter().rev().peekable();

        self.debug();
        // while let Some(t) = out.peek() {
        //     let mut count = 0;
        //     let next_group = out.peeking_take_while(|t| {
        //         dbg!(t);
        //         if t.len() == 1 {
        //             count += 1;
        //             true
        //         } else {
        //             false
        //         }
        //     });
        //     // dbg!(next_group);
        // }

        self.debug();
        self.graph
            .node_indices()
            .filter_map(|node| {
                let element = &self.graph[node];
                let ty = element.ty.or_else(|| {
                    self.errors.push(errors::needs_type(element.name.span()));
                    None
                })?;
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
