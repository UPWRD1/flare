use internment::Intern;

use petgraph::{
    prelude::StableDiGraph,
    stable_graph::{EdgeReference, NodeIndex},
    visit::EdgeRef as _,
};
use rustc_hash::FxHashMap;

use crate::{
    passes::frontend::matchmatrix::{self, DecisionTree, Occ, SigElem},
    resource::{
        errors::{self, CompResult, CompilerErr, ErrorCollection},
        rep::{
            common::{FlareSpan, Spanned, Syntax},
            frontend::{
                ast::{BinOp, Untyped},
                cst::{
                    CstExpr, Field, FieldDef, NodeKind, PackageCollection, PortKind, UntypedCst,
                },
                csttypes::CstType,
                entry::Item,
            },
        },
    },
};

#[derive(Debug)]
/// The main environment graph structure. Holds all the objects produced by
/// the  parser, and the index of the root.
///
/// Obviously, `Environment` does not implement `Copy`, but it also does not
/// implement `Clone`, since it is ridiculously expensive, and there is no
/// real reason to clone the environment.
#[non_exhaustive]
#[derive(Default)]
pub struct EnvironmentBuilder {
    pub graph: StableDiGraph<NodeKind, PortKind>,
    errors: Vec<CompilerErr>,
    scope: Vec<NodeIndex>,
}

pub type Environment<S> = FxHashMap<NodeIndex, Item<S>>;

impl EnvironmentBuilder {
    /// Build the environment from a given `PackageCollection`
    /// # Errors
    /// - on invalid names,
    ///
    pub fn build(program: PackageCollection<UntypedCst>) -> CompResult<Environment<UntypedCst>> {
        let mut graph: StableDiGraph<NodeKind, PortKind> = StableDiGraph::default();
        let root_node = graph.add_node(NodeKind::Record {});

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
            ..Default::default()
        };

        me.analyze_expr(root_obj, root_node); // dbg!(res);
        let env_map = me.lift();

        if me.errors.is_empty() {
            dbg!(&env_map);
            Ok(env_map)
        } else {
            Err(ErrorCollection::new(me.errors).into())
        }
    }

    #[allow(dead_code, clippy::unwrap_used, clippy::dbg_macro)]
    #[deprecated]
    pub fn debug(&self) {
        use petgraph::dot::Config;
        let render = |_, v: EdgeReference<'_, _>| format!("label = \"{:?}\"", v.weight());
        let dot = petgraph::dot::Dot::with_attr_getters(
            &self.graph,
            &[
                Config::EdgeNoLabel,
                Config::NodeNoLabel,
                Config::RankDir(petgraph::dot::RankDir::BT),
            ],
            &render,
            &|_, (i, e)| format!("label = \"{} = {};\"", i.index(), e),
        );
        dbg!(dot);
    }

    fn incoming_of(&self, node: NodeIndex) -> impl Iterator<Item = (NodeIndex, PortKind)> + '_ {
        self.graph
            .edges_directed(node, petgraph::Direction::Incoming)
            .map(|e| (e.source(), *e.weight()))
    }

    fn find_incoming_of(
        &self,
        node: NodeIndex,
        pred: impl Fn(NodeIndex, PortKind) -> bool,
    ) -> Option<(NodeIndex, PortKind)> {
        self.incoming_of(node).find(|(e, w)| pred(*e, *w))
    }

    // fn outputs_of(&self, node: NodeIndex) -> Option<(NodeIndex, PortKind)> {
    //     self.graph
    //         .edges_directed(node, petgraph::Direction::Outgoing)
    //         .map(|e| (e.target(), *e.weight()))
    //         .next()
    //         .inspect(|p| {
    //             dbg!(p);
    //         })
    // }

    /// Find a Def node by name among the direct children of `parent_record`.
    fn find_child_by_name(
        &self,
        parent_record: NodeIndex,
        name: Intern<String>,
    ) -> Option<NodeIndex> {
        self.incoming_of(parent_record)
            .filter(|(_, e)| matches!(e, PortKind::Input(_)))
            .find(
                |&(child, _)| matches!(&self.graph[child], NodeKind::Def { name: n } if *n == name),
            )
            .map(|(node, _)| node)
    }
    /// Resolve a name from a given node's context.
    fn resolve_name(&mut self, the_name: Spanned<Intern<String>>) -> NodeIndex {
        for &scope_node in self.scope.iter().rev() {
            match &self.graph[scope_node] {
                NodeKind::Def { name } if *name == the_name.0 => return scope_node,

                NodeKind::Lam | NodeKind::Pi => {
                    if let Some((binder, _)) =
                        self.find_incoming_of(scope_node, |_, pk| matches!(pk, PortKind::Input(0)))
                        && let NodeKind::Def { name } = &self.graph[binder]
                        && *name == the_name.0
                    {
                        return binder;
                    }
                }

                NodeKind::Record => {
                    if let Some(child) = self.find_child_by_name(scope_node, the_name.0) {
                        return child;
                    }
                }

                _ => {}
            }
        }

        self.errors.push(errors::not_defined(the_name));
        self.graph.add_node(NodeKind::Hole { name: the_name.0 })
    }

    fn analyze_expr(
        &mut self,
        expr: Spanned<Intern<CstExpr<UntypedCst>>>,
        current_node: NodeIndex,
    ) -> NodeIndex {
        self.scope.push(current_node);
        let out = match *expr.0 {
            CstExpr::Ident(n) => {
                let v = self.resolve_name(n.0);
                let ref_node = self.graph.add_node(NodeKind::Ref);
                // let Some((def_idx, _)) =
                //     self.find_input_of(v.index, |_, p| matches!(p, PortKind::Aux(0)))
                // else {
                //     panic!("Def has no value")
                // };
                self.graph.add_edge(v, ref_node, PortKind::Reference);
                ref_node
            }
            CstExpr::Lit(lit) => self.graph.add_node(NodeKind::Lit(lit)),
            CstExpr::Hole(_) => todo!(),
            CstExpr::Item(item_id) => todo!(),
            CstExpr::ProductConstructor { fields } => {
                let product_node = self.graph.add_node(NodeKind::Record {});
                let fields = self.pre_register_fields(fields.to_vec());
                self.resolve_fields(fields, product_node);
                product_node
            }
            CstExpr::VariantConstructor { name, value } => todo!(),
            CstExpr::Bin(l, bin_op, r) => {
                let bin = self.graph.add_node(NodeKind::Bin(bin_op));

                let l = self.analyze_expr(l, bin);
                let r = self.analyze_expr(r, bin);

                self.graph.add_edge(l, bin, PortKind::Input(0));
                self.graph.add_edge(r, bin, PortKind::Input(1));
                bin
            }
            CstExpr::Call(l, r) => {
                let app = self.graph.add_node(NodeKind::App);
                let l = self.analyze_expr(l, current_node);
                let r = self.analyze_expr(r, current_node);
                self.graph.add_edge(l, app, PortKind::Input(0));
                self.graph.add_edge(r, app, PortKind::Input(1));
                app
            }
            CstExpr::FieldAccess(spanned, label) => todo!(),
            CstExpr::Match(scrutinee, branches) => {
                let (patterns, actions): (Vec<_>, Vec<_>) =
                    branches.iter().map(|b| (b.pat, b.body)).unzip();
                let patterns: Vec<_> = patterns.iter().map(|p| *p.0).collect();
                let decision_tree = matchmatrix::compile(&patterns);
                decision_tree.print(0);
                self.translate_decision_tree(scrutinee, decision_tree, &actions, current_node)
            }
            CstExpr::Lambda(var, expr) => {
                let lam = self.graph.add_node(NodeKind::Lam);
                let arg = self.graph.add_node(NodeKind::Def { name: var.0.0 });
                self.graph.add_edge(arg, lam, PortKind::Input(0));

                let body = self.analyze_expr(expr, lam);
                self.graph.add_edge(body, lam, PortKind::Input(1));
                lam
            }
            CstExpr::Let(spanned, spanned1, spanned2) => todo!(),
            CstExpr::Type(_) => todo!(),
        };
        self.scope.pop();
        out
    }

    fn pre_register_fields(&self, fields: Vec<Field<UntypedCst>>) -> Vec<FieldDef<UntypedCst>> {
        fields
            .into_iter()
            .filter_map(|field| match field {
                Field::Def(field) => Some(field),

                Field::Macro(field_macro) => match field_macro {
                    crate::resource::rep::frontend::cst::FieldMacro::Import(_) => todo!(),
                    crate::resource::rep::frontend::cst::FieldMacro::Extend(extend) => {
                        todo!()
                    }
                    crate::resource::rep::frontend::cst::FieldMacro::Ret(value) => Some(FieldDef {
                        name: value.convert("%return".to_string()),
                        is_pub: true,
                        ty: None,
                        value,
                    }),
                },
            })
            .collect()
    }

    fn resolve_fields(&mut self, fields: Vec<FieldDef<UntypedCst>>, product_node: NodeIndex) {
        let definition_nodes: Vec<NodeIndex> = fields
            .iter()
            .enumerate()
            .map(|(position, field)| {
                let def_index = self.graph.add_node(NodeKind::Def { name: field.name.0 });
                self.graph
                    .add_edge(def_index, product_node, PortKind::Input(position));

                if let Some(ty) = field.ty {
                    let ty_node = self.resolve_type(ty, def_index);
                    self.graph.add_edge(def_index, ty_node, PortKind::Type);
                };
                def_index
            })
            .collect();
        for (nth, (field, def_node_idx)) in fields.into_iter().zip(definition_nodes).enumerate() {
            let result_node = self.analyze_expr(field.value, product_node);
            self.graph
                .add_edge(result_node, def_node_idx, PortKind::Output);
        }
    }

    fn resolve_type(
        &mut self,
        ty: <UntypedCst as Syntax>::Type,
        current_node: NodeIndex,
    ) -> NodeIndex {
        self.scope.push(current_node);
        let out = match *ty.0 {
            CstType::Primitive(p) => self.graph.add_node(NodeKind::PrimitiveTy(p)),
            CstType::Func(l, r) => {
                let pi = self.graph.add_node(NodeKind::Pi);
                let l = self.resolve_type(l, current_node);
                let r = self.resolve_type(r, current_node);
                self.graph.add_edge(l, pi, PortKind::Input(0));
                self.graph.add_edge(r, pi, PortKind::Input(1));
                pi
            }
            CstType::User(n) => {
                let v = self.resolve_name(n);
                let ref_node = self.graph.add_node(NodeKind::Ref);
                // let Some((def_idx, _)) =
                //     self.find_input_of(v.index, |_, p| matches!(p, PortKind::Aux(0)))
                // else {
                //     panic!("Def has no value")
                // };
                self.graph.add_edge(v, ref_node, PortKind::Reference);
                ref_node
            }

            _ => todo!("{ty:?}"),
        };
        self.scope.pop();
        out
    }

    fn lift(&self) -> Environment<UntypedCst> {
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
        todo!()
    }

    fn translate_dtree_occ(
        &mut self,
        occ: Occ,
        scrutinee: Spanned<Intern<CstExpr<UntypedCst>>>,
        current_node: NodeIndex,
    ) -> NodeIndex {
        match occ {
            Occ::Base => self.analyze_expr(scrutinee, current_node),
            Occ::Proj(occ, l) => {
                let subtree = self.translate_dtree_occ(*occ, scrutinee, current_node);
                todo!()
            }
            Occ::Unwrap(occ, l) => {
                let subtree = self.translate_dtree_occ(*occ, scrutinee, current_node);
                todo!()
            }
        }
    }

    fn translate_sigelem(&mut self, sigelem: SigElem, matchee_span: FlareSpan) -> NodeIndex {
        match sigelem {
            SigElem::Label(label) => unimplemented!("use translate_sigelem_pat"),
            SigElem::Lit(lit) => self.graph.add_node(NodeKind::Lit(lit)),
        }
    }

    fn translate_decision_tree(
        &mut self,
        matchee: Spanned<Intern<CstExpr<UntypedCst>>>,
        tree: DecisionTree,
        actions: &Vec<Spanned<Intern<CstExpr<UntypedCst>>>>,
        current_node: NodeIndex,
    ) -> NodeIndex {
        match tree {
            DecisionTree::Fail => todo!(),
            DecisionTree::Leaf(i) => self.analyze_expr(actions[i], current_node),
            DecisionTree::Switch {
                occ,
                cases,
                default,
            } => {
                let case_lambdas: Vec<NodeIndex> = todo!();
                //cases
                // .into_iter()
                // .enumerate()
                // .map(|(i, (label, subtree))| {
                //     let body =
                //         self.translate_decision_tree(matchee, subtree, actions, current_node);
                //     let param = Untyped(matchee.convert(i.to_string()));
                //     let arg = matchee.convert(Expr::Ident(param));

                //     let unlabeling: Spanned<Intern<Expr<Untyped>>> =
                //         matchee.convert(Expr::Unlabel(arg, label));
                //     body
                // })
                // .collect_vec();

                let branches = case_lambdas
                    .into_iter()
                    .reduce(|l, r| {
                        let branch = self.graph.add_node(NodeKind::Branch);
                        self.graph.add_edge(l, branch, PortKind::Input(0));
                        self.graph.add_edge(r, branch, PortKind::Input(1));
                        branch
                    })
                    .expect("Branches was empty; match has no arms");

                let matchee = self.translate_dtree_occ(occ, matchee, current_node);
                let app = self.graph.add_node(NodeKind::App);
                self.graph.add_edge(branches, app, PortKind::Input(0));
                self.graph.add_edge(matchee, app, PortKind::Input(1));
                app
            }
            DecisionTree::IfEq {
                occ,
                lit,
                then,
                else_,
            } => {
                let occ = self.translate_dtree_occ(occ, matchee, current_node);
                let lit = self.translate_sigelem(lit, matchee.1);
                let eq = self.graph.add_node(NodeKind::Bin(BinOp::Eq));
                self.graph.add_edge(occ, eq, PortKind::Input(0));
                self.graph.add_edge(lit, eq, PortKind::Input(1));

                let then = self.translate_decision_tree(matchee, *then, actions, current_node);
                let else_ = self.translate_decision_tree(matchee, *else_, actions, current_node);
                let if_node = self.graph.add_node(NodeKind::If);
                self.graph.add_edge(eq, if_node, PortKind::Input(0));
                self.graph.add_edge(then, if_node, PortKind::Input(1));
                self.graph.add_edge(else_, if_node, PortKind::Input(2));
                if_node
            }
        }
    }
}
