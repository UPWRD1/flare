use std::ops::ControlFlow;

use internment::Intern;

use petgraph::{
    prelude::StableDiGraph,
    stable_graph::{EdgeReference, NodeIndex},
    visit::EdgeRef as _,
};
use rustc_hash::FxHashMap;

use crate::resource::{
    errors::{self, CompResult, CompilerErr, ErrorCollection},
    rep::{
        common::{FlareSpan, Spanned, Syntax},
        frontend::{
            ast::{ItemId, Label, Untyped},
            cst::{
                CstExpr, Field, FieldDef, MatchArm, NodeKind, PackageCollection, Pattern, PortKind,
                UntypedCst,
            },
            csttypes::{CstClosedRow, CstType},
            entry::{FunctionItem, Item},
        },
    },
};

// #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct Key(Vec<Intern<String>>);

// impl Key {
//     pub fn from<T>(path: T) -> Self
//     where
//         T: Into<Vec<Intern<String>>>,
//     {
//         Self(path.into())
//     }
// }

// impl TrieKey for Key {
//     fn encode_bytes(&self) -> Vec<u8> {
//         self.0
//             .iter()
//             .flat_map(|s| s.bytes().collect::<Vec<u8>>())
//             .collect()
//     }
// }

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
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Var {
    name: Intern<String>,
    index: NodeIndex,
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

        me.analyze_expr(root_obj, &[]); // dbg!(res);
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
                Config::RankDir(petgraph::dot::RankDir::LR),
            ],
            &render,
            &|_, (i, e)| format!("label = \"{} = {};\"", i.index(), e),
        );
        dbg!(dot);
    }

    fn inputs_of(&self, node: NodeIndex) -> impl Iterator<Item = (NodeIndex, PortKind)> + '_ {
        self.graph
            .edges_directed(node, petgraph::Direction::Incoming)
            .map(|e| (e.source(), *e.weight()))
    }

    fn find_input_of(
        &self,
        node: NodeIndex,
        pred: impl Fn(NodeIndex, PortKind) -> bool,
    ) -> Option<(NodeIndex, PortKind)> {
        self.inputs_of(node).find(|(e, w)| pred(*e, *w))
    }

    fn outputs_of(&self, node: NodeIndex) -> (NodeIndex, PortKind) {
        self.graph
            .edges_directed(node, petgraph::Direction::Incoming)
            .map(|e| (e.target(), *e.weight()))
            .next()
            .unwrap()
    }

    fn analyze_expr(
        &mut self,
        expr: Spanned<Intern<CstExpr<UntypedCst>>>,
        vars: &[Var],
    ) -> NodeIndex {
        match *expr.0 {
            CstExpr::Ident(n) => {
                if let Some(v) = vars.iter().find(|v| v.name == n.0.0) {
                    let ref_node = self.graph.add_node(NodeKind::Ref);
                    // let Some((def_idx, _)) =
                    //     self.find_input_of(v.index, |_, p| matches!(p, PortKind::Aux(0)))
                    // else {
                    //     panic!("Def has no value")
                    // };
                    self.graph.add_edge(v.index, ref_node, PortKind::Reference);
                    ref_node
                } else {
                    self.errors.push(errors::not_defined(n.0));
                    self.graph.add_node(NodeKind::Hole)
                }
            }
            CstExpr::Lit(lit) => self.graph.add_node(NodeKind::Lit(lit)),
            CstExpr::Hole(_) => todo!(),
            CstExpr::Item(item_id) => todo!(),
            CstExpr::ProductConstructor { fields } => {
                let fields = self.pre_register_fields(fields.to_vec());
                let product_node = self.graph.add_node(NodeKind::Record {});
                self.resolve_fields(fields, vars, product_node);
                product_node
            }
            CstExpr::VariantConstructor { name, value } => todo!(),
            CstExpr::Bin(spanned, bin_op, spanned1) => todo!(),
            CstExpr::Call(spanned, spanned1) => todo!(),
            CstExpr::FieldAccess(spanned, label) => todo!(),
            CstExpr::Match(spanned, match_arms) => todo!(),
            CstExpr::Lambda(_, spanned) => todo!(),
            CstExpr::Let(spanned, spanned1, spanned2) => todo!(),
            CstExpr::Type(_) => todo!(),
        }
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

    fn resolve_fields(
        &mut self,
        fields: Vec<FieldDef<UntypedCst>>,
        vars: &[Var],
        product_node: NodeIndex,
    ) {
        let (label_vars, definition_nodes): (Vec<_>, Vec<NodeIndex>) = fields
            .iter()
            .enumerate()
            .map(|(position, field)| {
                let def_index = self.graph.add_node(NodeKind::Def { name: field.name.0 });
                self.graph
                    .add_edge(def_index, product_node, PortKind::Aux(position));

                if let Some(ty) = field.ty {
                    let ty_node = self.resolve_type(ty, vars);
                    self.graph.add_edge(def_index, ty_node, PortKind::Type);
                };
                (
                    Var {
                        name: field.name.0,
                        index: def_index,
                    },
                    def_index,
                )
            })
            .collect();
        let new_vars = [vars, &label_vars].concat();

        for (nth, (field, def_node_idx)) in fields.into_iter().zip(definition_nodes).enumerate() {
            let result_node = self.analyze_expr(field.value, &new_vars);
            self.graph
                .add_edge(result_node, def_node_idx, PortKind::Primary);
        }
        self.debug();
    }

    fn resolve_branch(
        &mut self,
        pat: Spanned<Intern<Pattern<UntypedCst>>>,
        body: Spanned<Intern<CstExpr<UntypedCst>>>,
        vars: &[Var],
    ) -> MatchArm<UntypedCst> {
        todo!()
        // let bindings = pat.0.bindings();
        // // Extend vars with bindings
        // let mut branch_vars: Vec<Var> = vars.to_vec();
        // branch_vars.extend(bindings.iter().map(|v| Var { name: v.0.0 }));
        // let body = self
        //     .analyze_expr(body, &branch_vars, Expect::Value)
        //     .into_value();
        // MatchArm { pat, body }
    }

    fn resolve_lambda(
        &mut self,
        expr: Spanned<Intern<CstExpr<UntypedCst>>>,
        arg: Untyped,
        body: Spanned<Intern<CstExpr<UntypedCst>>>,
        vars: &[Var],
    ) -> ControlFlow<Spanned<Intern<CstExpr<UntypedCst>>>, Spanned<Intern<CstExpr<UntypedCst>>>>
    {
        todo!()
        // let new_vars = &[vars, &[Var { name: arg.0.0 }]].concat();
        // let body = self
        //     .analyze_expr(body, new_vars, Expect::Value)
        //     .into_value();
        // ControlFlow::Continue(expr.convert(CstExpr::Lambda(arg, body)))
    }

    fn resolve_field_access(
        &mut self,
        expr: Spanned<Intern<CstExpr<UntypedCst>>>,
        vars: &[Var],
    ) -> ControlFlow<<UntypedCst as Syntax>::Expr, <UntypedCst as Syntax>::Expr> {
        todo!()
        // let CstExpr::FieldAccess(l, r) = *expr.0 else {
        //     unreachable!()
        // };

        // let base = self.analyze_expr(l, vars, Expect::Struct).into_value();

        // if let CstExpr::Item(item_id) = *base.0 {
        //     let base = NodeIndex::from(item_id.0 as u32);

        //     if let Some(target) = self.lookup_in(base, r.0.0) {
        //         return ControlFlow::Continue(match expect {
        //             Expect::Value => self.maybe_autoproject(target, expr.span()),
        //             Expect::Struct => expr.convert(CstExpr::Item(ItemId(target.index()))),
        //         });
        //     } else {
        //         self.errors.push(errors::not_defined(r.0));
        //     }
        // }

        // ControlFlow::Continue(expr.convert(CstExpr::FieldAccess(base, r)))
    }
    fn resolve_type(&mut self, ty: <UntypedCst as Syntax>::Type, vars: &[Var]) -> NodeIndex {
        match *ty.0 {
            CstType::Primitive(p) => self.graph.add_node(NodeKind::PrimitiveTy(p)),
            CstType::Func(l, r) => {
                let pi = self.graph.add_node(NodeKind::Pi);
                let l = self.resolve_type(l, vars);
                let r = self.resolve_type(r, vars);
                todo!()
            }

            _ => todo!("{ty:?}"),
        }
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
}
