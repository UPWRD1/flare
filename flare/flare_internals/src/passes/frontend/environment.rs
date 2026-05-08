use internment::Intern;

use petgraph::{
    prelude::StableDiGraph,
    stable_graph::{EdgeReference, NodeIndex},
    visit::{EdgeRef as _, },
};
use rustc_hash::FxHashMap;

use crate::resource::{
    errors::{self, CompResult, CompilerErr, ErrorCollection},
    rep::{
        common::{Spanned, Syntax},
        frontend::{
            ast::Untyped,
            cst::{
                CstExpr, Field, FieldDef, NodeKind, PackageCollection,  PortKind,
                UntypedCst,
            },
            csttypes::CstType,
            entry::Item,
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
                Config::RankDir(petgraph::dot::RankDir::LR),
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

    fn outputs_of(&self, node: NodeIndex) -> Option<(NodeIndex, PortKind)> {
        self.graph
            .edges_directed(node, petgraph::Direction::Outgoing)
            .map(|e| (e.target(), *e.weight()))
            .next().inspect(|p| {dbg!(p);})
    }

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
    fn resolve_name(
        &mut self,
        the_name: Spanned<Intern<String>>,
        current_node: NodeIndex,
    ) -> NodeIndex {
        macro_rules! push_parent_node {
            ($worklist:ident, $node:ident) => {{
                let Some((parent, _)) = self.outputs_of($node).filter(|(_, pk)| matches!(pk, PortKind::Input(_))) else {
            
                    println!("FAIL: {:?}", $node);
                        break;
                };
                $worklist.push(parent);
            }};
        }
        // Walk up the record containment chain.
        dbg!(current_node, the_name);
        let mut worklist = vec![current_node];
        while let Some(node) = worklist.pop() {
            dbg!(&worklist, node);
            match &self.graph[node] {
                NodeKind::Def { name } => {
                    if *name == the_name.0 {
                        return node;
                    } else {
                        push_parent_node!(worklist,node)
                    }
                }
                NodeKind::Ref => todo!(),
                NodeKind::Lam => {
                    let (binder, _) = self
                        .find_incoming_of(node, |_, pk| matches!(pk, PortKind::Input(0)))
                        .unwrap();
                    dbg!(binder);
                    if let NodeKind::Def { name } = self.graph[binder]
                        && name == the_name.0
                    {
                        return binder;
                    }
                    push_parent_node!(worklist, node)
                }
                NodeKind::Bin(_) => {
                    push_parent_node!(worklist, node)
                }
                NodeKind::App => todo!(),
                NodeKind::Record
                    // if self
                    //     .find_incoming_of(node, |idx, _| idx == current_node)
                    //     .is_some() =>
                    =>
                {
                    if let Some((sibling, _)) = self
                        .incoming_of(node)
                        .filter(|(idx, _)| *idx != node)
                        .find(|(idx, edge)| {
                            let weight = &self.graph[*idx];
                            
    matches!(&self.graph[*idx], NodeKind::Def { name: n } if *n == the_name.0)
                        })
                    {
                        return sibling;
                    } else {
                        push_parent_node!(worklist, node)
                       }
                }
                // NodeKind::Record => {
                //     let Some((parent, _)) = self.outputs_of(node) else {
                //         break;
                //     };
                //     worklist.push(parent);
                //     continue;
                // }
                NodeKind::Project { label } => todo!(),
                NodeKind::Extend { label } => todo!(),
                NodeKind::Inject { label } => todo!(),
                NodeKind::Match { labels } => todo!(),
                NodeKind::Lit(expr_lit) => todo!(),
                NodeKind::PrimitiveTy(primitive_type) => todo!(),
                NodeKind::Universe { level } => todo!(),
                NodeKind::Pi => {
                    let (domain, _) = self
                        .find_incoming_of(node, |_, pk| matches!(pk, PortKind::Input(0)))
                        .unwrap();

                    if let NodeKind::Def { name } = self.graph[domain]
                        && name == the_name.0
                    {
                        return domain;
                    }
                    push_parent_node!(worklist, node)
                }
                NodeKind::RowTy { labels, open } => todo!(),
                NodeKind::Mu => todo!(),
                NodeKind::CoreLam { arity } => todo!(),
                NodeKind::Erased => todo!(),
                NodeKind::Hole { .. } => todo!(),
            }
            continue;
        }

        self.errors.push(errors::not_defined(the_name));
        self.graph.add_node(NodeKind::Hole { name: the_name.0 })
    }

    fn analyze_expr(&mut self, expr: Spanned<Intern<CstExpr<UntypedCst>>>, current_node: NodeIndex) -> NodeIndex {
        self.debug();
        self.scope.push(current_node);
       let out =  match *expr.0 {
            CstExpr::Ident(n) => {
                let v = self.resolve_name(n.0, current_node);
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
                
                let l = self.analyze_expr(l,  bin)  ;              let r = self.analyze_expr(r, bin);

                self.graph.add_edge(l, bin, PortKind::Input(0));
                self.graph.add_edge(r, bin, PortKind::Input(1));
                bin
            }
            CstExpr::Call(spanned, spanned1) => todo!(),
            CstExpr::FieldAccess(spanned, label) => todo!(),
            CstExpr::Match(spanned, match_arms) => todo!(),
            CstExpr::Lambda(var, expr) => {
                let lam = self.graph.add_node(NodeKind::Lam);
                dbg!(lam);
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

    fn resolve_type(&mut self, ty: <UntypedCst as Syntax>::Type,current_node: NodeIndex) -> NodeIndex {
        match *ty.0 {
            CstType::Primitive(p) => self.graph.add_node(NodeKind::PrimitiveTy(p)),
            CstType::Func(l, r) => {
                let pi = self.graph.add_node(NodeKind::Pi);
                let l = self.resolve_type(l, current_node);
                let r = self.resolve_type(r,current_node);
                self.graph.add_edge(l, pi, PortKind::Input(0));
                self.graph.add_edge(r, pi, PortKind::Input(1));
                pi
            }
            CstType::User(n) => {
                let v = self.resolve_name(n, current_node);
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
