use internment::Intern;
use petgraph::graph::{DiGraph, NodeIndex};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    passes::midend::{environment::Environment, typing::Type},
    resource::{
        errors::{CompResult, DynamicErr},
        rep::{
            ast::{Expr, NodeId, Untyped},
            common::Ident,
            entry::{FunctionItem, ItemKind},
            quantifier::QualifierFragment,
        },
    },
};

pub struct Mentioned {
    mentions: FxHashSet<Expr<Untyped>>,
}

pub struct NameOut {
    items: FxHashMap<NodeId, Mentioned>,
}

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
    env: Environment,
    current_parent: QualifierFragment,
    pub dag: DiGraph<NodeIndex, ()>,
}

impl Resolver {
    pub fn new(env: Environment) -> Self {
        let mut s = Self {
            env,
            current_parent: QualifierFragment::Package(Intern::from_ref("Main")),
            dag: DiGraph::new(),
        };
        s.build();
        s
    }

    fn build(&mut self) {
        let mentions = self.analyze();
        // let out = self.modify()
        todo!()
    }

    fn analyze(&mut self) -> CompResult<()> {
        let main_idx = self
            .env
            .get_from_context(
                &QualifierFragment::Func(Intern::from_ref("main")),
                &QualifierFragment::Package(Intern::from_ref("Main")),
            )
            .unwrap();
        let main = self.env.value(main_idx)?;
        if let ItemKind::Function(f) = main.kind {
            self.analyze_func(&f)
        } else {
            panic!()
        }

        for item in self.env.graph.node_weights() {
            match item.kind {
                ItemKind::Filename(intern) => todo!(),
                ItemKind::Package(package_entry) => todo!(),
                ItemKind::Function(function_item) => todo!(),
                ItemKind::Type(_, simple_span) => todo!(),
                ItemKind::Extern { name, sig } => todo!(),
                ItemKind::Dummy(_) => todo!(),
            }
        }
    }

    fn analyze_type(&mut self, t: Intern<Type>) -> CompResult<Intern<Type>> {
        match *t {
            Type::Func(l, r) => {
                let l = self.analyze_type(l)?;
                let r = self.analyze_type(r)?;
                let t = Type::Func(Intern::from(*l), Intern::from(*r)).into();
                Ok(t)
            }
            Type::User(spanned) => {
                let frag = &QualifierFragment::Type(spanned.0);
                let the_item_idx = self.env.get_from_context(frag, &self.current_parent)?;
                let the_item = self.env.value(the_item_idx)?;
                if let ItemKind::Type(t, _) = the_item.kind {
                    Ok(t.into())
                } else {
                    Err(DynamicErr::new(format!("{} is not a type", spanned.0))
                        .label("", spanned.1)
                        .into())
                }
            }
            _ => Ok(t),
        }
    }

    fn analyze_func(&mut self, f: &FunctionItem<Untyped>) -> CompResult<()> {
        self.analyze_type(f.sig.0)?;
        todo!()
    }

    pub fn finish(self) -> Environment {
        self.env
    }
}
