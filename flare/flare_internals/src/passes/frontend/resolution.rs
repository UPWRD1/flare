use chumsky::span::SimpleSpan;
use internment::Intern;
use petgraph::{graph::NodeIndex, visit::IntoNodeReferences};

type DiGraph<N, E> = petgraph::graph::DiGraph<N, E>;
use crate::{
    passes::frontend::environment::Environment,
    resource::{
        errors::{CompResult, DynamicErr},
        rep::{
            common::Spanned,
            frontend::{
                ast::{Untyped, UntypedAst},
                cst::{CstExpr, UntypedCst},
                entry::{FunctionItem, Item, ItemKind, PackageEntry},
                quantifier::QualifierFragment,
            },
        },
    },
};

pub struct Resolver {
    env: Environment<UntypedCst>,
    current_parent: QualifierFragment,
    current_dag_node: Option<NodeIndex>,
    pub dag: DiGraph<usize, ()>,
    main_dag_idx: Option<NodeIndex>,
}
impl Resolver {
    pub fn new(env: Environment<UntypedCst>) -> Self {
        Self {
            env,
            current_parent: QualifierFragment::Root,
            current_dag_node: None,
            dag: DiGraph::new(),
            main_dag_idx: None,
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
        loop {}
    }

    fn analyze_item(&mut self, node_idx: NodeIndex, item: &Item<UntypedCst>) -> Item<UntypedCst> {
        let dag_idx = if let Some((node_idx, _)) = self
            .dag
            .node_references()
            .find(|(_, x)| **x == node_idx.index())
        {
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
                if *f.name.0 == "main" {
                    self.main_dag_idx = Some(dag_idx);
                }
                let f = self.analyze_func(f, dag_idx);

                Item::new(ItemKind::Function(f))
            }
            ItemKind::Type(n, g, t) => Item::new(ItemKind::Type(n, vec![].leak(), t)),
            ItemKind::Extern { name, args, sig } => Item::new(ItemKind::Extern { name, args, sig }),
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
    ) -> FunctionItem<UntypedCst> {
        self.in_context(
            |me| {
                let body = me.analyze_expr(the_func.body, &[]);
                FunctionItem { body, ..the_func }
            },
            idx,
        )
    }

    #[allow(unused_variables)]
    fn analyze_expr(
        &mut self,
        expr: Spanned<Intern<CstExpr<Untyped>>>,
        vars: &[(Intern<String>, Spanned<Intern<CstExpr<Untyped>>>)],
    ) -> Spanned<Intern<CstExpr<Untyped>>> {
        loop {}
    }
}
