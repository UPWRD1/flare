use internment::Intern;
use petgraph::graph::NodeIndex;

type DiGraph<N, E> = petgraph::graph::DiGraph<N, E>;
use crate::resource::rep::{
    common::Spanned,
    frontend::{
        ast::Untyped,
        cst::{CstExpr, UntypedCst},
        entry::FunctionItem,
    },
};

pub struct Resolver {
    current_dag_node: Option<NodeIndex>,
    pub dag: DiGraph<usize, ()>,
}
impl Resolver {
    pub fn new() -> Self {
        Self {
            current_dag_node: None,
            dag: DiGraph::new(),
        }
    }

    pub fn analyze(mut self) {
        self.analyze_func(loop {}, loop {});
        // let g = self
        //     .env
        //     .graph
        //     .clone()
        //     .map(|idx, item| self.analyze_item(idx, item), |idx, e| *e);
        // dbg!(g);
        loop {}
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
        _: Spanned<Intern<CstExpr<Untyped>>>,
        _: &[(Intern<String>, Spanned<Intern<CstExpr<Untyped>>>)],
    ) -> Spanned<Intern<CstExpr<Untyped>>> {
        loop {}
    }
}
