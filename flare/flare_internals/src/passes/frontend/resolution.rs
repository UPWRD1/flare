use internment::Intern;

type DiGraph<N, E> = petgraph::graph::DiGraph<N, E>;
use crate::resource::rep::{
    common::{Spanned, Syntax},
    frontend::{
        cst::{CstExpr, UntypedCst},
        // entry::FunctionItem,
    },
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct FunctionItem<S: Syntax> {
    pub name: S::Name,
    pub sig: S::Type,
    pub body: S::Expr,
    // extra_vars: usize,
}
pub struct Resolver {
    pub dag: DiGraph<usize, ()>,
}
impl Resolver {
    pub fn new() -> Self {
        Self {
            dag: DiGraph::new(),
        }
    }

    pub fn analyze(mut self) {
        self.analyze_func(loop {});
    }

    fn in_context<T>(&mut self, mut f: impl FnMut(&mut Self) -> T) -> T {
        let out = f(self);
        out
    }

    fn analyze_func(
        &mut self,
        the_func: FunctionItem<UntypedCst>,
        // idx: NodeIndex,
    ) -> FunctionItem<UntypedCst> {
        self.in_context(
            |me| {
                let body = me.analyze_expr();
                FunctionItem { body, ..the_func }
            },
            // idx,
        )
    }

    #[allow(unused_variables)]
    fn analyze_expr(&mut self) -> Spanned<Intern<CstExpr>> {
        loop {}
    }
}
