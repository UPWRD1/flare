use crate::{
    passes::backend::{lir::ClosureConvertOut, target::Target},
    resource::rep::midend::ir::IR,
};

#[derive(Clone, Copy, Default)]
pub struct IRTarget;

impl Target for IRTarget {
    type Input = IR;

    type Output = String;
    // type Input = IR;

    fn generate(&mut self, ir: Vec<ClosureConvertOut>) -> Self::Output {
        unimplemented!("don't do this")
    }

    fn ext(&self) -> &str {
        "ir"
    }
}
