use crate::passes::backend::{lir::ClosureConvertOut, target::Target};

#[derive(Copy, Clone, Default)]
pub struct C;
#[allow(unused_variables)]
impl Target for C {
    type Input = ClosureConvertOut;
    type Output = String;

    fn generate(&mut self, ir: Vec<ClosureConvertOut>) -> Self::Output {
        todo!()
    }

    fn ext(&self) -> &'static str {
        "c"
    }
}
