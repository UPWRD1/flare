use crate::{passes::backend::target::Target, resource::rep::midend::ir::IR};


#[derive(Clone, Copy, Default)]
pub struct IRTarget;

impl Target for IRTarget {
    type Partial = IR;

    type Output = String;
    type Input = IR;

    fn generate(&mut self, ir: IR) -> Self::Partial {
        ir
    }

    fn finish(&self, p: Vec<Self::Partial>) -> Self::Output {
        p.into_iter()
            .enumerate()
            .map(|(i, x)| format!("item #{i}: is\n{x}\nend item #{i}"))
            .collect::<Vec<String>>()
            .join("\n\n")
    }
    fn ext(&self) -> &str {
        "ir"
    }
    fn convert(&self, ir: Vec<IR>) -> Vec<Self::Input> {
        ir
    }
}