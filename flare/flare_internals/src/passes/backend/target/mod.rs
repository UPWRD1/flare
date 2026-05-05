use std::fmt::Debug;

use crate::passes::backend::lir::ClosureConvertOut;

pub mod irtarget;
pub mod lirtarget;
pub mod llvm;

pub trait Target: Clone {
    type Output: Into<Vec<u8>> + Default + Debug;
    type Input: Debug;
    fn generate(&mut self, input: Vec<ClosureConvertOut>) -> Self::Output;
    fn ext(&self) -> &str;
}

pub struct Generator<T: Target> {
    target: T,
    input: Vec<ClosureConvertOut>,
}

impl<T: Target> Generator<T> {
    pub fn generate(mut self) -> T::Output {
        self.target.generate(self.input)
    }
}

impl<T: Target> Generator<T> {
    pub fn new(target: T, input: Vec<ClosureConvertOut>) -> Self {
        Self { target, input }
    }
}
