use std::fmt::Debug;

use crate::passes::backend::lowering::ir::{IR, Type};

pub trait Target: Copy {
    type Partial: Default;
    type Output: Default + Debug;
    type Input: Debug;
    // fn generate(&mut self);
    // fn generate_item(&mut self, Type)
    fn generate(&mut self, ir: Self::Input) -> Self::Partial;
    fn finish(self, p: Vec<Self::Partial>) -> Self::Output;
    fn ext(&self) -> impl Into<String>;
    fn convert(ir: Vec<(IR, Type)>) -> Vec<Self::Input>;
}

pub struct Generator<T: Target> {
    target: T,
    inputs: Vec<T::Input>,
}

// impl Generator<C> {
//     pub fn generate(mut self) -> <C as Target>::Output {
//         let mut v = vec![];
//         for idx in self.env.graph.node_indices() {
//             let item = self.env.graph.node_weight(idx).unwrap();
//             assert!(
//                 item.is_checked(),
//                 "Items should have been reduced before generation"
//             );

//             v.push(self.target.generate_item(item))
//         }
//         self.target.finish()
//         // v.join("")
//     }
// }

impl<T: Target> Generator<T> {
    pub fn generate(mut self) -> T::Output {
        let mut v: Vec<T::Partial> = vec![];
        for input in self.inputs {
            v.push(self.target.generate(input))
        }
        self.target.finish(v)
    }
}
impl<T: Target> Generator<T> {
    pub fn new(target: T, inputs: Vec<T::Input>) -> Self {
        Self { target, inputs }
    }
}
