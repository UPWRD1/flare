use std::fmt::Debug;

use internment::Intern;

use crate::passes::backend::lowering::ir::{Type, IR};

pub trait Target: Copy {
    type Partial: Default;
    type Output: Default + Debug;
    // fn generate(&mut self);
    // fn generate_item(&mut self, Type)
    fn generate(&mut self, ir: IR) -> Self::Partial;
    fn finish(self, p: Vec<Self::Partial>) -> Self::Output;
    fn ext(&self) -> impl Into<String>;
}

pub struct Generator<T: Target> {
    target: T,
    items: Vec<(IR, Type)>,
    output: T::Output,
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
        for ir in self.items {
            v.push(self.target.generate(ir.0))
        }
        self.target.finish(v)
        // self.target.finish()
        // v.join("")
    }
}
impl<T: Target> Generator<T> {
    pub fn new(target: T, items: Vec<(IR, Type)>) -> Self {
        Self {
            target,
            items,
            output: T::Output::default(),
        }
    }
}
