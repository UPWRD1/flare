use std::fmt::Debug;

pub mod c;
pub mod irtarget;
pub mod lirtarget;
pub mod native;

pub trait Target: Clone {
    type Output: Into<Vec<u8>> + Default + Debug;
    type Input: Debug;
    // fn generate(&mut self);
    // fn generate_item(&mut self, Type)
    fn generate(&mut self, input: Vec<Self::Input>) -> Self::Output;
    fn ext(&self) -> &str;
}

pub struct Generator<T: Target> {
    target: T,
    input: Vec<T::Input>,
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
        self.target.generate(self.input)
    }
}

impl<T: Target> Generator<T> {
    pub fn new(target: T, input: Vec<T::Input>) -> Self {
        Self { target, input }
    }
}
