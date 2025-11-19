use std::{fmt::Write, io::BufReader};

use internment::Intern;

use crate::{
    passes::{backend::c::C, midend::environment::Environment},
    resource::{
        errors::FatalErr,
        rep::{
            ast::Expr,
            entry::{EnumEntry, FunctionItem, Item, ItemKind, StructEntry},
            types::Ty,
            Spanned,
        },
    },
};

pub trait Target: Copy {
    type Partial: Default;
    type Artifact: Default;
    type Output: Default;
    // fn generate(&mut self);
    fn generate_expr(&mut self, expr: Spanned<Intern<Expr>>);
    fn generate_item(&mut self, item: &Item) -> Self::Partial {
        match &item.kind {
            ItemKind::Root => Self::Partial::default(),
            ItemKind::Package(package_entry) => {
                //do nothing
                todo!()
            }
            ItemKind::Struct(struct_entry) => self.generate_struct(struct_entry),
            ItemKind::Enum(enum_entry) => self.generate_enum(enum_entry),
            ItemKind::Variant(spanned) => todo!(),
            ItemKind::Field(_) => todo!(),
            ItemKind::Function(function_item) => self.generate_func(function_item),
            ItemKind::Extern { name, sig } => todo!(),
            ItemKind::Dummy(_) => FatalErr::new("Cannot generate Dummy"),
            _ => todo!(),
        }
    }
    fn generate_func(&mut self, f: &FunctionItem) -> Self::Partial;
    fn generate_struct(&mut self, s: &StructEntry) -> Self::Partial;
    fn generate_enum(&mut self, s: &EnumEntry) -> Self::Partial;
    fn finish(self, p: Vec<Self::Partial>) -> Self::Output;
    fn convert_type(&mut self, ty: Ty) -> Self::Partial;
}

pub struct Generator<T: Target> {
    target: T,
    env: Environment,
    output: T::Output,
    artifact: T::Artifact,
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
        for idx in self.env.graph.node_indices() {
            let item = self.env.graph.node_weight(idx).unwrap();
            assert!(
                item.is_checked(),
                "Items should have been reduced before generation"
            );

            v.push(self.target.generate_item(item))
        }
        self.target.finish(v)
        // self.target.finish()
        // v.join("")
    }
}
impl<T: Target> Generator<T> {
    pub fn new(target: T, env: Environment) -> Self {
        Self {
            target,
            env,
            output: T::Output::default(),
            artifact: T::Artifact::default(),
        }
    }
}
