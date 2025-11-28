use internment::Intern;

use crate::{
    passes::{
        backend::target::Target,
        midend::typing::{Type, Typed},
    },
    resource::rep::{
        ast::{Expr, Variable},
        entry::FunctionItem,
        Spanned,
    },
};

#[derive(Copy, Clone, Default)]
pub struct C;
#[allow(unused_variables)]
impl Target for C {
    type Output = String;
    type Partial = String;
    type Artifact = Vec<String>;
    fn convert_type(&mut self, ty: Type) -> Self::Partial {
        match ty {
            Type::Num => "double".to_string(),
            _ => todo!(),
        }
    }

    fn generate_expr(&mut self, expr: Spanned<Intern<Expr<Typed>>>) {
        todo!()
    }

    // fn finish(self) -> Self::Output {
    //     let mut string_buf = String::new();
    //     let out = self
    //         .value
    //         .into_inner()
    //         .unwrap()
    //         .as_slice()
    //         .read_to_string(&mut string_buf)
    //         .unwrap();
    //     format!("{}{}", self.decls, out)
    // }

    fn generate_func<V: Variable>(&mut self, f: &FunctionItem<V>) -> Self::Output {
        let out_ty = f.sig;
        // let mut arg_types = vec![];
        let (args, ret) = out_ty.0.destructure_arrow();
        dbg!(args, ret);
        let converted_ret = self.convert_type(*ret);

        let output = format!("{} {}()", converted_ret, f.name.ident().unwrap().0);
        dbg!(output);
        todo!()
    }

    fn generate_type(&mut self, t: Type) -> Self::Partial {
        todo!()
    }

    // fn generate_struct(&mut self, s: &StructEntry) -> Self::Output {
    // todo!()
    // }

    // fn generate_enum(&mut self, s: &EnumEntry) -> Self::Output {
    // todo!()
    // }

    fn finish(self, p: Vec<Self::Partial>) -> Self::Output {
        p.join("")
    }
}

impl C {
    pub fn new() -> Self {
        Self
    }
}
