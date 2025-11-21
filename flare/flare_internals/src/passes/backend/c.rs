use internment::Intern;

use crate::{
    passes::{backend::target::Target, midend::typing::Typed},
    resource::rep::{
        ast::{Expr, Variable},
        entry::{EnumEntry, FunctionItem, StructEntry},
        concretetypes::Ty,
        Spanned,
    },
};

#[derive(Copy, Clone, Default)]
pub struct C;

impl Target for C {
    type Output = String;
    type Partial = String;
    type Artifact = Vec<String>;
    fn convert_type(&mut self, ty: Ty) -> Self::Partial {
        match ty {
            Ty::Primitive(p) => match p {
                crate::resource::rep::concretetypes::PrimitiveType::Num => "double".to_string(),
                crate::resource::rep::concretetypes::PrimitiveType::Str => "char*".to_string(),
                crate::resource::rep::concretetypes::PrimitiveType::Bool => "bool".to_string(),
                crate::resource::rep::concretetypes::PrimitiveType::Unit => "void".to_string(),
            },
            Ty::User(spanned, intern) => format!("{}", spanned),
            Ty::Tuple(intern) => todo!(),
            Ty::Seq(spanned) => todo!(),
            Ty::Arrow(spanned, spanned1) => todo!(),
            Ty::Myself => todo!(),
            Ty::Generic(spanned) => "void*".to_string(),
            Ty::Variant(enum_variant) => todo!(),
            Ty::Package(spanned) => todo!(),
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
        let out_ty = f.sig.get().unwrap();
        // let mut arg_types = vec![];
        let (args, ret) = out_ty.0.destructure_arrow();
        dbg!(args, ret);
        let converted_ret = self.convert_type(*ret.0);

        let output = format!("{} {}()", converted_ret, f.name.ident().unwrap().0);
        dbg!(output);
        todo!()
    }

    fn generate_struct(&mut self, s: &StructEntry) -> Self::Output {
        todo!()
    }

    fn generate_enum(&mut self, s: &EnumEntry) -> Self::Output {
        todo!()
    }

    fn finish(self, p: Vec<Self::Partial>) -> Self::Output {
        p.join("")
    }
}

impl C {
    pub fn new() -> Self {
        Self
    }
}
