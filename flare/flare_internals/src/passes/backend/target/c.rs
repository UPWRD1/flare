use internment::Intern;

use crate::{
    passes::{
        backend::{lir::ClosureConvertOut, target::Target},
        frontend::typing::{Type, Typed},
    },
    resource::rep::{
        common::Spanned,
        frontend::{
            ast::{Expr, Variable},
            entry::FunctionItem,
        },
    },
};

#[derive(Copy, Clone, Default)]
pub struct C;
#[allow(unused_variables)]
impl Target for C {
    type Output = String;

    fn generate(&mut self, ir: Vec<ClosureConvertOut>) -> Self::Output {
        todo!()
    }

    fn ext(&self) -> &str {
        "c"
    }
}

#[allow(dead_code, unused_variables, clippy::dbg_macro)]
impl C {
    pub fn new() -> Self {
        Self
    }

    fn convert_type(&mut self, ty: Type) -> String {
        match ty {
            Type::Num => "double".to_string(),
            Type::String => "char*".to_string(),
            Type::Var(_) => "void*".to_string(),
            _ => todo!("{ty:?}"),
        }
    }

    fn generate_func<V: Variable>(&mut self, f: &FunctionItem<V>) -> String {
        let out_ty = f.sig;
        // let mut arg_types = vec![];
        let (args, ret) = Type::destructure_arrow(out_ty);
        dbg!(args, ret);
        todo!();
        // let converted_ret = self.convert_type(ret);

        //let output = format!("{} {}()", converted_ret, f.name.ident().unwrap().0);
        // dbg!(output);
    }
    fn generate_expr(&mut self, expr: Spanned<Intern<Expr<Typed>>>) {
        todo!()
    }
}
