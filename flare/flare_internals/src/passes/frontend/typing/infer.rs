use internment::Intern;
use rustc_hash::FxBuildHasher;

use crate::{
    passes::frontend::typing::{GenOut, Solver, Type},
    resource::rep::{
        common::Spanned,
        frontend::ast::{Expr, Untyped},
    },
};

impl Solver<'_> {
    pub fn infer(
        &mut self,
        env: im::HashMap<Intern<String>, Spanned<Intern<Type>>, FxBuildHasher>,
        ast: Spanned<Intern<Expr<Untyped>>>,
    ) -> (GenOut, Spanned<Intern<Type>>) {
        loop {}
    }
}
