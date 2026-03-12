use crate::resource::rep::midend::{
    ir::IR,
    irtype::{IRType, Row, TyApp},
};
#[allow(dead_code)]
enum Arg<'a> {
    Val(&'a IR),
    Ty(&'a TyApp),
}

pub fn subst_ty_at(_: IR, _: IRType, _: usize) -> IR {
    loop {}
}
pub fn subst_ty(haystack: IR, payload: IRType) -> IR {
    subst_ty_at(haystack, payload, 0)
}

pub fn subst_row(haystack: IR, payload: Row) -> IR {
    subst_row_at(haystack, payload, 0)
}

pub fn subst_row_at(_: IR, _: Row, _: usize) -> IR {
    loop {}
}

pub fn simplify(_: Vec<IR>) -> Vec<IR> {
    loop {}
}
