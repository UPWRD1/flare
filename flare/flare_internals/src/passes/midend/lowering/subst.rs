use std::cmp::Ordering;

use crate::resource::rep::midend::irtype::{IRType, Row, TyApp, TypeVar};


#[derive(Clone)]
pub enum Subst {
    TyPayload(IRType),
    RowPayload(Row),
}

impl Subst {
    fn shift(&mut self) {
        match self {
            Self::RowPayload(row) => row.shift(),
            Self::TyPayload(ty) => ty.shift(),
        }
    }

    fn shifted(mut self) -> Self {
        self.shift();
        self
    }

    fn subst_row_var(self) -> Row {
        match self {
            Self::RowPayload(row) => row,
            Self::TyPayload(_) => {
                unreachable!("Kind mismatch. A type was substituted for a row")
            }
        }
    }

    fn subst_ty_var(self) -> IRType {
        match self {
            Self::TyPayload(ty) => ty,
            Self::RowPayload(_) => {
                unreachable!("ICE: Kind mismatch. A type was substituted for a row")
            }
        }
    }

    pub fn subst_row(self, haystack: Row, needle: usize) -> Row {
        match haystack {
            Row::Open(row_var) => match row_var.0.cmp(&needle) {
                Ordering::Equal => self.subst_row_var(),
                Ordering::Less => Row::Open(row_var),
                Ordering::Greater => Row::Open(TypeVar(row_var.0 - 1)),
            },
            Row::Closed(elems) => Row::Closed(
                elems
                    .into_iter()
                    .map(|elem| self.clone().subst_ty(elem, needle))
                    .collect(),
            ),
        }
    }

    pub fn subst_ty(self, haystack: IRType, needle: usize) -> IRType {
        match haystack {
            IRType::Num | IRType::Unit | IRType::Str | IRType::Bool | IRType::Particle(_) => haystack,
            IRType::Var(type_var) => match type_var.0.cmp(&needle) {
                Ordering::Equal => self.subst_ty_var(),
                Ordering::Less => IRType::Var(type_var),
                Ordering::Greater => IRType::Var(TypeVar(type_var.0 - 1)),
            },
            IRType::Fun(arg, ret) => IRType::fun(
                self.clone().subst_ty(*arg, needle),
                self.subst_ty(*ret, needle),
            ),
            IRType::TyFun(kind, body) => {
                IRType::ty_fun(kind, self.shifted().subst_ty(*body, needle + 1))

                // Type::ty_fun(kind, self.shifted().subst_ty(*body, needle))
            }
            IRType::Prod(row) => IRType::prod(self.subst_row(row, needle)),
            IRType::Sum(row) => IRType::sum(self.subst_row(row, needle)),
        }
        // dbg!(res)
    }

    pub fn subst_ty_final(self, haystack: IRType, needle: usize) -> IRType {
        match haystack {
            IRType::Num | IRType::Unit | IRType::Str | IRType::Bool | IRType::Particle(_) => haystack,
            IRType::Var(type_var) => match type_var.0.cmp(&needle) {
                Ordering::Equal => self.subst_ty_var(),
                Ordering::Less => IRType::Var(type_var),
                Ordering::Greater => IRType::Var(TypeVar(type_var.0 - 1)),
            },
            IRType::Fun(arg, ret) => IRType::fun(
                self.clone().subst_ty_final(*arg, needle),
                self.subst_ty_final(*ret, needle),
            ),
            IRType::TyFun(kind, body) => self.shifted().subst_ty_final(*body, needle),
            IRType::Prod(row) => IRType::prod(self.subst_row(row, needle)),
            IRType::Sum(row) => IRType::sum(self.subst_row(row, needle)),
        }
        // dbg!(res)
    }
}

impl Row {
    pub fn subst_ty(self, ty: IRType) -> Self {
        Subst::TyPayload(ty).subst_row(self, 0)
    }

    pub fn subst_row(self, row: Self) -> Self {
        Subst::RowPayload(row).subst_row(self, 0)
    }

    fn adjust(&mut self, cutoff: usize) {
        match self {
            Self::Open(type_var) => type_var.adjust(cutoff),
            Self::Closed(tys) => {
                for ty in tys {
                    ty.adjust(cutoff);
                }
            }
        }
    }

    fn shift(&mut self) {
        self.adjust(0);
    }
}

impl IRType {
    pub fn subst_app(self, payload: TyApp) -> Self {
        match payload {
            TyApp::Ty(ty) => self.subst_ty(ty),

            TyApp::Row(row) => self.subst_row(row),
        }
    }

    pub fn subst_app_final(self, payload: TyApp) -> Self {
        match payload {
            TyApp::Ty(ty) => self.subst_ty_final(ty),

            TyApp::Row(row) => self.subst_row(row),
        }
    }

    pub fn subst_ty(self, ty: Self) -> Self {
        Subst::TyPayload(ty).subst_ty(self, 0)
    }

    pub fn subst_ty_final(self, ty: Self) -> Self {
        Subst::TyPayload(ty).subst_ty_final(self, 0)
    }

    pub fn subst_row(self, row: Row) -> Self {
        Subst::RowPayload(row).subst_ty(self, 0)
    }

    fn adjust(&mut self, cutoff: usize) {
        match self {
            Self::Num | Self::Unit | Self::Str | Self::Bool | Self::Particle(_) => {}
            Self::Var(type_var) => type_var.adjust(cutoff),
            Self::Fun(arg, ret) => {
                arg.adjust(cutoff);
                ret.adjust(cutoff);
            }
            Self::TyFun(_, body) => {
                body.adjust(cutoff + 1);
            }
            Self::Prod(row) | Self::Sum(row) => row.adjust(cutoff),
        }
    }

    fn shift(&mut self) {
        self.adjust(0);
    }

    pub fn shifted(mut self) -> Self {
        self.shift();
        self
    }
}

impl TypeVar {
    fn adjust(&mut self, cutoff: usize) {
        if self.0 >= cutoff {
            self.0 += 1;
        }
    }
}
