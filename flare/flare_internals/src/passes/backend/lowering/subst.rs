use std::cmp::Ordering;

use crate::passes::backend::lowering::ir::{Row, TyApp, Type, TypeVar};

#[derive(Clone)]
pub enum Subst {
    TyPayload(Type),
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

    fn subst_ty_var(self) -> Type {
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

    pub fn subst_ty(self, haystack: Type, needle: usize) -> Type {
        match haystack {
            Type::Num | Type::Unit | Type::Str | Type::Bool | Type::Particle(_) => haystack,
            Type::Var(type_var) => match type_var.0.cmp(&needle) {
                Ordering::Equal => self.subst_ty_var(),
                Ordering::Less => Type::Var(type_var),
                Ordering::Greater => Type::Var(TypeVar(type_var.0 - 1)),
            },
            Type::Fun(arg, ret) => Type::fun(
                self.clone().subst_ty(*arg, needle),
                self.subst_ty(*ret, needle),
            ),
            Type::TyFun(kind, body) => {
                Type::ty_fun(kind, self.shifted().subst_ty(*body, needle + 1))

                // Type::ty_fun(kind, self.shifted().subst_ty(*body, needle))
            }
            Type::Prod(row) => Type::prod(self.subst_row(row, needle)),
            Type::Sum(row) => Type::sum(self.subst_row(row, needle)),
        }
        // dbg!(res)
    }

    pub fn subst_ty_final(self, haystack: Type, needle: usize) -> Type {
        match haystack {
            Type::Num | Type::Unit | Type::Str | Type::Bool | Type::Particle(_) => haystack,
            Type::Var(type_var) => match type_var.0.cmp(&needle) {
                Ordering::Equal => self.subst_ty_var(),
                Ordering::Less => Type::Var(type_var),
                Ordering::Greater => Type::Var(TypeVar(type_var.0 - 1)),
            },
            Type::Fun(arg, ret) => Type::fun(
                self.clone().subst_ty_final(*arg, needle),
                self.subst_ty_final(*ret, needle),
            ),
            Type::TyFun(kind, body) => self.shifted().subst_ty_final(*body, needle),
            Type::Prod(row) => Type::prod(self.subst_row(row, needle)),
            Type::Sum(row) => Type::sum(self.subst_row(row, needle)),
        }
        // dbg!(res)
    }
}

impl Row {
    pub fn subst_ty(self, ty: Type) -> Self {
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

impl Type {
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
