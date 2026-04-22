use internment::Intern;
use rustc_hash::FxHashMap;

use crate::{
    passes::frontend::typing::{
        Constraint, Evidence, Provenance, Row, TyUniVar, Type, TypeScheme, TypeVar,
        rows::{RowCombination, RowUniVar, RowVar},
    },
    resource::rep::common::Spanned,
};

pub struct Instantiate<'a> {
    // id: FlareSpan,
    tyvar_to_unifiers: &'a FxHashMap<TypeVar, TyUniVar>,
    rowvar_to_unifiers: &'a FxHashMap<RowVar, RowUniVar>,
}
impl<'a> Instantiate<'a> {
    pub fn new(
        // id: FlareSpan,
        tyvar_to_unifiers: &'a FxHashMap<TypeVar, TyUniVar>,
        rowvar_to_unifiers: &'a FxHashMap<RowVar, RowUniVar>,
    ) -> Self {
        Self {
            // id,
            tyvar_to_unifiers,
            rowvar_to_unifiers,
        }
    }

    pub fn type_scheme(
        &self,
        ty_scheme: Intern<TypeScheme>,
    ) -> (Vec<Constraint>, Spanned<Intern<Type>>) {
        let constraints = ty_scheme
            .evidence
            .iter()
            .map(|ev| self.evidence(&ev))
            .collect();
        let ty = self.ty(ty_scheme.ty);
        (constraints, ty)
    }

    fn evidence(&self, ev: &Evidence) -> Constraint {
        // dbg!(&ev);
        match ev {
            Evidence::RowEquation { left, right, goal } => Constraint::RowCombine(
                Provenance::ExpectedCombine(left.1, right.1),
                RowCombination {
                    left: self.row(*left),
                    right: self.row(*right),
                    goal: self.row(*goal),
                },
            ),
        }
    }

    fn row(&self, row: Spanned<Intern<Row>>) -> Spanned<Intern<Row>> {
        row.map_inner(|row| {
            match *row {
                // Type Scheme's should have been generalized and only contain Row::Open.
                // If we see a Unifier in a type scheme our type checker has a bug in it.
                Row::Unifier(_) => unreachable!("Leftover unifier in type scheme"),
                Row::Open(var) => self.rowvar_to_unifiers.get(&var).copied().map_or_else(
                    || {
                        unreachable!(
                            "Expected row var {:?} to be mapped to fresh unifier in instantiation",
                            var
                        )
                    },
                    Row::Unifier,
                ),
                Row::Closed(mut row) => {
                    row.values = row
                        .values
                        .iter()
                        .map(|ty| self.ty(*ty))
                        .collect::<Vec<_>>()
                        .leak();
                    Row::Closed(row)
                }
            }
        })
    }

    fn ty(&self, ty: Spanned<Intern<Type>>) -> Spanned<Intern<Type>> {
        ty.map_inner(|ty| match *ty {
            Type::Var(var) => self.tyvar_to_unifiers.get(&var).copied().map_or_else(
                || {
                    unreachable!(
                        "Expected type var {:?} to be mapped to fresh unifier in instantiation",
                        var
                    )
                },
                Type::Unifier,
            ),
            Type::Num
            | Type::Bool
            | Type::String
            | Type::Unit
            | Type::Unifier(_)
            | Type::Particle(_) => *ty,
            Type::Func(arg, ret) => {
                let arg = self.ty(arg);
                let ret = self.ty(ret);
                Type::Func(arg, ret)
            }
            Type::Prod(row) => Type::Prod(self.row(row)),
            Type::Sum(row) => Type::Sum(self.row(row)),
            Type::Label(label, ty) => Type::Label(label, ty),

            _ => todo!("{:?}", ty),
        })
    }
}
