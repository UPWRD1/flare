use chumsky::span::Span;
use internment::Intern;
use rustc_hash::FxHashMap;

use crate::{
    passes::midend::typing::{
        Constraint, Evidence, Provenance, Row, TyUniVar, Type, TypeScheme, TypeVar,
        rows::{RowCombination, RowUniVar, RowVar},
    },
    resource::rep::{Spanned, ast::NodeId},
};

pub struct Instantiate<'a> {
    id: NodeId,
    tyvar_to_unifiers: &'a FxHashMap<TypeVar, TyUniVar>,
    rowvar_to_unifiers: &'a FxHashMap<RowVar, RowUniVar>,
}
impl<'a> Instantiate<'a> {
    pub fn new(
        id: NodeId,
        tyvar_to_unifiers: &'a FxHashMap<TypeVar, TyUniVar>,
        rowvar_to_unifiers: &'a FxHashMap<RowVar, RowUniVar>,
    ) -> Self {
        Self {
            id,
            tyvar_to_unifiers,
            rowvar_to_unifiers,
        }
    }

    pub fn type_scheme(&self, ty_scheme: TypeScheme) -> (Vec<Constraint>, Spanned<Intern<Type>>) {
        let constraints = ty_scheme
            .evidence
            .into_iter()
            .map(|ev| self.evidence(ev))
            .collect();
        let ty = self.ty(ty_scheme.ty);
        (constraints, ty)
    }

    fn evidence(&self, ev: Evidence) -> Constraint {
        // dbg!(&ev);
        match ev {
            Evidence::RowEquation { left, right, goal } => Constraint::RowCombine(
                Provenance::ExpectedCombine(self.id),
                RowCombination {
                    left: self.row(left),
                    right: self.row(right),
                    goal: self.row(goal),
                },
            ),
        }
    }

    fn row(&self, row: Row) -> Row {
        match row {
            // Type Scheme's should have been generalized and only contain Row::Open.
            // If we see a Unifier in a type scheme our type checker has a bug in it.
            Row::Unifier(_) => unreachable!("Leftover unifier in type scheme"),
            Row::Open(var) => self
                .rowvar_to_unifiers
                .get(&var)
                .copied()
                .map(Row::Unifier)
                .unwrap_or_else(|| {
                    unreachable!(
                        "Expected row var {:?} to be mapped to fresh unifier in instantiation",
                        var
                    )
                }),
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
    }

    fn ty(&self, ty: Spanned<Intern<Type>>) -> Spanned<Intern<Type>> {
        match *ty.0 {
            Type::Var(var) => self
                .tyvar_to_unifiers
                .get(&var)
                .copied()
                .map(Type::Unifier)
                .map(|x| ty.convert(x))
                .unwrap_or_else(|| {
                    unreachable!(
                        "Expected type var {:?} to be mapped to fresh unifier in instantiation",
                        var
                    )
                }),
            Type::Num | Type::Bool | Type::String | Type::Unit | Type::Unifier(_) => ty,
            Type::Func(arg, ret) => {
                let arg = self.ty(arg);
                let ret = self.ty(ret);
                let s = arg.1.union(ret.1);
                Spanned(Type::Func(arg, ret).into(), s)
            }
            Type::Prod(row) => ty.convert(Type::Prod(self.row(row))),
            Type::Sum(row) => ty.convert(Type::Sum(self.row(row))),
            Type::Label(label, ty) => ty.convert(Type::Label(label, ty)),
            _ => todo!("{:?}", ty),
        }
    }
}
