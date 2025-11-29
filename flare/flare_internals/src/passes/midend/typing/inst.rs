use rustc_hash::FxHashMap;

use crate::{
    passes::midend::typing::{
        rows::{RowCombination, RowUniVar, RowVar},
        Constraint, Evidence, Row, TyUniVar, Type, TypeScheme, TypeVar,
    },
    resource::rep::ast::NodeId,
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

    pub fn type_scheme(&self, ty_scheme: TypeScheme) -> (Vec<Constraint>, Type) {
        let constraints = ty_scheme
            .evidence
            .into_iter()
            .map(|ev| self.evidence(ev))
            .collect();
        let ty = self.ty(ty_scheme.ty);
        (constraints, ty)
    }

    fn evidence(&self, ev: Evidence) -> Constraint {
        match ev {
            Evidence::RowEquation { left, right, goal } => Constraint::RowCombine(RowCombination {
                left: self.row(left),
                right: self.row(right),
                goal: self.row(goal),
            }),
        }
    }

    fn row(&self, row: Row) -> Row {
        match row {
            // Type Scheme's should have been generalized and only contain Row::Open.
            // If we see a Unifier in a type scheme our type checker has a bug in it.
            Row::Unifier(_) => panic!("Leftover unifier in type scheme"),
            Row::Open(var) => self
                .rowvar_to_unifiers
                .get(&var)
                .copied()
                .map(Row::Unifier)
                .unwrap_or_else(|| {
                    panic!(
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

    fn ty(&self, ty: Type) -> Type {
        match ty {
            Type::Var(var) => self
                .tyvar_to_unifiers
                .get(&var)
                .copied()
                .map(Type::Unifier)
                .unwrap_or_else(|| {
                    panic!(
                        "Expected type var {:?} to be mapped to fresh unifier in instantiation",
                        var
                    )
                }),
            ty @ Type::Num
            | ty @ Type::Bool
            | ty @ Type::String
            | ty @ Type::Unit
            | ty @ Type::Unifier(_) => ty,
            Type::Func(arg, ret) => {
                let arg = self.ty(*arg);
                let ret = self.ty(*ret);
                Type::fun(arg, ret)
            }
            Type::Prod(row) => Type::Prod(self.row(row)),
            Type::Sum(row) => Type::Sum(self.row(row)),
            Type::Label(label, ty) => Type::Label(label, ty),
            _ => panic!("{:?}", ty),
        }
    }
}
