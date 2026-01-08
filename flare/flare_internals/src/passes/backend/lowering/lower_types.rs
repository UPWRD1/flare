use std::collections::BTreeMap;

use rustc_hash::FxHashMap;

use crate::passes::{
    backend::lowering::ir::{Kind, Row, Type, TypeVar},
    midend::typing::{self, Evidence},
};

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum AstTypeVar {
    Ty(typing::TypeVar),
    Row(typing::RowVar),
}

impl AstTypeVar {
    pub fn kind(&self) -> Kind {
        match self {
            Self::Ty(_) => Kind::Type,
            Self::Row(_) => Kind::Row,
        }
    }
}

pub struct LowerTypes {
    pub env: FxHashMap<AstTypeVar, TypeVar>,
}

impl LowerTypes {
    pub fn lower_row_ty(&self, row: typing::Row) -> Row {
        match row {
            typing::Row::Open(var) => Row::Open(self.env[&AstTypeVar::Row(var)]),
            typing::Row::Closed(closed_row) => Row::Closed(self.lower_closed_row_ty(closed_row)),
            typing::Row::Unifier(_) => {
                unreachable!("Encountered unification row during lowering")
            }
        }
    }

    pub fn lower_closed_row_ty(&self, closed_row: typing::ClosedRow) -> Vec<Type> {
        closed_row
            .values
            .iter()
            .map(|ty| self.lower_ty(*ty.0))
            .collect()
    }

    pub fn lower_ty(&self, ty: typing::Type) -> Type {
        match ty {
            typing::Type::Num => Type::Num,
            typing::Type::String => Type::Str,
            typing::Type::Unit => Type::Unit,
            typing::Type::Bool => Type::Bool,

            typing::Type::Particle(p) => Type::Particle(p.0),
            typing::Type::Var(v) => Type::Var(self.env[&AstTypeVar::Ty(v)]),
            typing::Type::Func(arg, ret) => {
                let arg = self.lower_ty(*arg.0);
                let ret = self.lower_ty(*ret.0);
                Type::fun(arg, ret)
            }
            typing::Type::Label(_, ty) => self.lower_ty(*ty.0),
            typing::Type::Prod(row) => Type::prod(self.lower_row_ty(*row.0)),
            typing::Type::Sum(row) => Type::sum(self.lower_row_ty(*row.0)),
            _ => todo!("{ty:?}"),
        }
    }

    pub fn lower_ev_ty(&self, evidence: &typing::Evidence) -> Type {
        let typing::Evidence::RowEquation {
            left, right, goal, ..
        } = evidence;

        let left = self.lower_row_ty(*left.0);
        let (left_prod, left_sum) = (Type::prod(left.clone()), Type::sum(left));

        let right = self.lower_row_ty(*right.0);
        let (right_prod, right_sum) = (Type::prod(right.clone()), Type::sum(right));

        let goal = self.lower_row_ty(*goal.0);
        let (goal_prod, goal_sum) = (Type::prod(goal.clone()), Type::sum(goal));

        let concat = Type::funs([left_prod.clone(), right_prod.clone()], goal_prod.clone());

        let branch = {
            let a = TypeVar(0);
            Type::ty_fun(
                Kind::Type,
                Type::funs(
                    [
                        Type::fun(left_sum.clone().shifted(), Type::Var(a)),
                        Type::fun(right_sum.clone().shifted(), Type::Var(a)),
                        goal_sum.clone().shifted(),
                    ],
                    Type::Var(a),
                ),
            )
        };

        let prj_left = Type::fun(goal_prod.clone(), left_prod);
        let inj_left = Type::fun(left_sum, goal_sum.clone());

        let prj_right = Type::fun(goal_prod, right_prod);
        let inj_right = Type::fun(right_sum, goal_sum);

        Type::prod(Row::Closed(vec![
            concat,
            branch,
            Type::prod(Row::Closed(vec![prj_left, inj_left])),
            Type::prod(Row::Closed(vec![prj_right, inj_right])),
        ]))
    }
}

pub struct LoweredTyScheme {
    pub scheme: Type,
    pub lower_types: LowerTypes,
    pub kinds: Vec<Kind>,
    pub ev_to_ty: BTreeMap<Evidence, Type>,
}
