use std::collections::BTreeMap;

use rustc_hash::FxHashMap;

use crate::{
    passes::frontend::typing::{self, Evidence},
    resource::rep::midend::irtype::{IRType, Kind, Row, TypeVar},
};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
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

    pub fn lower_closed_row_ty(&self, closed_row: typing::ClosedRow) -> Vec<IRType> {
        closed_row
            .values
            .iter()
            .map(|ty| self.lower_ty(*ty.0))
            .collect()
    }

    pub fn lower_ty(&self, ty: typing::Type) -> IRType {
        match ty {
            typing::Type::Num => IRType::Num,
            typing::Type::String => IRType::Str,
            typing::Type::Unit => IRType::Unit,
            typing::Type::Bool => IRType::Bool,

            typing::Type::Particle(p) => IRType::Particle(p.0),
            typing::Type::Var(v) => IRType::Var(self.env[&AstTypeVar::Ty(v)]),
            typing::Type::Func(arg, ret) => {
                let arg = self.lower_ty(*arg.0);
                let ret = self.lower_ty(*ret.0);
                IRType::fun(arg, ret)
            }
            typing::Type::Label(_, ty) => self.lower_ty(*ty.0),
            typing::Type::Prod(row) => IRType::prod(self.lower_row_ty(*row.0)),
            typing::Type::Sum(row) => IRType::sum(self.lower_row_ty(*row.0)),
            typing::Type::Volatile(v) => IRType::volatile(self.lower_ty(*v.0)),
            // typing::Type::TypeFun(arg, t) => Type::ty_fun(Kind::Type, self.lower_ty(*t.0)),
            _ => todo!("{ty:?}"),
        }
    }

    pub fn lower_ev_ty(&self, evidence: &typing::Evidence) -> IRType {
        let typing::Evidence::RowEquation {
            left, right, goal, ..
        } = evidence;
        let left = self.lower_row_ty(*left.0);
        let (left_prod, left_sum) = (IRType::prod(left.clone()), IRType::sum(left));

        let right = self.lower_row_ty(*right.0);
        let (right_prod, right_sum) = (IRType::prod(right.clone()), IRType::sum(right));

        let goal = self.lower_row_ty(*goal.0);
        let (goal_prod, goal_sum) = (IRType::prod(goal.clone()), IRType::sum(goal));

        let concat = IRType::funs([left_prod.clone(), right_prod.clone()], goal_prod.clone());

        let branch = {
            let a = TypeVar(0);
            IRType::ty_fun(
                Kind::Type,
                IRType::funs(
                    [
                        IRType::fun(left_sum.clone().shifted(), IRType::Var(a)),
                        IRType::fun(right_sum.clone().shifted(), IRType::Var(a)),
                        goal_sum.clone().shifted(),
                    ],
                    IRType::Var(a),
                ),
            )
        };

        let prj_left = IRType::fun(goal_prod.clone(), left_prod);
        let inj_left = IRType::fun(left_sum, goal_sum.clone());

        let prj_right = IRType::fun(goal_prod, right_prod);
        let inj_right = IRType::fun(right_sum, goal_sum);

        IRType::prod(Row::Closed(vec![
            concat,
            branch,
            IRType::prod(Row::Closed(vec![prj_left, inj_left])),
            IRType::prod(Row::Closed(vec![prj_right, inj_right])),
        ]))
    }
}

pub struct LoweredTyScheme {
    pub scheme: IRType,
    pub lower_types: LowerTypes,
    pub kinds: Vec<Kind>,
    pub ev_to_ty: BTreeMap<Evidence, IRType>,
}
