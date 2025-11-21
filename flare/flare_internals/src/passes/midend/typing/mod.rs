mod check;
mod infer;
mod rows;
mod subst;
mod types;
mod unify;

pub use rows::{ClosedRow, Row};
use rows::{RowCombination, RowUniVar};
pub use types::{TyUniVar, Type};

use std::{collections::BTreeSet, hash::Hash, marker::PhantomData};

use ena::unify::InPlaceUnificationTable;
use internment::Intern;

use rustc_hash::FxHashMap;

use crate::{
    passes::midend::{environment::Environment, typing::subst::SubstOut},
    resource::{
        errors::{CompResult, CompilerErr, TypeErr},
        rep::{
            Spanned, ast::{Direction, Expr, NodeId, Untyped, Variable}, common::Ident, concretetypes::Ty, entry::Item, quantifier::QualifierFragment
        },
    },
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Typed(pub Untyped, pub Type);

impl Variable for Typed {}

impl Ident for Typed {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        self.0.ident()
    }
}

// #[derive(Debug, Clone, Copy)]
enum Constraint {
    TypeEqual(Provenance, Type, Type),
    RowCombine(RowCombination),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Evidence {
    RowEquation { left: Row, right: Row, goal: Row },
}

#[derive(Debug, Clone, Copy)]
enum Provenance {
    // A non function type encountered a Fun ast node, causing a type mismatch.
    UnexpectedFun(NodeId),
    // An application has an ast node in function position that does not have a function type.
    AppExpectedFun(NodeId),
    // Constraint produced by subsumption.
    ExpectedUnify(NodeId),
}

impl Provenance {
    fn id(&self) -> NodeId {
        match self {
            Self::UnexpectedFun(node_id)
            | Self::AppExpectedFun(node_id)
            | Self::ExpectedUnify(node_id) => *node_id,
        }
    }
}

pub struct GenOut {
    constraints: Vec<Constraint>,
    typed_ast: Spanned<Intern<Expr<Typed>>>,
}

impl GenOut {
    fn new(constraints: Vec<Constraint>, typed_ast: Spanned<Intern<Expr<Typed>>>) -> Self {
        Self {
            constraints,
            typed_ast,
        }
    }

    fn with_typed_ast(
        self,
        f: impl FnOnce(Spanned<Intern<Expr<Typed>>>) -> Spanned<Intern<Expr<Typed>>>,
    ) -> Self {
        Self {
            constraints: self.constraints,
            typed_ast: f(self.typed_ast),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct TypeScheme {
    pub unbound_types: BTreeSet<TyUniVar>,
    pub unbound_rows: BTreeSet<RowUniVar>,
    pub evidence: Vec<Evidence>,
    pub ty: Type,
}

pub struct TypeInferOut {
    pub ast: Spanned<Intern<Expr<Typed>>>,
    pub scheme: TypeScheme,
    pub errors: FxHashMap<NodeId, CompilerErr>,
    pub row_to_ev: FxHashMap<NodeId, Evidence>,
    pub branch_to_ret_ty: FxHashMap<NodeId, Type>,
}

#[derive(Default)]
pub struct Solver<'env> {
    unification_table: InPlaceUnificationTable<TyUniVar>,
    // hasher: FxHasher,
    phantom: PhantomData<&'env Environment>,
    errors: FxHashMap<NodeId, CompilerErr>,
    row_unification_table: InPlaceUnificationTable<RowUniVar>,
    partial_row_combs: BTreeSet<RowCombination>,
    row_to_combo: FxHashMap<NodeId, RowCombination>,
    branch_to_ret_ty: FxHashMap<NodeId, Type>,
}

impl<'env> Solver<'env> {
    fn fresh_ty_var(&mut self) -> TyUniVar {
        self.unification_table.new_key(None)
    }

    fn fresh_row_var(&mut self) -> RowUniVar {
        self.row_unification_table.new_key(None)
    }

    fn fresh_row_combination(&mut self) -> RowCombination {
        RowCombination {
            left: Row::Open(self.fresh_row_var()),
            right: Row::Open(self.fresh_row_var()),
            goal: Row::Open(self.fresh_row_var()),
        }
    }

    pub fn type_infer(ast: Spanned<Intern<Expr<Untyped>>>) -> TypeInferOut {
        let mut ctx = Solver::default();

        // Constraint generation
        let (out, ty) = ctx.infer(im::HashMap::default(), ast);

        // Constraint solving
        ctx.unification(out.constraints);

        // Apply our substition to our inferred types
        let subst_out = ctx
            .substitute_ty(ty)
            .merge(ctx.substitute_ast(out.typed_ast), |ty, ast| (ty, ast));
        // Return our typed ast and it's type scheme
        let mut ev_out = SubstOut::new(());
        let evidence = std::mem::take(&mut ctx.partial_row_combs)
            .into_iter()
            .filter_map(|row_comb| match row_comb {
                RowCombination {
                    left: Row::Open(left),
                    right,
                    goal,
                } if subst_out.unbound_rows.contains(&left) => Some(RowCombination {
                    left: Row::Open(left),
                    right,
                    goal,
                }),
                RowCombination {
                    left: Row::Closed(left),
                    right,
                    goal,
                } if left.mentions(&subst_out.unbound_tys, &subst_out.unbound_rows) => {
                    Some(RowCombination {
                        left: Row::Closed(left),
                        right,
                        goal,
                    })
                }
                RowCombination {
                    left,
                    right: Row::Open(right),
                    goal,
                } if subst_out.unbound_rows.contains(&right) => Some(RowCombination {
                    left,
                    right: Row::Open(right),
                    goal,
                }),
                RowCombination {
                    left,
                    right: Row::Closed(right),
                    goal,
                } if right.mentions(&subst_out.unbound_tys, &subst_out.unbound_rows) => {
                    Some(RowCombination {
                        left,
                        right: Row::Closed(right),
                        goal,
                    })
                }
                RowCombination {
                    left,
                    right,
                    goal: Row::Open(goal),
                    ..
                } if subst_out.unbound_rows.contains(&goal) => Some(RowCombination {
                    left,
                    right,
                    goal: Row::Open(goal),
                }),
                RowCombination {
                    left,
                    right,
                    goal: Row::Closed(goal),
                } if goal.mentions(&subst_out.unbound_tys, &subst_out.unbound_rows) => {
                    Some(RowCombination {
                        left,
                        right,
                        goal: Row::Closed(goal),
                    })
                }
                _ => None,
            })
            .map(|comb| {
                let out = ctx.substitute_row_comb(comb);
                ev_out.unbound_rows.extend(out.unbound_rows);
                ev_out.unbound_tys.extend(out.unbound_tys);
                out.value
            })
            .collect();

        let row_to_ev = std::mem::take(&mut ctx.row_to_combo)
            .into_iter()
            .map(|(id, combo)| {
                let out = ctx.substitute_row_comb(combo);
                ev_out.unbound_rows.extend(out.unbound_rows);
                ev_out.unbound_tys.extend(out.unbound_tys);
                (id, out.value)
            })
            .collect();
        let branch_to_ret_ty = std::mem::take(&mut ctx.branch_to_ret_ty)
            .into_iter()
            .map(|(id, ty)| {
                let out = ctx.substitute_ty(ty);
                ev_out.unbound_rows.extend(out.unbound_rows);
                ev_out.unbound_tys.extend(out.unbound_tys);
                (id, out.value)
            })
            .collect();
        let subst_out = subst_out.merge(ev_out, |l, _| l);
        // Return our typed ast and it's type scheme
        TypeInferOut {
            ast: subst_out.value.1,
            scheme: TypeScheme {
                unbound_rows: subst_out.unbound_rows,
                unbound_types: subst_out.unbound_tys,
                evidence,
                ty: subst_out.value.0,
            },
            errors: ctx.errors,
            row_to_ev,
            branch_to_ret_ty,
        }
    }
    /// Check a single item from the environment.
    fn check_item(
        &mut self,
        item: &'env Item,
        packctx: QualifierFragment,
    ) -> CompResult<&'env Item> {
        todo!()
    }
}
