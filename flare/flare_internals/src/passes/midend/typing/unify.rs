use crate::{
    passes::midend::typing::{
        rows::{ClosedRow, Row, RowCombination, RowUniVar},
        Constraint, Provenance, Solver, TyUniVar, Type,
    },
    resource::errors::{CompResult, TypeErr},
};

#[derive(Debug, Clone, Copy)]
enum UnificationError {
    TypeNotEqual(Type, Type),
    InfiniteType(TyUniVar, Type),

    RowsNotEqual((ClosedRow, ClosedRow)),
}

impl<'env> Solver<'env> {
    pub fn normalize_ty(&mut self, ty: Type) -> Type {
        match ty {
            Type::Num => ty,
            Type::Func(arg, ret) => {
                let arg = self.normalize_ty(*arg);
                let ret = self.normalize_ty(*ret);
                Type::Func(arg.into(), ret.into())
            }
            Type::Unifier(v) => match self.unification_table.probe_value(v) {
                Some(ty) => self.normalize_ty(ty),
                None => Type::Unifier(self.unification_table.find(v)),
            },
            _ => todo!("{ty:?}"),
        }
    }

    fn normalize_closed_row(&mut self, closed: ClosedRow) -> ClosedRow {
        ClosedRow {
            fields: closed.fields,
            values: closed
                .values
                .iter()
                .map(|ty| self.normalize_ty(*ty))
                .collect::<Vec<_>>()
                .leak(),
        }
    }

    fn normalize_row(&mut self, row: Row) -> Row {
        match row {
            Row::Open(var) => match self.row_unification_table.probe_value(var) {
                Some(closed) => Row::Closed(self.normalize_closed_row(closed)),
                None => row,
            },
            Row::Closed(closed) => Row::Closed(self.normalize_closed_row(closed)),
        }
    }

    fn unify_ty_ty(
        &mut self,
        unnorm_left: Type,
        unnorm_right: Type,
    ) -> Result<(), UnificationError> {
        let left = self.normalize_ty(unnorm_left);
        let right = self.normalize_ty(unnorm_right);
        match (left, right) {
            (Type::Num, Type::Num) => Ok(()),
            (Type::Func(a_arg, a_ret), Type::Func(b_arg, b_ret)) => {
                self.unify_ty_ty(*a_arg, *b_arg)
                    .map_err(|kind| match kind {
                        UnificationError::TypeNotEqual(a_arg, b_arg) => {
                            UnificationError::TypeNotEqual(
                                Type::Func(a_arg.into(), a_ret),
                                Type::Func(b_arg.into(), b_ret),
                            )
                        }
                        kind => kind,
                    })?;
                self.unify_ty_ty(*a_ret, *b_ret).map_err(|kind| match kind {
                    UnificationError::TypeNotEqual(a_ret, b_ret) => UnificationError::TypeNotEqual(
                        Type::Func(a_arg, a_ret.into()),
                        Type::Func(b_arg, b_ret.into()),
                    ),
                    kind => kind,
                })
            }
            (Type::Unifier(a), Type::Unifier(b)) => self
                .unification_table
                .unify_var_var(a, b)
                .map_err(|(l, r)| UnificationError::TypeNotEqual(l, r)),

            (Type::Unifier(v), ty) | (ty, Type::Unifier(v)) => {
                ty.occurs_check(v)
                    .map_err(|ty| UnificationError::InfiniteType(v, ty))?;
                self.unification_table
                    .unify_var_value(v, Some(ty))
                    .map_err(|(l, r)| UnificationError::TypeNotEqual(l, r))
            }
            (left, right) => Err(UnificationError::TypeNotEqual(left, right)),
        }
    }

    fn dispatch_any_solved(
        &mut self,
        var: RowUniVar,
        row: ClosedRow,
    ) -> Result<(), UnificationError> {
        let mut changed_combs = vec![];
        self.partial_row_combs = std::mem::take(&mut self.partial_row_combs)
            .into_iter()
            .filter_map(|comb| match comb {
                RowCombination { left, right, goal } if left == Row::Open(var) => {
                    changed_combs.push(RowCombination {
                        left: Row::Closed(row),
                        right,
                        goal,
                    });
                    None
                }
                RowCombination { left, right, goal } if right == Row::Open(var) => {
                    changed_combs.push(RowCombination {
                        left,
                        right: Row::Closed(row),
                        goal,
                    });
                    None
                }
                RowCombination { left, right, goal } if goal == Row::Open(var) => {
                    changed_combs.push(RowCombination {
                        left,
                        right,
                        goal: Row::Closed(row),
                    });
                    None
                }
                comb => Some(comb),
            })
            .collect();

        for row_comb in changed_combs {
            self.unify_row_comb(row_comb)?;
        }
        Ok(())
    }

    fn unify_row_row(&mut self, left: Row, right: Row) -> Result<(), UnificationError> {
        let left = self.normalize_row(left);
        let right = self.normalize_row(right);
        match (left, right) {
            (Row::Open(left), Row::Open(right)) => self
                .row_unification_table
                .unify_var_var(left, right)
                .map_err(UnificationError::RowsNotEqual),
            (Row::Open(var), Row::Closed(row)) | (Row::Closed(row), Row::Open(var)) => {
                self.row_unification_table
                    .unify_var_value(var, Some(row))
                    .map_err(UnificationError::RowsNotEqual)?;
                self.dispatch_any_solved(var, row)
            }
            (Row::Closed(left), Row::Closed(right)) => {
                if left.fields != right.fields {
                    return Err(UnificationError::RowsNotEqual((left, right)));
                }

                // If they are, our values are already in order so we can walk them and unify each
                // type
                let left_tys = left.values.iter();
                let right_tys = right.values.iter();
                for (left_ty, right_ty) in left_tys.zip(right_tys) {
                    self.unify_ty_ty(*left_ty, *right_ty)?;
                }
                Ok(())
            }
        }
    }

    /// Calculate the set difference of the goal row and the sub row, returning it as a new row.
    /// Unify the subset of the goal row that matches the sub row
    fn diff_and_unify(
        &mut self,
        goal: ClosedRow,
        sub: ClosedRow,
    ) -> Result<ClosedRow, UnificationError> {
        let mut diff_fields = vec![];
        let mut diff_values = vec![];
        for (field, value) in goal.fields.iter().zip(goal.values.iter()) {
            match sub.fields.binary_search(field) {
                Ok(indx) => {
                    self.unify_ty_ty(*value, sub.values[indx])?;
                }
                Err(_) => {
                    diff_fields.push(field);
                    diff_values.push(value);
                }
            }
        }
        Ok(ClosedRow {
            fields: diff_fields
                .into_iter()
                .map(|x| x.to_owned())
                .collect::<Vec<_>>()
                .leak(),
            values: diff_values
                .into_iter()
                .map(|x| x.to_owned())
                .collect::<Vec<_>>()
                .leak(),
        })
    }
    fn unify_row_comb(&mut self, row_comb: RowCombination) -> Result<(), UnificationError> {
        let left = self.normalize_row(row_comb.left);
        let right = self.normalize_row(row_comb.right);
        let goal = self.normalize_row(row_comb.goal);
        match (left, right, goal) {
            (Row::Closed(left), Row::Closed(right), goal) => {
                let calc_goal = ClosedRow::merge(left, right);
                self.unify_row_row(Row::Closed(calc_goal), goal)
            }
            (Row::Open(var), Row::Closed(sub), Row::Closed(goal))
            | (Row::Closed(sub), Row::Open(var), Row::Closed(goal)) => {
                let diff_row = self.diff_and_unify(goal, sub)?;
                self.unify_row_row(Row::Open(var), Row::Closed(diff_row))
            }

            (left, right, goal) => {
                let new_comb = RowCombination { left, right, goal };
                // Check if we've already seen an combination that we can unify against
                let poss_uni = self.partial_row_combs.iter().find_map(|comb| {
                    if comb.is_unifiable(&new_comb) {
                        Some(*comb)
                    // Check the commuted row combination
                    } else if comb.is_comm_unifiable(&new_comb) {
                        // Commute our combination so we unify the correct rows later
                        Some(RowCombination {
                            left: comb.right,
                            right: comb.left,
                            goal: comb.goal,
                        })
                    } else {
                        None
                    }
                });

                match poss_uni {
                    // Unify if we have a match
                    Some(match_comb) => {
                        self.unify_row_row(new_comb.left, match_comb.left)?;
                        self.unify_row_row(new_comb.right, match_comb.right)?;
                        self.unify_row_row(new_comb.goal, match_comb.goal)?;
                    }
                    // Otherwise add our combination to our list of
                    // partial combinations
                    None => {
                        self.partial_row_combs.insert(new_comb);
                    }
                }
                Ok(())
            }
        }
    }

    pub fn unification(&mut self, constraints: Vec<Constraint>) -> CompResult<()> {
        for constr in constraints {
            match constr {
                Constraint::TypeEqual(provenance, left, right) => {
                    if let Err(kind) = self.unify_ty_ty(left, right) {
                        let (node_id, mark) = match kind {
                            UnificationError::InfiniteType(type_var, ty) => {
                                (provenance.id(), TypeErr::InfiniteType { type_var, ty })
                            }
                            UnificationError::TypeNotEqual(left, right) => match provenance {
                                Provenance::UnexpectedFun(node_id) => (
                                    node_id,
                                    TypeErr::UnexpectedFun {
                                        expected_ty: left,
                                        fun_ty: right,
                                    },
                                ),
                                Provenance::AppExpectedFun(node_id) => (
                                    node_id,
                                    TypeErr::AppExpectedFun {
                                        inferred_ty: left,
                                        expected_fun_ty: right,
                                    },
                                ),
                                Provenance::ExpectedUnify(node_id) => (
                                    node_id,
                                    TypeErr::ExpectedUnify {
                                        checked: left,
                                        inferred: right,
                                    },
                                ),
                            },
                            UnificationError::RowsNotEqual((l, r)) => {
                                (provenance.id(), TypeErr::RowNotEqual(l, r))
                            }
                        };
                        self.errors.insert(node_id, mark.into());
                    }
                }
                Constraint::RowCombine(row_comb) => {
                    if let Err(kind) = self.unify_row_comb(row_comb) {
                        panic!()
                    }
                }
                _ => todo!(),
            }
        }

        Ok(())
    }
}
