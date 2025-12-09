use internment::Intern;

use crate::{
    passes::midend::typing::{
        Constraint,
        Provenance,
        Solver,
        TyUniVar,
        Type,
        // types::InternType,
        rows::{ClosedRow, Row, RowCombination, RowUniVar},
    },
    resource::{
        errors::{CompilerErr, DynamicErr, TypeErr},
        rep::ast::Label,
    },
};

#[derive(Debug, Clone, Copy)]
enum UnificationError {
    TypeNotEqual(Type, Type),
    InfiniteType(TyUniVar, Type),

    RowsNotEqual((Row, Row)),
}

pub struct UnificationFailure;

impl<'env> Solver<'env> {
    pub fn normalize_ty(&mut self, ty: Intern<Type>) -> Intern<Type> {
        match *ty {
            Type::Num | Type::String | Type::Bool | Type::Unit | Type::Particle(_) => ty,

            Type::Var(var) => Type::Var(var).into(),
            Type::Func(arg, ret) => {
                let arg = self.normalize_ty(arg);
                let ret = self.normalize_ty(ret);
                Type::Func(arg, ret).into()
            }
            Type::Unifier(v) => match self.tables.unification_table.probe_value(v) {
                Some(ty) => self.normalize_ty(ty.into()),
                // None => Type::Unifier(self.tables.unification_table.find(v)).into(),
                None => Type::Unifier(self.tables.unification_table.find(v)).into(),
            },

            Type::Label(label, ty) => {
                let ty = self.normalize_ty(ty);
                Type::Label(label, ty).into()
            }
            Type::Prod(row) => Type::Prod(self.normalize_row(row)).into(),
            Type::Sum(row) => Type::Sum(self.normalize_row(row)).into(),
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
            Row::Unifier(var) => match self.tables.row_unification_table.probe_value(var) {
                Some(Row::Closed(closed)) => Row::Closed(self.normalize_closed_row(closed)),
                Some(row) => row,
                None => row,
            },
            Row::Open(var) => Row::Open(var),
            Row::Closed(closed) => Row::Closed(self.normalize_closed_row(closed)),
        }
    }

    fn unify_ty_ty(
        &mut self,
        unnorm_left: Intern<Type>,
        unnorm_right: Intern<Type>,
    ) -> Result<(), UnificationError> {
        let left = self.normalize_ty(unnorm_left);
        let right = self.normalize_ty(unnorm_right);
        match (*left, *right) {
            (Type::Num, Type::Num) => Ok(()),
            (Type::String, Type::String) => Ok(()),
            (Type::Particle(p), Type::Particle(q)) if p == q => Ok(()),

            (Type::Var(a), Type::Var(b)) if a == b => Ok(()),
            (Type::Func(a_arg, a_ret), Type::Func(b_arg, b_ret)) => {
                self.unify_ty_ty(a_arg, b_arg)?;
                self.unify_ty_ty(a_ret, b_ret).map_err(|kind| match kind {
                    UnificationError::TypeNotEqual(a_ret, b_ret) => UnificationError::TypeNotEqual(
                        Type::Func(a_arg, a_ret.into()),
                        Type::Func(b_arg, b_ret.into()),
                    ),
                    kind => kind,
                })
            }
            (Type::Unifier(a), Type::Unifier(b)) => self
                .tables
                .unification_table
                .unify_var_var(a, b)
                .map_err(|(l, r)| UnificationError::TypeNotEqual(l, r)),

            (Type::Unifier(v), ty) | (ty, Type::Unifier(v)) => {
                ty.occurs_check(v)
                    .map_err(|ty| UnificationError::InfiniteType(v, ty))?;
                self.tables
                    .unification_table
                    .unify_var_value(v, Some(ty))
                    .map_err(|(l, r)| UnificationError::TypeNotEqual(l, r))
            }

            (Type::Prod(left), Type::Prod(right)) | (Type::Sum(left), Type::Sum(right)) => {
                self.unify_row_row(left, right)
            }
            (Type::Label(field, ty), Type::Prod(row))
            | (Type::Prod(row), Type::Label(field, ty))
            | (Type::Label(field, ty), Type::Sum(row))
            | (Type::Sum(row), Type::Label(field, ty)) => self.unify_row_row(
                Row::Closed(ClosedRow {
                    fields: vec![field].leak(),
                    values: vec![ty].leak(),
                }),
                row,
            ),
            (left, right) => Err(UnificationError::TypeNotEqual(left, right)),
        }
    }

    fn dispatch_any_solved(
        &mut self,
        var: RowUniVar,
        row: ClosedRow,
    ) -> Result<(), UnificationError> {
        let mut changed_combs = vec![];
        self.tables.partial_row_combs = std::mem::take(&mut self.tables.partial_row_combs)
            .into_iter()
            .filter_map(|comb| match comb {
                RowCombination { left, right, goal } if left == Row::Unifier(var) => {
                    changed_combs.push(RowCombination {
                        left: Row::Closed(row),
                        right,
                        goal,
                    });
                    None
                }
                RowCombination { left, right, goal } if right == Row::Unifier(var) => {
                    changed_combs.push(RowCombination {
                        left,
                        right: Row::Closed(row),
                        goal,
                    });
                    None
                }
                RowCombination { left, right, goal } if goal == Row::Unifier(var) => {
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
            (Row::Open(left), Row::Open(right)) if left == right => Ok(()),
            (Row::Unifier(l), Row::Unifier(r)) => self
                .tables
                .row_unification_table
                .unify_var_var(l, r)
                .map_err(|(_, _)| UnificationError::RowsNotEqual((left, right))),

            (Row::Unifier(var), Row::Open(row)) | (Row::Open(row), Row::Unifier(var)) => self
                .tables
                .row_unification_table
                .unify_var_value(var, Some(Row::Open(row)))
                .map_err(UnificationError::RowsNotEqual),
            (Row::Unifier(var), Row::Closed(row)) | (Row::Closed(row), Row::Unifier(var)) => {
                self.tables
                    .row_unification_table
                    .unify_var_value(var, Some(Row::Closed(row)))
                    .map_err(UnificationError::RowsNotEqual)?;
                self.dispatch_any_solved(var, row)
            }
            (Row::Closed(l), Row::Closed(r)) => {
                // RowCombination::is_unifiable(&self, other);
                dbg!(l, r);
                // dbg!(left.equatable(&right));
                // dbg!(l.subtyped(&r));
                // dbg!(r.is_subtype_of(&r));
                // if let Some(l) = l.is_subtype_of(&r) {

                // dbg!(l);
                if l.fields == r.fields {
                    // let offenders = FxHashSet::from_iter(l.fields);
                    // let d = offenders.difference(&FxHashSet::from_iter(r.fields));

                    // If they are, our values are already in order so we can walk them and unify each
                    // type
                    let left_tys = l.values.iter();
                    let right_tys = r.values.iter();
                    for (left_ty, right_ty) in left_tys.zip(right_tys) {
                        self.unify_ty_ty(*left_ty, *right_ty)?;
                    }
                    Ok(())
                } else if let Some(l) = l.is_subtype_of(&r) {
                    let res = self.diff_and_unify(r, l)?;
                    dbg!(res);
                    Ok(())
                } else {
                    Err(UnificationError::RowsNotEqual((left, right)))
                }
            }

            (_, _) => Err(UnificationError::RowsNotEqual((left, right))),
        }
    }

    /// Calculate the set difference of the goal row and the sub row, returning it as a new row.
    /// Unify the subset of the goal row that matches the sub row
    fn diff_and_unify(
        &mut self,
        goal: ClosedRow,
        sub: ClosedRow,
    ) -> Result<ClosedRow, UnificationError> {
        let mut diff_fields: Vec<Label> = vec![];
        let mut diff_values = vec![];
        for (field, value) in goal.fields.iter().zip(goal.values.iter()) {
            match sub.fields.binary_search(field) {
                Ok(indx) => {
                    self.unify_ty_ty(*value, sub.values[indx])?;
                }
                Err(_) => {
                    diff_fields.push(*field);
                    diff_values.push(*value);
                }
            }
        }
        Ok(ClosedRow {
            fields: diff_fields
                // .into_iter()
                // .map(|x| x.to_owned())
                // .collect::<Vec<_>>()
                .leak(),
            values: diff_values
                // .into_iter()
                // .map(|x| x.to_owned())
                // .collect::<Vec<_>>()
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
            (Row::Unifier(var), Row::Closed(sub), Row::Closed(goal))
            | (Row::Closed(sub), Row::Unifier(var), Row::Closed(goal)) => {
                let diff_row = self.diff_and_unify(goal, sub)?;
                self.unify_row_row(Row::Unifier(var), Row::Closed(diff_row))
            }

            (left, right, goal) => {
                // let sub = self.diff_and_unify(right, left)?;
                // self.unify_row_row(sub, right)
                let new_comb = RowCombination { left, right, goal };
                // Check if we've already seen an combination that we can unify against
                let mut poss_uni = None;
                self.tables.partial_row_combs = std::mem::take(&mut self.tables.partial_row_combs)
                    .into_iter()
                    .map(|comb| {
                        let comb = RowCombination {
                            left: self.normalize_row(comb.left),
                            right: self.normalize_row(comb.right),
                            goal: self.normalize_row(comb.goal),
                        };
                        if comb.is_unifiable(&new_comb) {
                            poss_uni = Some(comb);
                        //Row combinations commute so we have to check for that possible unification
                        } else if comb.is_comm_unifiable(&new_comb) {
                            // We commute our combination so we unify the correct rows later

                            poss_uni = Some(RowCombination {
                                left: comb.right,
                                right: comb.left,
                                goal: comb.goal,
                            });
                        }
                        comb
                    })
                    .collect();

                match poss_uni {
                    // Unify if we have a match
                    Some(match_comb) => {
                        // dbg!(&match_comb);
                        self.unify_row_row(new_comb.left, match_comb.left)?;
                        self.unify_row_row(new_comb.right, match_comb.right)?;
                        self.unify_row_row(new_comb.goal, match_comb.goal)?;
                    }
                    // Otherwise add our combination to our list of
                    // partial combinations
                    None => {
                        println!("here {new_comb:?}");
                        self.tables.partial_row_combs.insert(new_comb);
                    }
                }
                Ok(())
            }
        }
    }

    pub fn unification(&mut self, constraints: Vec<Constraint>) -> Result<(), UnificationFailure> {
        for constr in constraints {
            // dbg!(&constr);
            let (provenance, uni_state) = match constr {
                Constraint::TypeEqual(p, left, right) => (p, self.unify_ty_ty(left, right)),

                Constraint::RowCombine(p, row_comb) => (p, self.unify_row_comb(row_comb)),
                _ => todo!(),
            };

            if let Err(kind) = uni_state {
                let (node_id, mark): (_, CompilerErr) = match kind {
                    UnificationError::InfiniteType(type_var, ty) => (
                        provenance.id(),
                        TypeErr::InfiniteType { type_var, ty }.into(),
                    ),
                    UnificationError::TypeNotEqual(left, right) => match provenance {
                        Provenance::UnexpectedFun(node_id) => (
                            node_id,
                            TypeErr::UnexpectedFun {
                                expected_ty: left,
                                fun_ty: right,
                            }
                            .into(),
                        ),
                        Provenance::AppExpectedFun(node_id) => (
                            node_id,
                            TypeErr::AppExpectedFun {
                                inferred_ty: left,
                                expected_fun_ty: right,
                            }
                            .into(),
                        ),
                        Provenance::ExpectedUnify(l_id, r_id) => (
                            l_id,
                            DynamicErr::new(format!("Type mismatch between {left} and {right}"))
                                .label(
                                    format!("expected '{right}' here, found '{left}'"),
                                    provenance.id(),
                                )
                                .extra(format!("This is {}", left), l_id)
                                .extra(format!("This is {}", right), r_id)
                                .into(),
                        ),
                        // FIXME: Use actual rows and not types...
                        Provenance::ExpectedCombine(node_id) => {
                            let err = DynamicErr::new("Could not combine types").label(
                                format!("Expected {:?}, found {:?}", left, right),
                                provenance.id(),
                            );
                            (node_id, err.into())
                        }
                    },
                    UnificationError::RowsNotEqual((l, r)) => {
                        // dbg!(provenance);
                        let err = DynamicErr::new("Could not unify types")
                            .label(format!("Expected {:?}, found {:?}", l, r), provenance.id());
                        (provenance.id(), err.into())
                    }
                };
                self.tables.errors.insert(node_id, mark);
            }
        }
        if !self.tables.errors.is_empty() {
            Err(UnificationFailure)
        } else {
            Ok(())
        }
        // dbg!()
    }
}
