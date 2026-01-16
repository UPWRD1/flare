use chumsky::span::Span;
use internment::Intern;

use crate::{
    passes::midend::{
        resolution::subst_generic_type,
        typing::{
            Constraint, Provenance, Solver, TyUniVar, Type, TypeScheme,
            rows::{ClosedRow, Row, RowCombination, RowUniVar},
        },
    },
    resource::{
        errors::{CompilerErr, DynamicErr, TypeErr},
        rep::{Spanned, ast::Label},
    },
};

#[derive(Debug, Clone, Copy)]
enum UnificationError {
    TypeNotEqual(Spanned<Intern<Type>>, Spanned<Intern<Type>>),
    InfiniteType(TyUniVar, Type),

    RowsNotEqual(Spanned<Intern<Row>>, Spanned<Intern<Row>>),
}

pub struct UnificationFailure;

impl Solver<'_> {
    pub fn normalize_ty(&mut self, ty: Spanned<Intern<Type>>) -> Spanned<Intern<Type>> {
        match *ty.0 {
            Type::Num
            | Type::String
            | Type::Bool
            | Type::Unit
            | Type::Particle(_)
            // | Type::Template(_)
            | Type::Var(_) => ty,
            Type::Func(arg, ret) => {
                let arg = self.normalize_ty(arg);
                let ret = self.normalize_ty(ret);
                ty.modify(Type::Func(arg, ret))
            }
            Type::Unifier(v) => {
                match self.tables.unification_table.probe_value(v) {
                    Some(ty) => self.normalize_ty(ty),
                    // None => Type::Unifier(self.tables.unification_table.find(v)).into(),
                    None => ty.modify(Type::Unifier(self.tables.unification_table.find(v))),
                }
            }
            Type::Label(label, t) => {
                let t = self.normalize_ty(t);
                ty.modify(Type::Label(label, t))
            }
            Type::Prod(row) => ty.modify(Type::Prod(self.normalize_row(row))),
            Type::Sum(row) => ty.modify(Type::Sum(self.normalize_row(row))),
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

    fn normalize_row(&mut self, row: Spanned<Intern<Row>>) -> Spanned<Intern<Row>> {
        row.map(|row| match *row {
            Row::Unifier(var) => match self.tables.row_unification_table.probe_value(var) {
                Some(Row::Closed(closed)) => Row::Closed(self.normalize_closed_row(closed)).into(),
                Some(row) => row.into(),
                None => row,
            },
            Row::Open(var) => Row::Open(var).into(),
            Row::Closed(closed) => Row::Closed(self.normalize_closed_row(closed)).into(),
        })
    }

    fn unify_ty_ty(
        &mut self,
        unnorm_left: Spanned<Intern<Type>>,
        unnorm_right: Spanned<Intern<Type>>,
    ) -> Result<(), UnificationError> {
        let left = self.normalize_ty(unnorm_left);
        let right = self.normalize_ty(unnorm_right);
        // dbg!(left, right);
        match (*left.0, *right.0) {
            (Type::Num, Type::Num)
            | (Type::String, Type::String)
            | (Type::Bool, Type::Bool)
            | (Type::Unit, Type::Unit) => Ok(()),
            (Type::Particle(p), Type::Particle(q)) if p == q => Ok(()),

            (Type::Var(a), Type::Var(b)) if a.0 == b.0 => Ok(()),
            (Type::Func(a_arg, a_ret), Type::Func(b_arg, b_ret)) => {
                self.unify_ty_ty(a_arg, b_arg)?;
                self.unify_ty_ty(a_ret, b_ret).map_err(|kind| match kind {
                    UnificationError::TypeNotEqual(a_ret, b_ret) => UnificationError::TypeNotEqual(
                        left.modify(Type::Func(a_arg, a_ret)),
                        left.modify(Type::Func(b_arg, b_ret)),
                    ),
                    kind => kind,
                })
            }
            (Type::Unifier(a), Type::Unifier(b)) => self
                .tables
                .unification_table
                .unify_var_var(a, b)
                .map_err(|(l, r)| UnificationError::TypeNotEqual(l, r)),

            (Type::Unifier(v), ty) => {
                ty.occurs_check(v)
                    .map_err(|ty| UnificationError::InfiniteType(v, ty))?;
                self.tables
                    .unification_table
                    .unify_var_value(v, Some(right))
                    .map_err(|(l, r)| UnificationError::TypeNotEqual(l, r))
            }

            (ty, Type::Unifier(v)) => {
                ty.occurs_check(v)
                    .map_err(|ty| UnificationError::InfiniteType(v, ty))?;
                self.tables
                    .unification_table
                    .unify_var_value(v, Some(left))
                    .map_err(|(l, r)| UnificationError::TypeNotEqual(r, l))
            }

            (Type::Prod(left), Type::Prod(right)) | (Type::Sum(left), Type::Sum(right)) => {
                // dbg!(left, right);
                self.unify_row_row(left, right)
            }

            (Type::Label(field, ty), Type::Prod(row) | Type::Sum(row)) => self.unify_row_row(
                left.convert(Row::Closed(ClosedRow {
                    fields: vec![field].leak(),
                    values: vec![ty].leak(),
                })),
                row,
            ),

            (Type::Prod(row) | Type::Sum(row), Type::Label(field, ty)) => self.unify_row_row(
                right.convert(Row::Closed(ClosedRow {
                    fields: vec![field].leak(),
                    values: vec![ty].leak(),
                })),
                row,
            ),
            // (Type::TypeApp(typefun, rep), _) => {
            //     if let Type::TypeFun(arg, t) = *typefun.0 {
            //         let subst_t = subst_generic_type(t, arg.0, rep.0);

            //         self.unify_ty_ty(subst_t, right)
            //     } else {
            //         todo!()
            //     }
            // }

            // (_, Type::TypeApp(typefun, rep)) => {
            //     if let Type::TypeFun(arg, t) = *typefun.0 {
            //         let subst_t = subst_generic_type(t, arg.0, rep.0);

            //         self.unify_ty_ty(subst_t, left)
            //     } else {
            //         todo!()
            //     }
            // }
            // (Type::TypeFun(arg, t), rep) | (rep, Type::TypeFun(arg, t)) => {
            //     dbg!(arg, t, rep);
            //     todo!()
            //     // self.tables.unification_table.unify_var_value(a_id, b)
            // }
            (_, _) => {
                dbg!(left, right);
                Err(UnificationError::TypeNotEqual(left, right))
            }
        }
    }

    fn unify_typeapp(
        &mut self,
        tyfun: Spanned<Intern<Type>>,
        arg: Spanned<Intern<Type>>,
        rep: Spanned<Intern<Type>>,
    ) -> Result<(), UnificationError> {
        todo!()
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
                RowCombination { left, right, goal } if *left.0 == Row::Unifier(var) => {
                    changed_combs.push(RowCombination {
                        left: left.modify(Row::Closed(row)),
                        right,
                        goal,
                    });
                    None
                }
                RowCombination { left, right, goal } if *right.0 == Row::Unifier(var) => {
                    changed_combs.push(RowCombination {
                        left,
                        right: right.modify(Row::Closed(row)),
                        goal,
                    });
                    None
                }
                RowCombination { left, right, goal } if *goal.0 == Row::Unifier(var) => {
                    changed_combs.push(RowCombination {
                        left,
                        right,
                        goal: goal.modify(Row::Closed(row)),
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

    fn unify_row_row(
        &mut self,
        left: Spanned<Intern<Row>>,
        right: Spanned<Intern<Row>>,
    ) -> Result<(), UnificationError> {
        // dbg!(&self.tables.row_unification_table);
        let left = self.normalize_row(left);
        let right = self.normalize_row(right);
        // dbg!(left, right);
        match (*left.0, *right.0) {
            (Row::Open(left), Row::Open(right)) if left == right => Ok(()),
            (Row::Unifier(l), Row::Unifier(r)) => self
                .tables
                .row_unification_table
                .unify_var_var(l, r)
                .map_err(|(_, _)| UnificationError::RowsNotEqual(left, right)),

            (Row::Unifier(var), Row::Open(row)) | (Row::Open(row), Row::Unifier(var)) => self
                .tables
                .row_unification_table
                .unify_var_value(var, Some(Row::Open(row)))
                .map_err(|(_, _)| UnificationError::RowsNotEqual(left, right)),
            (Row::Unifier(var), Row::Closed(row)) | (Row::Closed(row), Row::Unifier(var)) => {
                // dbg!(&self.tables.row_unification_table);
                self.tables
                    .row_unification_table
                    .unify_var_value(var, Some(Row::Closed(row)))
                    .map_err(|(_, _)| UnificationError::RowsNotEqual(left, right))?;
                self.dispatch_any_solved(var, row)
            }
            (Row::Closed(l), Row::Closed(r)) => {
                // dbg!(l, r);
                // if l.fields
                //     .iter()
                //     .zip(r.fields)
                //     .all(|(lf, rf)| lf.0.0 == rf.0.0)
                if l.fields == r.fields
                // || l.fields.to_vec() == r.fields.into_iter().rev().collect::<Vec<_>>()
                {
                    // dbg!(l, r);
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
                } else {
                    Err(UnificationError::RowsNotEqual(left, right))
                }
            }

            (_, _) => Err(UnificationError::RowsNotEqual(left, right)),
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
            if let Ok(indx) = sub.fields.binary_search(field) {
                self.unify_ty_ty(*value, sub.values[indx])?;
            } else {
                diff_fields.push(*field);
                diff_values.push(*value);
            }
        }
        Ok(ClosedRow {
            fields: diff_fields.leak(),
            values: diff_values.leak(),
        })
    }

    fn unify_row_comb(&mut self, row_comb: RowCombination) -> Result<(), UnificationError> {
        let left = self.normalize_row(row_comb.left);
        let right = self.normalize_row(row_comb.right);
        let goal = self.normalize_row(row_comb.goal);

        match (*left.0, *right.0, *goal.0) {
            (Row::Closed(l), Row::Closed(r), g) => {
                let calc_goal = ClosedRow::merge(l, r);
                self.unify_row_row(
                    Spanned(Row::Closed(calc_goal).into(), left.1.union(right.1)),
                    goal.convert(g),
                )
            }
            (Row::Unifier(var), Row::Closed(sub), Row::Closed(goal)) => {
                let diff_row = self.diff_and_unify(goal, sub)?;

                self.unify_row_row(
                    Spanned(Row::Unifier(var).into(), left.1.union(right.1)),
                    right.convert(Row::Closed(diff_row)),
                )
            }

            (Row::Closed(sub), Row::Unifier(var), Row::Closed(goal)) => {
                let diff_row = self.diff_and_unify(goal, sub)?;

                self.unify_row_row(
                    Spanned(Row::Unifier(var).into(), left.1.union(right.1)),
                    left.convert(Row::Closed(diff_row)),
                )
            }

            (_l, _r, _g) => {
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
                        self.unify_row_row(new_comb.left, match_comb.left)?;
                        self.unify_row_row(new_comb.right, match_comb.right)?;
                        self.unify_row_row(new_comb.goal, match_comb.goal)?;
                    }
                    // Otherwise add our combination to our list of
                    // partial combinations
                    None => {
                        // println!("here {new_comb:#?}");
                        self.tables.partial_row_combs.insert(new_comb);
                    }
                }
                Ok(())
            }
        }
    }

    pub fn unification(
        &mut self,
        constraints: Vec<Constraint>,
        scheme: &TypeScheme,
    ) -> Result<(), UnificationFailure> {
        for constr in constraints {
            // dbg!(&constr);
            let (provenance, uni_state) = match constr {
                Constraint::TypeEqual(p, left, right) => (p, self.unify_ty_ty(left, right)),

                Constraint::RowCombine(p, row_comb) => (p, self.unify_row_comb(row_comb)),
            };

            if let Err(kind) = uni_state {
                let (node_id, mark): (_, CompilerErr) = match kind {
                    UnificationError::InfiniteType(type_var, ty) => (
                        provenance.id(),
                        TypeErr::InfiniteType { type_var, ty }.into(),
                    ),
                    UnificationError::TypeNotEqual(left, right) => {
                        match provenance {
                            Provenance::UnexpectedFun(node_id) => (
                                node_id,
                                DynamicErr::new("Encountered an unexpected function".to_string())
                                    .label("This is a function", node_id)
                                    .extra(format!("This is {}", left.0.render(scheme)), left.1)
                                    .extra(format!("This is {}", right.0.render(scheme)), right.1)
                                    .into(),
                            ),
                            Provenance::AppExpectedFun(node_id) => (
                                node_id,
                                DynamicErr::new("Expected a function".to_string())
                                    .label("This is not a function", node_id)
                                    .extra(format!("This is {}", left.0), left.1)
                                    .extra(format!("This is {}", right.0), right.1)
                                    .into(),
                            ),
                            Provenance::ExpectedUnify(l_id, r_id) => {
                                let left_rendered = left.0.render(scheme);
                                let right_rendered = right.0.render(scheme);
                                (
                                l_id,
                                DynamicErr::new(format!(
                                    "Type mismatch between {left_rendered} and {right_rendered}"
                                ))
                                .label(format!("expected '{right_rendered}' here, found '{left_rendered}'"), l_id)
                                .extra(format!("This is {}", left_rendered), left.1)
                                .extra(format!("This is {}", right_rendered), r_id)
                                .into(),
                            )
                            }
                            Provenance::ConditionIsBool(c_id) => {
                                let err = DynamicErr::new("Expected bool").label(
                                    format!("This condtio should be a bool, found {}", right.0),
                                    left.1,
                                );
                                (c_id, err.into())
                            }

                            Provenance::ExpectedCombine(l_span, r_span) => {
                                let err = DynamicErr::new(format!(
                                    "Row mismatch between {} and {}",
                                    left.0, right.0
                                ))
                                .label(
                                    format!(
                                        "Expected {} to combine with {}",
                                        right.0.render(scheme),
                                        left.0.render(scheme)
                                    ),
                                    l_span,
                                )
                                .extra("and here", r_span);
                                (l_span, err.into())
                            } // _ => unreachable!("Invalid providence"),
                        }
                    }
                    UnificationError::RowsNotEqual(l, r) => {
                        let err = match provenance {
                            Provenance::ExpectedCombine(l_span, r_span) => {
                                DynamicErr::new(format!("Row mismatch between {l} and {r}"))
                                    .label(
                                        format!(
                                            "Expected {} to combine with {}",
                                            r.render(scheme),
                                            l.render(scheme)
                                        ),
                                        l_span,
                                    )
                                    .extra("and here", r_span)
                            }

                            Provenance::ExpectedUnify(l_span, r_span) => {
                                DynamicErr::new(format!("Row mismatch between {l} and {r}"))
                                    .label(
                                        format!(
                                            "Expected {} to unify with {}",
                                            r.render(scheme),
                                            l.render(scheme)
                                        ),
                                        l_span,
                                    )
                                    .extra("and here", r_span)
                                    .extra("from", l.1)
                                    .extra("and from", r.1)
                            }
                            _ => DynamicErr::new(format!("Row mismatch between {l} and {r}"))
                                .label(
                                    format!(
                                        "Expected {}, found {}",
                                        r.render(scheme),
                                        l.render(scheme)
                                    ),
                                    provenance.id(),
                                ),
                        };
                        (provenance.id(), err.into())
                    }
                };
                self.tables.errors.insert(node_id, mark);
            }
        }
        if self.tables.errors.is_empty() {
            Ok(())
        } else {
            Err(UnificationFailure)
        }
        // dbg!()
    }
}
