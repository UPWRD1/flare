use internment::Intern;
use rustc_hash::FxBuildHasher;

use crate::{
    passes::frontend::typing::{
        Constraint, GenOut, Provenance, Solver, Type, Typed,
        rows::{Row, RowCombination},
    },
    resource::rep::{
        common::{HasSpan, Spanned},
        frontend::ast::{Direction, Expr, Untyped},
    },
};

impl Solver<'_> {
    pub fn check(
        &mut self,
        env: im::HashMap<Intern<String>, Spanned<Intern<Type>>, FxBuildHasher>,
        the_ast: Spanned<Intern<Expr<Untyped>>>,
        ty: impl Into<Spanned<Intern<Type>>>,
    ) -> GenOut {
        // dbg!(&env);
        let the_ty = ty.into();
        let id = the_ast.id();

        // dbg!(the_ast, the_ty);
        // dbg!(&self.tables.row_unification_table);
        match (*the_ast.0, *the_ty.0) {
            // Primitives
            // (Expr::Number(n), Type::Num) => {
            //     GenOut::new(vec![], Spanned(Expr::Number(n).into(), id))
            // }
            (Expr::String(s), Type::String) => {
                GenOut::new(vec![], Spanned(Expr::String(s).into(), id))
            }
            (Expr::Bool(b), Type::Bool) => GenOut::new(vec![], Spanned(Expr::Bool(b).into(), id)),

            (Expr::Unit, Type::Unit) => GenOut::new(vec![], Spanned(Expr::Unit.into(), id)),

            // Lambdas
            // (Expr::Lambda(arg, body), Type::Func(arg_ty, ret_ty)) => {
            //     let env = env.update(arg.0.0, arg_ty);
            //     self.check(env, body, ret_ty).with_typed_ast(|body| {
            //         Spanned(Expr::Lambda(Typed(arg, arg_ty), body).into(), id)
            //     })
            // }
            (Expr::Lambda(arg, body), ty) => {
                let mut constraints = vec![];
                let (arg_ty, ret_ty) = if let Type::Func(arg, ret) = ty {
                    (arg, ret)
                } else {
                    let arg_v = self.fresh_ty_var();
                    let ret_v = self.fresh_ty_var();
                    let arg = arg.0.convert(Type::Unifier(arg_v));
                    let ret = body.convert(Type::Unifier(ret_v));
                    constraints.push(Constraint::TypeEqual(
                        Provenance::UnexpectedFun(id),
                        the_ty,
                        the_ty.modify(Type::Func(arg, ret)),
                    ));

                    (arg, ret)
                };
                let env = env.update(arg.0.0, arg_ty);
                let body_out = self.check(env, body, ret_ty);

                constraints.extend(body_out.constraints);
                GenOut::new(
                    constraints,
                    the_ast.convert(Expr::Lambda(Typed(arg, arg_ty), body_out.typed_ast)),
                )
            }
            (Expr::If(cond, then, other), _) => {
                // dbg!(the_ty);
                let mut constraints = vec![];
                let cond = self.check(env.clone(), cond, cond.convert(Type::Bool));
                let then = self.check(env.clone(), then, the_ty);
                let other = self.check(env, other, the_ty);
                constraints.extend(cond.constraints);
                constraints.extend(then.constraints);
                constraints.extend(other.constraints);

                GenOut::new(
                    constraints,
                    the_ast.convert(Expr::If(cond.typed_ast, then.typed_ast, other.typed_ast)),
                )
            }

            // Row Types
            (Expr::Label(ast_lbl, term), Type::Label(ty_lbl, ty)) if ast_lbl.0.0 == ty_lbl.0.0 => {
                self.check(env, term, ty)
                    .with_typed_ast(|term| Spanned(Expr::Label(ast_lbl, term).into(), id))
            }

            (Expr::Concat(_, _) | Expr::Project(_, _), Type::Label(lbl, ty)) => {
                // Cast a singleton row into a product
                self.check(
                    env,
                    the_ast,
                    Spanned(
                        Type::Prod(Spanned(Row::single(lbl, ty).into(), id)).into(),
                        lbl.0.1,
                    ),
                )
            }
            (Expr::Branch(_, _) | Expr::Inject(_, _), Type::Label(lbl, ty)) => self.check(
                env,
                the_ast,
                // Spanned(ast.into(), id),
                Spanned(
                    Type::Sum(Spanned(Row::single(lbl, ty).into(), id)).into(),
                    lbl.0.1,
                    // the_ast.1,
                ),
            ),

            (Expr::Unlabel(term, lbl), _) => self
                .check(env, term, Spanned(Type::Label(lbl, the_ty).into(), lbl.0.1))
                .with_typed_ast(|term| Spanned(Expr::Unlabel(term, lbl).into(), id)),

            (Expr::Concat(left, right), Type::Prod(goal_row)) => {
                let left_row = Row::Unifier(self.fresh_row_var());
                let right_row = Row::Unifier(self.fresh_row_var());

                let left_row = left.convert(left_row);
                let right_row = right.convert(right_row);

                let left_out = self.check(env.clone(), left, the_ty.modify(Type::Prod(left_row)));
                let right_out = self.check(env, right, the_ty.modify(Type::Prod(right_row)));
                let mut constraints = left_out.constraints;
                constraints.extend(right_out.constraints);
                let row_comb = RowCombination {
                    left: left_row,
                    right: right_row,
                    goal: goal_row,
                };

                constraints.push(Constraint::RowCombine(
                    Provenance::ExpectedCombine(left.1, right.1),
                    row_comb,
                ));
                self.tables.row_to_combo.insert(id, row_comb);
                let typed_ast = Expr::Concat(left_out.typed_ast, right_out.typed_ast);
                GenOut::new(constraints, Spanned(typed_ast.into(), id))
            }

            (Expr::Project(dir, goal), Type::Prod(sub_row)) => {
                let goal_row = goal.convert(Row::Unifier(self.fresh_row_var()));

                let (left, right) = match dir {
                    Direction::Left => (sub_row, goal.convert(Row::Unifier(self.fresh_row_var()))),
                    Direction::Right => (goal.convert(Row::Unifier(self.fresh_row_var())), sub_row),
                };

                let mut out = self.check(env, goal, the_ty.modify(Type::Prod(goal_row)));
                let row_comb = RowCombination {
                    left,
                    right,
                    goal: goal_row,
                };
                out.constraints.push(Constraint::RowCombine(
                    Provenance::ExpectedCombine(the_ast.1, the_ty.1),
                    row_comb,
                ));

                self.tables.row_to_combo.insert(id, row_comb);
                out.with_typed_ast(|ast| Spanned(Expr::Project(dir, ast).into(), id))
            }

            (Expr::Branch(left_ast, right_ast), Type::Func(arg_ty, ret_ty)) => {
                let mut constraints = vec![];
                let goal = if let Type::Sum(goal) = *arg_ty.0 {
                    goal
                } else {
                    let goal = arg_ty.convert(Row::Unifier(self.fresh_row_var()));
                    constraints.push(Constraint::TypeEqual(
                        Provenance::ExpectedUnify(right_ast.1, left_ast.1),
                        arg_ty,
                        arg_ty.modify(Type::Sum(goal)),
                    ));
                    goal
                };

                let left = left_ast.convert(Row::Unifier(self.fresh_row_var()));
                let right = right_ast.convert(Row::Unifier(self.fresh_row_var()));

                let left_out = self.check(
                    env.clone(),
                    left_ast,
                    the_ty.modify(Type::Func(left_ast.convert(Type::Sum(left)), ret_ty)),
                );
                let right_out = self.check(
                    env,
                    right_ast,
                    the_ty.modify(Type::Func(right_ast.convert(Type::Sum(right)), ret_ty)),
                );

                constraints.extend(left_out.constraints);
                constraints.extend(right_out.constraints);
                let row_comb = RowCombination { left, right, goal };
                constraints.push(Constraint::RowCombine(
                    Provenance::ExpectedCombine(id, the_ty.1),
                    row_comb,
                ));
                // dbg!(id, left.1, right.1);
                self.tables.row_to_combo.insert(id, row_comb);
                self.tables.branch_to_ret_ty.insert(id, ret_ty);

                GenOut::new(
                    constraints,
                    Spanned(
                        Expr::Branch(left_out.typed_ast, right_out.typed_ast).into(),
                        id,
                    ),
                )
            }

            (Expr::Inject(dir, value), Type::Sum(goal)) => {
                // dbg!(value.1, goal.1, id);
                let sub_row = Row::Unifier(self.fresh_row_var());
                let mut out = self.check(
                    env,
                    value,
                    the_ty.convert(Type::Sum(value.convert(sub_row))),
                );

                let (left, right) = match dir {
                    Direction::Left => (
                        value.convert(sub_row),
                        goal.convert(Row::Unifier(self.fresh_row_var())),
                        // goal.convert(Row::Unifier(self.fresh_row_var())),
                    ),
                    Direction::Right => (
                        // goal.convert(Row::Unifier(self.fresh_row_var())),
                        goal.convert(Row::Unifier(self.fresh_row_var())),
                        value.convert(sub_row),
                    ),
                };
                let row_comb = RowCombination { left, right, goal };
                out.constraints.push(Constraint::RowCombine(
                    Provenance::ExpectedCombine(id, the_ty.1),
                    row_comb,
                ));

                self.tables.row_to_combo.insert(id, row_comb);
                out.with_typed_ast(|ast| Spanned(Expr::Inject(dir, ast).into(), id))
            }
            (Expr::Let(name, def, body), _) => {
                let def_var = self.fresh_ty_var();
                let def_ty = def.convert(Type::Unifier(def_var));
                // let (mut def_out, real_def_ty) = self.infer(env.clone(), def);

                let mut def_out = self.check(env.clone(), def, def_ty);
                let env = env.update(name.0.0, def_ty);
                let body_out = self.check(env, body, the_ty);
                def_out.constraints.extend(body_out.constraints);

                def_out.with_typed_ast(|def| {
                    the_ast.convert(Expr::Let(Typed(name, def_ty), def, body_out.typed_ast))
                })
            }
            (Expr::Access(base, field), _) => {
                dbg!(the_ty);
                // τ (expected_ty) is already known — this is the bidirectional payoff.
                // Construct: (field ▸ τ) ⊙ ζ_rest ∼ ζ_base
                let field_row_span = field.0.1; // use field's span for the singleton row
                // Infer base first — gives us the concrete row directly
                // let (base_out, base_ty) = self.infer(env, base);
                // let mut constraints = base_out.constraints;

                // // Extract or constrain the base row without introducing a fresh variable
                // let base_row = match *base_ty.0 {
                //     Type::Prod(row) => row,
                //     Type::Unifier(_) => {
                //         // Base type is still unknown — we do need a fresh row here,
                //         // but only in this case
                //         let fresh = base.convert(Row::Unifier(self.fresh_row_var()));
                //         constraints.push(Constraint::TypeEqual(
                //             Provenance::FieldAccess(base.1, field),
                //             base_ty,
                //             base.convert(Type::Prod(fresh)),
                //         ));
                //         fresh
                //     }
                //     _ => {
                //         // Type error: base is not a product
                //         // emit diagnostic and return error type
                //         panic!("base is not a product type")
                //     }
                // };
                let base_row = base.convert(Row::Unifier(self.fresh_row_var()));
                let base_infer_ty = base.convert(Type::Prod(base_row));
                let base_out = self.check(env, base, base_infer_ty);
                let mut constraints = base_out.constraints;
                let field_singleton = field.0.convert(Row::single(field, the_ty));
                let goal_row = the_ast.convert(Row::Unifier(self.fresh_row_var()));

                let row_comb = RowCombination {
                    left: field_singleton,
                    right: goal_row,
                    goal: base_row,
                };

                constraints.push(Constraint::RowCombine(
                    Provenance::FieldAccess(base.1, field),
                    row_comb,
                ));

                // Record the combination so codegen can recover the projection path
                self.tables.row_to_combo.insert(the_ast.1, row_comb);

                GenOut::new(
                    constraints,
                    the_ast.convert(Expr::Access(base_out.typed_ast, field)),
                )
            }

            // Wildcard
            (_, _) => {
                // dbg!(the_ast, the_ty);
                let (mut out, actual_ty) = self.infer(env, the_ast);
                // dbg!(actual_ty);
                {
                    out.constraints.push(Constraint::TypeEqual(
                        Provenance::ExpectedUnify(id, the_ty.1),
                        the_ty,
                        actual_ty,
                    ));
                    out
                }
            }
        }
    }
}
