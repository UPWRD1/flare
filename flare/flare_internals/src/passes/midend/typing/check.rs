use internment::Intern;
use rustc_hash::FxBuildHasher;

use crate::{
    passes::midend::typing::{
        Constraint, GenOut, Provenance, Solver, Type, Typed,
        rows::{Row, RowCombination},
    },
    resource::rep::{
        Spanned,
        ast::{Direction, Expr, Untyped},
    },
};

impl<'env> Solver<'env> {
    pub fn check(
        &mut self,
        env: im::HashMap<Intern<String>, Spanned<Intern<Type>>, FxBuildHasher>,
        the_ast: Spanned<Intern<Expr<Untyped>>>,
        ty: impl Into<Spanned<Intern<Type>>>,
    ) -> GenOut {
        // dbg!(&env);
        let the_ty = ty.into();
        let id = the_ast.id();

        dbg!(the_ast, the_ty);
        match (*the_ast.0, *the_ty.0) {
            // Primitives
            (Expr::Number(n), Type::Num) => {
                GenOut::new(vec![], Spanned(Expr::Number(n).into(), id))
            }

            (Expr::String(s), Type::String) => {
                GenOut::new(vec![], Spanned(Expr::String(s).into(), id))
            }
            (Expr::Bool(b), Type::Bool) => GenOut::new(vec![], Spanned(Expr::Bool(b).into(), id)),

            (Expr::Unit, Type::Unit) => GenOut::new(vec![], Spanned(Expr::Unit.into(), id)),

            // Lambdas
            (Expr::Lambda(arg, body, is_anon), ty) => {
                let mut constraints = vec![];
                let (arg_ty, ret_ty) = match ty {
                    Type::Func(arg, ret) => (arg, ret),
                    _ => {
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
                    }
                };
                let env = env.update(arg.0.0, arg_ty);
                let body_out = self.check(env, body, ret_ty);

                constraints.extend(body_out.constraints);
                GenOut::new(
                    constraints,
                    the_ast.convert(Expr::Lambda(
                        Typed(arg, arg_ty),
                        body_out.typed_ast,
                        is_anon,
                    )),
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

            (ast @ Expr::Concat(_, _), Type::Label(lbl, ty))
            | (ast @ Expr::Project(_, _), Type::Label(lbl, ty)) => {
                // Cast a singleton row into a product
                self.check(
                    env,
                    Spanned(ast.into(), id),
                    Spanned(Type::Prod(Row::single(lbl, ty)).into(), lbl.0.1),
                )
            }
            (ast @ Expr::Branch(_, _), Type::Label(lbl, ty))
            | (ast @ Expr::Inject(_, _), Type::Label(lbl, ty)) => self.check(
                env,
                Spanned(ast.into(), id),
                Spanned(Type::Sum(Row::single(lbl, ty)).into(), lbl.0.1),
            ),

            (Expr::Unlabel(term, lbl), _) => self
                .check(env, term, Spanned(Type::Label(lbl, the_ty).into(), lbl.0.1))
                .with_typed_ast(|term| Spanned(Expr::Unlabel(term, lbl).into(), id)),

            (Expr::Concat(left, right), Type::Prod(goal_row)) => {
                let left_row = Row::Unifier(self.fresh_row_var());
                let right_row = Row::Unifier(self.fresh_row_var());

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
                    Provenance::ExpectedCombine(id),
                    row_comb,
                ));
                self.tables.row_to_combo.insert(id, row_comb);
                let typed_ast = Expr::Concat(left_out.typed_ast, right_out.typed_ast);
                GenOut::new(constraints, Spanned(typed_ast.into(), id))
            }

            (Expr::Project(dir, goal), Type::Prod(sub_row)) => {
                let goal_row = Row::Unifier(self.fresh_row_var());

                let (left, right) = match dir {
                    Direction::Left => (sub_row, Row::Unifier(self.fresh_row_var())),
                    Direction::Right => (Row::Unifier(self.fresh_row_var()), sub_row),
                };

                let mut out = self.check(env, goal, the_ty.modify(Type::Prod(goal_row)));
                let row_comb = RowCombination {
                    left,
                    right,
                    goal: goal_row,
                };
                out.constraints.push(Constraint::RowCombine(
                    Provenance::ExpectedCombine(id),
                    row_comb,
                ));

                self.tables.row_to_combo.insert(id, row_comb);
                out.with_typed_ast(|ast| Spanned(Expr::Project(dir, ast).into(), id))
            }

            (Expr::Branch(left_ast, right_ast), Type::Func(arg_ty, ret_ty)) => {
                let mut constraints = vec![];
                let goal = match *arg_ty.0 {
                    Type::Sum(goal) => goal,
                    _ => {
                        let goal = self.fresh_row_var();
                        constraints.push(Constraint::TypeEqual(
                            Provenance::ExpectedUnify(left_ast.1, right_ast.1),
                            arg_ty,
                            arg_ty.modify(Type::Sum(Row::Unifier(goal))),
                        ));
                        Row::Unifier(goal)
                    }
                };
                let left = Row::Unifier(self.fresh_row_var());
                let right = Row::Unifier(self.fresh_row_var());

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
                    Provenance::ExpectedCombine(id),
                    row_comb,
                ));
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
                let sub_row = self.fresh_row_var();
                let mut out =
                    self.check(env, value, value.convert(Type::Sum(Row::Unifier(sub_row))));
                let (left, right) = match dir {
                    Direction::Left => (sub_row, self.fresh_row_var()),
                    Direction::Right => (self.fresh_row_var(), sub_row),
                };
                let row_comb = RowCombination {
                    left: Row::Unifier(left),
                    right: Row::Unifier(right),
                    goal,
                };
                out.constraints.push(Constraint::RowCombine(
                    Provenance::ExpectedCombine(id),
                    row_comb,
                ));
                self.tables.row_to_combo.insert(id, row_comb);
                out.with_typed_ast(|ast| Spanned(Expr::Inject(dir, ast).into(), id))
            }
            // (Expr::Let(name, def, body), _) => {
            //     let (mut def_out, def_ty) = self.infer(env.clone(), def);
            //     let env = env.update(name.0.0, def_ty);
            //     let body_out = self.check(env, body, the_ty);
            //     def_out.constraints.extend(body_out.constraints);
            //     def_out.with_typed_ast(|def| {
            //         the_ast.convert(Expr::Let(Typed(name, def_ty), def, body_out.typed_ast))
            //     })
            // }

            // Wildcard
            (_, _) => {
                // dbg!(the_ast, the_ty);
                let (mut out, actual_ty) = self.infer(env, the_ast);

                match *actual_ty.0 {
                    // Type::Prod(row) | Type::Sum(row) => {
                    //     let mut fresh_comb = self.fresh_row_combination();
                    //     fresh_comb.left = row;

                    //     out.constraints.push(Constraint::RowCombine(
                    //         Provenance::ExpectedCombine(id),
                    //         fresh_comb,
                    //     ));
                    //     out
                    // }
                    Type::Label(l, _) => {
                        out.constraints.push(Constraint::TypeEqual(
                            Provenance::ExpectedUnify(id, l.0.1),
                            the_ty,
                            actual_ty,
                        ));
                        out
                    }
                    _ => {
                        out.constraints.push(Constraint::TypeEqual(
                            Provenance::ExpectedUnify(id, the_ast.1),
                            the_ty,
                            actual_ty,
                        ));
                        out
                    }
                }
                // if matches!(the_ast.0, Expr:)
            }
        }
    }
}
