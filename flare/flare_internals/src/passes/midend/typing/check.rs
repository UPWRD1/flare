use internment::Intern;

use crate::{
    passes::midend::typing::{
        rows::{Row, RowCombination},
        Constraint, GenOut, Provenance, Solver, Type, Typed,
    },
    resource::rep::{
        ast::{Direction, Expr, Untyped},
        Spanned,
    },
};

impl<'env> Solver<'env> {
    pub fn check(
        &mut self,
        env: im::HashMap<Untyped, Type>,
        the_ast: Spanned<Intern<Expr<Untyped>>>,
        ty: Type,
    ) -> GenOut {
        let span = the_ast.1;
        match (*the_ast.0, ty) {
            // Primitives
            (Expr::Number(n), Type::Num) => {
                GenOut::new(vec![], Spanned(Expr::Number(n).into(), span))
            }

            (Expr::String(s), Type::String) => {
                GenOut::new(vec![], Spanned(Expr::String(s).into(), span))
            }
            (Expr::Bool(b), Type::Bool) => GenOut::new(vec![], Spanned(Expr::Bool(b).into(), span)),

            (Expr::Unit, Type::Unit) => GenOut::new(vec![], Spanned(Expr::Unit.into(), span)),

            // Lambdas
            (Expr::Lambda(arg, body, is_anon), ty) => {
                let mut constraints = vec![];
                let (arg_ty, ret_ty) = match ty {
                    Type::Func(arg, ret) => (*arg, *ret),
                    ty => {
                        let arg = self.fresh_ty_var();
                        let ret = self.fresh_ty_var();

                        constraints.push(Constraint::TypeEqual(
                            Provenance::UnexpectedFun(span),
                            ty,
                            Type::Func(Type::Unifier(arg).into(), Type::Unifier(ret).into()),
                        ));
                        (Type::Unifier(arg), Type::Unifier(ret))
                    }
                };
                let env = env.update(arg, arg_ty);
                let body_out = self.check(env, body, ret_ty);
                constraints.extend(body_out.constraints);
                GenOut {
                    typed_ast: Spanned(
                        Expr::Lambda(Typed(arg, arg_ty), body_out.typed_ast, is_anon).into(),
                        span,
                    ),
                    constraints,
                }
            }

            // Row Types
            (Expr::Label(ast_lbl, val), Type::Label(ty_lbl, ty)) if ast_lbl == ty_lbl => self
                .check(env, val, *ty)
                .with_typed_ast(|term| Spanned(Expr::Label(ast_lbl, term).into(), span)),

            (ast @ Expr::Concat(_, _), Type::Label(lbl, ty))
            | (ast @ Expr::Project(_, _), Type::Label(lbl, ty)) => {
                // Cast a singleton row into a product
                self.check(
                    env,
                    Spanned(ast.into(), span),
                    Type::Prod(Row::single(lbl, *ty)),
                )
            }
            (ast @ Expr::Branch(_, _), Type::Label(lbl, ty))
            | (ast @ Expr::Inject(_, _), Type::Label(lbl, ty)) => self.check(
                env,
                Spanned(ast.into(), span),
                Type::Sum(Row::single(lbl, *ty)),
            ),

            (Expr::Unlabel(term, lbl), ty) => self
                .check(env, term, Type::Label(lbl, ty.into()))
                .with_typed_ast(|term| Spanned(Expr::Unlabel(term, lbl).into(), span)),

            (Expr::Concat(left, right), Type::Prod(goal_row)) => {
                let left_row = Row::Unifier(self.fresh_row_var());
                let right_row = Row::Unifier(self.fresh_row_var());

                let left_out = self.check(env.clone(), left, Type::Prod(left_row));
                let right_out = self.check(env, right, Type::Prod(right_row));
                let mut constraints = left_out.constraints;
                constraints.extend(right_out.constraints);
                let row_comb = RowCombination {
                    left: left_row,
                    right: right_row,
                    goal: goal_row,
                };
                constraints.push(Constraint::RowCombine(row_comb));
                self.row_to_combo.insert(span, row_comb);
                let typed_ast = Expr::Concat(left_out.typed_ast, right_out.typed_ast);
                GenOut {
                    constraints,
                    typed_ast: Spanned(typed_ast.into(), span),
                }
            }

            (Expr::Project(dir, goal), Type::Prod(sub_row)) => {
                let goal_row = Row::Unifier(self.fresh_row_var());

                let (left, right) = match dir {
                    Direction::Left => (sub_row, Row::Unifier(self.fresh_row_var())),
                    Direction::Right => (Row::Unifier(self.fresh_row_var()), sub_row),
                };

                let mut out = self.check(env, goal, Type::Prod(goal_row));
                let row_comb = RowCombination {
                    left,
                    right,
                    goal: goal_row,
                };
                out.constraints.push(Constraint::RowCombine(row_comb));

                self.row_to_combo.insert(span, row_comb);
                out.with_typed_ast(|ast| Spanned(Expr::Project(dir, ast).into(), span))
            }

            (Expr::Branch(left_ast, right_ast), Type::Func(arg_ty, ret_ty)) => {
                let mut constraints = vec![];
                let goal = match *arg_ty {
                    Type::Sum(goal) => goal,
                    _ => {
                        let goal = self.fresh_row_var();
                        constraints.push(Constraint::TypeEqual(
                            Provenance::ExpectedUnify(span),
                            *arg_ty,
                            Type::Sum(Row::Unifier(goal)),
                        ));
                        Row::Unifier(goal)
                    }
                };
                let left = Row::Unifier(self.fresh_row_var());
                let right = Row::Unifier(self.fresh_row_var());

                let left_out = self.check(
                    env.clone(),
                    left_ast,
                    Type::Func(Type::Sum(left).into(), ret_ty),
                );
                let right_out =
                    self.check(env, right_ast, Type::Func(Type::Sum(right).into(), ret_ty));

                constraints.extend(left_out.constraints);
                constraints.extend(right_out.constraints);
                let row_comb = RowCombination { left, right, goal };
                constraints.push(Constraint::RowCombine(row_comb));
                self.row_to_combo.insert(span, row_comb);
                self.branch_to_ret_ty.insert(span, *ret_ty);

                GenOut {
                    constraints,
                    typed_ast: Spanned(
                        Expr::Branch(left_out.typed_ast, right_out.typed_ast).into(),
                        span,
                    ),
                }
            }

            (Expr::Inject(dir, value), Type::Sum(goal)) => {
                let sub_row = self.fresh_row_var();
                let mut out = self.check(env, value, Type::Sum(Row::Unifier(sub_row)));
                let (left, right) = match dir {
                    Direction::Left => (sub_row, self.fresh_row_var()),
                    Direction::Right => (self.fresh_row_var(), sub_row),
                };
                let row_comb = RowCombination {
                    left: Row::Unifier(left),
                    right: Row::Unifier(right),
                    goal,
                };
                out.constraints.push(Constraint::RowCombine(row_comb));
                self.row_to_combo.insert(span, row_comb);
                out.with_typed_ast(|ast| Spanned(Expr::Inject(dir, ast).into(), span))
            }
            // Wildcard
            (_, expected_ty) => {
                let (mut out, actual_ty) = self.infer(env, the_ast);
                out.constraints.push(Constraint::TypeEqual(
                    Provenance::ExpectedUnify(span),
                    expected_ty,
                    actual_ty,
                ));
                out
            }
        }
    }
}
