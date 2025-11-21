use internment::Intern;

use crate::{
    passes::midend::typing::{Constraint, GenOut, Provenance, Solver, Type, Typed},
    resource::rep::{
        ast::{Direction, Expr, Untyped},
        Spanned,
    },
};

impl<'env> Solver<'env> {
    pub fn infer(
        &mut self,
        env: im::HashMap<Untyped, Type>,
        ast: Spanned<Intern<Expr<Untyped>>>,
    ) -> (GenOut, Type) {
        match *ast.0 {
            Expr::Number(n) => (
                GenOut::new(vec![], Spanned(Expr::Number(n).into(), ast.1)),
                Type::Num,
            ),
            Expr::Ident(v) => {
                let ty = env[&v];
                (
                    GenOut::new(vec![], Spanned(Expr::Ident(Typed(v, ty)).into(), ast.1)),
                    ty,
                )
            }
            Expr::Lambda(arg, body, is_anon) => {
                let arg_tyvar = self.fresh_ty_var();
                let env = env.update(arg, Type::Unifier(arg_tyvar));

                let (body_out, body_ty) = self.infer(env, body);
                (
                    GenOut {
                        typed_ast: Spanned(
                            Expr::Lambda(
                                Typed(arg, Type::Unifier(arg_tyvar)),
                                body_out.typed_ast,
                                is_anon,
                            )
                            .into(),
                            ast.1,
                        ),
                        ..body_out
                    },
                    Type::Func(Type::Unifier(arg_tyvar).into(), body_ty.into()),
                )
            }
            Expr::Call(fun, arg) => {
                let (fun_out, supposed_fun_ty) = self.infer(env.clone(), fun);
                let mut constraint = fun_out.constraints;
                let (arg_ty, ret_ty) = match supposed_fun_ty {
                    Type::Func(arg, ret) => (*arg, *ret),
                    ty => {
                        let arg = self.fresh_ty_var();
                        let ret = self.fresh_ty_var();

                        constraint.push(Constraint::TypeEqual(
                            Provenance::AppExpectedFun(ast.1),
                            ty,
                            Type::Func(Type::Unifier(arg).into(), Type::Unifier(ret).into()),
                        ));

                        (Type::Unifier(arg), Type::Unifier(ret))
                    }
                };
                let arg_out = self.check(env, arg, arg_ty);
                constraint.extend(arg_out.constraints);
                (
                    GenOut::new(
                        constraint,
                        Spanned(
                            Expr::Call(fun_out.typed_ast, arg_out.typed_ast).into(),
                            ast.1,
                        ),
                    ),
                    ret_ty,
                )
            }
            Expr::Hole(v) => {
                let var = self.fresh_ty_var();
                (
                    GenOut::new(
                        vec![],
                        Spanned(Expr::Hole(Typed(v, Type::Unifier(var))).into(), ast.1),
                    ),
                    Type::Unifier(var),
                )
            }

            Expr::Label(label, value) => {
                let (out, value_ty) = self.infer(env, value);
                (
                    out.with_typed_ast(|ast| Spanned(Expr::Label(label, ast).into(), ast.1)),
                    Type::Label(label, value_ty.into()),
                )
            }
            Expr::Unlabel(value, label) => {
                let value_var = self.fresh_ty_var();
                let expected_ty = Type::Label(label, Type::Unifier(value_var).into());
                let out = self.check(env, value, expected_ty);
                (
                    out.with_typed_ast(|ast| Spanned(Expr::Unlabel(ast, label).into(), ast.1)),
                    Type::Unifier(value_var),
                )
            }

            Expr::Concat(left, right) => {
                let row_comb = self.fresh_row_combination();

                let left_out = self.check(env.clone(), left, Type::Prod(row_comb.left));
                let right_out = self.check(env, right, Type::Prod(row_comb.right));

                let out_ty = Type::Prod(row_comb.goal);
                let mut constraints = left_out.constraints;
                constraints.extend(right_out.constraints);
                constraints.push(Constraint::RowCombine(row_comb));
                self.row_to_combo.insert(ast.1, row_comb);
                let typed_ast = Spanned(
                    Expr::Concat(left_out.typed_ast, right_out.typed_ast).into(),
                    ast.1,
                );
                (
                    GenOut {
                        constraints,
                        typed_ast,
                    },
                    out_ty,
                )
            }
            Expr::Project(dir, goal) => {
                let row_comb = self.fresh_row_combination();
                let sub_row = match dir {
                    Direction::Left => row_comb.left,
                    Direction::Right => row_comb.right,
                };

                let mut out = self.check(env, goal, Type::Prod(row_comb.goal));

                out.constraints.push(Constraint::RowCombine(row_comb));
                (
                    out.with_typed_ast(|ast| Spanned(Expr::Project(dir, ast).into(), ast.1)),
                    Type::Prod(sub_row),
                )
            }
            Expr::Branch(left, right) => {
                let row_comb = self.fresh_row_combination();
                let ret_ty = self.fresh_ty_var();

                let left_out = self.check(
                    env.clone(),
                    left,
                    Type::Func(
                        Type::Sum(row_comb.left).into(),
                        Type::Unifier(ret_ty).into(),
                    ),
                );

                let right_out = self.check(
                    env.clone(),
                    right,
                    Type::Func(
                        Type::Sum(row_comb.right).into(),
                        Type::Unifier(ret_ty).into(),
                    ),
                );

                let out_ty = Type::Func(
                    Type::Sum(row_comb.goal).into(),
                    Type::Unifier(ret_ty).into(),
                );

                let mut constraints = left_out.constraints;
                constraints.extend(right_out.constraints);
                constraints.push(Constraint::RowCombine(row_comb));
                self.row_to_combo.insert(ast.1, row_comb);
                self.branch_to_ret_ty.insert(ast.1, Type::Unifier(ret_ty));
                let typed_ast = Expr::Branch(left_out.typed_ast, right_out.typed_ast);
                (
                    GenOut::new(constraints, Spanned(typed_ast.into(), ast.1)),
                    out_ty,
                )
            }
            Expr::Inject(dir, value) => {
                let row_comb = self.fresh_row_combination();

                let sub_row = match dir {
                    Direction::Left => row_comb.left,
                    Direction::Right => row_comb.right,
                };

                let out_ty = Type::Sum(row_comb.goal);

                let mut out = self.check(env, value, Type::Sum(sub_row));
                out.constraints.push(Constraint::RowCombine(row_comb));
                self.row_to_combo.insert(ast.1, row_comb);
                (
                    out.with_typed_ast(|ast| Spanned(Expr::Inject(dir, ast).into(), ast.1)),
                    // Our goal row is the type of our output
                    out_ty,
                )
            }
            _ => todo!(),
        }
    }
}
