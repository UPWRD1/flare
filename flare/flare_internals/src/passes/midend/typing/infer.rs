use internment::Intern;
use rustc_hash::FxHashMap;

use crate::{
    passes::midend::typing::{
        inst::Instantiate, Constraint, Evidence, GenOut, ItemWrapper, Provenance, Row, Solver,
        Type, Typed,
    },
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
            Expr::Number(n) => (GenOut::new(vec![], ast.update(Expr::Number(n))), Type::Num),
            Expr::Ident(v) => {
                let ty = env[&v];
                (
                    GenOut::new(vec![], ast.update(Expr::Ident(Typed(v, ty)))),
                    ty,
                )
            }
            Expr::Lambda(arg, body, is_anon) => {
                let arg_tyvar = self.fresh_ty_var();
                let env = env.update(arg, Type::Unifier(arg_tyvar));

                let (body_out, body_ty) = self.infer(env, body);
                (
                    GenOut {
                        typed_ast: ast.update(Expr::Lambda(
                            Typed(arg, Type::Unifier(arg_tyvar)),
                            body_out.typed_ast,
                            is_anon,
                        )),
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
                        ast.update(Expr::Call(fun_out.typed_ast, arg_out.typed_ast)),
                    ),
                    ret_ty,
                )
            }
            Expr::Hole(v) => {
                let var = self.fresh_ty_var();
                (
                    GenOut::new(vec![], ast.update(Expr::Hole(Typed(v, Type::Unifier(var))))),
                    Type::Unifier(var),
                )
            }

            Expr::Label(label, value) => {
                let (out, value_ty) = self.infer(env, value);
                (
                    out.with_typed_ast(|ast| ast.update(Expr::Label(label, ast))),
                    Type::Label(label, value_ty.into()),
                )
            }
            Expr::Unlabel(value, label) => {
                let value_var = self.fresh_ty_var();
                let expected_ty = Type::Label(label, Type::Unifier(value_var).into());
                let out = self.check(env, value, expected_ty);
                (
                    out.with_typed_ast(|ast| ast.update(Expr::Unlabel(ast, label))),
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
                let typed_ast = ast.update(Expr::Concat(left_out.typed_ast, right_out.typed_ast));
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
                    out.with_typed_ast(|ast| ast.update(Expr::Project(dir, ast))),
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
                (GenOut::new(constraints, ast.update(typed_ast)), out_ty)
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
                    out.with_typed_ast(|ast| ast.update(Expr::Inject(dir, ast))),
                    // Our goal row is the type of our output
                    out_ty,
                )
            }
            Expr::Item(item_id, kind) => {
                let ty_scheme = self.item_source.type_of_item(item_id);

                // Create fresh unifiers for each type and row variable in our type scheme.
                let mut wrapper_tyvars = vec![];
                let tyvar_to_unifiers = ty_scheme
                    .unbound_types
                    .iter()
                    .map(|ty_var| {
                        let unifier = self.fresh_ty_var();
                        wrapper_tyvars.push(Type::Unifier(unifier));
                        (*ty_var, unifier)
                    })
                    .collect::<FxHashMap<_, _>>();
                let mut wrapper_rowvars = vec![];
                let rowvar_to_unifiers = ty_scheme
                    .unbound_rows
                    .iter()
                    .map(|row_var| {
                        let unifier = self.fresh_row_var();
                        wrapper_rowvars.push(Row::Unifier(unifier));
                        (*row_var, unifier)
                    })
                    .collect::<FxHashMap<_, _>>();

                // Instantiate our scheme mapping it's variables to the fresh unifiers we just generated.
                // After this we'll have a list of constraints and a type that only reference the fresh
                // unfiers.
                let (constraints, ty) =
                    Instantiate::new(ast.1, &tyvar_to_unifiers, &rowvar_to_unifiers)
                        .type_scheme(ty_scheme);
                let wrapper = ItemWrapper {
                    types: wrapper_tyvars,
                    rows: wrapper_rowvars,
                    evidence: constraints
                        .clone()
                        .into_iter()
                        .filter_map(|c| match c {
                            Constraint::RowCombine(row_combo) => Some(Evidence::RowEquation {
                                left: row_combo.left,
                                right: row_combo.right,
                                goal: row_combo.goal,
                            }),
                            _ => None,
                        })
                        .collect(),
                };
                self.item_wrappers.insert(ast.1, wrapper);
                (
                    GenOut::new(constraints, ast.update(Expr::Item(item_id, kind))),
                    ty,
                )
            }
            _ => todo!(),
        }
    }
}
