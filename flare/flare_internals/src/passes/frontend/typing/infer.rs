use internment::Intern;
use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::{
    passes::frontend::typing::{
        Constraint, Evidence, GenOut, ItemWrapper, Provenance, Row, Solver, Type, Typed,
        inst::Instantiate,
    },
    resource::rep::{
        common::Spanned,
        frontend::ast::{BinOp, Direction, Expr, Untyped},
    },
};

impl Solver<'_> {
    pub fn infer(
        &mut self,
        env: im::HashMap<Intern<String>, Spanned<Intern<Type>>, FxBuildHasher>,
        ast: Spanned<Intern<Expr<Untyped>>>,
    ) -> (GenOut, Spanned<Intern<Type>>) {
        // dbg!(&env);
        let id = ast.1;
        match *ast.0 {
            Expr::Number(n) => (
                GenOut::new(vec![], ast.convert(Expr::Number(n))),
                ast.convert(Type::Num),
            ),

            // Expr::Number(n) => (
            //                 GenOut::new(vec![], ast.convert(Expr::Number(n))),
            //                 ast.convert(Type::Num),
            //             ),
            Expr::String(s) => (
                GenOut::new(vec![], ast.convert(Expr::String(s))),
                ast.convert(Type::String),
            ),
            Expr::Bool(v) => (
                GenOut::new(vec![], ast.convert(Expr::Bool(v))),
                ast.convert(Type::Bool),
            ),
            Expr::Particle(p) => (
                GenOut::new(vec![], ast.convert(Expr::Particle(p))),
                ast.convert(Type::Particle(p)),
            ),

            Expr::Unit => (
                GenOut::new(vec![], ast.convert(Expr::Unit)),
                ast.convert(Type::Unit),
            ),
            Expr::Ident(v) => {
                // dbg!(v.0.0);
                // dbg!(env.keys().collect::<Vec<_>>());
                let ty = &env[&v.0.0];
                // dbg!(ty.1, v.0.1);
                (
                    GenOut::new(vec![], ast.convert(Expr::Ident(Typed(v, *ty)))),
                    *ty,
                )
            }
            Expr::Lambda(arg, body) => {
                let arg_tyvar = self.fresh_ty_var();
                let arg_ty = arg.0.convert(Type::Unifier(arg_tyvar));
                let env = env.update(arg.0.0, arg_ty);

                let (body_out, body_ty) = self.infer(env, body);
                (
                    GenOut {
                        typed_ast: ast
                            .convert(Expr::Lambda(Typed(arg, arg_ty), body_out.typed_ast)),
                        ..body_out
                    },
                    ast.convert(Type::Func(arg_ty, body_ty)),
                )
            }

            Expr::Call(fun, arg) => {
                // let (arg_out, arg_ty) = self.infer(env.clone(), arg);
                // let ret_ty: Spanned<Intern<Type>> = fun.convert(Type::Unifier(self.fresh_ty_var()));
                // let fun_ty = ast.convert(Type::Func(arg_ty, ret_ty));
                // let mut fun_out = self.check(env, fun, fun_ty);
                // fun_out.constraints.extend(arg_out.constraints);
                // (
                //     fun_out.with_typed_ast(|fun_ast| {
                //         ast.convert(Expr::Call(fun_ast, arg_out.typed_ast))
                //     }),
                //     ret_ty,
                // )
                let fun_id = fun.id();
                let (fun_out, supposed_fun_ty) = self.infer(env.clone(), fun);
                let mut constraint = fun_out.constraints;
                let (arg_ty, ret_ty) = if let Type::Func(arg, ret) = *supposed_fun_ty.0 {
                    (arg, ret)
                } else {
                    let arg_tyvar = self.fresh_ty_var();
                    let arg_ty = arg.convert(Type::Unifier(arg_tyvar));
                    let ret_tyvar = self.fresh_ty_var();
                    let ret_ty = arg.convert(Type::Unifier(ret_tyvar));
                    constraint.push(Constraint::TypeEqual(
                        Provenance::AppExpectedFun(fun_id),
                        supposed_fun_ty,
                        fun_out.typed_ast.convert(Type::Func(arg_ty, ret_ty)),
                    ));

                    (arg_ty, ret_ty)
                };

                let arg_out = self.check(env, arg, arg_ty);
                constraint.extend(arg_out.constraints);
                (
                    GenOut::new(
                        constraint,
                        ast.convert(Expr::Call(fun_out.typed_ast, arg_out.typed_ast)),
                    ),
                    ret_ty,
                )
            }
            Expr::Hole(v) => {
                let var = self.fresh_ty_var();
                let t = ast.convert(Type::Unifier(var));
                (GenOut::new(vec![], ast.convert(Expr::Hole(Typed(v, t)))), t)
            }

            Expr::If(c, t, o) => {
                let (cond_out, cond_infer) = self.infer(env.clone(), c);
                let mut constraints = cond_out.constraints;

                let (then_out, then_infer) = self.infer(env.clone(), t);
                constraints.extend(then_out.constraints);

                let (other_out, other_infer) = self.infer(env, o);
                constraints.extend(other_out.constraints);

                constraints.push(Constraint::TypeEqual(
                    Provenance::ConditionIsBool(c.1),
                    cond_infer,
                    cond_infer.convert(Type::Bool),
                ));

                constraints.push(Constraint::TypeEqual(
                    Provenance::ExpectedUnify(t.1, o.1),
                    then_infer,
                    other_infer,
                ));

                (
                    GenOut::new(
                        constraints,
                        ast.convert(Expr::If(
                            cond_out.typed_ast,
                            then_out.typed_ast,
                            other_out.typed_ast,
                        )),
                    ),
                    then_infer,
                )
            }

            Expr::Let(name, def, body) => {
                let (mut def_out, def_ty) = self.infer(env.clone(), def);
                let env = env.update(name.0.0, def_ty);
                let (body_out, body_ty) = self.infer(env, body);
                def_out.constraints.extend(body_out.constraints);
                (
                    def_out.with_typed_ast(|def| {
                        ast.convert(Expr::Let(Typed(name, def_ty), def, body_out.typed_ast))
                    }),
                    body_ty,
                )
            }
            Expr::Bin(left, op, right) => match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                    let num_ty = ast.convert(Type::Num);
                    let left_out = self.check(env.clone(), left, num_ty);
                    let right_out = self.check(env, right, num_ty);

                    let mut constraints = left_out.constraints;
                    constraints.extend(right_out.constraints);

                    (
                        GenOut::new(
                            constraints,
                            ast.convert(Expr::Bin(left_out.typed_ast, op, right_out.typed_ast)),
                        ),
                        num_ty,
                    )
                }
                BinOp::Eq | BinOp::Neq | BinOp::Gt | BinOp::Lt | BinOp::Gte | BinOp::Lte => {
                    let (l_out, l_ty) = self.infer(env.clone(), left);
                    let (r_out, r_ty) = self.infer(env, right);

                    let mut constraints = vec![];
                    constraints.extend(l_out.constraints);
                    constraints.extend(r_out.constraints);

                    constraints.push(Constraint::TypeEqual(
                        Provenance::ExpectedUnify(left.1, right.1),
                        l_ty,
                        r_ty,
                    ));

                    (
                        GenOut::new(
                            constraints,
                            ast.convert(Expr::Bin(l_out.typed_ast, op, r_out.typed_ast)),
                        ),
                        ast.convert(Type::Bool),
                    )
                }
                _ => todo!(),
            },

            Expr::Label(label, value) => {
                let (out, value_ty) = self.infer(env, value);
                (
                    out.with_typed_ast(|ast| ast.convert(Expr::Label(label, ast))),
                    ast.convert(Type::Label(label, value_ty)),
                )
            }
            Expr::Unlabel(value, label) => {
                let value_var = self.fresh_ty_var();
                let value_ty = value.convert(Type::Unifier(value_var));
                let expected_ty = label.0.convert(Type::Label(label, value_ty));
                let out = self.check(env, value, expected_ty);
                (
                    out.with_typed_ast(|ast| ast.convert(Expr::Unlabel(ast, label))),
                    value_ty,
                )
            }

            Expr::Concat(left, right) => {
                let row_comb = self.fresh_row_combination(left.1, right.1, id);

                let left_out =
                    self.check(env.clone(), left, left.convert(Type::Prod(row_comb.left)));
                let right_out = self.check(env, right, right.convert(Type::Prod(row_comb.right)));

                let out_ty = ast.convert(Type::Prod(row_comb.goal));
                let mut constraints = left_out.constraints;
                constraints.extend(right_out.constraints);
                constraints.push(Constraint::RowCombine(
                    // Provenance::ExpectedCombine(id),
                    Provenance::ExpectedCombine(left.1, right.1),
                    row_comb,
                ));
                self.tables.row_to_combo.insert(id, row_comb);
                let typed_ast = ast.convert(Expr::Concat(left_out.typed_ast, right_out.typed_ast));
                (
                    GenOut {
                        constraints,
                        typed_ast,
                    },
                    out_ty,
                )
            }
            Expr::Project(dir, goal) => {
                let row_comb = self.fresh_row_combination(goal.1, goal.1, ast.1);
                let sub_row = match dir {
                    Direction::Left => row_comb.left,
                    Direction::Right => row_comb.right,
                };

                let mut out = self.check(env, goal, goal.convert(Type::Prod(row_comb.goal)));

                out.constraints.push(Constraint::RowCombine(
                    Provenance::ExpectedCombine(goal.1, sub_row.1),
                    row_comb,
                ));
                (
                    out.with_typed_ast(|ast| ast.convert(Expr::Project(dir, ast))),
                    ast.convert(Type::Prod(sub_row)),
                )
            }
            Expr::Branch(left, right) => {
                let row_comb = self.fresh_row_combination(left.1, right.1, ast.1);
                let ret_var = self.fresh_ty_var();
                let ret_ty = ast.convert(Type::Unifier(ret_var));
                let left_out = self.check(
                    env.clone(),
                    left,
                    left.convert(Type::Func(left.convert(Type::Sum(row_comb.left)), ret_ty)),
                );

                let right_out = self.check(
                    env,
                    right,
                    right.convert(Type::Func(right.convert(Type::Sum(row_comb.right)), ret_ty)),
                );

                let out_ty = ast.convert(Type::Func(ast.convert(Type::Sum(row_comb.goal)), ret_ty));

                let mut constraints = left_out.constraints;
                constraints.extend(right_out.constraints);
                constraints.push(Constraint::RowCombine(
                    Provenance::ExpectedCombine(left.1, right.1),
                    row_comb,
                ));
                // dbg!(ast.1, left.1, right.1);
                self.tables.row_to_combo.insert(id, row_comb);
                self.tables.branch_to_ret_ty.insert(id, ret_ty);
                let typed_ast = Expr::Branch(left_out.typed_ast, right_out.typed_ast);
                (GenOut::new(constraints, ast.convert(typed_ast)), out_ty)
            }
            Expr::Inject(dir, value) => {
                let row_comb = self.fresh_row_combination(value.1, value.1, ast.1);

                let sub_row = match dir {
                    Direction::Left => row_comb.left,
                    Direction::Right => row_comb.right,
                };

                let mut out = self.check(env, value, ast.convert(Type::Sum(sub_row)));
                out.constraints.push(Constraint::RowCombine(
                    Provenance::ExpectedCombine(value.1, sub_row.1),
                    row_comb,
                ));

                self.tables.row_to_combo.insert(id, row_comb);
                (
                    out.with_typed_ast(|nast| ast.convert(Expr::Inject(dir, nast))),
                    // Our goal row is the type of our output
                    ast.convert(Type::Sum(row_comb.goal)),
                )
            }
            Expr::Item(item_id) => {
                // dbg!(kind);
                let ty_scheme = self.item_source.type_of_item(item_id);

                // Create fresh unifiers for each type and row variable in our type scheme.
                let mut wrapper_tyvars: Vec<Spanned<Intern<Type>>> = vec![];
                let tyvar_to_unifiers = ty_scheme
                    .unbound_types
                    .iter()
                    .map(|ty_var| {
                        let unifier = self.fresh_ty_var();
                        wrapper_tyvars.push(ast.convert(Type::Unifier(unifier)));
                        (*ty_var, unifier)
                    })
                    .collect::<FxHashMap<_, _>>();
                let mut wrapper_rowvars = vec![];
                let rowvar_to_unifiers = ty_scheme
                    .unbound_rows
                    .iter()
                    .map(|row_var| {
                        let unifier = self.fresh_row_var();
                        wrapper_rowvars.push(ast.convert(Row::Unifier(unifier)));
                        (*row_var, unifier)
                    })
                    .collect::<FxHashMap<_, _>>();

                // Instantiate our scheme mapping it's variables to the fresh unifiers we just generated.
                // After this we'll have a list of constraints and a type that only reference the fresh
                // unfiers.
                let (constraints, ty) = Instantiate::new(&tyvar_to_unifiers, &rowvar_to_unifiers)
                    .type_scheme(ty_scheme);
                let wrapper = ItemWrapper {
                    types: wrapper_tyvars,
                    rows: wrapper_rowvars,
                    evidence: constraints
                        .clone()
                        .into_iter()
                        .filter_map(|c| match c {
                            Constraint::RowCombine(_, row_combo) => Some(
                                // row_combo.into_evidence(),
                                Evidence::RowEquation {
                                    left: row_combo.left,
                                    right: row_combo.right,
                                    goal: row_combo.goal,
                                },
                            ),
                            Constraint::TypeEqual(..) => None,
                        })
                        .collect(),
                };
                self.tables.item_wrappers.insert(id, wrapper);
                (
                    GenOut::new(constraints, ast.convert(Expr::Item(item_id))),
                    ty,
                )
            }
            Expr::Access(base, field) => {
                // dbg!(base.1, field.0.1, id);

                let field_ty = self.fresh_ty_var();
                let field_ty_spanned = field.0.convert(Type::Unifier(field_ty));
                let out = self.check(env, ast, field_ty_spanned); // check the whole Access node
                (out, field_ty_spanned)
            }
        }
    }
}
