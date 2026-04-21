use internment::Intern;
use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::{
    passes::frontend::typing::{
        Constraint, Evidence, GenOut, ItemWrapper, Provenance, Row, Solver, Type, Typed,
        inst::Instantiate,
    },
    resource::rep::{
        common::Spanned,
        frontend::ast::{Direction, Expr, Untyped},
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
                GenOut::new(vec![], ast.convert(Expr::Number(n)), env),
                ast.convert(Type::Num),
            ),

            // Expr::Number(n) => (
            //                 GenOut::new(vec![], ast.convert(Expr::Number(n))),
            //                 ast.convert(Type::Num),
            //             ),
            Expr::String(s) => (
                GenOut::new(vec![], ast.convert(Expr::String(s)), env),
                ast.convert(Type::String),
            ),
            Expr::Bool(v) => (
                GenOut::new(vec![], ast.convert(Expr::Bool(v)), env),
                ast.convert(Type::Bool),
            ),
            Expr::Particle(p) => (
                GenOut::new(vec![], ast.convert(Expr::Particle(p)), env),
                ast.convert(Type::Particle(p)),
            ),

            Expr::Unit => (
                GenOut::new(vec![], ast.convert(Expr::Unit), env),
                ast.convert(Type::Unit),
            ),
            Expr::Ident(v) => {
                // dbg!(v.0.0);
                // dbg!(env.keys().collect::<Vec<_>>());
                if let Some(ty) = env.get(&v.0.0).copied() {
                    // dbg!(ty.1, v.0.1);
                    (
                        GenOut::new(vec![], ast.convert(Expr::Ident(Typed(v, ty))), env),
                        ty,
                    )
                } else {
                    panic!("Undefined ident {}", v)
                }
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
                let (mut fun_out, supposed_fun_ty) = self.infer(env.clone(), fun);

                let (arg_ty, ret_ty) = if let Type::Func(arg, ret) = *supposed_fun_ty.0 {
                    (arg, ret)
                } else {
                    let arg_tyvar = self.fresh_ty_var();
                    let arg_ty = arg.convert(Type::Unifier(arg_tyvar));
                    let ret_tyvar = self.fresh_ty_var();
                    let ret_ty = arg.convert(Type::Unifier(ret_tyvar));
                    fun_out.constraints.push(Constraint::TypeEqual(
                        Provenance::AppExpectedFun(fun_id),
                        supposed_fun_ty,
                        fun_out.typed_ast.convert(Type::Func(arg_ty, ret_ty)),
                    ));

                    (arg_ty, ret_ty)
                };

                let mut arg_out = self.check(env, arg, arg_ty);
                arg_out.typed_ast = ast.convert(Expr::Call(fun_out.typed_ast, arg_out.typed_ast));
                let merge =
                    fun_out.merge_with([arg_out], |fun, [arg]| ast.convert(Expr::Call(fun, arg)));
                (merge, ret_ty)
            }
            Expr::Hole(v) => {
                let var = self.fresh_ty_var();
                let t = ast.convert(Type::Unifier(var));
                (
                    GenOut::new(vec![], ast.convert(Expr::Hole(Typed(v, t))), env),
                    t,
                )
            }

            Expr::If(c, t, o) => {
                let (cond_out, cond_infer) = self.infer(env.clone(), c);

                let (then_out, then_infer) = self.infer(env.clone(), t);

                let (other_out, other_infer) = self.infer(env, o);

                let mut merge_out = cond_out
                    .merge_with([then_out, other_out], |cond, [then, other]| {
                        ast.convert(Expr::If(cond, then, other))
                    });

                merge_out.constraints.push(Constraint::TypeEqual(
                    Provenance::ConditionIsBool(c.1),
                    cond_infer,
                    cond_infer.convert(Type::Bool),
                ));

                merge_out.constraints.push(Constraint::TypeEqual(
                    Provenance::ExpectedUnify(t.1, o.1),
                    then_infer,
                    other_infer,
                ));

                (merge_out, then_infer)
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
            Expr::Add(l, r) => {
                let num_ty = ast.convert(Type::Num);
                let left_out = self.check(env.clone(), l, num_ty);
                let right_out = self.check(env, r, num_ty);
                let merge_out = left_out.merge_with([right_out], |left, [right]| {
                    ast.convert(Expr::Add(left, right))
                });

                (merge_out, num_ty)
            }

            Expr::Sub(l, r) => {
                let num_ty = ast.convert(Type::Num);
                let left_out = self.check(env.clone(), l, num_ty);
                let right_out = self.check(env, r, num_ty);
                let merge_out = left_out.merge_with([right_out], |left, [right]| {
                    ast.convert(Expr::Sub(left, right))
                });

                (merge_out, num_ty)
            }

            Expr::Mul(l, r) => {
                let num_ty = ast.convert(Type::Num);
                let left_out = self.check(env.clone(), l, num_ty);
                let right_out = self.check(env, r, num_ty);
                let merge_out = left_out.merge_with([right_out], |left, [right]| {
                    ast.convert(Expr::Mul(left, right))
                });

                (merge_out, num_ty)
            }

            Expr::Div(l, r) => {
                let num_ty = ast.convert(Type::Num);
                let left_out = self.check(env.clone(), l, num_ty);
                let right_out = self.check(env, r, num_ty);
                let merge_out = left_out.merge_with([right_out], |left, [right]| {
                    ast.convert(Expr::Div(left, right))
                });

                (merge_out, num_ty)
            }

            Expr::Comparison(left, op, right) => {
                let (l_out, l_ty) = self.infer(env.clone(), left);
                let (r_out, r_ty) = self.infer(env, right);

                let mut merge_out = l_out.merge_with([r_out], |left, [right]| {
                    ast.convert(Expr::Comparison(left, op, right))
                });
                merge_out.constraints.push(Constraint::TypeEqual(
                    Provenance::ExpectedUnify(left.1, right.1),
                    l_ty,
                    r_ty,
                ));

                (merge_out, ast.convert(Type::Bool))
            }

            // Expr::ProductConstructor { fields } => {
            // todo!()
            // }
            Expr::Label(label, value) => {
                let value_var = self.fresh_ty_var();
                let value_ty = value.convert(Type::Unifier(value_var));
                let env = env.update(label.0.0, value_ty);
                let out = self.check(env, value, value_ty);
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

                let mut merge_out = left_out.merge_with([right_out], |left, [right]| {
                    ast.convert(Expr::Concat(left, right))
                });

                let out_ty = ast.convert(Type::Prod(row_comb.goal));
                merge_out.constraints.push(Constraint::RowCombine(
                    // Provenance::ExpectedCombine(id),
                    Provenance::ExpectedCombine(left.1, right.1),
                    row_comb,
                ));
                self.tables.row_to_combo.insert(id, row_comb);
                (merge_out, out_ty)
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

                let mut merge_out = left_out.merge_with([right_out], |left, [right]| {
                    ast.convert(Expr::Branch(left, right))
                });

                let out_ty = ast.convert(Type::Func(ast.convert(Type::Sum(row_comb.goal)), ret_ty));

                // let mut constraints = left_out.constraints;
                merge_out.constraints.push(Constraint::RowCombine(
                    Provenance::ExpectedCombine(left.1, right.1),
                    row_comb,
                ));
                // dbg!(ast.1, left.1, right.1);
                self.tables.row_to_combo.insert(id, row_comb);
                self.tables.branch_to_ret_ty.insert(id, ret_ty);

                (merge_out, out_ty)
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
                    GenOut::new(constraints, ast.convert(Expr::Item(item_id)), env),
                    ty,
                )
            }

            // Expr::Access(base, field) => {}
            // Expr::Access(l, r) => {
            //     let ret_var = self.fresh_row_var();
            //     let (out, ty) = self.infer(env.clone(), l);

            //     let row: Row = ty.0.to_row();
            //     // let row = self.normalize_row(ty.convert(row)).0;
            //     let path = path_to_field(row.field_index(r), row.len_fields());
            //     let projections = apply_field_path(l, &path, r);
            //     self.infer(env, ast.convert(*projections.0))
            // }
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
