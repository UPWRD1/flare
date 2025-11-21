mod rows;
mod types;


use rows::{Row, RowCombination, RowVar};
pub use types::{Type, TyUniVar};

use std::{collections::BTreeSet, hash::Hash, marker::PhantomData};

use ena::unify::{InPlaceUnificationTable};
use internment::Intern;

use rustc_hash::{FxHashMap};

use crate::{
    passes::midend::{environment::Environment},
    resource::{
        errors::{CompResult, CompilerErr, TypeErr},
        rep::{
            Spanned, ast::{Expr, NodeId, Untyped, Variable}, common::Ident,  entry::Item, quantifier::QualifierFragment
        },
    },
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Typed(pub Untyped, pub Type);

impl Variable for Typed {}

impl Ident for Typed {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        self.0.ident()
    }
}

// #[derive(Debug, Clone, Copy)]
enum Constraint {
    TypeEqual(Provenance, Type, Type),
    RowCombine(RowCombination),
}


#[derive(Debug, Clone, Copy)]
enum Provenance {
    // A non function type encountered a Fun ast node, causing a type mismatch.
    UnexpectedFun(NodeId),
    // An application has an ast node in function position that does not have a function type.
    AppExpectedFun(NodeId),
    // Constraint produced by subsumption.
    ExpectedUnify(NodeId),
}

#[derive(Debug, Clone, Copy)]
enum UnificationError {
    TypeNotEqual(Type, Type),
    InfiniteType(TyUniVar, Type),
}

impl Provenance {
    fn id(&self) -> NodeId {
        match self {
            Self::UnexpectedFun(node_id)
            | Self::AppExpectedFun(node_id)
            | Self::ExpectedUnify(node_id) => *node_id,
        }
    }
}

struct GenOut {
    constraints: Vec<Constraint>,
    typed_ast: Spanned<Intern<Expr<Typed>>>,
}

impl GenOut {
    fn new(constraints: Vec<Constraint>, typed_ast: Spanned<Intern<Expr<Typed>>>) -> Self {
        Self {
            constraints,
            typed_ast,
        }
    }

    fn with_typed_ast(
        self,
        f: impl FnOnce(Spanned<Intern<Expr<Typed>>>) -> Spanned<Intern<Expr<Typed>>>,
    ) -> Self {
        Self {
            constraints: self.constraints,
            typed_ast: f(self.typed_ast),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
struct TypeScheme {
    pub unbound: BTreeSet<TyUniVar>,
    pub ty: Type,
}

pub struct TypeInferOut {
    pub ast: Spanned<Intern<Expr<Typed>>>,
    pub scheme: TypeScheme,
    pub errors: FxHashMap<NodeId, CompilerErr>,
}

#[derive(Default)]
pub struct Solver<'env> {
    unification_table: InPlaceUnificationTable<TyUniVar>,
    // hasher: FxHasher,
    phantom: PhantomData<&'env Environment>,
    errors: FxHashMap<NodeId, CompilerErr>,
    row_unification_table: InPlaceUnificationTable<RowVar>,
    partial_row_combs: BTreeSet<RowCombination>,
    row_to_combo: FxHashMap<NodeId, RowCombination>,
    branch_to_ret_ty: FxHashMap<NodeId, Type>,
}

impl<'env> Solver<'env> {
    fn fresh_ty_var(&mut self) -> TyUniVar {
        self.unification_table.new_key(None)
    }

    fn fresh_row_var(&mut self) -> RowVar {
        self.row_unification_table.new_key(None)
    }

    fn fresh_row_combination(&mut self) -> RowCombination {
        RowCombination {
            left: Row::Open(self.fresh_row_var()),
            right: Row::Open(self.fresh_row_var()),
            goal: Row::Open(self.fresh_row_var()),
        }
    }

    fn infer(
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
                todo!()
            }
            _ => todo!(),
        }
    }

    pub fn check(
        &mut self,
        env: im::HashMap<Untyped, Type>,
        the_ast: Spanned<Intern<Expr<Untyped>>>,
        ty: Type,
    ) -> GenOut {
        let span = the_ast.1;
        match (*the_ast.0, ty) {
            (Expr::Number(n), Type::Num) => {
                GenOut::new(vec![], Spanned(Expr::Number(n).into(), span))
            }

            (Expr::String(s), Type::String) => {
                GenOut::new(vec![], Spanned(Expr::String(s).into(), span))
            }
            (Expr::Bool(b), Type::Bool) => {
                GenOut::new(vec![], Spanned(Expr::Bool(b).into(), span))
            }

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

            (Expr::Unit, Type::Unit) => GenOut::new(vec![], Spanned(Expr::Unit.into(), span)),
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

    fn normalize_ty(&mut self, ty: Type) -> Type {
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

    fn unification(&mut self, constraints: Vec<Constraint>) -> CompResult<()> {
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
                        };
                        self.errors.insert(node_id, mark.into());
                    }
                }
            }
        }

        Ok(())
    }

    fn substitute(&mut self, ty: Type) -> (BTreeSet<TyUniVar>, Type) {
        match ty {
            Type::Num => (BTreeSet::new(), Type::Num),
            Type::Unifier(v) => {
                let root = self.unification_table.find(v);
                match self.unification_table.probe_value(root) {
                    Some(ty) => self.substitute(ty),
                    None => {
                        let mut unbound = BTreeSet::new();
                        unbound.insert(root);
                        (unbound, Type::Unifier(root))
                    }
                }
            }
            Type::Func(arg, ret) => {
                let (mut arg_unbound, arg) = self.substitute(*arg);
                let (ret_unbound, ret) = self.substitute(*ret);
                arg_unbound.extend(ret_unbound);
                (arg_unbound, Type::Func(arg.into(), ret.into()))
            }
            _ => todo!(),
        }
    }

    fn substitute_ast(
        &mut self,
        ast: Spanned<Intern<Expr<Typed>>>,
    ) -> (BTreeSet<TyUniVar>, Spanned<Intern<Expr<Typed>>>) {
        let id = ast.1;
        match *ast.0 {
            Expr::Ident(v) => {
                let (unbound, ty) = self.substitute(v.1);
                (unbound, Spanned(Expr::Ident(Typed(v.0, ty)).into(), id))
            }
            Expr::Number(i) => (BTreeSet::new(), Spanned(Expr::Number(i).into(), id)),
            Expr::Hole(v) => {
                let (unbound, ty) = self.substitute(v.1);
                (unbound, Spanned(Expr::Hole(Typed(v.0, ty)).into(), id))
            }
            Expr::Lambda(arg, body, is_anon) => {
                let (mut unbound, ty) = self.substitute(arg.1);
                let arg = Typed(arg.0, ty);

                let (unbound_body, body) = self.substitute_ast(body);
                unbound.extend(unbound_body);

                (
                    unbound,
                    Spanned(Expr::Lambda(arg, body, is_anon).into(), id),
                )
            }
            Expr::Call(fun, arg) => {
                let (mut unbound_fun, fun) = self.substitute_ast(fun);
                let (unbound_arg, arg) = self.substitute_ast(arg);
                unbound_fun.extend(unbound_arg);
                (unbound_fun, Spanned(Expr::Call(fun, arg).into(), id))
            }
            _ => todo!(),
        }
    }
    pub fn type_infer(ast: Spanned<Intern<Expr<Untyped>>>) -> TypeInferOut {
        let mut ctx = Solver::default();

        // Constraint generation
        let (out, ty) = ctx.infer(im::HashMap::default(), ast);

        // Constraint solving
        ctx.unification(out.constraints);

        // Apply our substition to our inferred types
        let (mut unbound, ty) = ctx.substitute(ty);
        let (unbound_ast, typed_ast) = ctx.substitute_ast(out.typed_ast);
        unbound.extend(unbound_ast);

        // Return our typed ast and it's type scheme
        TypeInferOut {
            ast: typed_ast,
            scheme: TypeScheme { unbound, ty },
            errors: ctx.errors,
        }
    }
    /// Check a single item from the environment.
    fn check_item(
        &mut self,
        item: &'env Item,
        packctx: QualifierFragment,
    ) -> CompResult<&'env Item> {
        todo!()
    }
}
