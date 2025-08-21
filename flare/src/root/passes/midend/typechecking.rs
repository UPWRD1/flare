use std::fmt;

use chumsky::span::SimpleSpan;

use crate::root::{passes::parser::{Expr, Spanned}, resource::errors::failure};


#[derive(Copy, Clone, Debug, PartialEq)]
struct TyVar(usize);

#[derive(Copy, Clone, Debug)]
enum TyInfo {
    Unknown,
    Ref(TyVar),
    Unit,
    Num,
    Bool,
    String,
    Func(TyVar, TyVar),
}

impl fmt::Display for TyInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TyInfo::Unknown => write!(f, "?"),
            TyInfo::Ref(_) => write!(f, "<ref>"),
            TyInfo::Num => write!(f, "Num"),
            TyInfo::Bool => write!(f, "Bool"),
            TyInfo::Func(_, _) => write!(f, "(_ -> _)"),
            TyInfo::Unit => write!(f, "Unit"),
            TyInfo::String => write!(f, "String"),
        }
    }
}

#[derive(Debug)]
enum Ty {
    Num,
    Bool,
    Func(Box<Self>, Box<Self>),
    String,
    Unit,
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ty::Num => write!(f, "Num"),
            Ty::Bool => write!(f, "Bool"),
            Ty::Func(x, y) => write!(f, "{x} -> {y}"),
            Ty::String => write!(f, "String"),
            Ty::Unit => write!(f, "Unit"),
        }
    }
}

struct Solver<'src> {
    src: &'src str,
    vars: Vec<(TyInfo, SimpleSpan)>,
}

impl Solver<'_> {
    fn create_ty(&mut self, info: TyInfo, span: SimpleSpan) -> TyVar {
        self.vars.push((info, span));
        TyVar(self.vars.len() - 1)
    }

    fn unify(&mut self, a: TyVar, b: TyVar, span: SimpleSpan) {
        match (self.vars[a.0].0, self.vars[b.0].0) {
            (TyInfo::Unknown, _) => self.vars[a.0].0 = TyInfo::Ref(b),
            (_, TyInfo::Unknown) => self.vars[b.0].0 = TyInfo::Ref(a),
            (TyInfo::Ref(a), _) => self.unify(a, b, span),
            (_, TyInfo::Ref(b)) => self.unify(a, b, span),
            (TyInfo::Num, TyInfo::Num) | (TyInfo::Bool, TyInfo::Bool) => {}
            (TyInfo::Func(a_i, a_o), TyInfo::Func(b_i, b_o)) => {
                self.unify(b_i, a_i, span); // Order swapped: function args are contravariant
                self.unify(a_o, b_o, span);
            }
            (a_info, b_info) => failure(
                format!("Type mismatch between {a_info} and {b_info}"),
                ("mismatch occurred here".to_string(), span),
                vec![
                    (format!("{a_info}"), self.vars[a.0].1),
                    (format!("{b_info}"), self.vars[b.0].1),
                ],
                self.src,
            ),
        }
    }

    fn check<'src>(
        &mut self,
        expr: &Spanned<Expr>,
        env: &mut Vec<(Expr, TyVar)>,
    ) -> TyVar {
        match &expr.0 {
            Expr::Unit => self.create_ty(TyInfo::Unit, expr.1),
            Expr::Number(_) => self.create_ty(TyInfo::Num, expr.1),
            Expr::String(_) => self.create_ty(TyInfo::String, expr.1),
            Expr::Bool(_) => self.create_ty(TyInfo::Bool, expr.1),
            Expr::Ident(name) => {
                env.iter()
                    .rev()
                    .find(|(n, _)| *n == Expr::Ident(name.to_string()))
                    .unwrap_or_else(|| {
                        failure(
                            format!("No such local '{name}'"),
                            ("not found in scope".to_string(), expr.1),
                            None,
                            self.src,
                        )
                    })
                    .1
            }
            Expr::Let(lhs, ref rhs, ref then ) => {
                let rhs_ty = self.check(&rhs, env);
                env.push((lhs.0.clone(), rhs_ty));
                let out_ty = self.check(&then, env);
                env.pop();
                out_ty
            }
            Expr::Lambda (arg, body ) => {
                let arg_ty = self.create_ty(TyInfo::Unknown, arg.1);
                env.push((arg.0.clone(), arg_ty));
                let body_ty = self.check(&body, env);
                env.pop();
                self.create_ty(TyInfo::Func(arg_ty, body_ty), expr.1)
            }
            Expr::Call (func, arg ) => {
                let func_ty = self.check(&func, env);
                let arg_ty = self.check(&arg, env);
                let out_ty = self.create_ty(TyInfo::Unknown, expr.1);
                let func_req_ty = self.create_ty(TyInfo::Func(arg_ty, out_ty), func.1);
                self.unify(func_req_ty, func_ty, expr.1);
                out_ty
            }
            Expr::Add(l, r) | Expr::Mul(l, r) => {
                let out_ty = self.create_ty(TyInfo::Num, expr.1);
                let l_ty = self.check(&l, env);
                self.unify(out_ty, l_ty, expr.1);
                let r_ty = self.check(&r, env);
                self.unify(out_ty, r_ty, expr.1);
                out_ty
            }
            _ => todo!()
        }
    }

    pub fn solve(&self, var: TyVar) -> Ty {
        match self.vars[var.0].0 {
            TyInfo::Unknown => failure(
                "Cannot infer type".to_string(),
                ("has unknown type".to_string(), self.vars[var.0].1),
                None,
                self.src,
            ),
            TyInfo::Ref(var) => self.solve(var),
            TyInfo::Num => Ty::Num,
            TyInfo::Bool => Ty::Bool,
            TyInfo::String => Ty::String,
            TyInfo::Unit => Ty::Unit,
            TyInfo::Func(i, o) => Ty::Func(Box::new(self.solve(i)), Box::new(self.solve(o))),
        }
    }
}