use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use chumsky::{extra::Err, prelude::todo, span::SimpleSpan};
use trie_rs::map::Trie;
//use ptrie::Trie;
//use token_trie::Trie;
//use radix_trie::{Trie, TrieCommon};

use crate::{
    quantifier,
    root::{
        passes::midend::environment::{Entry, Environment, Quantifier, SimpleQuant},
        resource::{
            errors::{CompResult, CompilerErr, DynamicErr},
            rep::{Expr, OptSpanned, PrimitiveType, Spanned, Ty},
        },
    },
};

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct TyVar(usize);

impl fmt::Display for TyVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "tv{}", self.0)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum TyInfo {
    Unknown,
    Ref(TyVar),
    Unit,
    Num,
    Bool,
    String,
    Func(TyVar, TyVar),
    //Func(Rc<TyInfo>, Box<TyInfo>),
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

// impl From<Ty> for TyInfo {
//     fn from(value: Ty) -> Self {
//         match value {
//             Ty::Primitive(primitive_type) => match primitive_type {
//                 PrimitiveType::Num => TyInfo::Num,
//                 PrimitiveType::Str => TyInfo::String,
//                 PrimitiveType::Bool => TyInfo::Bool,
//                 PrimitiveType::Unit => TyInfo::Unit,
//             },
//             Ty::User(opt_spanned, opt_spanneds) => todo!(),
//             Ty::Tuple(opt_spanneds) => todo!(),
//             Ty::Arrow(l, r) => todo!(), //TyInfo::Func(l.t.into(), r.t.into()),
//             Ty::Generic(opt_spanned) => todo!(),
//         }
// }
//}

// #[derive(Debug, Hash, PartialEq, Eq, Clone)]
// pub enum Ty {
//     Num,
//     Bool,
//     Func(Box<Self>, Box<Self>),
//     String,
//     Unit,
// }

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ty::Primitive(p) => match p {
                crate::root::resource::rep::PrimitiveType::Num => write!(f, "Num"),
                crate::root::resource::rep::PrimitiveType::Bool => write!(f, "Bool"),
                crate::root::resource::rep::PrimitiveType::Str => write!(f, "String"),
                crate::root::resource::rep::PrimitiveType::Unit => write!(f, "Unit"),
            },

            Ty::Tuple(t) => {
                write!(f, "{{")?;
                for i in t {
                    write!(f, "{}, ", i.t)?
                }
                write!(f, "}}")
            }

            Ty::Arrow(l, r) => write!(f, "({} -> {})", l.t, r.t),
            Ty::Generic(n) => write!(f, "Generic({})", n.t.get_ident().unwrap_or("?".to_string())),
            Ty::User(n, args) => {
                write!(f, "{}[", n.t.get_ident().unwrap_or("?".to_string()))?;
                for a in args {
                    write!(f, "{}, ", a.t)?
                }
                write!(f, "]")
            }
        }
    }
}

pub struct Solver<'env> {
    //src: &'src str,
    master_env: &'env Environment, /*Trie<SimpleQuant, Rc<RefCell<Entry>>>*/
    vars: Vec<(TyInfo, SimpleSpan)>,
    env: Vec<(Expr, TyVar)>,
    //current_parent: Quantifier,
}

impl<'env> Solver<'env> {
    pub fn new(
        master_env: &'env Environment, /*Trie<SimpleQuant, Rc<RefCell<Entry>>>*/
    ) -> Solver<'env> {
        Solver {
            //src,
            master_env,
            vars: vec![],
            env: vec![],
        }
    }

    fn create_ty(&mut self, info: TyInfo, span: SimpleSpan) -> TyVar {
        self.vars.push((info, span));
        TyVar(self.vars.len() - 1)
    }

    fn convert_ty(&mut self, t: Ty) -> TyInfo {
        let info = match &t {
            Ty::Primitive(primitive_type) => match primitive_type {
                PrimitiveType::Num => TyInfo::Num,
                PrimitiveType::Str => TyInfo::String,
                PrimitiveType::Bool => TyInfo::Bool,
                PrimitiveType::Unit => TyInfo::Unit,
            },
            Ty::User(opt_spanned, opt_spanneds) => todo!(),
            Ty::Tuple(opt_spanneds) => todo!(),
            Ty::Arrow(l, r) => {
                let lty = self.convert_ty(l.t.clone());
                let rty = self.convert_ty(r.t.clone());
                TyInfo::Func(
                    self.create_ty(lty, l.span.unwrap_or(SimpleSpan::from(0..0))),
                    self.create_ty(rty, r.span.unwrap_or(SimpleSpan::from(0..0))),
                )
            }
            Ty::Generic(opt_spanned) => todo!(),
        };
        //println!("Converted {:?} => {:?}", t, info);
        info
    }

    fn unify(&mut self, a: TyVar, b: TyVar, span: SimpleSpan) -> CompResult<TyInfo> {
        use TyInfo::*;

        let (a_info, b_info) = (self.vars[a.0].0, self.vars[b.0].0);
        match (a_info, b_info) {
            (Unknown, _) => {
                self.vars[a.0].0 = Ref(b);
                Ok(Ref(b))
            }
            (_, Unknown) => {
                self.vars[b.0].0 = Ref(a);
                Ok(Ref(a))
            }
            (Ref(a1), _) => self.unify(a1, b, span),
            (_, Ref(b1)) => self.unify(a, b1, span),
            (Num, Num) | (Bool, Bool) | (String, String) | (Unit, Unit) => Ok(a_info),
            (Func(a_i, a_o), Func(b_i, b_o)) => {
                let _ = self.unify(b_i, a_i, span)?;
                let _ = self.unify(a_o, b_o, span)?;
                Ok(Func(a_i, a_o))
            }
            (a_info, b_info) => Err(DynamicErr::new(format!(
                "Type mismatch between {a_info} and {b_info}"
            ))
            .label(("mismatch occurred here".to_string(), span))
            .extra_labels(vec![
                (format!("{a_info}"), self.vars[a.0].1),
                (format!("{b_info}"), self.vars[b.0].1),
            ])
            .into()),
        }
    }

    pub fn check_expr(
        &mut self,
        expr: &Spanned<Expr>,
        //env: &mut Vec<(Expr, TyVar)>,
    ) -> CompResult<TyVar> {
        match &expr.0 {
            Expr::Unit => Ok(self.create_ty(TyInfo::Unit, expr.1)),
            Expr::Number(_) => Ok(self.create_ty(TyInfo::Num, expr.1)),
            Expr::String(_) => Ok(self.create_ty(TyInfo::String, expr.1)),
            Expr::Bool(_) => Ok(self.create_ty(TyInfo::Bool, expr.1)),
            Expr::Ident(name) => {
                //dbg!(&self.env);
                if let Some((e, tv)) = self
                    .env
                    .iter()
                    .rev()
                    .find(|(n, _)| *n.get_ident().unwrap() == name.to_string())
                {
                    Ok(*tv)
                } else {
                    //dbg!(&self.master_env);
                    let search: Vec<(Vec<SimpleQuant>, &Rc<RefCell<Entry>>)> = self
                        .master_env
                        .items
                        .postfix_search(&vec![SimpleQuant::Func(name.to_string())])
                        .collect();
                    //dbg!(&search);
                    if let Some((_q, e)) = search.last() {
                        //todo!();

                        let mut fty = self.master_env.check_entry(e)?.borrow_mut();
                        //dbg!(&fty);
                        if let Entry::Let { ref mut sig, .. } = *fty {
                            let (l, r) = sig.as_ref().unwrap().get_arrow();
                            let converted_l = self.convert_ty(l.t);
                            let converted_r = self.convert_ty(r.t);
                            let lty = self
                                .create_ty(converted_l, l.span.unwrap_or(SimpleSpan::from(0..0)));
                            let rty = self
                                .create_ty(converted_r, r.span.unwrap_or(SimpleSpan::from(0..0)));
                            let fn_ty = self.create_ty(TyInfo::Func(lty, rty), expr.1);
                            //dbg!("{:?}", e.clone())
                            Ok(fn_ty)
                        } else {
                            panic!("Should be a function")
                        }

                        //e.get_sig()
                    } else {
                        Err(DynamicErr::new(format!("No such symbol '{name}'"))
                            .filename("Type Error")
                            .label((format!("not found in scope"), expr.1))
                            //.src(self.src.to_string())
                            .into())
                    }
                }
            }
            Expr::Let(lhs, ref rhs, ref then) => {
                let rhs_ty = self.check_expr(&rhs)?;
                self.env.push((lhs.0.clone(), rhs_ty));
                let out_ty = self.check_expr(&then)?;
                self.env.pop();
                Ok(out_ty)
            }
            Expr::Lambda(arg, body) => {
                let arg_ty = self.create_ty(TyInfo::Unknown, arg.1);
                self.env.push((arg.0.clone(), arg_ty));
                let body_ty = self.check_expr(&body)?;
                self.env.pop();
                Ok(self.create_ty(TyInfo::Func(arg_ty, body_ty), expr.1))
            }
            Expr::Call(func, arg) => {
                let func_ty = self.check_expr(&func)?;
                let arg_ty = self.check_expr(&arg)?;
                let out_ty = self.create_ty(TyInfo::Unknown, expr.1);
                let func_req_ty = self.create_ty(TyInfo::Func(arg_ty, out_ty), func.1);
                self.unify(func_req_ty, func_ty, expr.1)?;
                Ok(out_ty)
            }
            Expr::Add(l, r) | Expr::Mul(l, r) | Expr::Sub(l, r) | Expr::Div(l, r) => {
                let out_ty = self.create_ty(TyInfo::Num, expr.1);
                let l_ty = self.check_expr(&l)?;
                self.unify(out_ty, l_ty, expr.1)?;
                let r_ty = self.check_expr(&r)?;
                self.unify(out_ty, r_ty, expr.1)?;
                Ok(out_ty)
            }

            Expr::FieldAccess(l, r) => {
                let l_ty = self.check_expr(&l)?;
                let r_ty = self.check_expr(&r)?;

                todo!("{:?}.{:?}", l_ty, r_ty);
            }
            _ => todo!("Failed to check {:?}", expr.0),
        }
    }

    pub fn solve(&self, var: TyVar) -> CompResult<Ty> {
        let t = match self.vars[var.0].0 {
            TyInfo::Unknown => {
                //panic!("cannot infer type {:?}, is Unknown", var)
                Err(
                    DynamicErr::new(format!("cannot infer type {:?}, is Unknown", var))
                        .label(("has unknown type".to_string(), self.vars[var.0].1))
                        .filename("typerror")
                        .src("")
                        .extra_labels(vec![])
                        //.src(self.src.to_string())
                        .into(),
                )
            }
            TyInfo::Ref(var) => Ok(self.solve(var)?),
            TyInfo::Num => Ok(Ty::Primitive(PrimitiveType::Num)),
            TyInfo::Bool => Ok(Ty::Primitive(PrimitiveType::Bool)),
            TyInfo::String => Ok(Ty::Primitive(PrimitiveType::Str)),
            TyInfo::Unit => Ok(Ty::Primitive(PrimitiveType::Unit)),
            TyInfo::Func(i, o) => Ok(Ty::Arrow(
                Box::new(OptSpanned::new(self.solve(i)?, None)),
                Box::new(OptSpanned::new(self.solve(o)?, None)),
            )),
        };
        // let _ = t
        //     .as_ref()
        //     .inspect(|t| println!("    Solved {} => {}", var, t));
        t
    }
}
