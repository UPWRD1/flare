use std::{cell::RefCell, fmt, rc::Rc};

use chumsky::span::SimpleSpan;
//use ptrie::Trie;
//use token_trie::Trie;
//use radix_trie::{Trie, TrieCommon};

use crate::{
    passes::midend::environment::{Entry, Environment, SimpleQuant},
    resource::{
        errors::{CompResult, DynamicErr},
        rep::{Expr, FileID, PrimitiveType, Spanned, Ty},
    },
};

use log::info;

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
    User(&'static str),
    Unit,
    Num,
    Bool,
    String,
    Func(TyVar, TyVar),
    Tuple(TyVar, usize),
    //Func(Rc<TyInfo>, Box<TyInfo>),
}

impl fmt::Display for TyInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TyInfo::Unknown => write!(f, "Unknown"),
            TyInfo::Ref(n) => write!(f, "{n}"),
            TyInfo::User(n) => write!(f, "User({})", n),
            TyInfo::Num => write!(f, "num"),
            TyInfo::Bool => write!(f, "bool"),
            TyInfo::Func(l, r) => write!(f, "({l} -> {r})"),
            TyInfo::Unit => write!(f, "unit"),
            TyInfo::String => write!(f, "str"),
            TyInfo::Tuple(t, s) => write!(f, "{{{}; {}}}", t, s),
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
                crate::resource::rep::PrimitiveType::Num => write!(f, "num"),
                crate::resource::rep::PrimitiveType::Bool => write!(f, "bool"),
                crate::resource::rep::PrimitiveType::Str => write!(f, "str"),
                crate::resource::rep::PrimitiveType::Unit => write!(f, "unit"),
            },

            Ty::Tuple(t, s) => {
                write!(f, "{{")?;
                for i in t {
                    write!(f, "{}, ", i.0)?
                }
                write!(f, "}}")
            }

            Ty::Arrow(l, r) => write!(f, "({} -> {})", l.0, r.0),
            Ty::Generic(n) => write!(f, "Generic({})", n.0.get_ident().unwrap_or("?".to_string())),
            Ty::User(n, args) => {
                write!(f, "{}[", n.0.get_ident().unwrap_or("?".to_string()))?;
                for a in args {
                    write!(f, "{}, ", a.0)?
                }
                write!(f, "]")
            }
        }
    }
}

pub struct Solver<'env> {
    //src: &'src str,
    master_env: &'env Environment, /*Trie<SimpleQuant, Rc<RefCell<Entry>>>*/
    vars: Vec<(TyInfo, SimpleSpan<usize, FileID>)>,
    env: Vec<(Expr, TyVar)>,
    //current_parent: SimpleQuant,
}

impl<'env> Solver<'env> {
    pub fn new(
        master_env: &'env Environment, /*Trie<SimpleQuant, Rc<RefCell<Entry>>>*/
    ) -> Solver<'env> {
        Solver {
            //src,
            //current_parent: SimpleQuant::Root,
            master_env,
            vars: vec![],
            env: vec![],
        }
    }

    fn create_ty(&mut self, info: TyInfo, span: SimpleSpan<usize, FileID>) -> TyVar {
        self.vars.push((info, span));
        let v = TyVar(self.vars.len() - 1);
        info!("{} = {}", v, self.vars[v.0].0);
        v
    }

    fn convert_ty(&mut self, t: &Ty) -> TyInfo {
        match t {
            Ty::Primitive(primitive_type) => match primitive_type {
                PrimitiveType::Num => TyInfo::Num,
                PrimitiveType::Str => TyInfo::String,
                PrimitiveType::Bool => TyInfo::Bool,
                PrimitiveType::Unit => TyInfo::Unit,
            },
            Ty::Arrow(l, r) => {
                let lty = self.convert_ty(&l.0);
                let rty = self.convert_ty(&r.0);
                TyInfo::Func(
                    self.create_ty(lty, l.1),
                    self.create_ty(rty, r.1),
                )
            }
            Ty::User(n, g) => TyInfo::User(n.0.get_ident().unwrap_or("?".to_string()).leak()),
            _ => todo!("{:?}", t),
        }
    }

    fn search_masterenv(&self, q: SimpleQuant, s: SimpleSpan<usize, u64>) -> CompResult<&Rc<RefCell<Entry>>> {
        let search: Vec<(Vec<SimpleQuant>, &Rc<RefCell<Entry>>)> = self
            .master_env
            .items
            .postfix_search(vec![q.clone()])
            .collect();
        if let Some((_q, e)) = search.last() {
            Ok(e)
        } else {
            Err(DynamicErr::new(format!("'{q}' hasn't been defined"))
                .label((
                    format!("{:?} not found in scope", q),
                    s,
                ))
                //.src(self.src.to_string())
                .into())
        }
    }

    fn unify(&mut self, a: TyVar, b: TyVar, span: SimpleSpan<usize, FileID>) -> CompResult<TyInfo> {
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
            (Num, Num) | (Bool, Bool) | (String, String) | (Unit, Unit) => Ok(a_info.clone()),
            (Func(a_i, a_o), Func(b_i, b_o)) => {
                let _ = self.unify(b_i, a_i, span)?;
                let _ = self.unify(a_o, b_o, span)?;
                Ok(Func(a_i, a_o))
            }
            (User(a_name), User(b_name)) if a_name == b_name => Ok(User(a_name)),
            (a_info, b_info) => Err(DynamicErr::new(format!(
                "Type mismatch between {a_info} and {b_info}"
            ))
            .label((format!("expected '{b_info}' here, found '{a_info}'"), span))
            .extra_labels(vec![
                (format!("this is {a_info}"), self.vars[a.0].1),
                (format!("this is {b_info}"), self.vars[b.0].1),
            ])
            .into()),
        }
        .inspect(|x| {info!("    {} U {} => {}", a, b, x)})
    }

    pub fn check_expr(
        &mut self,
        expr: &Spanned<Expr>,
        //env: &mut Vec<(Expr, TyVar)>,
    ) -> CompResult<TyVar> {
        //dbg!(&self.current_parent);

        match &expr.0 {
            Expr::Unit => Ok(self.create_ty(TyInfo::Unit, expr.1)),
            Expr::Number(_) => Ok(self.create_ty(TyInfo::Num, expr.1)),
            Expr::String(_) => Ok(self.create_ty(TyInfo::String, expr.1)),
            Expr::Bool(_) => Ok(self.create_ty(TyInfo::Bool, expr.1)),
            Expr::Ident(name) => {
                //dbg!(&self.env);
                if let Some((_e, tv)) = self
                    .env
                    .iter()
                    .rev()
                    .find(|(n, _)| *n.get_ident().unwrap() == *name)
                {
                    Ok(*tv)
                } else {
                    self.resolve_name(expr, name)
                }
            }
            Expr::Let(lhs, ref rhs, ref then) => {
                let rhs_ty = self.check_expr(rhs)?;
                self.env.push((lhs.0.clone(), rhs_ty));
                let out_ty = self.check_expr(then)?;
                self.env.pop();
                Ok(out_ty)
            }
            Expr::Lambda(arg, body) => {
                let arg_ty = self.create_ty(TyInfo::Unknown, arg.1);
                self.env.push((arg.0.clone(), arg_ty));
                let body_ty = self.check_expr(body)?;
                self.env.pop();
                Ok(self.create_ty(TyInfo::Func(arg_ty, body_ty), expr.1))
            }
            Expr::Call(func, arg) => {
                
                //let backup_parent = self.current_parent.clone();
                //self.current_parent =SimpleQuant::Func(func.0.get_ident().unwrap_or("?".to_string()));
                let func_ty = self.check_expr(func)?;
                let arg_ty = self.check_expr(arg)?;
                let out_ty = self.create_ty(TyInfo::Unknown, expr.1);
                let func_req_ty = self.create_ty(TyInfo::Func(arg_ty, out_ty), func.1);
                self.unify( func_ty, func_req_ty, arg.1)?;
                //self.current_parent = backup_parent;
                Ok(out_ty)
            }
            Expr::Add(l, r) | Expr::Mul(l, r) | Expr::Sub(l, r) | Expr::Div(l, r) => {
                let out_ty = self.create_ty(TyInfo::Num, expr.1);
                let l_ty = self.check_expr(l)?;
                self.unify(l_ty, out_ty, l.1)?;
                let r_ty = self.check_expr(r)?;
                self.unify(r_ty, out_ty, r.1)?;
                Ok(out_ty)
            }
            Expr::Comparison(l, _op, r) => {
                let out_ty = self.create_ty(TyInfo::Bool, expr.1);
                let l_ty = self.check_expr(l)?;
                let r_ty = self.check_expr(r)?;
                self.unify(l_ty, r_ty, expr.1)?;
                Ok(out_ty)
            }

            Expr::If(cond, then, otherwise) => {
                let out_ty = self.create_ty(TyInfo::Bool, cond.1);
                let cond_ty = self.check_expr(cond)?;
                self.unify(cond_ty, out_ty, cond.1)?;
                let then_ty = self.check_expr(then)?;
                //dbg!(&then_ty);
                let else_ty = self.check_expr(otherwise)?;
                self.unify(then_ty, else_ty, otherwise.1)?;
                Ok(then_ty)
            }
            Expr::FieldAccess(l, r) => {
                let l_ty = self.check_expr(l)?;
                let solved = self.solve(l_ty)?;
                let search: Vec<(Vec<SimpleQuant>, &Rc<RefCell<Entry>>)> = self
                    .master_env
                    .items
                    .postfix_search(vec![SimpleQuant::Type(solved.get_user_name().unwrap())])
                    .collect();
                if let Entry::Struct { ref fields, .. } = *search.last().unwrap().1.borrow() {
                    let the_field = fields.iter().find(|(n, _)| n.0 == r.0).unwrap();
                    let expected_ty = self.convert_ty(&the_field.1.0);
                    let expected_ty_var = self.create_ty(
                        expected_ty,
                        the_field.1.1,
                    );
                    Ok(expected_ty_var)
                } else {
                    panic!("Should be a struct")
                }

                //todo!("{:?}.{:?}", l_ty, r_ty);
            }
            Expr::Tuple(es) => {
                let mut iter = es.iter();
                let first_ty = self.check_expr(iter.next().unwrap())?;
                let out_ty = self.create_ty(TyInfo::Tuple(first_ty, es.len()), expr.1);
                for element in iter {
                    let e_ty = self.check_expr(element)?;
                    self.unify(e_ty, first_ty, element.1)?;
                }
                Ok(out_ty)
            }
            Expr::FieldedConstructor(name, given_fields) => {
                let search: Vec<(Vec<SimpleQuant>, &Rc<RefCell<Entry>>)> = self
                    .master_env
                    .items
                    .postfix_search(vec![SimpleQuant::Type(
                        name.0.get_ident().unwrap().to_string(),
                    )])
                    .collect();
                //dbg!(&search);
                if let Some((_q, e)) = search.last() {
                    //todo!();

                    let mut sty = self.master_env.check_entry(e)?.borrow_mut();
                    //dbg!(&fty);
                    if let Entry::Struct {
                        ref name,
                        ref mut fields,
                        ref ty,
                        ref parent,
                    } = *sty
                    {
                        if fields.len() != given_fields.len() {
                            return Err(DynamicErr::new(format!(
                                "Field count mismatch: expected {}, found {}",
                                fields.len(),
                                given_fields.len()
                            ))
                            .label((format!("expected {} fields", fields.len()), name.1))
                            .label((format!("found {} fields", given_fields.len()), expr.1))
                            .into());
                        }
                        //let paired_fields = fields.iter().zip(given_fields.iter());
                        let map: Vec<(Spanned<Expr>, Spanned<Ty>)> =
                            fields.iter().cloned().map(|(n, t)| (n, t)).collect();
                        //dbg!(&map);
                        for (fname, value) in given_fields {
                            //dbg!(fname);
                            let def_ty: Spanned<Ty> = map.iter().filter(|x| x.0.0 == fname.0).last().ok_or(DynamicErr::new(format!(
                                "No such field '{}' in struct '{}'",
                                fname.0.get_ident().unwrap(),
                                name.0.get_ident().unwrap()
                            )).label((format!("{:?} doesn't exist", fname.0), fname.1)))?.1.clone();
                            let given_ty = self.check_expr(value)?;
                            let expected_ty = self.convert_ty(&def_ty.0);
                            let expected_ty_var = self.create_ty(
                                expected_ty,
                                def_ty.1,
                            );
                            self.unify(given_ty, expected_ty_var, value.1)?;
                        }
                        let out_ty = self.create_ty(
                            TyInfo::User(name.0.get_ident().unwrap().to_string().leak()),
                            name.1,
                        );
                        Ok(out_ty)
                    } else {
                        panic!("Should be a struct")
                    }

                    //e.get_sig()
                } else {
                    Err(DynamicErr::new(format!("No such struct '{name:?}'"))
                        .label((
                            format!("{:?} not found in scope", expr.0),
                            expr.1,
                        ))
                        //.src(self.src.to_string())
                        .into())
                }
            }
                        Expr::ExternFunc(name) => {
                let search: Vec<(Vec<SimpleQuant>, &Rc<RefCell<Entry>>)> = self
                    .master_env
                    .items
                    .postfix_search(vec![name.last().unwrap().clone()])
                    .collect();
                dbg!(&search);
                if let Some((_q, e)) = search.last() {
                    if let Entry::Extern { ref sig, .. } = *e.borrow() {
                        let converted = self.convert_ty(&sig);
                        let out_ty = self.create_ty(converted, expr.1);
                        Ok(out_ty)
                    } else {
                        panic!("Should be an extern")
                    }
                } else {
                    panic!("Should exist")
                    // Err(DynamicErr::new(format!("No such struct '{name:?}'"))
                    //     .filename("Type Error")
                    //     .label((
                    //         format!("{:?} not found in scope", *expr.value()),
                    //         *expr.span(),
                    //     ))
                    //     //.src(self.src.to_string())
                    //     .into())
                }
            }
            _ => todo!("Failed to check {:?}", expr.0),
        }
    }

    fn resolve_name(
        &mut self,
        expr: &Spanned<Expr>,
        name: &String,
    ) -> Result<TyVar, crate::resource::errors::CompilerErr> {
        //let e = self.search_masterenv(SimpleQuant::Func(name.to_string()))?;
        //dbg!(expr);
        //dbg!(&self.master_env);

        let search_func: Vec<(Vec<SimpleQuant>, &Rc<RefCell<Entry>>)> = self
            .master_env
            .items
            .postfix_search(vec![SimpleQuant::Func(name.to_string())])
            .collect();
        if let Some((_q, e)) = search_func.last() {

            // BEAUTIFUL!
            let fty = if let Ok(entry) = self.master_env.check_entry(e)?.try_borrow() {entry} else {
                let l = self.create_ty(TyInfo::Unknown, expr.1);
                let r = self.create_ty(TyInfo::Unknown, expr.1);
                return Ok(self.create_ty(TyInfo::Func(l, r), expr.1))
            };
            //dbg!(&fty);
            if let Entry::Let { ref sig, ref name, .. } = *fty {
                let (l, r) = sig.as_ref().unwrap().get_arrow();
                let converted_l = self.convert_ty(&l.0);
                let converted_r = self.convert_ty(&r.0);
                let lty = self.create_ty(converted_l, l.1);
                let rty = self.create_ty(converted_r, r.1);
                let fn_ty = self.create_ty(TyInfo::Func(lty, rty), expr.1);
                Ok(fn_ty)
            } else if let Entry::Extern{ref sig, ..} = *fty {
                let (l, r) = sig.get_arrow();
                let converted_l = self.convert_ty(&l.0);
                let converted_r = self.convert_ty(&r.0);
                let lty = self.create_ty(converted_l, l.1);
                let rty = self.create_ty(converted_r, r.1);
                let fn_ty = self.create_ty(TyInfo::Func(lty, rty), expr.1);
                Ok(fn_ty)
            } else {
                panic!("Should be a function")
            }

            //e.get_sig()
        } else {
            Err(DynamicErr::new(format!("'{name}' hasn't been defined"))
                .label((
                    format!("{:?} not found in scope", expr.0),
                    expr.1,
                ))
                //.src(self.src.to_string())
                .into())
        }
    }

    pub fn solve(&self, var: TyVar) -> CompResult<Ty> {
        match self.vars[var.0].0 {
            TyInfo::Unknown => {
                //panic!("cannot infer type {:?}, is Unknown", var)
                
                Err(
                    DynamicErr::new(format!("cannot infer type {:?}, is Unknown", var))
                        .label(("has unknown type".to_string(), self.vars[var.0].1))
                        .into(),
                )
            }
            TyInfo::Ref(var) => Ok(self.solve(var)?),
            TyInfo::Num => Ok(Ty::Primitive(PrimitiveType::Num)),
            TyInfo::Bool => Ok(Ty::Primitive(PrimitiveType::Bool)),
            TyInfo::String => Ok(Ty::Primitive(PrimitiveType::Str)),
            TyInfo::Unit => Ok(Ty::Primitive(PrimitiveType::Unit)),
            TyInfo::Func(i, o) => Ok(Ty::Arrow(
                Box::new((self.solve(i)?, self.vars[i.0].1)),
                Box::new((self.solve(o)?, self.vars[o.0].1)),
            )),
            TyInfo::User(n) => {
                let search: Vec<(Vec<SimpleQuant>, &Rc<RefCell<Entry>>)> = self
                    .master_env
                    .items
                    .postfix_search(vec![SimpleQuant::Type(n.to_string())])
                    .collect();
                if let Some((_q, e)) = search.last() {
                    if let Entry::Struct {
                        name: _,
                        parent: _,
                        ref fields,
                        ref ty,
                    } = *e.borrow()
                    {
                        Ok(ty.clone().unwrap())
                    } else {
                        panic!("Should be a struct")
                    }
                } else {
                    panic!("Should exist")
                }
            }
            TyInfo::Tuple(t, s) => {
                let inner_ty = self.solve(t)?;
                Ok(Ty::Tuple(vec![(inner_ty, self.vars[t.0].1)], s))
            }
        }
        .inspect(|t| {info!("Solved {} => {}", var, t)})
        // let _ = t
        //     .as_ref()
        //     .inspect(|t| println!("    Solved {} => {}", var, t));
    }
}
