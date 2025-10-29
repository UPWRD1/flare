use std::cell::OnceCell;

use chumsky::span::SimpleSpan;

use crate::{
    passes::midend::environment::Environment,
    resource::{
        errors::{self, CompResult, DynamicErr, GeneralErr},
        rep::{
            ast::Expr,
            entry::{EnumEntry, Item, StructEntry},
            files::FileID,
            quantifier::SimpleQuant,
            types::{EnumVariant, PrimitiveType, Ty},
            Spanned,
        },
    },
};

use log::trace;

pub use checkable_types::{TyInfo, TyVar};
mod checkable_types {

    use std::{arch::naked_asm, cell::OnceCell, fmt};
    #[derive(Copy, Clone, Debug, PartialEq)]
    pub struct TyVar(pub usize);

    impl fmt::Display for TyVar {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "tv{}", self.0)
        }
    }

    #[derive(Copy, Clone, Debug)]
    pub enum TyInfo {
        Unknown,
        Ref(TyVar),
        User(&'static str, &'static [TyVar]),
        Variant(TyVar, &'static str),
        Unit,
        Num,
        Bool,
        String,
        Func(TyVar, TyVar),
        Tuple(TyVar, usize),
        Generic(&'static str),
        //tmpGeneric(&'static str),
        //Func(Rc<TyInfo>, Box<TyInfo>),
    }

    impl fmt::Display for TyInfo {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                TyInfo::Unknown => write!(f, "Unknown"),
                TyInfo::Ref(n) => write!(f, "{n}"),
                TyInfo::User(n, g) => write!(f, "{n}{g:?}"),
                TyInfo::Num => write!(f, "num"),
                TyInfo::Bool => write!(f, "bool"),
                TyInfo::Func(l, r) => write!(f, "({l} -> {r})"),
                TyInfo::Variant(l, r) => write!(f, "({l}.{r})"),

                TyInfo::Unit => write!(f, "unit"),
                TyInfo::String => write!(f, "str"),
                TyInfo::Tuple(t, s) => write!(f, "{{{t}; {s}}}"),
                TyInfo::Generic(n) => write!(f, "?{n}"),
            }
        }
    }

    impl TyInfo {
        pub fn get_user_name(&self) -> String {
            match self {
                TyInfo::User(n, _) => n.to_string(),
                TyInfo::Generic(n) => n.to_string(),
                _ => panic!("{self}"),
            }
        }
    }

    impl PartialEq for TyInfo {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (_, Self::Generic(_)) => true,

                (Self::Ref(l0), Self::Ref(r0)) => l0 == r0,
                (Self::User(l0, l1), Self::User(r0, r1)) => l0 == r0 && l1 == r1,
                (Self::Variant(l0, l1), Self::Variant(r0, r1)) => l0 == r0 && l1 == r1,
                (Self::Func(l0, l1), Self::Func(r0, r1)) => l0 == r0 && l1 == r1,
                (Self::Tuple(l0, l1), Self::Tuple(r0, r1)) => l0 == r0 && l1 == r1,
                _ => core::mem::discriminant(self) == core::mem::discriminant(other),
            }
        }
    }
}
pub struct Solver<'env> {
    //src: &'src str,
    master_env: &'env Environment, /*Trie<SimpleQuant, Rc<RefCell<Entry>>>*/
    vars: Vec<(TyInfo, SimpleSpan<usize, FileID>)>,
    env: Vec<(Expr, TyVar)>,
    package: SimpleQuant,
    //current_parent: SimpleQuant,
}

impl<'env> Solver<'env> {
    #[must_use]
    pub fn new(
        master_env: &'env Environment,
        package: SimpleQuant, /*Trie<SimpleQuant, Rc<RefCell<Entry>>>*/
    ) -> Solver<'env> {
        Solver {
            //src,
            package,
            //current_parent: SimpleQuant::Root,
            master_env,
            vars: vec![],
            env: vec![],
        }
    }

    fn search_masterenv(
        &self,
        q: &SimpleQuant,
        s: &SimpleSpan<usize, u64>,
    ) -> CompResult<&'env Item> {
        //trace!("searching env for {q}");
        let search = self.master_env.get_from_context(q, &self.package);
        if let Some(i) = search {
            Ok(self.master_env.graph.node_weight(i).unwrap())
        } else {
            Err(errors::not_defined(q, s))
        }
    }

    fn create_ty(&mut self, info: TyInfo, span: SimpleSpan<usize, FileID>) -> TyVar {
        self.vars.push((info, span));
        let v = TyVar(self.vars.len() - 1);
        trace!("{} = {}", v, self.render_ty(self.vars[v.0].0));
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
                TyInfo::Func(self.create_ty(lty, l.1), self.create_ty(rty, r.1))
            }
            Ty::User(ref n, ref g) => {
                let mut generics: Vec<TyVar> = vec![];
                for (name, s) in g {
                    let l = self.convert_ty(&name);
                    let lty = self.create_ty(l, *s);
                    // dbg!(info);
                    generics.push(lty)
                }

                TyInfo::User(
                    n.0.get_ident().unwrap_or("?".to_string()).leak(),
                    generics.leak(),
                )
            }

            //Ty::Variant(v) => TyInfo::User(v.get_name().leak()),
            Ty::Generic(n) => {
                //let unknown = self.create_ty(TyInfo::Unknown, n.1);
                //TyInfo::Unknown
                TyInfo::Generic(n.0.get_ident().unwrap().leak())
                // TyInfo::Generic(n.0.get_ident().unwrap_or"Generic".to_string()).leak())
            }
            _ => todo!("{:?}", t),
        }
    }

    fn convert_ty_with_generics(&mut self, t: &Ty, g: Vec<TyVar>) -> TyInfo {
        match t {
            Ty::User(ref n, ..) => {
                TyInfo::User(n.0.get_ident().unwrap_or("?".to_string()).leak(), g.leak())
            }
            _ => todo!("{:?}", t),
        }
    }

    fn render_ty(&self, t: TyInfo) -> String {
        match t {
            TyInfo::Unknown => "Unknown".to_string(),
            TyInfo::User(n, g) => {
                let mut accum = String::new();

                if !g.is_empty() {
                    let mut g = g.iter();
                    accum = self.render_ty(self.vars[g.next().unwrap().0].0);
                    for g in g {
                        let tr = self.render_ty(self.vars[g.0].0);
                        accum = format!("{accum}, {tr}")
                    }
                }
                format!("{n}[{accum}]")
            }
            TyInfo::Func(l, r) => {
                format!(
                    "({} -> {})",
                    self.render_ty(self.vars[l.0].0),
                    self.render_ty(self.vars[r.0].0)
                )
            }
            TyInfo::Ref(t) => format!("{t}:{}", self.render_ty(self.vars[t.0].0)),
            TyInfo::Generic(n) => format!("?{n}"),
            _ => format!("{}", t),
        }
    }

    fn unify(&mut self, a: TyVar, b: TyVar, span: SimpleSpan<usize, FileID>) -> CompResult<TyInfo> {
        use TyInfo::{Bool, Func, Generic, Num, Ref, String, Unit, Unknown, User};
        trace!("Unify {a} {b}");
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

            // Can only expect generics
            (Ref(a1), _) => self.unify(a1, b, span),
            (_, Ref(b1)) => self.unify(a, b1, span),
            (Num, Num) | (Bool, Bool) | (String, String) | (Unit, Unit) => Ok(a_info),
            (Func(a_i, a_o), Func(b_i, b_o)) => {
                let i = self.unify(b_i, a_i, span)?;
                let o = self.unify(a_o, b_o, span)?;
                dbg!(o);
                Ok(Func(b_i, b_o))
            }
            (User(a_name, a_g), User(b_name, b_g))
                if a_name == b_name && {
                    for (l, r) in a_g.iter().zip(b_g.iter()) {
                        self.unify(*l, *r, span)?;
                    }
                    true
                } =>
            {
                Ok(User(b_name, b_g))
            }
            (_, Generic(t)) => {
                //dbg!(a, t);
                self.vars[b.0].0 = Ref(a);
                Ok(Ref(a))
                //Ok(Ref(a))
            }
            (Generic(_), _) => {
                self.vars[a.0].0 = Ref(b);
                Ok(Ref(b))
            }

            (a_info, b_info) => {
                let a_info = self.render_ty(a_info);
                let b_info = self.render_ty(b_info);
                Err(
                    DynamicErr::new(format!("Type mismatch between {a_info} and {b_info}i"))
                        .label((format!("expected '{b_info}' here, found '{a_info}'"), span))
                        .extra_labels(vec![
                            (format!("this is {a_info}"), self.vars[a.0].1),
                            (format!("this is {b_info}"), self.vars[b.0].1),
                        ])
                        .into(),
                )
            }
        }
        .inspect(|x| {
            trace!(
                "    {a} U {b} => {}",
                self.render_ty(*x),
                //self.render_ty(self.vars[a.0].0),
                //self.render_ty(self.vars[b.0].0),
            )
        })
        .inspect_err(|x| trace!("!! Could not unify {a} {b}"))
    }

    pub fn check_expr(
        &mut self,
        expr: &'env Spanned<Expr>,
        //env: &mut Vec<(Expr, TyVar)>,
    ) -> CompResult<TyVar> {
        //dbg!(&self.current_parent);
        //trace!("checking {:?}\n", expr.0);
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
                    self.resolve_name(expr)
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
                dbg!(func_ty);
                let arg_ty = self.check_expr(arg)?;
                dbg!(arg_ty);
                let out_ty = self.create_ty(TyInfo::Unknown, expr.1);
                let func_req_ty = self.create_ty(TyInfo::Func(arg_ty, out_ty), func.1);
                //homelet t = self.unify(func_ty, func_req_ty, arg.1)?;
                let t = self.unify(func_req_ty, func_ty, arg.1)?;
                self.create_ty(t, expr.1);
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
                let left_ty = self.check_expr(l)?;
                let info = self.vars[left_ty.0].0;
                if !matches!(info, TyInfo::Unknown) && !matches!(info, TyInfo::Generic(_))
                // if let Ok(t) = self.solve(left_ty)
                {
                    let ident = SimpleQuant::Type(info.get_user_name());
                    let fields = self
                        .master_env
                        .get_children(&ident, &self.package)
                        .ok_or(errors::not_defined(&ident, &expr.1))?;
                    let desired_field_q = SimpleQuant::Field(r.0.get_ident().unwrap());
                    let f = fields.iter().find(|x| x.0.is(&desired_field_q)).unwrap();
                    //dbg!(&f);
                    let fty = self.convert_ty(&f.1.get_ty().unwrap().0);
                    let converted = self.create_ty(fty, expr.1);

                    //dbg!(fields);
                    Ok(converted)
                } else {
                    let path = SimpleQuant::from_expr(expr);

                    //dbg!(&path);
                    let [_heads @ .., second, last] = path.as_slice() else {
                        panic!()
                    };
                    let entry = self
                        .master_env
                        .get_node(second, &self.package)
                        .ok_or(errors::not_defined(last, &expr.1))?;
                    let entry_ty = entry.get_ty().unwrap().0;
                    let tyinfo = self.convert_ty(&entry_ty);
                    let tv = self.create_ty(tyinfo, expr.1);
                    Ok(tv)
                }

                //todo!("{:?} {:?}", l_ty, solved_l);
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
                let ident = SimpleQuant::Type(name.0.get_ident().unwrap().to_string());

                let (i, fields) = self
                    .master_env
                    .get_node_and_children(&ident, &self.package)
                    .ok_or(errors::not_defined(&ident, &name.1))?;

                let sty = self.master_env.check_item(i, &self.package)?;
                //dbg!(&fty);
                if let Item::Struct(StructEntry { ref ty }) = *sty {
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
                    let map: Vec<&(Spanned<Expr>, Spanned<Ty>)> = fields
                        .iter()
                        .map(move |f| {
                            if let Item::Field(v) = f.1 {
                                v
                            } else {
                                panic!()
                            }
                        })
                        .collect();

                    for (fname, value) in given_fields {
                        //dbg!(fname);
                        let def_ty: Spanned<Ty> = map
                            .iter()
                            .filter(|x| x.0 .0 == fname.0)
                            .next_back()
                            .ok_or(
                                DynamicErr::new(format!(
                                    "No such field '{}' in struct '{}'",
                                    fname.0.get_ident().unwrap(),
                                    name.0.get_ident().unwrap()
                                ))
                                .label((format!("{:?} doesn't exist", fname.0), fname.1)),
                            )?
                            .1
                            .clone();
                        let given_ty = self.check_expr(value)?;
                        let expected_ty = self.convert_ty(&def_ty.0);
                        let expected_ty_var = self.create_ty(expected_ty, def_ty.1);
                        self.unify(given_ty, expected_ty_var, value.1)?;
                    }
                    let struct_tyinfo = self.convert_ty(i.get_ty().map(|x| x.0).as_ref().unwrap());

                    let out_ty = self.create_ty(struct_tyinfo, name.1);
                    Ok(out_ty)
                } else {
                    panic!("Should be a struct")
                }
                //todo!()
                //e.get_sig()
            }
            Expr::ExternFunc(name) => {
                let e = self.search_masterenv(&name.last().unwrap().clone(), &expr.1)?;
                if let Item::Extern { ref sig, .. } = *e {
                    let converted = self.convert_ty(&sig.0);
                    let out_ty = self.create_ty(converted, expr.1);
                    Ok(out_ty)
                } else {
                    panic!("Should be an extern")
                }
            }
            Expr::Constructor(name, given_fields) => {
                //let e = self.check_expr(name)?;
                let ident = name.0.get_ident().unwrap().to_string();
                let quant = SimpleQuant::Type(ident.clone());

                // Enum Variants
                let (the_enum, variants) = self
                    .master_env
                    .get_node_and_children(&quant, &self.package)
                    .ok_or(errors::not_defined(&quant, &name.1))?;

                //dbg!(&variants);
                let variant = SimpleQuant::from_expr(name).last().cloned().unwrap();
                let item = variants
                    .into_iter()
                    .filter(|x| x.0.is(&variant))
                    .map(|x| x.1)
                    .next()
                    .unwrap();

                // Variant Fields
                if let Item::Variant((EnumVariant { name, types }, _)) = item {
                    let fields = types;
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
                    let mut generics: Vec<TyVar> = vec![];
                    for (value, def_ty) in given_fields.iter().zip(fields.iter()) {
                        //dbg!(fname);

                        let given_ty = self.check_expr(value)?;
                        let expected_ty = self.convert_ty(&def_ty.0);
                        let expected_ty_var = self.create_ty(expected_ty, def_ty.1);
                        if matches!(expected_ty, TyInfo::Generic(_)) {
                            generics.push(given_ty)
                        }
                        self.unify(given_ty, expected_ty_var, value.1)?;
                    }

                    let enum_tyinfo = self.convert_ty_with_generics(
                        the_enum.get_ty().map(|x| x.0).as_ref().unwrap(),
                        generics,
                    );
                    let out_ty = self.create_ty(enum_tyinfo, name.1);
                    //self.unify(e.unwrap(), out_ty, expr.1)?;
                    Ok(out_ty)
                } else {
                    panic!()
                }
            }

            _ => todo!("Failed to check {:?}", expr.0),
        }
    }

    fn resolve_name(
        &mut self,
        expr: &Spanned<Expr>,
    ) -> Result<TyVar, crate::resource::errors::CompilerErr> {
        let name = expr.0.get_ident().unwrap();
        if let Ok(e) = self.search_masterenv(&SimpleQuant::Func(name.to_string()), &expr.1) {
            // BEAUTIFUL!
            let fty = if self
                .master_env
                .check_item(e, &self.package)?
                .get_sig()
                .is_some()
            {
                e
            } else {
                let l = self.create_ty(TyInfo::Unknown, expr.1);
                let r = self.create_ty(TyInfo::Unknown, expr.1);
                return Ok(self.create_ty(TyInfo::Func(l, r), expr.1));
            };
            //dbg!(&fty);
            if let Item::Let { ref sig, .. } = *fty {
                //let (l, r) = sig.get().unwrap().0.get_arrow();

                // let converted_l = self.convert_ty(&l.0);
                // let converted_r = self.convert_ty(&r.0);
                // let lty = self.create_ty(converted_l, l.1);
                // let rty = self.create_ty(converted_r, r.1);
                let info = self.convert_ty(&sig.get().unwrap().0);
                let fn_ty = self.create_ty(info, expr.1);
                Ok(fn_ty)
            } else if let Item::Extern { ref sig, .. } = *fty {
                let info = self.convert_ty(&sig.0);
                let fn_ty = self.create_ty(info, expr.1);
                Ok(fn_ty)
            } else {
                unreachable!("Should be a function")
            }
        } else if let Ok(e) = self.search_masterenv(&SimpleQuant::Type(name.to_string()), &expr.1) {
            // BEAUTIFUL!
            let ty = self
                .master_env
                .check_item(e, &self.package)?
                .get_ty()
                .unwrap();
            let info = self.convert_ty(&ty.0);
            Ok(self.create_ty(info, ty.1))
        } else {
            Err(errors::not_defined(&SimpleQuant::Wildcard(name), &expr.1))
        }

        //e.get_sig()
        // } else {
        //     Err(DynamicErr::new(format!("'{name}' hasn't been defined"))
        //         .label((format!("{:?} not found in scope", expr.0), expr.1))
        //         //.src(self.src.to_string())
        //         .into())
        // }
    }

    pub fn solve(&self, var: TyVar) -> CompResult<Spanned<Ty>> {
        let span = self.vars[var.0].1;
        match self.vars[var.0].0 {
            TyInfo::Unknown => {
                //panic!("cannot infer type {:?}, is Unknown", var)

                Err(
                    DynamicErr::new(format!("cannot infer type {var:?}, is Unknown"))
                        .label(("has unknown type".to_string(), self.vars[var.0].1))
                        .into(),
                )
            }
            TyInfo::Ref(var) => Ok(self.solve(var)?),
            TyInfo::Num => Ok((Ty::Primitive(PrimitiveType::Num), span)),
            TyInfo::Bool => Ok((Ty::Primitive(PrimitiveType::Bool), span)),
            TyInfo::String => Ok((Ty::Primitive(PrimitiveType::Str), span)),
            TyInfo::Unit => Ok((Ty::Primitive(PrimitiveType::Unit), span)),
            TyInfo::Func(i, o) => Ok((
                Ty::Arrow(
                    Box::new((self.solve(i)?.0, self.vars[i.0].1)),
                    Box::new((self.solve(o)?.0, self.vars[o.0].1)),
                ),
                span,
            )),
            TyInfo::User(n, g) => {
                let e =
                    self.search_masterenv(&SimpleQuant::Type(n.to_string()), &self.vars[var.0].1)?;
                let mut generics = vec![];
                for gen in g {
                    generics.push(self.solve(*gen)?);
                }
                //dbg!(&generics);

                if let Item::Struct(StructEntry { ref ty, .. }) = *e {
                    Ok(ty.clone())
                } else if let Item::Enum(EnumEntry { ref ty, .. }) = *e {
                    Ok(ty.clone())
                } else {
                    Err(
                        DynamicErr::new(format!("{} should be a type", self.vars[var.0].0))
                            .label((
                                format!("{:?} is not a type", self.vars[var.0].0),
                                self.vars[var.0].1,
                            ))
                            .into(),
                    )
                }
            }
            TyInfo::Variant(p, _) => self.solve(p),
            TyInfo::Tuple(t, s) => {
                let inner_ty = self.solve(t)?;
                Ok((Ty::Tuple(vec![(inner_ty)], s), span))
            }
            TyInfo::Generic(n) => Ok((
                Ty::Generic((Expr::Ident(n.to_string()), self.vars[var.0].1)),
                self.vars[var.0].1,
            )), //self.solve(t.unwrap()),
        }
        .inspect(move |t| trace!("Solved {} => {}", self.render_ty(self.vars[var.0].0), t.0))
        // let _ = t
        //     .as_ref()
        //     .inspect(|t| println!("    Solved {} => {}", var, t));
    }
}
