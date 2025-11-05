use std::hash::{Hash, Hasher};

use chumsky::span::SimpleSpan;
use itertools::Itertools;
use ordered_float::{Float, OrderedFloat};
use rustc_hash::FxHasher;

use crate::{
    passes::midend::environment::Environment,
    resource::{
        errors::{self, CompResult, DynamicErr},
        rep::{
            ast::{Expr, Pattern},
            entry::{FunctionItem, Item, StructEntry},
            files::FileID,
            quantifier::QualifierFragment,
            types::{EnumVariant, PrimitiveType, Ty},
            Spanned,
        },
    },
};

use log::trace;

pub use checkable_types::{TyInfo, TyVar};
mod checkable_types {

    use std::{fmt, hash::Hash};

    use ordered_float::OrderedFloat;
    #[derive(Copy, Clone, Debug, PartialEq, Hash)]
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
        Num(OrderedFloat<f64>),
        Bool,
        String,
        Func(TyVar, TyVar),
        Tuple(&'static [TyVar]),
        Seq(&'static TyVar),
        Generic(&'static str),
    }

    impl fmt::Display for TyInfo {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Self::Unknown => write!(f, "Unknown"),
                Self::Ref(n) => write!(f, "{n}"),
                Self::User(n, g) => write!(f, "{n}{g:?}"),
                Self::Num(_) => write!(f, "num"),
                Self::Bool => write!(f, "bool"),
                Self::Func(l, r) => write!(f, "({l} -> {r})"),
                Self::Variant(l, r) => write!(f, "({l}.{r})"),

                Self::Unit => write!(f, "unit"),
                Self::String => write!(f, "str"),
                Self::Tuple(t) => write!(f, "{{{t:?}}}"),
                Self::Seq(t) => write!(f, "Seq {t:?}"),
                Self::Generic(n) => write!(f, "?{n}"),
            }
        }
    }

    impl TyInfo {
        pub fn get_user_name(&self) -> Option<&'static str> {
            match self {
                Self::User(n, _) => Some(n),
                Self::Generic(n) => Some(n),
                _ => None,
            }
        }

        pub fn get_tuple_index(&self, idx: usize) -> Option<&TyVar> {
            match self {
                Self::Tuple(v) => v.get(idx),
                _ => None,
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
                (Self::Tuple(l), Self::Tuple(r)) => l == r,
                _ => core::mem::discriminant(self) == core::mem::discriminant(other),
            }
        }
    }

    impl Hash for TyInfo {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            match self {
                Self::Ref(r) => r.hash(state),
                Self::User(n, v) => {
                    n.hash(state);
                    v.hash(state);
                }
                Self::Variant(p, n) => {
                    p.hash(state);
                    n.hash(state);
                }
                Self::Func(l, r) => {
                    l.hash(state);
                    r.hash(state);
                }
                Self::Tuple(l) => {
                    l.hash(state);
                }
                _ => core::mem::discriminant(self).hash(state),
            }
        }
    }
}

pub struct Solver<'env> {
    //src: &'src str,
    master_env: &'env Environment, /*Trie<SimpleQuant, Rc<RefCell<Entry>>>*/
    vars: Vec<(TyInfo, SimpleSpan<usize, FileID>)>,
    env: Vec<(Spanned<Expr>, TyVar)>,
    package: &'env QualifierFragment,
    hasher: FxHasher,
}

impl<'env> Solver<'env> {
    #[must_use]
    pub fn new(
        master_env: &'env Environment,
        package: &'env QualifierFragment, /*Trie<SimpleQuant, Rc<RefCell<Entry>>>*/
    ) -> Self {
        Solver {
            //src,
            package,
            //current_parent: SimpleQuant::Root,
            master_env,
            vars: vec![],
            env: vec![],
            hasher: FxHasher::default(),
            // phantom: PhantomData,
        }
    }

    fn search_masterenv(
        &self,
        q: &QualifierFragment,
        s: &SimpleSpan<usize, u64>,
    ) -> CompResult<&'env Item> {
        //trace!("searching env for {q}");
        let search = self.master_env.get_from_context(q, self.package);
        if let Some(i) = search {
            Ok(self.master_env.graph.node_weight(i).unwrap())
        } else {
            Err(errors::not_defined(q, s))
        }
    }

    fn create_ty(&mut self, info: TyInfo, span: SimpleSpan<usize, FileID>) -> TyVar {
        let info = match info {
            TyInfo::Generic(n) => {
                if let Some((i, _v)) = self
                    .vars
                    .iter()
                    .enumerate()
                    .find(|(_i, x)| x.0.get_user_name() == Some(n))
                {
                    TyInfo::Ref(TyVar(i))
                    // v.0
                } else {
                    info
                }
            }
            _ => info,
        };
        self.vars.push((info, span));
        let v = TyVar(self.vars.len() - 1);
        trace!("{} = {}", v, self.render_ty(self.vars[v.0].0));
        v
    }

    fn new_anon_generic(&mut self, span: SimpleSpan<usize, FileID>) -> TyVar {
        let hasher = &mut self.hasher;
        self.vars.hash(hasher);
        self.create_ty(
            TyInfo::Generic(format!("{}", self.hasher.finish()).leak()),
            span,
        )
    }

    fn convert_ty(&mut self, t: &Ty) -> CompResult<TyInfo> {
        match t {
            Ty::Primitive(primitive_type) => match primitive_type {
                PrimitiveType::Num => Ok(TyInfo::Num(OrderedFloat::nan())),
                PrimitiveType::Str => Ok(TyInfo::String),
                PrimitiveType::Bool => Ok(TyInfo::Bool),
                PrimitiveType::Unit => Ok(TyInfo::Unit),
            },
            Ty::Arrow(l, r) => {
                let lty = self.convert_ty(&l.0)?;
                let rty = self.convert_ty(&r.0)?;
                Ok(TyInfo::Func(
                    self.create_ty(lty, l.1),
                    self.create_ty(rty, r.1),
                ))
            }
            Ty::User(ref n, g) => {
                let mut generics: Vec<TyVar> = vec![];
                for (name, s) in g.iter() {
                    let l = self.convert_ty(name);
                    let lty = self.create_ty(l?, *s);
                    // dbg!(info);
                    generics.push(lty)
                }

                Ok(TyInfo::User(
                    n.0.get_ident(n.1).unwrap_or("?"),
                    generics.leak(),
                ))
            }

            //Ty::Variant(v) => TyInfo::User(v.get_name().leak()),
            Ty::Generic(n) => {
                if let Some(v) = self.vars.iter().find(|x| {
                    let x = x.0.get_user_name();
                    x.is_some() && x == Some(n.0.get_ident(n.1).unwrap())
                }) {
                    Ok(v.0)
                } else {
                    Ok(TyInfo::Generic(n.0.get_ident(n.1)?))
                }
                //let unknown = self.create_ty(TyInfo::Unknown, n.1);
                //TyInfo::Unknown

                // TyInfo::Generic(n.0.get_ident().unwrap_or"Generic".to_string()).leak())
            }
            Ty::Variant(EnumVariant {
                parent_name,
                name,
                types,
            }) => {
                // let target = TyInfo::User parent_name.unwrap(), vec![].leak());
                let parent = self
                    .vars
                    .iter()
                    .find(|x| x.0.get_user_name() == *parent_name)
                    .unwrap();
                let parent_var = self.create_ty(parent.0, parent.1);
                Ok(TyInfo::Variant(parent_var, name.0.get_ident(name.1)?))
            }

            _ => todo!("{:?}", t),
        }
    }

    fn convert_ty_with_generics(&mut self, t: &Ty, g: Vec<TyVar>) -> TyInfo {
        match t {
            Ty::User(ref n, ..) => TyInfo::User(n.0.get_ident(n.1).unwrap_or("?"), g.leak()),
            _ => todo!("{:?}", t),
        }
    }

    fn render_ty(&self, t: TyInfo) -> String {
        match t {
            TyInfo::Unknown => "Unknown".to_string(),
            TyInfo::User(n, g) => {
                let accum: String = if !g.is_empty() {
                    g.iter()
                        .map(|x| self.render_ty(self.vars[x.0].0))
                        .collect::<Vec<String>>()
                        .join(", ")
                } else {
                    String::default()
                };
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
            TyInfo::Tuple(t) => {
                format!(
                    "{{{}}}",
                    t.iter()
                        .map(|x| self.render_ty(self.vars[x.0].0))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
                // c let mut accum = String::new;

                // for v in t.iter() {

                // }
            }
            _ => format!("{}", t),
        }
    }

    fn unify(&mut self, a: TyVar, b: TyVar, span: SimpleSpan<usize, FileID>) -> CompResult<TyInfo> {
        use TyInfo::{Bool, Func, Generic, Num, Ref, String, Unit, Unknown, User};
        //trace!("Unify {a} {b}");
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
            (Num(_), Num(_)) | (Bool, Bool) | (String, String) | (Unit, Unit) => Ok(a_info),
            (Func(a_i, a_o), Func(b_i, b_o)) => {
                self.unify(a_i, b_i, span)?;
                self.unify(a_o, b_o, span)?;
                //dbg!(&self.render_ty(a_info));
                //coresdbg!(o);
                Ok(Func(a_i, a_o))
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
            (_, Generic(_)) => {
                //dbg!(a, t);
                self.vars[b.0].0 = Ref(a);
                Ok(Ref(a))
                //Ok(Ref(a))
            }
            // (Generic(_), _) => {
            //     //self.vars[a.0].0 = Ref(b);
            //     Ok(Ref(b))
            // }
            (a_info, b_info) => {
                let a_info = self.render_ty(a_info);
                let b_info = self.render_ty(b_info);
                Err(
                    DynamicErr::new(format!("Type mismatch between {a_info} and {b_info}"))
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
        .inspect_err(|_| trace!("!! Could not unify {a} {b}"))
    }

    pub fn check_expr(
        &mut self,
        expr: &Spanned<Expr>,
        //env: &mut Vec<(Expr, TyVar)>,
    ) -> CompResult<TyVar> {
        //dbg!(&self.current_parent);
        //trace!("checking {:?}\n", expr.0);
        match &expr.0 {
            Expr::Unit => Ok(self.create_ty(TyInfo::Unit, expr.1)),
            Expr::Number(n) => Ok(self.create_ty(TyInfo::Num(*n), expr.1)),
            Expr::String(_) => Ok(self.create_ty(TyInfo::String, expr.1)),
            Expr::Bool(_) => Ok(self.create_ty(TyInfo::Bool, expr.1)),
            Expr::Ident(name) => {
                //dbg!(&self.env);
                if let Some((_e, tv)) = self
                    .env
                    .iter()
                    .rev()
                    .find(|(n, _)| n.0.get_ident(n.1).unwrap() == *name)
                {
                    Ok(*tv)
                } else {
                    self.resolve_name(expr)
                }
            }
            Expr::Let(lhs, rhs, then) => {
                let rhs_ty = self.check_expr(rhs)?;
                self.env.push((**lhs, rhs_ty));
                let out_ty = self.check_expr(then)?;
                self.env.pop();
                Ok(out_ty)
            }
            Expr::Lambda(arg, body) => {
                let arg_ty = self.new_anon_generic(arg.1);
                self.env.push((**arg, arg_ty));
                let body_ty = self.check_expr(body)?;
                self.env.pop();
                Ok(self.create_ty(TyInfo::Func(arg_ty, body_ty), expr.1))
            }
            Expr::Call(func, arg) => {
                let func_ty = self.check_expr(func)?;
                let arg_ty = self.check_expr(arg)?;
                let out_ty = self.create_ty(TyInfo::Unknown, expr.1);
                let func_req_ty = self.create_ty(TyInfo::Func(arg_ty, out_ty), func.1);

                let t = self.unify(func_req_ty, func_ty, arg.1)?;
                self.create_ty(t, expr.1);
                //self.current_parent = backup_parent;
                Ok(out_ty)
            }
            Expr::Add(l, r) | Expr::Mul(l, r) | Expr::Sub(l, r) | Expr::Div(l, r) => {
                let out_ty = self.create_ty(TyInfo::Num(OrderedFloat::nan()), expr.1);

                // let curry = self.create_ty(TyInfo::Func(out_ty, out_ty), r.1);
                // let implicit_func = self.create_ty(TyInfo::Func(out_ty, curry), l.1);

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
                let (info, span) = self.vars[left_ty.0];
                // dbg!(left_ty, r);
                if matches!(info, TyInfo::User(_, _))
                // if !matches!(info, TyInfo::Unknown) && !matches!(info, TyInfo::Generic(_))
                // if let Ok(t) = self.solve(left_ty)
                {
                    let ident = QualifierFragment::Type(info.get_user_name().unwrap());
                    let fields = self
                        .master_env
                        .get_children(&ident, self.package)
                        .ok_or(errors::not_defined(&ident, &expr.1))?;
                    let fields: Vec<_> = fields
                        .iter()
                        .filter(|x| matches!(x.0, QualifierFragment::Field(_)))
                        .collect();
                    let desired_field_q = QualifierFragment::Field(r.0.get_ident(r.1)?);
                    // dbg!(&desired_field_q);
                    // dbg!(&fields);
                    let f = fields
                        .iter()
                        .find(|x| x.0.is(&desired_field_q))
                        .ok_or(errors::not_defined(&desired_field_q, &r.1))?;
                    //dbg!(&f);
                    let fty = self.convert_ty(&f.1.get_ty().unwrap().0)?;
                    let converted = self.create_ty(fty, expr.1);

                    //dbg!(fields);
                    Ok(converted)
                } else if let TyInfo::Tuple(v) = info {
                    let r_ty = self.check_expr(r)?;
                    let num = self.create_ty(TyInfo::Num(OrderedFloat::nan()), r.1);
                    self.unify(r_ty, num, expr.1)?;
                    //dbg!(r_ty);
                    if let TyInfo::Num(n) = self.vars[r_ty.0].0 {
                        if Float::fract(n) != 0f64 {
                            Err(DynamicErr::new(
                                "Cannot index by a floating point number".to_string(),
                            )
                            .label(("this".to_string(), r.1))
                            .into())
                        } else {
                            Ok(*v.get(n.0 as usize).ok_or(
                                DynamicErr::new("Index out of range".to_string())
                                    .label((
                                        format!(
                                            "'{}' is out of range for a tuple with {} fields",
                                            n.0 as usize,
                                            v.len()
                                        ),
                                        r.1,
                                    ))
                                    .extra_labels(vec![(
                                        format!(
                                            "{} tuples use zero-based indexing",
                                            self.render_ty(info)
                                        ),
                                        span,
                                    )]),
                            )?)
                        }
                    } else {
                        Err(
                            DynamicErr::new("Cannot index a tuple by a non-numeric expression")
                                .label(("this is not Num", r.1))
                                .into(),
                        )
                        // Err()
                    }
                    // if matches!(r.0, Expr::Number(_))
                    // Ok(info.get_tuple_index(r.get_num_literal()?).unwrap())
                } else {
                    let path = QualifierFragment::from_expr(expr)?;

                    //dbg!(&path);
                    let [_heads @ .., second, last] = path.as_slice() else {
                        panic!()
                    };
                    let entry = self
                        .master_env
                        .get_node(second, self.package)
                        .ok_or(errors::not_defined(last, &expr.1))?;
                    let entry_ty = entry.get_ty().unwrap().0;
                    let tyinfo = self.convert_ty(&entry_ty)?;
                    let tv = self.create_ty(tyinfo, expr.1);
                    Ok(tv)
                }

                //todo!("{:?} {:?}", l_ty, solved_l);
            }
            Expr::Tuple(es) => {
                let iter = es.iter();
                // let first_ty = self.check_expr(iter.next().unwrap())?;
                // let out_ty = self.create_ty(TyInfo::Tuple(first_ty, es.len()), expr.1);
                // for element in iter {
                // let e_ty = self.check_expr(element)?;
                // self.unify(e_ty, first_ty, element.1)?;
                // }

                let mut vec = vec![];
                for x in iter {
                    let ty = self.check_expr(x)?;
                    vec.push(ty);
                }

                let out_ty = self.create_ty(TyInfo::Tuple(vec.leak()), expr.1);
                Ok(out_ty)
            }
            Expr::FieldedConstructor(name, given_fields) => {
                let ident = QualifierFragment::Type(name.0.get_ident(name.1).unwrap());

                let (i, fields) = self
                    .master_env
                    .get_node_and_children(&ident, self.package)
                    .ok_or(errors::not_defined(&ident, &name.1))?;

                let fields: Vec<_> = fields
                    .iter()
                    .filter(|x| matches!(x.0, QualifierFragment::Field(_)))
                    .collect();

                let sty = self.master_env.check_item(i, self.package)?;
                //dbg!(&fty);
                if let Item::Struct(StructEntry { .. }) = *sty {
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

                    for (fname, value) in given_fields.iter() {
                        //dbg!(fname);
                        let def_ty: Spanned<Ty> = map
                            .iter()
                            .filter(|x| x.0 .0 == fname.0)
                            .next_back()
                            .ok_or(
                                DynamicErr::new(format!(
                                    "No such field '{}' in struct '{}'",
                                    fname.0.get_ident(fname.1)?,
                                    name.0.get_ident(fname.1)?
                                ))
                                .label((format!("{:?} doesn't exist", fname.0), fname.1)),
                            )?
                            .1;
                        let given_ty = self.check_expr(value)?;
                        let expected_ty = self.convert_ty(&def_ty.0)?;
                        let expected_ty_var = self.create_ty(expected_ty, def_ty.1);
                        self.unify(given_ty, expected_ty_var, value.1)?;
                    }
                    let struct_tyinfo =
                        self.convert_ty(i.get_ty().map(|x| x.0).as_ref().unwrap())?;

                    let out_ty = self.create_ty(struct_tyinfo, name.1);
                    Ok(out_ty)
                } else {
                    panic!("Should be a struct")
                }
                //todo!()
                //e.get_sig()
            }
            Expr::ExternFunc(name) => {
                let e = self.search_masterenv(name.last().unwrap(), &expr.1)?;
                if let Item::Extern { ref sig, .. } = *e {
                    let converted = self.convert_ty(&sig.0);
                    let out_ty = self.create_ty(converted?, expr.1);
                    Ok(out_ty)
                } else {
                    panic!("Should be an extern")
                }
            }
            Expr::Constructor(name, given_fields) => {
                //let e = self.check_expr(name)?;
                let ident = name.0.get_ident(name.1)?;
                let quant = QualifierFragment::Type(ident);

                // Enum Variants
                let (the_enum, variants) = self
                    .master_env
                    .get_node_and_children(&quant, self.package)
                    .ok_or(errors::not_defined(&quant, &name.1))?;

                //dbg!(&variants);
                let vname = QualifierFragment::from_expr(name)?;
                let variant = vname.last().unwrap();
                let item = variants
                    .into_iter()
                    .filter(|x| x.0.is(variant))
                    .map(|x| x.1)
                    .next()
                    .unwrap();

                // Variant Fields
                if let Item::Variant((
                    EnumVariant {
                        parent_name,
                        name,
                        types,
                    },
                    _,
                )) = item
                {
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
                        let expected_ty = self.convert_ty(&def_ty.0)?;
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
            Expr::Match(matchee, patterns) => {
                let matchee_tyvar = self.check_expr(matchee)?;
                let matchee_realtype = self.solve(matchee_tyvar)?;
                let qfrag = QualifierFragment::Type(matchee_realtype.0.get_user_name().unwrap());
                let (item, children) = self
                    .master_env
                    .raw_get_node_and_children_indexes(&qfrag, self.package)
                    .ok_or(errors::not_defined(&qfrag, &expr.1))?;
                let unknown = self.create_ty(TyInfo::Unknown, patterns[0].1 .1);

                // insert the pattern bindings for each arm
                for (pattern, then) in patterns.iter() {
                    let mut accum: Vec<(Spanned<Expr>, TyVar)> = vec![];
                    struct PatternUpdater<'s, T> {
                        pub f: &'s dyn Fn(
                            &'s PatternUpdater<'s, T>,
                            &mut T,
                            Option<Spanned<Ty>>,
                            &mut Vec<(Spanned<Expr>, TyVar)>,
                            &Spanned<Pattern>,
                        ) -> CompResult<()>,
                    }
                    let mut pu = PatternUpdater {
                        f: &|rec: &'_ PatternUpdater<'_, _>,
                             self_: &mut Self,
                             context: Option<Spanned<Ty>>,
                             accum: &mut Vec<_>,
                             p: &Spanned<Pattern>| {
                            match p.0 {
                                Pattern::Atom(pattern_atom) => match pattern_atom {
                                    crate::resource::rep::ast::PatternAtom::Variable(v) => {
                                        let tyvar = if let Some(t) = context {
                                            let info = self_.convert_ty(&t.0)?;
                                            self_.create_ty(info, t.1)
                                        } else {
                                            self_.create_ty(TyInfo::Unknown, p.1)
                                        };
                                        // dbg!(tyvar);
                                        // Add the variable to the accumulator
                                        accum.push(((Expr::Ident(v), p.1), tyvar));
                                        // count += 1;
                                        Ok(())
                                    }
                                    _ => Ok(()),
                                },
                                Pattern::Tuple(items) => todo!(),
                                Pattern::Variant(n, fields) => {
                                    let vqfrag =
                                        QualifierFragment::Variant(n.0.get_ident(n.1).unwrap());
                                    // Check if the variant is valid
                                    if let Some(idx) = children.iter().find(|x| {
                                        self.master_env.value(**x).unwrap().name() == vqfrag.name()
                                    }) {
                                        if let Item::Variant((EnumVariant { types, .. }, s)) =
                                            self.master_env.value(*idx).unwrap()
                                        {
                                            for (pat_field, ty) in fields.iter().zip(types.iter()) {
                                                (rec.f)(rec, self_, Some(*ty), accum, pat_field)?;
                                            }
                                            Ok(())
                                        } else {
                                            panic!()
                                        }
                                    } else {
                                        Err(errors::not_defined(&vqfrag, &n.1))
                                    }
                                }
                            }
                        },
                    };
                    // Bind the variables in the pattern
                    (pu.f)(&mut pu, self, None, &mut accum, pattern)?;
                    let len = accum.len();
                    for var in accum {
                        self.env.push(var);
                    }
                    let then_ty = self.check_expr(then)?;
                    self.unify(then_ty, unknown, then.1)?;
                    // Remove the variables created during pattern matching
                    self.env.truncate(self.env.len() - len);
                }

                Ok(unknown)
                // todo!()
            }
            // Expr::MethodAccess(o, m) => {
            //     let obj_ident = o.0.get_ident(o.1)?;
            //     let obj_tyvar = self.check_expr(o)?;
            //     let obj_type = self.solve(obj_tyvar)?;
            //     let obj_qual = QualifierFragment::Type(obj_type.0.get_user_name().unwrap());
            //     let children = self
            //         .master_env
            //         .get_children(&obj_qual, self.package)
            //         .ok_or(errors::not_defined(&obj_qual, &o.1))?;
            //     // dbg!(&children);
            //     let target = QualifierFragment::Method(m.0.get_ident(m.1).unwrap());
            //     let (_, v) = children.iter().find(|x| matches!(x.0, target)).unwrap();
            //     let sig = v.get_sig().unwrap();
            //     let method_type = self.convert_ty(&sig.0);
            //     let method_tyvar = self.create_ty(method_type, sig.1);

            //     dbg!(m);
            //     let new_call = m.0.inject_call_start(**o, m.1);
            //     dbg!(new_call);

            //     self.env.push((new_call, method_tyvar));
            //     let res = self.check_expr(m)?;
            //     self.env.pop();
            //     Ok(res)

            //     // todo!("{:?}", v);
            // }
            _ => todo!("Failed to check {:?}", expr.0),
        }
    }

    fn resolve_name(
        &mut self,
        expr: &Spanned<Expr>,
    ) -> Result<TyVar, crate::resource::errors::CompilerErr> {
        let name = expr.0.get_ident(expr.1)?;
        if let Ok(e) = self.search_masterenv(&QualifierFragment::Func(name), &expr.1) {
            // BEAUTIFUL!
            let fty = if self
                .master_env
                .check_item(e, self.package)?
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
            if let Item::Function(FunctionItem { sig, .. }) = *fty {
                //let (l, r) = sig.get().unwrap().0.get_arrow();

                // let converted_l = self.convert_ty(&l.0);
                // let converted_r = self.convert_ty(&r.0);
                // let lty = self.create_ty(converted_l, l.1);
                // let rty = self.create_ty(converted_r, r.1);
                let info = self.convert_ty(&sig.get().unwrap().0)?;
                let fn_ty = self.create_ty(info, expr.1);
                Ok(fn_ty)
            } else if let Item::Extern { ref sig, .. } = *fty {
                let info = self.convert_ty(&sig.0)?;
                let fn_ty = self.create_ty(info, expr.1);
                Ok(fn_ty)
            } else {
                unreachable!("Should be a function")
            }
        } else if let Ok(e) = self.search_masterenv(&QualifierFragment::Type(name), &expr.1) {
            // BEAUTIFUL!
            let ty = self
                .master_env
                .check_item(e, self.package)?
                .get_ty()
                .unwrap();
            let info = self.convert_ty(&ty.0)?;
            Ok(self.create_ty(info, ty.1))
        } else {
            Err(errors::not_defined(
                &QualifierFragment::Wildcard(name),
                &expr.1,
            ))
        }

        //e.get_sig()
        // } else {
        //     Err(DynamicErr::new(format!("'{name}' hasn't been defined"))
        //         .label((format!("{:?} not found in scope", expr.0), expr.1))
        //         //.src(self.src.to_string())
        //         .into())
        // }
    }

    pub fn solve(&mut self, var: TyVar) -> CompResult<Spanned<Ty>> {
        let span = self.vars[var.0].1;
        match self.vars[var.0].0 {
            // TyInfo::Generic(n) => Err(DynamicErr::new(format!(
            //     "could not solve for ?{n}, check your types"
            // ))
            // .label(("this".to_string(), self.vars[var.0].1))
            // .into()),
            TyInfo::Unknown => {
                //panic!("cannot infer type {:?}, is Unknown", var)

                Err(
                    DynamicErr::new(format!("cannot infer type {var:?}, is Unknown"))
                        .label(("has unknown type".to_string(), self.vars[var.0].1))
                        .into(),
                )
            }
            TyInfo::Ref(var) => Ok(self.solve(var)?),
            TyInfo::Num(_) => Ok((Ty::Primitive(PrimitiveType::Num), span)),
            TyInfo::Bool => Ok((Ty::Primitive(PrimitiveType::Bool), span)),
            TyInfo::String => Ok((Ty::Primitive(PrimitiveType::Str), span)),
            TyInfo::Unit => Ok((Ty::Primitive(PrimitiveType::Unit), span)),
            TyInfo::Func(i, o) => Ok((
                Ty::Arrow(
                    Box::leak(Box::new((self.solve(i)?.0, self.vars[i.0].1))),
                    Box::leak(Box::new((self.solve(o)?.0, self.vars[o.0].1))),
                ),
                span,
            )),
            TyInfo::User(n, g) => {
                let e = self.search_masterenv(&QualifierFragment::Type(n), &self.vars[var.0].1)?;
                let mut generics = vec![];

                for gen in g {
                    generics.push(self.solve(*gen)?);
                }
                //dbg!(&generics);

                if let Some(t) = e.get_ty() {
                    let new = t.0.monomorph_user(generics.leak());
                    //dbg!(&new);
                    Ok((new, t.1))
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
            TyInfo::Tuple(t) => {
                let mut v = vec![];
                for t in t {
                    v.push(self.solve(*t)?);
                }

                Ok((Ty::Tuple(v.leak()), span))
            }
            TyInfo::Seq(t) => {
                let t = self.solve(*t)?;
                Ok((Ty::Seq(Box::leak(Box::new(t))), span))
            }
            TyInfo::Generic(n) => Ok((
                Ty::Generic((Expr::Ident(n), self.vars[var.0].1)),
                self.vars[var.0].1,
            )), //self.solve(t.unwrap()),
        }
        .inspect(|t| trace!("Solved {} => {}", self.render_ty(self.vars[var.0].0), t.0))
        // let _ = t
        //     .as_ref()
        //     .inspect(|t| println!("    Solved {} => {}", var, t));
    }
}
