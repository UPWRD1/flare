use core::unreachable;
use std::{
    hash::{Hash, Hasher},
    marker::PhantomData,
};

use chumsky::span::{SimpleSpan, Span};
use internment::Intern;
use ordered_float::{Float, OrderedFloat};
use petgraph::{
    dot::Config,
    graph::{EdgeReference, NodeIndex},
};
use rustc_hash::FxHasher;

use crate::{
    passes::midend::environment::Environment,
    resource::{
        errors::{self, CompResult, DynamicErr, FatalErr},
        rep::{
            ast::{Expr, Pattern, Untyped, Variable},
            common::{Ident, Named},
            entry::{FunctionItem, Item, ItemKind, StructEntry},
            files::FileID,
            quantifier::QualifierFragment,
            types::{EnumVariant, PrimitiveType, Ty},
            Spanned,
        },
    },
};

use log::{info, trace};

pub use checkable_types::{TyInfo, TyVar};
mod checkable_types {

    #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
    pub struct Typed(pub Untyped, pub TyVar);

    impl Variable for Typed {}

    impl Ident for Typed {
        fn ident(&self) -> crate::resource::errors::CompResult<Intern<String>> {
            self.0.ident()
        }
    }

    use std::{fmt, hash::Hash};

    use internment::Intern;
    use ordered_float::OrderedFloat;

    use crate::resource::rep::{
        ast::{Expr, Untyped, Variable},
        common::{Ident, Named},
        Spanned,
    };
    #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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
        User(Spanned<Intern<Expr>>, &'static [TyVar]),
        Variant(TyVar, Spanned<Intern<Expr>>),
        Unit,
        Num(OrderedFloat<f64>),
        Bool,
        String,
        Func(TyVar, TyVar),
        Tuple(&'static [TyVar]),
        Seq(&'static TyVar),
        Generic(Spanned<Intern<Expr>>),
        Package(Spanned<Intern<Expr>>),
    }

    impl Named for TyInfo {
        fn get_name(&self) -> Option<Spanned<Intern<Expr>>> {
            match self {
                Self::User(spanned, ty_vars) => Some(*spanned),
                Self::Variant(ty_var, spanned) => Some(*spanned),
                Self::Generic(spanned) => Some(*spanned),
                Self::Package(spanned) => Some(*spanned),
                _ => None,
            }
        }
    }

    impl TyInfo {
        #[must_use]
        pub const fn get_user_name(&self) -> Option<Spanned<Intern<Expr>>> {
            match self {
                Self::User(n, _) | Self::Generic(n) => Some(*n),
                _ => None,
            }
        }
        #[must_use]
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

#[derive(Debug, Clone)]
struct SolverEnv {
    vars: Vec<(TyInfo, SimpleSpan<usize, FileID>)>,
    env: Vec<(Spanned<Intern<Expr>>, TyVar)>,
    package: QualifierFragment,
    // phantom: PhantomData<&'env Ty>,
}

impl SolverEnv {
    fn env_push(&mut self, expr: Spanned<Intern<Expr>>, var: TyVar) {
        self.env.push((expr, var));
    }

    // fn env_pop(&mut self) {
    //     self.env.pop();
    // }

    fn convert_ty(&mut self, ty: &Ty) -> CompResult<TyInfo> {
        match ty {
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
                for Spanned(name, s) in g.iter() {
                    let l = self.convert_ty(name);
                    let lty = self.create_ty(l?, *s);
                    // dbg!(info);
                    generics.push(lty);
                }

                Ok(TyInfo::User(*n, generics.leak()))
            }

            Ty::Generic(n) => self
                .vars
                .iter()
                .find(|x| x.0.get_user_name().is_some_and(|x| x.0 == n.0))
                .map_or_else(|| Ok(TyInfo::Generic(*n)), |v| Ok(v.0)),
            Ty::Variant(EnumVariant {
                parent_name,
                name,
                types: _,
            }) => {
                // let target = TyInfo::User parent_name.unwrap(), vec![].leak());
                let parent = self
                    .vars
                    .iter()
                    .find(|x| {
                        x.0.get_user_name().is_some_and(|x| {
                            x.name()
                                .is_ok_and(|x| parent_name.is_some_and(|p| x.0 == p.0))
                        })
                    })
                    .ok_or_else(|| errors::not_defined(name, &name.1))?;
                let parent_var = self.create_ty(parent.0, parent.1);
                Ok(TyInfo::Variant(parent_var, *name))
            }

            Ty::Package(n) => Ok(TyInfo::Package(*n)),

            _ => todo!("{:?}", ty),
        }
    }

    // fn convert_ty_with_generics(&mut self, t: &Ty, g: Vec<TyVar>) -> TyInfo {
    //     match t {
    //         Ty::User(ref n, ..) => TyInfo::User(n.0, g.leak()),
    //         _ => todo!("{:?}", t),
    //     }
    // }

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

        trace!("{:?} = {:?}", info, self.vars.len() - 1);
        TyVar(self.vars.len() - 1)
    }
}

pub struct Solver<'env> {
    master_env: &'env Environment,
    local: SolverEnv,
    hasher: FxHasher,
    phantom: PhantomData<&'env Environment>,
}

impl<'env> Solver<'env> {
    #[must_use]
    pub const fn new(
        master_env: &'env Environment,
        package: QualifierFragment, /*Trie<SimpleQuant, Rc<RefCell<Entry>>>*/
    ) -> Self {
        let local = SolverEnv {
            vars: vec![],
            env: vec![],
            package,
            // phantom: PhantomData,
        };
        Solver {
            master_env,
            local,
            hasher: FxHasher::default(),
            phantom: PhantomData,
        }
    }

    fn unify(
        &mut self,
        a: TyVar,
        b: TyVar,
        span: SimpleSpan<usize, FileID>,
        // local: &mut SolverEnv<'src>,
    ) -> CompResult<TyInfo> {
        use TyInfo::{Bool, Func, Generic, Num, Ref, String, Unit, Unknown, User};
        //trace!("Unify {a} {b}");
        let (a_info, b_info) = (self.local.vars[a.0].0, self.local.vars[b.0].0);
        match (a_info, b_info) {
            (Unknown, _) => {
                self.local.vars[a.0].0 = Ref(b);
                Ok(Ref(b))
            }
            (_, Unknown) => {
                self.local.vars[b.0].0 = Ref(a);
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
                if a_name.0 == b_name.0 && {
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
                self.local.vars[b.0].0 = Ref(a);
                Ok(Ref(a))
                //Ok(Ref(a))
            }
            // (Generic(_), _) => {
            // self.local.vars[a.0].0 = Ref(b);
            // Ok(Ref(b))
            // }
            (a_info, b_info) => {
                let a_info = self.render_ty(a_info);
                let b_info = self.render_ty(b_info);
                Err(
                    DynamicErr::new(format!("Type mismatch between {a_info} and {b_info}"))
                        .label(format!("expected '{b_info}' here, found '{a_info}'"), span)
                        .extra_labels(vec![
                            (format!("this is {a_info}"), self.local.vars[a.0].1),
                            (format!("this is {b_info}"), self.local.vars[b.0].1),
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
            );
        })
        .inspect_err(|_| trace!("!! Could not unify {a} {b}"))
    }

    fn render_ty(&self, t: TyInfo) -> String {
        match t {
            TyInfo::Unknown => "Unknown".to_string(),
            TyInfo::User(n, g) => {
                let accum: String = if g.is_empty() {
                    String::default()
                } else {
                    g.iter()
                        .map(|x| self.render_ty(self.local.vars[x.0].0))
                        .collect::<Vec<String>>()
                        .join(", ")
                };
                format!(
                    "{}[{accum}]",
                    n.ident().expect("Could not get user type identifier")
                )
            }
            TyInfo::Func(l, r) => {
                format!(
                    "({} -> {})",
                    self.render_ty(self.local.vars[l.0].0),
                    self.render_ty(self.local.vars[r.0].0)
                )
            }
            TyInfo::Ref(t) => format!("{t}:{}", self.render_ty(self.local.vars[t.0].0)),
            TyInfo::Generic(n) => {
                format!("?{}", n.ident().expect("Could not get generic identifier"))
            }
            TyInfo::Tuple(t) => {
                format!(
                    "{{{}}}",
                    t.iter()
                        .map(|x| self.render_ty(self.local.vars[x.0].0))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            TyInfo::Variant(ty_var, n) => {
                format!(
                    "{}.{}",
                    self.render_ty(self.local.vars[ty_var.0].0),
                    n.ident().expect("Could not get variant identifier")
                )
            }
            TyInfo::Unit => "unit".to_string(),
            TyInfo::Num(ordered_float) => format!(
                "num{}",
                if ordered_float.is_nan() {
                    String::new()
                } else {
                    format!(" {ordered_float}")
                }
            ),
            TyInfo::Bool => "bool".to_string(),
            TyInfo::String => "str".to_string(),
            TyInfo::Seq(ty_var) => format!("Seq[{}]", self.render_ty(self.local.vars[ty_var.0].0)),
            TyInfo::Package(n) => format!(
                "Package {}",
                n.ident().expect("Could not get package identifier")
            ),
        }
    }

    /// Helper function to search the `master_env` for a quantifier in the `Solver's` package.
    /// TODO: Route all `master_env` queries through this function or similar.
    fn search_masterenv(
        &self,
        q: &QualifierFragment,
        s: &SimpleSpan<usize, u64>,
    ) -> CompResult<&'env Item>
// where
        // 'env: 'tc,
    {
        //trace!("searching env for {q}");
        let search = self.master_env.get_from_context(q, &self.local.package);
        search.map_or_else(
            |_| Err(errors::not_defined(q, s)),
            // SAFETY: This operation is safe because at this point, we KNOW the node exists within the graph.
            |i| Ok(unsafe { self.master_env.value(i).unwrap_unchecked() }),
        )
    }

    /// Create a new generic type with an autogenerated name
    fn new_anon_generic(
        &mut self,
        span: SimpleSpan<usize, FileID>,
        // locl: &mut SolverEnv<'src>,
    ) -> TyVar {
        self.local.vars.hash(&mut self.hasher);
        span.hash(&mut self.hasher);
        let name = Intern::from(format!("{}", self.hasher.finish()));
        self.local.create_ty(
            TyInfo::Generic(Spanned(Intern::from(Expr::Ident(name)), span)),
            span,
        )
    }

    /// Monomorph a user type with generics.
    fn convert_ty_with_generics(&self, t: &Ty, g: Vec<TyVar>) -> TyInfo {
        match t {
            Ty::User(ref n, ..) => TyInfo::User(*n, g.leak()),
            _ => todo!("{:?}", t),
        }
    }

    /// Check an expression and produce a `TyVar` to the type of that expression.
    fn check_expr(&mut self, expr: &Spanned<Intern<Expr>>) -> CompResult<TyVar> {
        // trace!("checking {:?}\n", expr.0);
        match &*expr.0 {
            Expr::Unit => Ok(self.local.create_ty(TyInfo::Unit, expr.1)),
            Expr::Number(n) => Ok(self.local.create_ty(TyInfo::Num(*n), expr.1)),
            Expr::String(_) => Ok(self.local.create_ty(TyInfo::String, expr.1)),
            Expr::Bool(_) => Ok(self.local.create_ty(TyInfo::Bool, expr.1)),
            Expr::Ident(name) => {
                //dbg!(&self.env);
                if let Some((_e, tv)) = self
                    .local
                    .env
                    .iter()
                    .rev()
                    .find(|(n, _)| n.ident().is_ok_and(|n| *n == **name))
                {
                    Ok(*tv)
                } else {
                    self.resolve_name(expr)
                }
            }
            Expr::Let(lhs, rhs, then) => {
                let rhs_ty = self.check_expr(rhs)?;
                self.local.env_push(*lhs, rhs_ty);
                let out_ty = self.check_expr(then)?;
                self.local.env.pop();
                Ok(out_ty)
            }
            Expr::Lambda(arg, body, is_anon) => {
                // anonymous lambdas need to infer their argument types from the local context,
                // therefore they use Unknown as their argument type..
                // lambdas created by implicit currying are a public interface,
                // they need to use generics, so they can be exported back into the masterenv
                // let arg_ty = if *is_anon {
                //     self.local.create_ty(TyInfo::Unknown, arg.1)
                // } else {
                //     self.new_anon_generic(arg.1)
                // };

                let arg_ty = self.local.create_ty(TyInfo::Unknown, arg.1);
                self.local.env_push(*arg, arg_ty);
                let body_ty = self.check_expr(body)?;
                self.local.env.pop();
                Ok(self.local.create_ty(TyInfo::Func(arg_ty, body_ty), expr.1))
            }
            Expr::Call(func, arg) => {
                let func_ty = self.check_expr(func)?;
                let arg_ty = self.check_expr(arg)?;
                let out_ty = self.local.create_ty(TyInfo::Unknown, expr.1);
                let func_req_ty = self.local.create_ty(TyInfo::Func(arg_ty, out_ty), func.1);

                let t = self.unify(func_req_ty, func_ty, arg.1)?;
                self.local.create_ty(t, expr.1);
                //self.current_parent = backup_parent;
                Ok(out_ty)
            }
            Expr::Add(l, r) | Expr::Mul(l, r) | Expr::Sub(l, r) | Expr::Div(l, r) => {
                let out_ty = self
                    .local
                    .create_ty(TyInfo::Num(OrderedFloat::nan()), expr.1);

                let l_ty = self.check_expr(l)?;
                self.unify(l_ty, out_ty, l.1)?;
                let r_ty = self.check_expr(r)?;
                self.unify(r_ty, out_ty, r.1)?;
                Ok(out_ty)
            }
            Expr::Comparison(l, _op, r) => {
                let out_ty = self.local.create_ty(TyInfo::Bool, expr.1);
                let l_ty = self.check_expr(l)?;
                let r_ty = self.check_expr(r)?;
                self.unify(l_ty, r_ty, expr.1)?;
                Ok(out_ty)
            }

            Expr::If(cond, then, otherwise) => {
                let out_ty = self.local.create_ty(TyInfo::Bool, cond.1);
                let cond_ty = self.check_expr(cond)?;
                self.unify(cond_ty, out_ty, cond.1)?;

                let then_ty = self.check_expr(then)?;
                let else_ty = self.check_expr(otherwise)?;
                self.unify(then_ty, else_ty, otherwise.1)?;
                Ok(then_ty)
            }
            Expr::FieldAccess(l, r) => self.check_expr_field_access(expr, l, r),
            Expr::Tuple(es) => {
                let vec: Result<Vec<_>, _> = es.iter().map(|x| self.check_expr(x)).collect();
                let vec = vec?;

                let out_ty = self.local.create_ty(TyInfo::Tuple(vec.leak()), expr.1);
                Ok(out_ty)
            }
            Expr::FieldedConstructor(name, given_fields) => {
                self.check_expr_fielded_constructor(expr, name, *given_fields)
            }
            Expr::ExternFunc(name) => {
                let e = self.search_masterenv(
                    name.last().expect("Could not get last element in name"),
                    &expr.1,
                )?;
                if let ItemKind::Extern { ref sig, .. } = e.kind {
                    let converted = self.local.convert_ty(&sig.0)?;
                    let out_ty = self.local.create_ty(converted, expr.1);
                    Ok(out_ty)
                } else {
                    // In theory, this should never occur, since
                    // Expr::ExternFunc is never constructed.
                    unreachable!()
                }
            }
            Expr::Constructor(name, given_fields) => {
                self.check_expr_constructor(expr, name, *given_fields)
            }
            Expr::Match(matchee, patterns) => {
                self.check_expr_match(expr, matchee, patterns)
                // todo!()
            }

            _ => todo!("Failed to check {:?}", expr.0),
        }
    }

    fn check_expr_match(
        &mut self,
        expr: &Spanned<Intern<Expr>>,
        matchee: &Spanned<Intern<Expr>>,
        patterns: &[(Spanned<Pattern>, Spanned<Intern<Expr>>)],
    ) -> Result<TyVar, errors::CompilerErr> {
        let matchee_tyvar = self.check_expr(matchee)?;
        // dbg!(matchee);
        let matchee_realtype = self.solve(matchee_tyvar)?;
        let qfrag = QualifierFragment::Type(matchee_realtype.name()?.ident()?);
        let (_, children) = self
            .master_env
            .raw_get_node_and_children_indexes(&qfrag, &self.local.package)
            .map_err(|_| errors::not_defined(qfrag, &expr.1))?;
        let unknown = self.local.create_ty(TyInfo::Unknown, patterns[0].1 .1);

        // insert the pattern bindings for each arm
        for (pattern, then) in patterns {
            let len = self.prepare_pat_context(*pattern, None, &children)?;

            // Bind the variables in the pattern
            let then_ty = self.check_expr(then)?;
            self.unify(then_ty, unknown, then.1)?;
            // Remove the variables created during pattern matching
            self.local.env.truncate(self.local.env.len() - len);
        }

        Ok(unknown)
    }

    fn check_expr_constructor(
        &mut self,
        expr: &Spanned<Intern<Expr>>,
        name: &Spanned<Intern<Expr>>,
        given_fields: Intern<Vec<Spanned<Intern<Expr>>>>,
    ) -> Result<TyVar, errors::CompilerErr> {
        //let e = self.check_expr(name)?;
        let ident = name.ident()?;

        let quant = QualifierFragment::Type(ident);

        // Enum Variants
        let (the_enum, variants) = self
            .master_env
            .get_node_and_children(&quant, &self.local.package)
            .map_err(|_| errors::not_defined(quant, &name.1))?;

        //dbg!(&variants);
        let vname = QualifierFragment::from_expr(name)?;
        let variant = vname
            .last()
            .expect("Could not get the last element in the path");
        let item = variants
            .into_iter()
            .filter(|x| x.0.is(variant))
            .map(|x| x.1)
            .next()
            .ok_or_else(|| errors::not_defined(variant, &name.1))?;

        // Variant Fields
        if let Item {
            kind:
                ItemKind::Variant(Spanned(
                    EnumVariant {
                        parent_name,
                        name,
                        types,
                    },
                    _,
                )),
            ..
        } = item
        {
            let fields = types;
            if fields.len() != given_fields.len() {
                let parent_name = parent_name.expect("Could not get parent name");
                return Err(DynamicErr::new(format!(
                    "'{}' variant '{}' expects {} fields, found {}",
                    parent_name.ident()?,
                    name.ident()?,
                    fields.len(),
                    given_fields.len()
                ))
                .label(format!("found {} fields", given_fields.len()), expr.1)
                .extra_labels(vec![
                    (format!("expected {} fields", fields.len()), name.1),
                    ("In this enum".to_string(), parent_name.1),
                ])
                .into());
            }
            let mut generics: Vec<TyVar> = vec![];
            for (value, def_ty) in given_fields.iter().zip(fields.iter()) {
                //dbg!(fname);

                let given_ty = self.check_expr(value)?;

                let expected_ty = self.local.convert_ty(&def_ty.0)?;
                let expected_ty_var = self.local.create_ty(expected_ty, def_ty.1);
                if matches!(expected_ty, TyInfo::Generic(_)) {
                    generics.push(given_ty);
                }
                self.unify(given_ty, expected_ty_var, value.1)?;
            }

            let enum_tyinfo =
                self.convert_ty_with_generics(the_enum.get_ty().map(|x| x.0)?.as_ref(), generics);
            let out_ty = self.local.create_ty(enum_tyinfo, name.1);
            //self.unify(e.unwrap(), out_ty, expr.1)?;
            Ok(out_ty)
        } else {
            let item = item.name()?;
            Err(
                DynamicErr::new("Cannot use a fielded constructor to create a non-struct type")
                    .label(format!("This is {}", item.ident()?), name.1)
                    .extra_labels(vec![(format!("'{}' defined here", item.ident()?), item.1)])
                    .into(),
            )
        }
    }

    fn check_expr_fielded_constructor(
        &mut self,
        expr: &Spanned<Intern<Expr>>,
        name: &Spanned<Intern<Expr>>,
        given_fields: Intern<Vec<(Spanned<Intern<Expr>>, Spanned<Intern<Expr>>)>>,
    ) -> Result<TyVar, errors::CompilerErr> {
        let ident = QualifierFragment::Type(name.ident()?);

        let (i, fields) = self
            // .resolve_name(name)
            .master_env
            .get_node_and_children(&ident, &self.local.package)
            .map_err(|_| errors::not_defined(ident, &name.1))?;

        let fields: Vec<_> = fields
            .iter()
            .filter(|x| matches!(x.0, QualifierFragment::Field(_)))
            .collect();

        let item = self.check_item(i, self.local.package)?;
        //dbg!(&fty);
        if let Item {
            kind: ItemKind::Struct(StructEntry { .. }),
            ..
        } = item
        {
            if fields.len() != given_fields.len() {
                return Err(DynamicErr::new(format!(
                    "Field count mismatch: expected {}, found {}",
                    fields.len(),
                    given_fields.len()
                ))
                .label(format!("expected {} fields", fields.len()), name.1)
                .label(format!("found {} fields", given_fields.len()), expr.1)
                .into());
            }

            let map: CompResult<Vec<&(Spanned<Intern<Expr>>, Spanned<Intern<Ty>>)>> = fields
                .into_iter()
                .map(move |f| -> CompResult<_> {
                    if let ItemKind::Field(v) = &f.1.kind {
                        Ok(v)
                    } else {
                        let name = item.name()?.ident()?;
                        FatalErr::new(format!("The children of {} should all be fields", name))
                    }
                })
                .collect();
            //let map = if let Ok(m) = map { m } else { unreachable!() };
            // // Consider unchecked? The code should panic when the program encounters an error
            // SAFETY: If one of the children is not a field, the creation of the FatalErr will terminate execution before the map is unwrapped.
            let map = unsafe { map.unwrap_unchecked() };

            for (fname, value) in given_fields.iter() {
                //dbg!(fname);
                let def_ty: Spanned<Intern<Ty>> = map
                    .iter()
                    .filter(|x| *x.0 .0 == *fname.0)
                    .next_back()
                    .ok_or(
                        DynamicErr::new(format!(
                            "No such field '{}' in struct '{}'",
                            fname.ident()?,
                            name.ident()?
                        ))
                        .label(format!("{:?} doesn't exist", fname.0), fname.1),
                    )?
                    .1;
                let given_ty = self.check_expr(value)?;
                let expected_ty = self.local.convert_ty(&def_ty.0)?;
                let expected_ty_var = self.local.create_ty(expected_ty, def_ty.1);
                self.unify(given_ty, expected_ty_var, value.1)?;
            }
            let struct_tyinfo = self.local.convert_ty(i.get_ty().map(|x| x.0)?.as_ref())?;

            let out_ty = self.local.create_ty(struct_tyinfo, name.1);
            Ok(out_ty)
        } else {
            let item = item.name()?;
            Err(
                DynamicErr::new("Cannot use a fielded constructor to create a non-struct type")
                    .label(format!("This is {}", item.ident()?), name.1)
                    .extra_labels(vec![(format!("'{}' defined here", item.ident()?), item.1)])
                    .into(),
            )
        }
    }

    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
    fn check_expr_field_access(
        &mut self,
        expr: &Spanned<Intern<Expr>>,
        l: &Spanned<Intern<Expr>>,
        r: &Spanned<Intern<Expr>>,
    ) -> Result<TyVar, errors::CompilerErr> {
        let left_ty = self.check_expr(l)?;
        let (info, span) = self.local.vars[left_ty.0];
        // dbg!(info);

        if matches!(info, TyInfo::User(_, _)) {
            let info_name = info.name()?;
            let ident = QualifierFragment::Type(info_name.ident()?);
            let fields = self
                .master_env
                // .clone()
                .get_children(&ident, &self.local.package)
                .map_err(|_| errors::not_defined(ident, &expr.1))?;
            let fields: Vec<_> = fields
                .iter()
                .filter(|x| matches!(x.0, QualifierFragment::Field(_)))
                .collect();
            let desired_field_q = QualifierFragment::Field(r.ident()?);
            // dbg!(desired_field_q);
            let f = fields
                .iter()
                .find(|x| x.0.is(&desired_field_q))
                .ok_or_else(|| errors::not_defined(desired_field_q, &r.1))?;
            let fty = self.local.convert_ty(&f.1.get_ty()?.0)?;
            let converted = self.local.create_ty(fty, expr.1);

            Ok(converted)
        } else if let TyInfo::Tuple(v) = info {
            let r_ty = self.check_expr(r)?;
            let num = self.local.create_ty(TyInfo::Num(OrderedFloat::nan()), r.1);
            self.unify(r_ty, num, expr.1)?;
            if let TyInfo::Num(n) = self.local.vars[r_ty.0].0 {
                if Float::fract(n) == 0f64 {
                    Ok(*v.get(n.0 as usize).ok_or_else(|| {
                        DynamicErr::new("Index out of range".to_string())
                            .label(
                                format!(
                                    "'{}' is out of range for a tuple with {} fields",
                                    n.0 as usize,
                                    v.len()
                                ),
                                r.1,
                            )
                            .extra_labels(vec![(
                                format!("{} tuples use zero-based indexing", self.render_ty(info)),
                                span,
                            )])
                    })?)
                } else {
                    Err(
                        DynamicErr::new("Cannot index by a floating point number".to_string())
                            .label("this".to_string(), r.1)
                            .into(),
                    )
                }
            } else {
                Err(
                    DynamicErr::new("Cannot index a tuple by a non-numeric expression")
                        .label("this is not Num", r.1)
                        .into(),
                )
            }
        } else if let TyInfo::Package(_) = info {
            let path = QualifierFragment::from_expr(expr)?;
            // Get the last and next-to-last item in the path. These will be used as the item and context.
            let [_heads @ .., second, last] = path.as_slice() else {
                // Should be unreachable, as the expression by definition has two parts.
                FatalErr::new(format!("The path '{path:?}' was not long enough"));
            };
            let children = self
                .master_env
                .get_children(second, &self.local.package)
                .map_err(|_| errors::not_defined(second, &expr.1))?;
            let child = children
                .iter()
                .find(|x| x.0.is(last))
                .ok_or_else(|| errors::not_defined(last, &r.1))?;

            let target_ty = child.1.get_ty()?;
            // dbg!(child_ty);
            let tyinfo = self.local.convert_ty(&target_ty.0)?;
            let tv = self.local.create_ty(tyinfo, expr.1);
            // self.unify(left_ty, tv, tyinfo.get_user_name().unwrap().1)?;
            Ok(tv)
        } else {
            let path = QualifierFragment::from_expr(expr)?;

            let [_heads @ .., second, last] = path.as_slice() else {
                FatalErr::new(format!("The path '{path:?}' was not long enough"));
                // unreachable!("The path '{}'"")
            };
            let (entry, children) = self
                .master_env
                .get_node_and_children(second, &self.local.package)
                .map_err(|_| errors::not_defined(second, &expr.1))?;
            let _child = children
                .iter()
                .find(|x| x.0.is(last))
                .ok_or_else(|| errors::not_defined(last, &r.1))?;
            let entry_ty = entry.get_ty()?.0;
            let tyinfo = self.local.convert_ty(&entry_ty)?;
            let tv = self.local.create_ty(tyinfo, expr.1);
            Ok(tv)
        }
    }

    #[allow(clippy::single_match)]
    fn prepare_pat_context(
        &mut self,
        p: Spanned<Pattern>,
        context: Option<&'env Spanned<Intern<Ty>>>,
        children: &[NodeIndex],
    ) -> CompResult<usize> {
        let mut count = 0usize;

        match p.0 {
            Pattern::Atom(pattern_atom) => match pattern_atom {
                crate::resource::rep::ast::PatternAtom::Variable(v) => {
                    let tyvar = if let Some(t) = context {
                        let info = self.local.convert_ty(&t.0)?;
                        self.local.create_ty(info, t.1)
                    } else {
                        self.local.create_ty(TyInfo::Unknown, p.1)
                    };
                    // dbg!(tyvar);
                    // Add the variable to the accumulator
                    self.local
                        .env_push(Spanned(Intern::from(Expr::Ident(v.ident()?)), p.1), tyvar);

                    count += 1;
                }
                _ => {}
            },
            Pattern::Tuple(_items) => todo!(),
            Pattern::Variant(n, fields) => {
                let vqfrag = QualifierFragment::Variant(n.ident()?);
                // Check if the variant is valid
                if let Some(idx) = children.iter().find(|x| {
                    self.master_env.value(**x).is_ok_and(|x| {
                        x.name()
                            .is_ok_and(|x| x.ident().is_ok_and(|x| x == *vqfrag.name()))
                    })
                }) {
                    if let Item {
                        kind: ItemKind::Variant(Spanned(EnumVariant { types, .. }, _)),
                        ..
                    } = self.master_env.value(*idx)?
                    {
                        for (pat_field, ty) in fields.iter().zip(types.iter()) {
                            self.prepare_pat_context(*pat_field, Some(ty), children)?;
                        }
                    } else {
                        return Err(DynamicErr::new("Expected a variant")
                            .label(
                                format!(
                                    "This is {}",
                                    self.master_env.value(*idx)?.name()?.ident()?
                                ),
                                n.1,
                            )
                            .into());
                    }
                } else {
                    return Err(errors::not_defined(vqfrag, &n.1));
                }
            }
        }
        Ok(count)
    }

    /// Resolve a name from the `master_env` given an expression.
    fn resolve_name(
        &mut self,
        expr: &Spanned<Intern<Expr>>,
        // local: &mut SolverEnv<'src>,
    ) -> Result<TyVar, crate::resource::errors::CompilerErr> {
        let name = expr.ident()?;
        let package = self.local.package;
        if let Ok(e) = self.search_masterenv(&QualifierFragment::Func(name), &expr.1) {
            // BEAUTIFUL!
            let fty = if self.check_item(e, package)?.get_sig().is_some() {
                e
            } else {
                let l = self.local.create_ty(TyInfo::Unknown, expr.1);
                let r = self.local.create_ty(TyInfo::Unknown, expr.1);
                return Ok(self.local.create_ty(TyInfo::Func(l, r), expr.1));
            };
            //dbg!(&fty);
            if let Item {
                kind: ItemKind::Function(FunctionItem { sig, .. }),
                ..
            } = fty
            {
                let info = self.local.convert_ty(&sig.get().unwrap().0)?;
                let fn_ty = self.local.create_ty(info, expr.1);
                Ok(fn_ty)
            } else if let Item {
                kind: ItemKind::Extern { ref sig, .. },
                ..
            } = fty
            {
                let info = self.local.convert_ty(&sig.0)?;
                let fn_ty = self.local.create_ty(info, expr.1);
                Ok(fn_ty)
            } else {
                unreachable!("Should be a function")
            }
        } else if let Ok(e) = self.search_masterenv(&QualifierFragment::Type(name), &expr.1) {
            // BEAUTIFUL!
            let ty = self.check_item(e, self.local.package)?.get_ty()?;
            let info = self.local.convert_ty(&ty.0)?;
            Ok(self.local.create_ty(info, ty.1))
        } else {
            Err(errors::not_defined(
                QualifierFragment::Wildcard(name),
                &expr.1,
            ))
        }
    }

    fn solve(
        &mut self,
        var: TyVar,
        // local: &'env mut SolverEnv<'src>,
    ) -> CompResult<Spanned<Intern<Ty>>> {
        let span = self.local.vars[var.0].1;
        match self.local.vars[var.0].0 {
            TyInfo::Unknown => Err(DynamicErr::new(format!(
                "cannot infer type {var:?}, is Unknown"
            ))
            .label("has unknown type".to_string(), span)
            .into()),
            TyInfo::Ref(var) => Ok(self.solve(var)?),
            TyInfo::Num(_) => Ok(Spanned(
                Intern::from(Ty::Primitive(PrimitiveType::Num)),
                span,
            )),
            TyInfo::Bool => Ok(Spanned(
                Intern::from(Ty::Primitive(PrimitiveType::Bool)),
                span,
            )),
            TyInfo::String => Ok(Spanned(
                Intern::from(Ty::Primitive(PrimitiveType::Str)),
                span,
            )),
            TyInfo::Unit => Ok(Spanned(
                Intern::from(Ty::Primitive(PrimitiveType::Unit)),
                span,
            )),
            TyInfo::Func(i, o) => Ok(Spanned(
                Intern::from(Ty::Arrow(
                    Spanned(self.solve(i)?.0, self.local.vars[i.0].1),
                    Spanned(self.solve(o)?.0, self.local.vars[o.0].1),
                )),
                span,
            )),
            TyInfo::User(n, g) => {
                let e = self.search_masterenv(
                    &QualifierFragment::Type(n.ident()?),
                    &self.local.vars[var.0].1.clone(),
                )?;
                let mut generics = vec![];

                for gen in g {
                    generics.push(self.solve(*gen)?);
                }
                //dbg!(&generics);

                let t = e.get_ty()?;
                let new = t.0.monomorph_user(Intern::from(generics));
                Ok(Spanned(Intern::from(new), t.1))
            }
            TyInfo::Variant(p, _) => self.solve(p),
            TyInfo::Tuple(t) => {
                let mut v = vec![];
                for t in t {
                    v.push(self.solve(*t)?);
                }

                Ok(Spanned(Intern::from(Ty::Tuple(Intern::from(v))), span))
            }
            TyInfo::Seq(t) => {
                let t = self.solve(*t)?;
                Ok(Spanned(Intern::from(Ty::Seq(t)), span))
            }
            TyInfo::Generic(n) => Ok(Spanned(
                Intern::from(Ty::Generic(Spanned(
                    Intern::from(Expr::Ident(n.ident()?)),
                    span,
                ))),
                span,
            )),
            TyInfo::Package(p) => Err(DynamicErr::new("cannot return package").into()),
        }
        .inspect(|&t| {
            trace!(
                "Solved {} => {}",
                self.render_ty(self.local.vars[var.0].0),
                t.0
            );
        })
    }

    /// Check the environment loaded into `self`, starting from the main function.
    /// TODO: Add "targeted" version to check one item specifically
    ///
    /// # Errors
    /// On typechecking error.
    ///
    /// # Examples
    /// ```rust
    /// let tc = Solver::new(env, QualifierFragment::Root);
    /// assert!(Ok(()), tc.check())
    /// ```
    pub fn check(&mut self) -> CompResult<()> {
        if cfg!(debug_assertions) {
            let render =
                |_, k: EdgeReference<QualifierFragment>| format!("label = \"{}\"", k.weight());
            let dot = petgraph::dot::Dot::with_attr_getters(
                &self.master_env.graph,
                &[
                    Config::EdgeNoLabel,
                    Config::NodeNoLabel,
                    Config::RankDir(petgraph::dot::RankDir::LR),
                ],
                &render,
                &|_, _| String::new(),
            );
            info!("{dot:?}");
        }

        let mainpack = Intern::from_ref("Main");
        let main_idx = self
            .master_env
            .get_from_context(
                &QualifierFragment::Func(Intern::from_ref("main")),
                &QualifierFragment::Package(mainpack),
            )
            //.get(&quantifier!(Root, Package("Main"), Func("main"), End).into_simple())
            ?;

        let main_item = self.master_env.value(main_idx)?;

        self.check_item(main_item, QualifierFragment::Package(mainpack))?;
        // Ok(self.master_env)
        Ok(())
        //dbg!(&main);
    }

    /// Check a single item from the environment.
    fn check_item(
        &mut self,
        item: &'env Item,
        packctx: QualifierFragment,
    ) -> CompResult<&'env Item> {
        // dbg!(item.name());
        let Item { kind, is_checked } = item;
        if is_checked.get() {
            return Ok(item);
        }
        // If the item is a function
        if let ItemKind::Function(FunctionItem {
            ref name,
            ref sig,
            ref body,
            ..
        }) = kind
        {
            let old_local = self.local.clone();
            self.local = SolverEnv {
                vars: vec![],
                env: vec![],
                package: packctx,
            };
            // self.local.package = packctx;
            // let mut tc = Solver::new(self.master_env, packctx);
            // let tv = tc.check_expr(body)?;
            // let fn_sig = tc.solve(tv)?;

            let inferred_tyvar = self.check_expr(body)?;

            if let Some(sig) = sig.get() {
                let sig_span = sig.1;
                let sig_ty_info = self.local.convert_ty(&sig.0)?;
                let converted_tyvar = self.local.create_ty(sig_ty_info, sig_span);
                self.unify(inferred_tyvar, converted_tyvar, sig_span)?;
            }

            let fn_sig = self.solve(inferred_tyvar)?;
            // dbg!(name, fn_sig);
            let val = Spanned(
                fn_sig.0,
                SimpleSpan::new(name.1.context, name.1.into_range()),
            );
            // dbg!(sig);
            sig.replace(Some(val));
            // checked.replace(true);
            self.local = old_local;
        }

        is_checked.replace(true);
        Ok(item)
    }
}
