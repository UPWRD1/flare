// A small implementation of a Calculus-of-Constructions-style core extended
// with extensible records/variants (rows, Remy/Leijen style) and
// iso-recursive types via explicit fold/unfold. Hindley-Milner-style
// let-generalization is used over both ordinary type variables and row
// variables, which is what gives you row polymorphism.
//
// This is a teaching-scale, single-file implementation: a typechecker
// (unification over types and rows) plus a tree-walking evaluator, so the
// whole pipeline from surface term to a value can be seen end to end.

use std::collections::HashMap;
use std::rc::Rc;

pub type Label = String;

// ---------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Var(u32),
    Con(String),
    Arrow(Rc<Type>, Rc<Type>),
    Record(Rc<Row>),
    Variant(Rc<Row>),
    Mu(Rc<Type>), // mu X. T ; `SelfRef` inside T stands for the bound X
    SelfRef,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Row {
    Empty,
    Extend(Label, Rc<Type>, Rc<Row>),
    Var(u32),
}

// ---------------------------------------------------------------------
// Terms
// ---------------------------------------------------------------------

#[derive(Clone, Debug)]
pub enum Term {
    Var(String),
    Lit(i64),
    Lam(String, Box<Term>),
    App(Box<Term>, Box<Term>),
    RecordEmpty,
    RecordExtend {
        new_label: Label,
        definition: Box<Term>,
        rest: Box<Term>,
    },
    RecordSelect(Box<Term>, Label),
    RecordRestrict(Box<Term>, Label),
    Variant(Label, Box<Term>),
    // branches: (label, bound var, body); default: (bound var, body)
    Case {
        scrutinee: Box<Term>,
        branches: Vec<(Label, String, Term)>,
        default: Option<(String, Box<Term>)>,
    },
    // fold/unfold carry the mu-type explicitly: bidirectional typing avoids
    // having to guess which recursive type a term is meant to inhabit.
    Fold(Type, Box<Term>),
    Unfold(Type, Box<Term>),
}

// ---------------------------------------------------------------------
// Substitution store (mutable union-find-ish binding table)
// ---------------------------------------------------------------------

#[derive(Default)]
pub struct Store {
    next: u32,
    types: HashMap<u32, Type>,
    rows: HashMap<u32, Row>,
}

impl Store {
    fn fresh_id(&mut self) -> u32 {
        let n = self.next;
        self.next += 1;
        n
    }
    fn fresh_type(&mut self) -> Type {
        Type::Var(self.fresh_id())
    }
    fn fresh_row(&mut self) -> Row {
        Row::Var(self.fresh_id())
    }

    fn walk_type(&self, t: &Type) -> Type {
        match t {
            Type::Var(v) => match self.types.get(v) {
                Some(t2) => self.walk_type(t2),
                None => t.clone(),
            },
            _ => t.clone(),
        }
    }
    fn walk_row(&self, r: &Row) -> Row {
        match r {
            Row::Var(v) => match self.rows.get(v) {
                Some(r2) => self.walk_row(r2),
                None => r.clone(),
            },
            _ => r.clone(),
        }
    }
    fn bind_type(&mut self, v: u32, t: Type) {
        self.types.insert(v, t);
    }
    fn bind_row(&mut self, v: u32, r: Row) {
        self.rows.insert(v, r);
    }
}

#[derive(Debug)]
pub enum TypeError {
    Mismatch(String, String),
    MissingLabel(Label),
    OccursCheck,
    UnboundVar(String),
    NotAMu(String),
}
impl Store {
    // ---------------------------------------------------------------------
    // Unification
    // ---------------------------------------------------------------------
    fn unify_type(&mut self, t1: &Type, t2: &Type) -> Result<(), TypeError> {
        let w1 = self.walk_type(t1);
        let w2 = self.walk_type(t2);
        match (w1, w2) {
            (Type::Var(a), Type::Var(b)) if a == b => Ok(()),
            (Type::Var(a), t) | (t, Type::Var(a)) => {
                if self.occurs_type_in_type(a, &t) {
                    return Err(TypeError::OccursCheck);
                }
                self.bind_type(a, t);
                Ok(())
            }
            (Type::Con(a), Type::Con(b)) if a == b => Ok(()),
            (Type::Arrow(a1, b1), Type::Arrow(a2, b2)) => {
                self.unify_type(&a1, &a2)?;
                self.unify_type(&b1, &b2)
            }
            (Type::Record(r1), Type::Record(r2)) => self.unify_row(&r1, &r2),
            (Type::Variant(r1), Type::Variant(r2)) => self.unify_row(&r1, &r2),
            (Type::Mu(a), Type::Mu(b)) => self.unify_type(&a, &b),
            (Type::SelfRef, Type::SelfRef) => Ok(()),
            (a, b) => Err(TypeError::Mismatch(format!("{:?}", a), format!("{:?}", b))),
        }
    }

    // Remy/Leijen row unification: peel a label off one row and search for it
    // (possibly rewriting) in the other, threading a fresh row variable through
    // when the search runs into an unresolved row variable.
    fn unify_row(&mut self, r1: &Row, r2: &Row) -> Result<(), TypeError> {
        let w1 = self.walk_row(r1);
        let w2 = self.walk_row(r2);
        match (w1, w2) {
            (Row::Empty, Row::Empty) => Ok(()),
            (Row::Var(a), Row::Var(b)) if a == b => Ok(()),
            (Row::Var(a), r) | (r, Row::Var(a)) => {
                if self.occurs_row_in_row(a, &r) {
                    return Err(TypeError::OccursCheck);
                }
                self.bind_row(a, r);
                Ok(())
            }
            (Row::Extend(l1, t1, rest1), other) => {
                let (t2, rest2) = self.rewrite_row(&other, &l1)?;
                self.unify_type(&t1, &t2)?;
                self.unify_row(&rest1, &rest2)
            }
            (Row::Empty, Row::Extend(l, _, _)) => Err(TypeError::MissingLabel(l)),
        }
    }

    // Find `label` in `row`, returning its type and the row with that one entry
    // removed. If we hit an unresolved row variable first, we don't fail: we
    // commit to that row containing the label, via two fresh variables.
    fn rewrite_row(&mut self, row: &Row, label: &Label) -> Result<(Type, Row), TypeError> {
        match self.walk_row(row) {
            Row::Empty => Err(TypeError::MissingLabel(label.clone())),
            Row::Extend(l, t, rest) => {
                if &l == label {
                    Ok(((*t).clone(), (*rest).clone()))
                } else {
                    let (t2, rest2) = self.rewrite_row(&rest, label)?;
                    Ok((t2, Row::Extend(l, t, Rc::new(rest2))))
                }
            }
            Row::Var(v) => {
                let beta = self.fresh_type();
                let rho = self.fresh_row();
                self.bind_row(
                    v,
                    Row::Extend(label.clone(), Rc::new(beta.clone()), Rc::new(rho.clone())),
                );
                Ok((beta, rho))
            }
        }
    }
}

impl Store {
    // ---------------------------------------------------------------------
    // Occurs checks (separately for type vars and row vars, each of which can
    // appear inside the other's structure)
    // ---------------------------------------------------------------------

    fn occurs_type_in_type(&self, v: u32, t: &Type) -> bool {
        match self.walk_type(t) {
            Type::Var(v2) => v2 == v,
            Type::Con(_) | Type::SelfRef => false,
            Type::Arrow(a, b) => self.occurs_type_in_type(v, &a) || self.occurs_type_in_type(v, &b),
            Type::Record(r) | Type::Variant(r) => self.occurs_type_in_row(v, &r),
            Type::Mu(t2) => self.occurs_type_in_type(v, &t2),
        }
    }

    fn occurs_type_in_row(&self, v: u32, r: &Row) -> bool {
        match self.walk_row(r) {
            Row::Empty | Row::Var(_) => false,
            Row::Extend(_, t, rest) => {
                self.occurs_type_in_type(v, &t) || self.occurs_type_in_row(v, &rest)
            }
        }
    }
    fn occurs_row_in_type(&self, v: u32, t: &Type) -> bool {
        match self.walk_type(t) {
            Type::Var(_) | Type::Con(_) | Type::SelfRef => false,
            Type::Arrow(a, b) => self.occurs_row_in_type(v, &a) || self.occurs_row_in_type(v, &b),
            Type::Record(r) | Type::Variant(r) => self.occurs_row_in_row(v, &r),
            Type::Mu(t2) => self.occurs_row_in_type(v, &t2),
        }
    }
    fn occurs_row_in_row(&self, v: u32, r: &Row) -> bool {
        match self.walk_row(r) {
            Row::Empty => false,
            Row::Var(v2) => v2 == v,
            Row::Extend(_, t, rest) => {
                self.occurs_row_in_type(v, &t) || self.occurs_row_in_row(v, &rest)
            }
        }
    }
}

// ---------------------------------------------------------------------
// mu substitution: replace `SelfRef` by `repl`, stopping at nested `Mu`
// binders since their `SelfRef` occurrences belong to that inner scope.
// ---------------------------------------------------------------------

fn subst_selfref(t: &Type, repl: &Type) -> Type {
    match t {
        Type::SelfRef => repl.clone(),
        Type::Var(v) => Type::Var(*v),
        Type::Con(c) => Type::Con(c.clone()),
        Type::Arrow(a, b) => Type::Arrow(
            Rc::new(subst_selfref(a, repl)),
            Rc::new(subst_selfref(b, repl)),
        ),
        Type::Record(r) => Type::Record(Rc::new(subst_selfref_row(r, repl))),
        Type::Variant(r) => Type::Variant(Rc::new(subst_selfref_row(r, repl))),
        Type::Mu(inner) => Type::Mu(inner.clone()),
    }
}
fn subst_selfref_row(r: &Row, repl: &Type) -> Row {
    match r {
        Row::Empty => Row::Empty,
        Row::Var(v) => Row::Var(*v),
        Row::Extend(l, t, rest) => Row::Extend(
            l.clone(),
            Rc::new(subst_selfref(t, repl)),
            Rc::new(subst_selfref_row(rest, repl)),
        ),
    }
}

// ---------------------------------------------------------------------
// Type schemes, environments, generalization/instantiation
// ---------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct Scheme {
    pub type_vars: Vec<u32>,
    pub row_vars: Vec<u32>,
    pub ty: Type,
}
fn mono(ty: Type) -> Scheme {
    Scheme {
        type_vars: vec![],
        row_vars: vec![],
        ty,
    }
}

pub type Env = HashMap<String, Scheme>;

fn instantiate(store: &mut Store, s: &Scheme) -> Type {
    let mut mt = HashMap::new();
    let mut mr = HashMap::new();
    for &v in &s.type_vars {
        mt.insert(v, store.fresh_id());
    }
    for &v in &s.row_vars {
        mr.insert(v, store.fresh_id());
    }
    subst_fresh_type(&s.ty, &mt, &mr)
}
fn subst_fresh_type(t: &Type, mt: &HashMap<u32, u32>, mr: &HashMap<u32, u32>) -> Type {
    match t {
        Type::Var(v) => Type::Var(*mt.get(v).unwrap_or(v)),
        Type::Con(c) => Type::Con(c.clone()),
        Type::Arrow(a, b) => Type::Arrow(
            Rc::new(subst_fresh_type(a, mt, mr)),
            Rc::new(subst_fresh_type(b, mt, mr)),
        ),
        Type::Record(r) => Type::Record(Rc::new(subst_fresh_row(r, mt, mr))),
        Type::Variant(r) => Type::Variant(Rc::new(subst_fresh_row(r, mt, mr))),
        Type::Mu(t2) => Type::Mu(Rc::new(subst_fresh_type(t2, mt, mr))),
        Type::SelfRef => Type::SelfRef,
    }
}
fn subst_fresh_row(r: &Row, mt: &HashMap<u32, u32>, mr: &HashMap<u32, u32>) -> Row {
    match r {
        Row::Empty => Row::Empty,
        Row::Var(v) => Row::Var(*mr.get(v).unwrap_or(v)),
        Row::Extend(l, t, rest) => Row::Extend(
            l.clone(),
            Rc::new(subst_fresh_type(t, mt, mr)),
            Rc::new(subst_fresh_row(rest, mt, mr)),
        ),
    }
}

fn free_vars_type(store: &Store, t: &Type, tv: &mut Vec<u32>, rv: &mut Vec<u32>) {
    match store.walk_type(t) {
        Type::Var(v) => {
            if !tv.contains(&v) {
                tv.push(v)
            }
        }
        Type::Con(_) | Type::SelfRef => {}
        Type::Arrow(a, b) => {
            free_vars_type(store, &a, tv, rv);
            free_vars_type(store, &b, tv, rv);
        }
        Type::Record(r) | Type::Variant(r) => free_vars_row(store, &r, tv, rv),
        Type::Mu(t2) => free_vars_type(store, &t2, tv, rv),
    }
}
fn free_vars_row(store: &Store, r: &Row, tv: &mut Vec<u32>, rv: &mut Vec<u32>) {
    match store.walk_row(r) {
        Row::Empty => {}
        Row::Var(v) => {
            if !rv.contains(&v) {
                rv.push(v)
            }
        }
        Row::Extend(_, t, rest) => {
            free_vars_type(store, &t, tv, rv);
            free_vars_row(store, &rest, tv, rv);
        }
    }
}
fn free_vars_env(store: &Store, env: &Env, tv: &mut Vec<u32>, rv: &mut Vec<u32>) {
    for s in env.values() {
        let (mut t2, mut r2) = (vec![], vec![]);
        free_vars_type(store, &s.ty, &mut t2, &mut r2);
        for v in t2 {
            if !s.type_vars.contains(&v) && !tv.contains(&v) {
                tv.push(v);
            }
        }
        for v in r2 {
            if !s.row_vars.contains(&v) && !rv.contains(&v) {
                rv.push(v);
            }
        }
    }
}
// Fully resolve every bound variable into the concrete structure it stands
// for. This matters because a variable can be bound to a type that itself
// contains other (still-free) variables: without zonking, those nested
// variables stay hidden behind the outer Var node and generalize/instantiate
// can't see or rename them.
fn zonk_type(store: &Store, t: &Type) -> Type {
    match store.walk_type(t) {
        Type::Var(v) => Type::Var(v),
        Type::Con(c) => Type::Con(c),
        Type::Arrow(a, b) => {
            Type::Arrow(Rc::new(zonk_type(store, &a)), Rc::new(zonk_type(store, &b)))
        }
        Type::Record(r) => Type::Record(Rc::new(zonk_row(store, &r))),
        Type::Variant(r) => Type::Variant(Rc::new(zonk_row(store, &r))),
        Type::Mu(inner) => Type::Mu(Rc::new(zonk_type(store, &inner))),
        Type::SelfRef => Type::SelfRef,
    }
}

fn zonk_row(store: &Store, r: &Row) -> Row {
    match store.walk_row(r) {
        Row::Empty => Row::Empty,
        Row::Var(v) => Row::Var(v),
        Row::Extend(l, t, rest) => Row::Extend(
            l,
            Rc::new(zonk_type(store, &t)),
            Rc::new(zonk_row(store, &rest)),
        ),
    }
}

fn generalize(store: &Store, env: &Env, ty: &Type) -> Scheme {
    let (mut tv, mut rv) = (vec![], vec![]);
    free_vars_type(store, ty, &mut tv, &mut rv);
    let (mut etv, mut erv) = (vec![], vec![]);
    free_vars_env(store, env, &mut etv, &mut erv);
    Scheme {
        type_vars: tv.into_iter().filter(|v| !etv.contains(v)).collect(),
        row_vars: rv.into_iter().filter(|v| !erv.contains(v)).collect(),
        ty: zonk_type(store, ty),
    }
}

// ---------------------------------------------------------------------
// Type inference
// ---------------------------------------------------------------------

fn infer(store: &mut Store, env: &Env, term: &Term) -> Result<Type, TypeError> {
    match term {
        Term::Var(x) => {
            let s = env.get(x).ok_or_else(|| TypeError::UnboundVar(x.clone()))?;
            Ok(instantiate(store, s))
        }
        Term::Lit(_) => Ok(Type::Con("Int".into())),
        Term::Lam(x, body) => {
            let arg_ty = store.fresh_type();
            let mut env2 = env.clone();
            env2.insert(x.clone(), mono(arg_ty.clone()));
            let body_ty = infer(store, &env2, body)?;
            Ok(Type::Arrow(Rc::new(arg_ty), Rc::new(body_ty)))
        }
        Term::App(f, a) => {
            let f_ty = infer(store, env, f)?;
            let a_ty = infer(store, env, a)?;
            let ret_ty = store.fresh_type();
            store.unify_type(&f_ty, &Type::Arrow(Rc::new(a_ty), Rc::new(ret_ty.clone())))?;
            Ok(ret_ty)
        }
        Term::RecordEmpty => Ok(Type::Record(Rc::new(Row::Empty))),
        Term::RecordExtend {
            new_label: l,
            definition: e,
            rest,
        } => {
            let t_e = infer(store, env, e)?;
            let t_rest = infer(store, env, rest)?;
            let rho = store.fresh_row();
            store.unify_type(&t_rest, &Type::Record(Rc::new(rho.clone())))?;
            Ok(Type::Record(Rc::new(Row::Extend(
                l.clone(),
                Rc::new(t_e),
                Rc::new(rho),
            ))))
        }
        Term::RecordSelect(e, l) => {
            let t_e = infer(store, env, e)?;
            let field_ty = store.fresh_type();
            let rho = store.fresh_row();
            store.unify_type(
                &t_e,
                &Type::Record(Rc::new(Row::Extend(
                    l.clone(),
                    Rc::new(field_ty.clone()),
                    Rc::new(rho),
                ))),
            )?;
            Ok(field_ty)
        }
        Term::RecordRestrict(e, l) => {
            let t_e = infer(store, env, e)?;
            let field_ty = store.fresh_type();
            let rho = store.fresh_row();
            store.unify_type(
                &t_e,
                &Type::Record(Rc::new(Row::Extend(
                    l.clone(),
                    Rc::new(field_ty),
                    Rc::new(rho.clone()),
                ))),
            )?;
            Ok(Type::Record(Rc::new(rho)))
        }
        Term::Variant(l, e) => {
            let t_e = infer(store, env, e)?;
            let rho = store.fresh_row();
            Ok(Type::Variant(Rc::new(Row::Extend(
                l.clone(),
                Rc::new(t_e),
                Rc::new(rho),
            ))))
        }
        Term::Case {
            scrutinee,
            branches,
            default,
        } => {
            let t_scrutinee = infer(store, env, scrutinee)?;
            let result_ty = store.fresh_type();
            let scrutinee_row_start = store.fresh_row();
            let mut tail = scrutinee_row_start.clone();
            for (l, x, body) in branches {
                let field_ty = store.fresh_type();
                let next_tail = store.fresh_row();
                store.unify_row(
                    &tail,
                    &Row::Extend(
                        l.clone(),
                        Rc::new(field_ty.clone()),
                        Rc::new(next_tail.clone()),
                    ),
                )?;
                let mut env2 = env.clone();
                env2.insert(x.clone(), mono(field_ty));
                let branch_ty = infer(store, &env2, body)?;
                store.unify_type(&branch_ty, &result_ty)?;
                tail = next_tail;
            }
            match default {
                Some((x, body)) => {
                    let mut env2 = env.clone();
                    env2.insert(x.clone(), mono(Type::Variant(Rc::new(tail))));
                    let branch_ty = infer(store, &env2, body)?;
                    store.unify_type(&branch_ty, &result_ty)?;
                }
                None => store.unify_row(&tail, &Row::Empty)?,
            }
            store.unify_type(&t_scrutinee, &Type::Variant(Rc::new(scrutinee_row_start)))?;
            Ok(result_ty)
        }
        Term::Fold(mu_type, term) => {
            let inner_type = match mu_type {
                Type::Mu(i) => (**i).clone(),
                _ => return Err(TypeError::NotAMu(format!("{:?}", mu_type))),
            };
            let expected_type = subst_selfref(&inner_type, mu_type);
            let term_type = infer(store, env, term)?;
            store.unify_type(&term_type, &expected_type)?;
            Ok(mu_type.clone())
        }
        Term::Unfold(mu_ty, e) => {
            let inner = match mu_ty {
                Type::Mu(i) => (**i).clone(),
                _ => return Err(TypeError::NotAMu(format!("{:?}", mu_ty))),
            };
            let t_e = infer(store, env, e)?;
            store.unify_type(&t_e, mu_ty)?;
            Ok(subst_selfref(&inner, mu_ty))
        }
    }
}

// ---------------------------------------------------------------------
// Pretty-printing (resolves substitutions on the way out)
// ---------------------------------------------------------------------
pub mod pretty;

// ---------------------------------------------------------------------
// Evaluator. fold/unfold are purely compile-time typing devices: they
// erase to nothing at runtime, exactly as promised for iso-recursive types.
// ---------------------------------------------------------------------
pub mod eval;

// ---------------------------------------------------------------------
// Demo
// ---------------------------------------------------------------------
#[cfg(test)]
mod tests;
