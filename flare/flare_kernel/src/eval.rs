use std::rc::Rc;

use crate::{Label, Term};

pub(crate) enum VEnv {
    Empty,
    Cons(String, Value, Rc<VEnv>),
}

impl VEnv {
    pub(crate) fn lookup(&self, x: &str) -> Option<Value> {
        match self {
            VEnv::Empty => None,
            VEnv::Cons(y, v, rest) => {
                if y == x {
                    Some(v.clone())
                } else {
                    rest.lookup(x)
                }
            }
        }
    }
    pub(crate) fn extend(self: &Rc<Self>, x: String, v: Value) -> Rc<VEnv> {
        Rc::new(VEnv::Cons(x, v, self.clone()))
    }
}

#[derive(Clone)]
pub(crate) enum Value {
    Int(i64),
    Closure(String, Rc<Term>, Rc<VEnv>),
    Record(Vec<(Label, Value)>),
    Variant(Label, Box<Value>),
}

pub(crate) fn eval(env: &Rc<VEnv>, term: &Term) -> Value {
    match term {
        Term::Var(x) => env
            .lookup(x)
            .unwrap_or_else(|| panic!("unbound var at runtime: {}", x)),
        Term::Lit(n) => Value::Int(*n),
        Term::Lam(x, body) => Value::Closure(x.clone(), Rc::new((**body).clone()), env.clone()),
        Term::App(f, a) => {
            let fv = eval(env, f);
            let av = eval(env, a);
            match fv {
                Value::Closure(x, body, cenv) => {
                    let env2 = cenv.extend(x, av);
                    eval(&env2, &body)
                }
                _ => panic!("apply of a non-function"),
            }
        }
        Term::RecordEmpty => Value::Record(vec![]),
        Term::RecordExtend {
            new_label: l,
            definition: e,
            rest,
        } => {
            let v = eval(env, e);
            let vrest = eval(env, rest);
            match vrest {
                Value::Record(mut fields) => {
                    fields.push((l.clone(), v));
                    Value::Record(fields)
                }
                _ => panic!("extend of a non-record"),
            }
        }
        Term::RecordSelect(e, l) => {
            let v = eval(env, e);
            match v {
                Value::Record(fields) => fields
                    .iter()
                    .rev()
                    .find(|(fl, _)| fl == l)
                    .map(|(_, fv)| fv.clone())
                    .unwrap_or_else(|| panic!("missing field {}", l)),
                _ => panic!("select on a non-record"),
            }
        }
        Term::RecordRestrict(e, l) => {
            let v = eval(env, e);
            match v {
                Value::Record(fields) => {
                    Value::Record(fields.into_iter().filter(|(fl, _)| fl != l).collect())
                }
                _ => panic!("restrict of a non-record"),
            }
        }
        Term::Variant(l, e) => Value::Variant(l.clone(), Box::new(eval(env, e))),
        Term::Case {
            scrutinee: scrut,
            branches,
            default,
        } => {
            let v = eval(env, scrut);
            match v {
                Value::Variant(l, inner) => {
                    if let Some((_, x, body)) = branches.iter().find(|(bl, _, _)| bl == &l) {
                        let env2 = env.extend(x.clone(), *inner);
                        eval(&env2, body)
                    } else if let Some((x, body)) = default {
                        let env2 = env.extend(x.clone(), Value::Variant(l, inner));
                        eval(&env2, body)
                    } else {
                        panic!("non-exhaustive case on label {}", l)
                    }
                }
                _ => panic!("case on a non-variant"),
            }
        }
        Term::Fold(_, e) => eval(env, e),
        Term::Unfold(_, e) => eval(env, e),
    }
}
