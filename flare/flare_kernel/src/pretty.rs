use crate::{Row, Store, Type, eval::Value};

pub(crate) fn print_type(store: &Store, t: &Type) -> String {
    match store.walk_type(t) {
        Type::Var(v) => format!("t{}", v),
        Type::Con(c) => c,
        Type::Arrow(a, b) => {
            format!("({} -> {})", print_type(store, &a), print_type(store, &b))
        }
        Type::Record(r) => format!("{{{}}}", print_row(store, &r)),
        Type::Variant(r) => format!("<{}>", print_row(store, &r)),
        Type::Mu(inner) => format!("(mu X. {})", print_type(store, &inner)),
        Type::SelfRef => "X".to_string(),
    }
}

pub(crate) fn print_row(store: &Store, r: &Row) -> String {
    let mut fields = vec![];
    let mut cur = store.walk_row(r);
    loop {
        match cur {
            Row::Empty => break,
            Row::Var(v) => {
                fields.push(format!("..t{}", v));
                break;
            }
            Row::Extend(l, t, rest) => {
                fields.push(format!("{}: {}", l, print_type(store, &t)));
                cur = store.walk_row(&rest);
            }
        }
    }
    fields.join(", ")
}

pub(crate) fn print_value(v: &Value) -> String {
    match v {
        Value::Int(n) => n.to_string(),
        Value::Closure(..) => "<closure>".to_string(),
        Value::Record(fields) => {
            let inner: Vec<String> = fields
                .iter()
                .map(|(l, v)| format!("{}={}", l, print_value(v)))
                .collect();
            format!("{{{}}}", inner.join(", "))
        }
        Value::Variant(l, v) => format!("{}({})", l, print_value(v)),
    }
}
