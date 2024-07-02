use crate::root::resource::{ast::*, itypes::*};

#[derive(Debug, Clone)]
pub struct Entry {
    pub name: String,
    pub value: Itype,
}

impl Entry {
    pub fn new(name: String, value: Itype) -> Self {
        Self { name, value }
    }
}
#[derive(Debug, Clone)]
pub struct Environment {
    pub enclosing: Option<Box<Environment>>,
    pub values: Vec<Entry>,
}
impl Environment {
    pub fn new() -> Self {
        Self {
            enclosing: None,
            values: vec![],
        }
    }

    pub fn new_with_enclosing(en: Environment) -> Self {
        Self {
            enclosing: Some(Box::new(en)),
            values: vec![],
        }
    }

    pub fn define(&mut self, name: String, value: Itype) {
        self.values.push(Entry::new(name, value))
    }

    pub fn get(&mut self, name: String) -> Itype {
        for i in self.values.clone() {
            if i.name == name {
                return i.value;
            }
        }

        if self.enclosing.is_some() {
            return self.enclosing.clone().unwrap().get(name);
        }
        panic!("{} is not defined", name)
    }
}

pub struct TypeChecker {
    pub env: Environment
}

impl TypeChecker {
    pub fn check(&mut self, prg: Program) {
        for func in prg.funcs {
            self.check_stmt(func.code)
        }
    }

    fn check_stmt(&mut self, s: Stmt) {
        match s {
            Stmt::Expr(e) => {
                self.resolve_expr(e);
            }
            Stmt::Block(b) => {
                let previous = self.env.clone();
                self.env = Environment::new_with_enclosing(previous.clone());
                for t in b {
                    self.check_stmt(t)
                }

                self.env = previous
            }
            _ => panic!("{s:?} not implemented"),
        };
    }

    fn resolve_expr(&mut self, e: Expr) -> Itype {
        match e {
            Expr::Scalar(s) => s,
            Expr::BinaryOp(op, e) => {
                let (l, r) = *e;
                if op == BinOp::Assign {
                    let rtype = self.resolve_expr(r);
                    self.env.define(l.to_string(), rtype.clone());
                    rtype
                } else {
                    let ltype = self.resolve_expr(l);
                    let rtype = self.resolve_expr(r);
                    if ltype != rtype {
                        panic!("Types did not match")
                    } else {
                        ltype
                    }
                }

            }
            Expr::UnaryOp(op, v) => match op {
                UnaOp::Neg => {
                    let rt = self.resolve_expr(*v.clone());
                    self.expect_expr(*v, rt.clone(), vec![Itype::Int(None), Itype::Flt(None)]);
                    rt
                }
                UnaOp::Not => todo!(),
            },
            Expr::Variable(v) => {
                return self.env.get(v)
            }
            _ => panic!("{e:?} not implemented"),
        }
    }

    fn expect_expr(&mut self, e: Expr, actual: Itype, expected: Vec<Itype>) {
        for t in expected.clone() {
            if actual == t {
                return;
            }
        }

        panic!("Expected {e:?} to be of type {expected:?} but found {actual:?}")
    }
}
