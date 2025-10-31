use std::rc::Rc;

use super::super::errors::CompResult;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Quantifier {
    Root(Rc<Self>),
    Package(String, Rc<Self>),
    Type(String, Rc<Self>),
    Effect(String, Rc<Self>),
    Func(String, Rc<Self>),
    Variable(String),
    Variant(String, Rc<Self>),
    Field(String),
    End,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SimpleQuant {
    Root,
    Package(String),
    Type(String),
    Func(String),
    Variant(String),
    Field(String),
    Wildcard(String),
}

impl std::fmt::Display for SimpleQuant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Root => write!(f, "Root"),
            Self::Package(n) => write!(f, "Package {n}"),
            Self::Type(n) => write!(f, "Type {n}"),
            Self::Func(n) => write!(f, "Function {n}"),
            Self::Field(n) => write!(f, "Field {n}"),
            Self::Variant(n) => write!(f, "Variant {n}"),
            Self::Wildcard(n) => write!(f, "{n}"),
        }
    }
}
use crate::resource::rep::{ast::Expr, Spanned};

impl SimpleQuant {
    pub fn name(&self) -> String {
        match self {
            SimpleQuant::Root => String::from("root"),

            SimpleQuant::Package(n)
            | SimpleQuant::Type(n)
            | SimpleQuant::Func(n)
            | SimpleQuant::Variant(n)
            | SimpleQuant::Field(n)
            | SimpleQuant::Wildcard(n) => n.clone(),
        }
    }

    pub fn is(&self, rhs: &Self) -> bool {
        self.name() == rhs.name()
    }

    pub fn from_expr(expr: &Spanned<Expr>) -> CompResult<Vec<Self>> {
        struct CheckFieldAccess<'s> {
            f: &'s dyn Fn(&Self, &Spanned<Expr>, Vec<SimpleQuant>) -> CompResult<Vec<SimpleQuant>>,
        }
        let cfa = CheckFieldAccess {
            f: &|cfa: &CheckFieldAccess<'_>,
                 e: &Spanned<Expr>,
                 mut accum: Vec<SimpleQuant>|
             -> CompResult<Vec<SimpleQuant>> {
                //dbg!(&q);
                match &e.0 {
                    Expr::FieldAccess(l, r) => {
                        accum.push(Self::Wildcard(l.0.get_ident().unwrap()));

                        (cfa.f)(cfa, r, accum)
                        //self.graph.node_weight(n).cloned()
                    }

                    e => {
                        accum.push(Self::Wildcard(e.get_ident().unwrap()));
                        Ok(accum)
                    }
                }
            },
        };
        (cfa.f)(&cfa, expr, vec![])
    }
}

impl Quantifier {
    #[must_use]
    pub fn append(&self, a: Self) -> Self {
        match self {
            Self::Root(quantifier) => Self::Root(Rc::new(quantifier.append(a))),
            Self::Package(n, quantifier) => {
                Self::Package(n.to_string(), Rc::new(quantifier.append(a)))
            }
            Self::Type(n, quantifier) => Self::Type(n.to_string(), Rc::new(quantifier.append(a))),
            Self::Effect(n, quantifier) => Self::Type(n.to_string(), Rc::new(quantifier.append(a))),
            Self::Func(n, quantifier) => Self::Func(n.to_string(), Rc::new(quantifier.append(a))),
            Self::Variable(_) => todo!(),
            Self::Variant(n, quantifier) => {
                Self::Variant(n.to_string(), Rc::new(quantifier.append(a)))
            }
            Self::Field(_) => a,

            Self::End => a,
        }
    }

    #[must_use]
    pub fn get_func_name(&self) -> Option<&String> {
        match self {
            Quantifier::Root(e) | Quantifier::Package(_, e) => e.get_func_name(),
            Quantifier::Func(n, _) => Some(n),
            Quantifier::End => None,
            _ => panic!(),
        }
    }

    #[must_use = "Quantifiers should be consumed for queries or generation"]
    pub fn into_simple(&self) -> Vec<SimpleQuant> {
        let mut res = vec![];
        fn collapse(top: &Quantifier, result: &mut Vec<SimpleQuant>) {
            match top {
                Quantifier::Root(q) => {
                    //result.push(SimpleQuant::Root);
                    collapse(q, result);
                }
                Quantifier::Package(n, q) => {
                    result.push(SimpleQuant::Package(n.to_string()));
                    collapse(q, result);
                }
                Quantifier::Type(n, q) => {
                    result.push(SimpleQuant::Type(n.to_string()));
                    collapse(q, result);
                }
                Quantifier::Func(n, q) => {
                    result.push(SimpleQuant::Func(n.to_string()));
                    collapse(q, result);
                }
                Quantifier::Field(n) => {
                    result.push(SimpleQuant::Field(n.to_string()));
                }
                Quantifier::Variant(n, q) => {
                    result.push(SimpleQuant::Field(n.to_string()));
                    collapse(q, result);
                }
                Quantifier::End => {}
                _ => todo!(),
            }
        }
        collapse(self, &mut res);
        //res.reverse();
        res

        // let mut h = DefaultHasher::new();
        // self.hash(&mut h);
        // h.finish().to_be_bytes().to_vec()
    }
}

#[macro_export]
macro_rules! quantifier {

    // Base case: just End
    (End) => {

        Quantifier::End
    };

    // Variable case (no children)
    (Variable($name:expr)) => {
        Quantifier::Variable($name)
    };

    // Root with child
    (Root, $($rest:tt)*) => {
        Quantifier::Root(std::rc::Rc::new(quantifier!($($rest)*)))
    };

    // Module with child
    (Package($name:expr), $($rest:tt)*) => {
        Quantifier::Package($name.to_string(), std::rc::Rc::new(quantifier!($($rest)*)))
    };

    // Type with child
    (Type($name:expr), $($rest:tt)*) => {
        Quantifier::Type($name.to_string(), std::rc::Rc::new(quantifier!($($rest)*)))
    };

    (Effect($name:expr), $($rest:tt)*) => {
        Quantifier::Effect($name.to_string(), std::rc::Rc::new(quantifier!($($rest)*)))
    };

    // Func with child
    (Func($name:expr), $($rest:tt)*) => {
        Quantifier::Func($name.to_string(), std::rc::Rc::new(quantifier!($($rest)*)))
    };
}
