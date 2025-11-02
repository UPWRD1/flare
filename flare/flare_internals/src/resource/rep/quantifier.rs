use super::super::errors::CompResult;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QualifierFragment {
    Root,
    Package(&'static str),
    Type(&'static str),
    Func(&'static str),
    Variant(&'static str),
    Field(&'static str),
    Wildcard(&'static str),
}

impl std::fmt::Display for QualifierFragment {
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

impl QualifierFragment {
    pub fn name(&self) -> &'static str {
        match self {
            QualifierFragment::Root => "root",

            QualifierFragment::Package(n)
            | QualifierFragment::Type(n)
            | QualifierFragment::Func(n)
            | QualifierFragment::Variant(n)
            | QualifierFragment::Field(n)
            | QualifierFragment::Wildcard(n) => n,
        }
    }

    pub fn is(&self, rhs: &Self) -> bool {
        self.name() == rhs.name()
    }

    pub fn from_expr(expr: &Spanned<Expr>) -> CompResult<Vec<Self>> {
        struct CheckFieldAccess<'s> {
            f: &'s dyn Fn(
                &Self,
                &Spanned<Expr>,
                Vec<QualifierFragment>,
            ) -> CompResult<Vec<QualifierFragment>>,
        }
        let cfa = CheckFieldAccess {
            f: &|cfa: &CheckFieldAccess<'_>,
                 e: &Spanned<Expr>,
                 mut accum: Vec<QualifierFragment>|
             -> CompResult<Vec<QualifierFragment>> {
                //dbg!(&q);
                match &e.0 {
                    Expr::FieldAccess(l, r) => {
                        accum.push(Self::Wildcard(l.0.get_ident(l.1).unwrap()));

                        (cfa.f)(cfa, r, accum)
                        //self.graph.node_weight(n).cloned()
                    }

                    ex => {
                        accum.push(Self::Wildcard(ex.get_ident(e.1).unwrap()));
                        Ok(accum)
                    }
                }
            },
        };
        (cfa.f)(&cfa, expr, vec![])
    }
}
