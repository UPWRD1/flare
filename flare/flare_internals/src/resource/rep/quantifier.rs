use super::super::errors::CompResult;

use internment::Intern;

// use rkyv::with::{ArchiveWith, DeserializeWith, With};
// use rkyv::{Archive, Deserialize, Serialize};
// use rkyv_with::{ArchiveWith, DeserializeWith};
// use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QualifierFragment {
    Root,
    // #[with(InternedString)]
    Package(Intern<String>),
    Type(Intern<String>),
    Func(Intern<String>),
    Method(Intern<String>),
    Variant(Intern<String>),
    Field(Intern<String>),
    Wildcard(Intern<String>),
}

//
// impl std::fmt::Display for QualifierFragment {
//     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//         match self {
//             Self::Root => write!(f, "Root"),
//             Self::Package(n) => write!(f, "Package {n}"),
//             Self::Type(n) => write!(f, "Type {n}"),
//             Self::Func(n) => write!(f, "Function {n}"),
//             Self::Method(n) => write!(f, "Method {n}"),
//             Self::Field(n) => write!(f, "Field {n}"),
//             Self::Variant(n) => write!(f, "Variant {n}"),
//             Self::Wildcard(n) => write!(f, "{n}"),
//         }
//     }
// }
use crate::resource::rep::{ast::Expr, Spanned};

impl QualifierFragment {
    pub fn name(&self) -> &Intern<String> {
        match self {
            Self::Root => panic!(),
            Self::Package(n)
            | Self::Type(n)
            | Self::Func(n)
            | Self::Method(n)
            | Self::Variant(n)
            | Self::Field(n)
            | Self::Wildcard(n) => n,
        }
    }

    pub fn is(&self, rhs: &Self) -> bool {
        self.name() == rhs.name()
    }

    pub fn from_expr(expr: &Spanned<Intern<Expr>>) -> CompResult<Vec<QualifierFragment>> {
        struct CheckFieldAccess<'s> {
            f: &'s dyn Fn(
                &'s Self,
                &'s Spanned<Intern<Expr>>,
                Vec<QualifierFragment>,
            ) -> CompResult<Vec<QualifierFragment>>,
        }
        let cfa = CheckFieldAccess {
            f: &|cfa: &CheckFieldAccess<'_>,
                 e: &Spanned<Intern<Expr>>,
                 mut accum: Vec<QualifierFragment>|
             -> CompResult<Vec<QualifierFragment>> {
                //dbg!(&q);
                match &*e.0 {
                    Expr::FieldAccess(l, r) => {
                        accum.push(Self::Wildcard(*l.0.get_ident(l.1).unwrap()));

                        (cfa.f)(cfa, r, accum)
                        //self.graph.node_weight(n).cloned()
                    }

                    ex => {
                        accum.push(Self::Wildcard(*ex.get_ident(e.1).unwrap()));
                        Ok(accum)
                    }
                }
            },
        };
        (cfa.f)(&cfa, expr, vec![])
    }
}
impl std::fmt::Display for QualifierFragment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Root => write!(f, "Root"),
            Self::Package(n) => write!(f, "Package {n}"),
            Self::Type(n) => write!(f, "Type {n}"),
            Self::Func(n) => write!(f, "Function {n}"),
            Self::Method(n) => write!(f, "Method {n}"),
            Self::Field(n) => write!(f, "Field {n}"),
            Self::Variant(n) => write!(f, "Variant {n}"),
            Self::Wildcard(n) => write!(f, "{n}"),
        }
    }
}
