use super::super::errors::CompResult;

// use chumsky::input::ValueInput;
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
    Dummy(&'static str),
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
use crate::resource::rep::{
    ast::{Expr, Variable},
    common::Ident,
    Spanned,
};

impl QualifierFragment {
    pub fn name(&self) -> &Intern<String> {
        match self {
            Self::Root => Box::leak(Box::new(Intern::from_ref("ROOT"))),
            Self::Package(n)
            | Self::Type(n)
            | Self::Func(n)
            | Self::Method(n)
            | Self::Variant(n)
            | Self::Field(n)
            | Self::Wildcard(n) => n,
            Self::Dummy(_) => unreachable!("Should not be used in production"),
        }
    }

    pub fn is(&self, rhs: &Self) -> bool {
        self.name() == rhs.name()
    }

    pub fn from_expr<V: Variable>(
        expr: &Spanned<Intern<Expr<V>>>,
    ) -> CompResult<Vec<QualifierFragment>> {
        struct CheckFieldAccess<'s, V: Variable> {
            f: &'s dyn Fn(
                &'s Self,
                &'s Spanned<Intern<Expr<V>>>,
                Vec<QualifierFragment>,
            ) -> CompResult<Vec<QualifierFragment>>,
        }
        let cfa = CheckFieldAccess {
            f: &|cfa: &CheckFieldAccess<'_, V>,
                 e: &Spanned<Intern<Expr<V>>>,
                 mut accum: Vec<QualifierFragment>|
             -> CompResult<Vec<QualifierFragment>> {
                //dbg!(&q);
                match &*e.0 {
                    Expr::FieldAccess(l, r) => {
                        accum.push(Self::Wildcard(l.ident()?.0));

                        (cfa.f)(cfa, r, accum)
                        //self.graph.node_weight(n).cloned()
                    }

                    _ => {
                        accum.push(Self::Wildcard(e.ident()?.0));
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
            Self::Dummy(n) => write!(f, "{n}"),
        }
    }
}
