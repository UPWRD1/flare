use crate::resource::{
    errors::CompResult,
    rep::{
        common::{Ident, Spanned, Variable},
        frontend::cst::CstExpr,
    },
};

// use chumsky::input::ValueInput;
use internment::Intern;
use radix_trie::TrieKey;
use rustc_hash::FxHashSet;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FullQualifier(Vec<QualifierFragment>);
impl TrieKey for FullQualifier {
    fn encode_bytes(&self) -> Vec<u8> {
        self.0
            .iter()
            .flat_map(|q| q.name().to_string().into_bytes())
            .collect()
    }
}

impl From<Vec<QualifierFragment>> for FullQualifier {
    fn from(value: Vec<QualifierFragment>) -> Self {
        Self(value)
    }
}

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
        match (self, rhs) {
            (Self::Wildcard(_), _) | (_, Self::Wildcard(_)) => self.is_unstrict(rhs),
            (_, _) => self == rhs,
        }
    }

    pub fn is_unstrict(&self, rhs: &Self) -> bool {
        self.name() == rhs.name()
    }

    #[allow(clippy::type_complexity)]
    pub fn from_expr<V: Variable>(
        expr: &Spanned<Intern<CstExpr<V>>>,
    ) -> CompResult<FxHashSet<Vec<QualifierFragment>>> {
        struct CheckFieldAccess<'rec, V: Variable> {
            f: &'rec dyn Fn(
                &'rec Self,
                &'rec Spanned<Intern<CstExpr<V>>>,
                &[QualifierFragment],
                &mut FxHashSet<Vec<QualifierFragment>>,
            ) -> CompResult<Vec<QualifierFragment>>,
        }
        let mut paths = FxHashSet::default();
        let cfa = CheckFieldAccess {
            f: &|cfa: &CheckFieldAccess<'_, V>,
                 e: &Spanned<Intern<CstExpr<V>>>,
                 accum: &[QualifierFragment],
                 paths: &mut FxHashSet<Vec<QualifierFragment>>|
             -> CompResult<Vec<QualifierFragment>> {
                match &*e.0 {
                    CstExpr::FieldAccess(l, r) => {
                        let accum = if accum.is_empty() {
                            [(Self::Package(l.ident()?.0))].to_vec()
                        } else {
                            [accum, &[(Self::Wildcard(l.ident()?.0))]].concat()
                        };

                        (cfa.f)(cfa, r, &accum, paths)
                        //self.graph.node_weight(n).cloned()
                    }
                    CstExpr::Concat(l, r) => {
                        (cfa.f)(cfa, l, accum, paths)?;
                        (cfa.f)(cfa, r, accum, paths)?;
                        // paths.insert(l_path);
                        // paths.insert(r_path);
                        Ok(accum.to_vec())
                    }

                    CstExpr::Label(_, v) => {
                        let accum = [accum, &[(Self::Wildcard(v.ident()?.0))]].concat();
                        paths.insert(accum.clone());
                        Ok(accum.to_vec())
                    }

                    _ => {
                        let accum = [accum, &[(Self::Wildcard(e.ident()?.0))]].concat();
                        paths.insert(accum.clone());
                        Ok(accum.to_vec())
                    }
                }
            },
        };
        (cfa.f)(&cfa, expr, &[], &mut paths)?;
        // dbg!(&paths);
        Ok(paths)
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
