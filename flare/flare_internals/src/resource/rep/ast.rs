use std::{fmt::Debug, hash::Hash};

use crate::resource::{
    errors::{CompResult, DynamicErr},
    rep::{
        common::{Ident, Named},
        files::FileID,
    },
};

use chumsky::span::SimpleSpan;
use internment::Intern;
use ordered_float::OrderedFloat;
use petgraph::Direction::Outgoing;

use super::{
    concretetypes::{EnumVariant, Ty},
    quantifier::QualifierFragment,
    Spanned,
};

pub trait Variable:
    Clone + PartialEq + Debug + Eq + Hash + Copy + Sync + Send + 'static + Ident
{
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
#[repr(transparent)]
pub struct Untyped(pub Spanned<Intern<String>>);

impl Variable for Untyped {}

impl Ident for Untyped {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        Ok(self.0)
    }
}

/// Type representing an atomic value within a pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum PatternAtom<V: Variable> {
    // #[serde(deserialize_with = "deserialize_static_str")]
    Strlit(Intern<String>),
    Num(OrderedFloat<f64>),

    // #[serde(deserialize_with = "deserialize_static_str")]
    Variable(Spanned<Intern<Expr<V>>>),

    // #[serde(deserialize_with = "deserialize_static")]
    Type(Spanned<Intern<Ty>>),
}

/// Type representing a Pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Pattern<V: Variable> {
    Atom(PatternAtom<V>),

    Tuple(Intern<Vec<Spanned<Self>>>),

    Variant(Spanned<Intern<Expr<V>>>, Intern<Vec<Spanned<Self>>>),
}

// impl<V: Variable> Named for Spanned<Pattern<V>> {
//     fn get_name(&self) -> Option<Spanned<Intern<Expr<V>>>> {
//         match self.0 {
//             Pattern::Variant(n, _) => n.name().ok(),
//             Pattern::Atom(a) => match a {
//                 PatternAtom::Type(t) => t.name().ok(),
//                 PatternAtom::Variable(s) => s.name().ok(),
//                 _ => None, // errors::bad_ident(expr, s),
//             },
//             Pattern::Tuple(_) => None,
//         }
//     }
// }

impl<V: Variable> Ident for Spanned<Pattern<V>> {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        match self.0 {
            Pattern::Variant(n, _) => n.ident(),
            Pattern::Atom(a) => match a {
                PatternAtom::Type(t) => t.ident(),
                PatternAtom::Variable(s) => s.ident(),
                _ => panic!(), // errors::bad_ident(expr, s),
            },
            Pattern::Tuple(_) => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum ComparisonOp {
    Eq,
    Neq,
    Gt,
    Lt,
    Gte,
    Lte,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Direction {
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct Label(pub Spanned<Intern<String>>);

impl PartialOrd for Label {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Label {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0 .0.cmp(&other.0 .0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct ItemId(pub usize);

/// Type representing an Expression.
/// You will typically encounter ```Expr<V>``` as a ```Spanned<Expr<V>>```, which is decorated with a span for diagnostic information.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Expr<V>
where
    V: Variable,
{
    Ident(V),
    Number(ordered_float::OrderedFloat<f64>),
    String(Spanned<Intern<String>>),
    Bool(bool),

    Hole(V),

    Item(ItemId),

    Concat(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Project(Direction, Spanned<Intern<Self>>),

    Inject(Direction, Spanned<Intern<Self>>),
    Branch(Spanned<Intern<Self>>, Spanned<Intern<Self>>),

    Label(Label, Spanned<Intern<Self>>),
    Unlabel(Spanned<Intern<Self>>, Label),

    ExternFunc(Intern<Vec<QualifierFragment>>),
    Unit,
    // Constructor(Spanned<Intern<Self>>, Vec<Spanned<Self>>),
    Constructor(Spanned<Intern<Self>>, Intern<Vec<Spanned<Intern<Self>>>>),
    FieldedConstructor(
        Spanned<Intern<Self>>,
        Intern<Vec<(Spanned<Intern<Self>>, Spanned<Intern<Self>>)>>,
    ),

    Pat(Spanned<Pattern<V>>),

    Mul(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Div(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Add(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Sub(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Comparison(Spanned<Intern<Self>>, ComparisonOp, Spanned<Intern<Self>>),

    Access(Spanned<Intern<Self>>),
    Call(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    FieldAccess(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    MethodAccess(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Myself,

    If(
        Spanned<Intern<Self>>,
        Spanned<Intern<Self>>,
        Spanned<Intern<Self>>,
    ),
    Match(
        Spanned<Intern<Self>>,
        Intern<Vec<(Spanned<Pattern<V>>, Spanned<Intern<Self>>)>>,
    ),
    Lambda(V, Spanned<Intern<Self>>, bool),
    Let(V, Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Struct(Intern<Vec<(Spanned<Intern<Self>>, Spanned<Intern<Self>>)>>),
    Tuple(Intern<Vec<Spanned<Intern<Self>>>>),
}

impl<V: Variable> Named<V> for Spanned<Intern<Expr<V>>> {
    fn get_name(&self) -> Option<Spanned<Intern<Expr<V>>>> {
        match *self.0 {
            Expr::Ident(_) => Some(*self),
            Expr::FieldAccess(base, _field) => {
                base.name().ok()
                //todo!()
                //Some(base.0.get_ident()?.append(field.0.get_ident()?))
            }
            Expr::Access(expr) => expr.name().ok(),
            Expr::Call(func, _) => func.name().ok(),
            Expr::Pat(p) => {
                if let Pattern::Atom(PatternAtom::Variable(s)) = &p.0 {
                    s.name().ok()
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl<V: Variable> Ident for Spanned<Intern<Expr<V>>> {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        match *self.0 {
            Expr::Ident(s) => s.ident(),
            _ => self.name()?.ident(), // _ => Err(DynamicErr::new("cannot get ident")
                                       //     .label(format!("{self:?}"), self.get_span())
                                       //     .into()),
        }
    }
}

pub type NodeId = SimpleSpan<usize, u64>;

impl<V: Variable> Spanned<Intern<Expr<V>>> {
    pub fn id(&self) -> SimpleSpan<usize, u64> {
        self.1
    }

    pub fn parents_of(&self, id: NodeId) -> Option<Vec<Self>> {
        match *self.0 {
            Expr::Ident(_) | Expr::Number(_) | Expr::String(_) | Expr::Unit | Expr::Bool(_) => None,
            Expr::Call(fun, arg) => {
                if id == fun.id() || id == arg.id() {
                    return Some(vec![*self]);
                }
                fun.parents_of(id)
                    .or_else(|| arg.parents_of(id))
                    .map(|mut parents| {
                        parents.push(*self);
                        parents
                    })
            }
            Expr::Lambda(_, body, _) => {
                if id == body.id() {
                    return Some(vec![*self]);
                }
                body.parents_of(id).map(|mut parents| {
                    parents.push(*self);
                    parents
                })
            }
            Expr::Let(_, spanned1, spanned2) => todo!(),
            _ => todo!(),
        }
    }

    pub fn parent_of(&self, id: NodeId) -> Option<Self> {
        // The first element of `parents_of` will be the nearest parent to `id`
        self.parents_of(id)
            .and_then(|parents| parents.into_iter().next())
    }
}

impl<V: Variable> Expr<V> {
    // #[inline]
    // pub fn get_ident(&self, span: SimpleSpan<usize, u64>) -> CompResult<Spanned<Intern<String>>> {
    //     match self {
    //         Expr<V>::Ident(s) => Ok(*s),
    //         _ => Err(DynamicErr::new("cannot get ident")
    //             .label(format!("{self:?}"), span)
    //             .into()),
    //     }
    // }

    pub fn get_num(&self, span: SimpleSpan<usize, u64>) -> CompResult<OrderedFloat<f64>> {
        match self {
            Self::Number(n) => Ok(*n),
            _ => Err(DynamicErr::new("Not a number").label("here", span).into()),
        }
    }

    pub fn inject_call_start(
        self,
        arg: Spanned<Intern<Self>>,
        span: SimpleSpan<usize, u64>,
    ) -> Spanned<Intern<Self>> {
        match self {
            Self::Call(l, r) => Spanned(
                Intern::from(Self::Call(l.0.inject_call_start(arg, span), r)),
                span,
            ),
            Self::Ident(_n) => Spanned(
                Intern::from(Self::Call(Spanned(Intern::from(self), span), arg)),
                span,
            ),
            _ => panic!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StructDef {
    pub the_ty: Spanned<Intern<Ty>>,
    pub fields: Vec<(Spanned<Intern<String>>, Spanned<Intern<Ty>>)>,
    //pub generics: Vec<Spanned<Expr<V>>>,
}

#[derive(Debug, PartialEq)]
pub struct EnumDef {
    pub the_ty: Spanned<Intern<Ty>>,
    pub variants: Vec<Spanned<EnumVariant>>,
}

#[derive(Debug, PartialEq)]
pub struct ImportItem<V: Variable> {
    pub items: Vec<Spanned<Intern<Expr<V>>>>,
}

#[derive(Debug, PartialEq)]
pub struct ImplDef<V: Variable> {
    pub the_ty: Spanned<Intern<Ty>>,
    pub methods: Vec<(
        Spanned<Intern<String>>,
        Spanned<Intern<Expr<V>>>,
        Spanned<Intern<Ty>>,
    )>,
}

#[derive(Debug, PartialEq)]
pub enum Definition<V: Variable> {
    Import(Spanned<Intern<Expr<V>>>),
    Struct(StructDef),
    Enum(EnumDef),
    Let(V, Spanned<Intern<Expr<V>>>, Option<Spanned<Intern<Ty>>>),
    Extern(Spanned<Intern<String>>, Spanned<Intern<Ty>>),
    ImplDef(ImplDef<V>),
}

#[derive(Debug, PartialEq)]
pub struct Package<V: Variable> {
    pub name: Spanned<Intern<String>>,
    pub items: Vec<Definition<V>>,
}

#[derive(Debug, PartialEq)]
pub struct Program<V: Variable> {
    pub packages: Vec<(Package<V>, FileID)>,
}
