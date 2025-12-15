use std::{
    fmt::{self, Debug, Display},
    hash::Hash,
};

use crate::{
    passes::midend::typing::{Row, Type, TypeScheme},
    resource::{
        errors::{CompResult, DynamicErr},
        rep::{
            common::{Ident, Named},
            files::FileID,
        },
    },
};

use chumsky::span::SimpleSpan;
use internment::Intern;
use ordered_float::OrderedFloat;

use super::{
    Spanned,
    // concretetypes::{EnumVariant, Ty},
    quantifier::QualifierFragment,
};

pub trait Variable:
    Clone + PartialEq + Debug + Eq + Hash + Copy + Sync + Send + 'static + Ident + Display
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

impl Display for Untyped {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.0)
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
    Type(Spanned<Intern<Type>>),
}

/// Type representing a Pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Pattern<V: Variable> {
    Atom(PatternAtom<V>),

    Tuple(Intern<Vec<Spanned<Self>>>),

    Variant(Spanned<Intern<Expr<V>>>, Intern<Vec<Spanned<Self>>>),
}

impl<V: Variable> Ident for Spanned<Pattern<V>> {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        match self.0 {
            Pattern::Variant(n, _) => n.ident(),
            Pattern::Atom(a) => match a {
                PatternAtom::Variable(s) => s.ident(),
                _ => panic!(), // errors::bad_ident(expr, s),
            },
            Pattern::Tuple(_) => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum BinOp {
    Eq,
    Neq,
    Gt,
    Lt,
    Gte,
    Lte,
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Eq => write!(f, "=="),
            BinOp::Neq => write!(f, "!="),
            BinOp::Gt => write!(f, ">"),
            BinOp::Lt => write!(f, "<"),
            BinOp::Gte => write!(f, ">="),
            BinOp::Lte => write!(f, "<="),
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Direction {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy)]
pub struct Label(pub Spanned<Intern<String>>);

impl PartialEq for Label {
    fn eq(&self, other: &Self) -> bool {
        self.0.0 == other.0.0
    }
}

impl Hash for Label {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.0.hash(state);
    }
}

impl Eq for Label {}

impl PartialOrd for Label {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Label {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.0.cmp(&other.0.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
pub struct ItemId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Kind {
    Ty,
    Func,
    Param,
    Extern(&'static str),
}

/// Type representing an Expression.
/// You will typically encounter ```Expr<V>``` as a ```Spanned<Expr<V>>```, which is decorated with a span for diagnostic information.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Expr<V>
where
    V: Variable,
{
    Ident(V),
    // Param(V),
    Number(ordered_float::OrderedFloat<f64>),
    String(Spanned<Intern<String>>),
    Bool(bool),

    Particle(Spanned<Intern<String>>),

    Hole(V),

    Item(ItemId, Kind),

    Concat(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Project(Direction, Spanned<Intern<Self>>),

    Inject(Direction, Spanned<Intern<Self>>),
    Branch(Spanned<Intern<Self>>, Spanned<Intern<Self>>),

    Label(Label, Spanned<Intern<Self>>),
    Unlabel(Spanned<Intern<Self>>, Label),

    ExternFunc(ItemId, Spanned<Intern<String>>, Spanned<Intern<Type>>),
    Unit,

    Pat(Spanned<Pattern<V>>),

    Mul(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Div(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Add(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Sub(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Comparison(Spanned<Intern<Self>>, BinOp, Spanned<Intern<Self>>),

    // Access(Spanned<Intern<Self>>),
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
    Lambda(V, Spanned<Intern<Self>>, LambdaInfo),
    Let(V, Spanned<Intern<Self>>, Spanned<Intern<Self>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LambdaInfo {
    Anon,
    Curried,
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

            // Expr::Access(expr) => expr.name().ok(),
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
            Expr::Number(n) => {
                let n: usize = if n.fract() > 0.0 {
                    Err(DynamicErr::new(
                        "Cannot use a floating point value as an index",
                    ))
                } else {
                    Ok(n.trunc() as usize)
                }?;

                Ok(self.convert(n.to_string()))
            }
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

impl<V: Variable> fmt::Display for Expr<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Ident(n) => write!(f, "{}", n),
            Expr::Number(ordered_float) => write!(f, "{ordered_float}"),
            Expr::String(s) => write!(f, "\"{s}\""),
            Expr::Bool(v) => todo!(),
            Expr::Hole(_) => todo!(),

            Expr::Lambda(v, b, _) => write!(f, "|{v}| {b}"),
            Expr::Call(l, r) => write!(f, "{l}({r})"),

            Expr::Add(l, r) => write!(f, "{l} + {r}"),
            Expr::Sub(l, r) => write!(f, "{l} - {r}"),
            Expr::Mul(l, r) => write!(f, "{l} * {r}"),
            Expr::Div(l, r) => write!(f, "{l} / {r}"),
            _ => write!(f, "{self:?}"),
        }
    }
}

// #[derive(Debug, PartialEq)]
// pub struct StructDef {
//     // pub the_ty: Row,
//     //pub generics: Vec<Spanned<Expr<V>>>,
// }

#[derive(Debug, PartialEq)]
pub struct TypeDecl {
    pub name: Spanned<Intern<String>>,
    pub ty: Type,
}

#[derive(Debug, PartialEq)]
pub struct ImportItem<V: Variable> {
    pub items: Vec<Spanned<Intern<Expr<V>>>>,
}

#[derive(Debug, PartialEq)]
pub struct ImplDef<V: Variable> {
    pub the_ty: Spanned<Intern<String>>,
    pub methods: Vec<(
        Spanned<Intern<String>>,
        Spanned<Intern<Expr<V>>>,
        Spanned<Intern<Type>>,
    )>,
}

#[derive(Debug, PartialEq)]
pub enum Definition<V: Variable> {
    Import(Spanned<Intern<Expr<V>>>),
    Type(Spanned<Intern<String>>, Spanned<Intern<Type>>),
    Let(V, Spanned<Intern<Expr<V>>>, Spanned<Intern<Type>>),
    Extern(Spanned<Intern<String>>, Spanned<Intern<Type>>),
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
