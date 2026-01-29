use std::{
    fmt::{self, Debug, Display},
    hash::Hash,
};

use crate::{
    passes::midend::typing::Type,
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

use super::Spanned;

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

//GENERATED: Claude

/// Type representing a Pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Pattern<V: Variable> {
    Wildcard,

    // Variable pattern: matches anything, binds to variable
    Var(V),

    // Literal patterns: match exact values
    Number(ordered_float::OrderedFloat<f64>),
    String(Spanned<Intern<String>>),
    Particle(Spanned<Intern<String>>),
    Bool(bool),
    Unit,

    // Constructor pattern: matches labeled variant
    // Pattern::Ctor(label, inner_pattern)
    // Examples: Some(x), None, Ok(y), Err(msg)
    Ctor(Label, Spanned<Intern<Self>>),

    // Record pattern: matches record fields
    // Pattern::Record(fields, is_open)
    // Closed: {x, y} matches exactly x and y fields
    // Open: {x, y, ..} matches at least x and y fields
    Record {
        fields: &'static [(Label, Spanned<Intern<Self>>)],
        open: bool, // true for {x, ..}, false for {x}
    },

    // Tuple pattern: matches fixed-size products
    Tuple(&'static [Spanned<Intern<Self>>]),

    // As pattern: matches and binds to variable
    // x as Some(y) matches Some variant, binds whole to x, inner to y
    As(V, Spanned<Intern<Self>>),

    // Or pattern: matches if any sub-pattern matches
    // Some(0) | None matches either
    Or(&'static [Pattern<V>]),

    // Guard pattern: pattern with boolean condition
    // x when x > 0
    Guard(Spanned<Intern<Self>>, Spanned<Intern<Expr<V>>>),
}
/// End Claude

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
    And,
    Or,
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
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Direction {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label(pub Spanned<Intern<String>>);

// impl PartialEq for Label {
//     fn eq(&self, other: &Self) -> bool {
//         self.0.0 == other.0.0
//     }
// }

// impl Hash for Label {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         self.0.0.hash(state);
//     }
// }

// impl Eq for Label {}

// impl PartialOrd for Label {
//     fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
//         Some(self.cmp(other))
//     }
// }

// impl Ord for Label {
//     fn cmp(&self, other: &Self) -> std::cmp::Ordering {
//         self.0.0.cmp(&other.0.0)
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
pub struct ItemId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Default)]
pub enum Kind {
    #[default]
    Ty,
    Func,
    Param,
    Extern(Intern<String>),
    Package,
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
    Unit,
    Particle(Spanned<Intern<String>>),

    Hole(V),

    Item(ItemId, Kind),

    Concat(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Project(Direction, Spanned<Intern<Self>>),

    Inject(Direction, Spanned<Intern<Self>>),
    Branch(Spanned<Intern<Self>>, Spanned<Intern<Self>>),

    Label(Label, Spanned<Intern<Self>>),
    Unlabel(Spanned<Intern<Self>>, Label),

    Pat(Spanned<Pattern<V>>),

    Mul(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Div(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Add(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Sub(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Comparison(Spanned<Intern<Self>>, BinOp, Spanned<Intern<Self>>),

    // Access(Spanned<Intern<Self>>),
    Call(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    FieldAccess(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    MethodAccess {
        obj: Spanned<Intern<Self>>,
        prop: Option<Spanned<Intern<String>>>,
        method: Spanned<Intern<Self>>,
    },
    Myself,

    If(
        Spanned<Intern<Self>>,
        Spanned<Intern<Self>>,
        Spanned<Intern<Self>>,
    ),
    Match(Spanned<Intern<Self>>, &'static [MatchArm<V>]),
    Lambda(V, Spanned<Intern<Self>>, LambdaInfo),
    Let(V, Spanned<Intern<Self>>, Spanned<Intern<Self>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MatchArm<V: Variable> {
    pub pat: Spanned<Intern<Pattern<V>>>,
    pub body: Spanned<Intern<Expr<V>>>,
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
}

impl<V: Variable> Expr<V> {
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
            Expr::Bool(v) => write!(f, "{v}"),
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ImplDef<V: Variable> {
    pub the_ty: Spanned<Intern<String>>,
    pub methods: &'static [(
        Spanned<Intern<String>>,
        Spanned<Intern<Expr<V>>>,
        Spanned<Intern<Type>>,
    )],
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Definition<V: Variable> {
    Import(Spanned<Intern<Expr<V>>>),
    Type(
        Spanned<Intern<String>>,
        &'static [Spanned<Intern<Type>>],
        Spanned<Intern<Type>>,
    ),
    Let(V, Spanned<Intern<Expr<V>>>, Spanned<Intern<Type>>),
    Extern(Spanned<Intern<String>>, &'static [V], Spanned<Intern<Type>>),
    ImplDef(ImplDef<V>),
}
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ItemDefinition<V: Variable> {
    pub def: Definition<V>,
    pub is_pub: bool,
}

#[derive(Debug, PartialEq)]
pub struct Package<V: Variable> {
    pub name: Spanned<Intern<String>>,
    pub items: Vec<ItemDefinition<V>>,
}

#[derive(Debug, PartialEq)]
pub struct Program<V: Variable> {
    pub packages: Vec<(Package<V>, FileID)>,
}
