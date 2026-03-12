use std::{
    fmt::{self, Debug, Display},
    hash::Hash,
};

use crate::resource::{
    errors::CompResult,
    rep::common::{HasSpan, Ident, Spanned, Variable},
};

use chumsky::span::SimpleSpan;
use internment::Intern;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
#[repr(transparent)]
pub struct Untyped(pub Spanned<Intern<String>>);

impl Variable for Untyped {}

impl HasSpan for Untyped {
    fn span(&self) -> SimpleSpan<usize, u64> {
        self.0.1
    }
}

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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, PartialOrd, Ord, Default)]
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
    Number(ordered_float::OrderedFloat<f32>),
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

    Mul(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Div(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Add(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Sub(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Comparison(Spanned<Intern<Self>>, BinOp, Spanned<Intern<Self>>),

    Access(Spanned<Intern<Self>>, Label),

    Call(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    If(
        Spanned<Intern<Self>>,
        Spanned<Intern<Self>>,
        Spanned<Intern<Self>>,
    ),
    Lambda(V, Spanned<Intern<Self>>),
    Let(V, Spanned<Intern<Self>>, Spanned<Intern<Self>>),
}

/// Trait for entities that have Names. Implementing this trait is preferred
/// over a custom name implementation. Currently the only major type that
/// implements its own name getter is `QualifierFragment`, since it doesn't
/// carry span information (since it is statically created within the compiler)
pub trait Named<V: Variable>: std::fmt::Debug {
    // #[clippy::deny()]
    /// Internal get_name that returns a name or `None`. Users should implement this function, but shouldn't call it.
    fn get_name(&self) -> Option<Spanned<Intern<Expr<V>>>>;

    fn name(&self) -> CompResult<Spanned<Intern<Expr<V>>>>
// where
        // Self: std::fmt::Debug,
    {
        let n = self.get_name();
        match n {
            Some(d) => Ok(d),
            None => todo!("Cannot get name, {self:?}"),
            // None => DynamicErr::new(format!("Cannot get name of {:?}", self))
            // .label("here", self.to_owned()),
        }
    }
}
