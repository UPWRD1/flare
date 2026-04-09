use std::{
    fmt::{self, Debug, Display},
    hash::Hash,
};

use crate::{
    passes::frontend::typing::TypeScheme,
    resource::{
        errors::{CompResult, DynamicErr},
        rep::common::{HasSpan, Ident, Spanned, Syntax, Variable},
    },
};

use chumsky::span::SimpleSpan;
use internment::Intern;
use ordered_float::OrderedFloat;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UntypedAst;

impl Syntax for UntypedAst {
    type Expr = Spanned<Intern<Expr<Self::Variable>>>;
    // type Type = Spanned<Intern<Type>>;
    type Type = TypeScheme;
    type Variable = Untyped;
    type Name = Spanned<Intern<String>>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Direction {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label(pub Spanned<Intern<String>>);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, PartialOrd, Ord, Default)]
pub struct ItemId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Copy, Default)]
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
impl<V: Variable> Named<V> for Spanned<Intern<Expr<V>>> {
    fn get_name(&self) -> Option<Spanned<Intern<Expr<V>>>> {
        match *self.0 {
            Expr::Ident(_) => Some(*self),
            // Expr::Access(expr) => expr.name().ok(),
            Expr::Call(func, _) => func.name().ok(),
            _ => None,
        }
    }
}

impl<V: Variable> Spanned<Intern<Expr<V>>> {
    pub fn id(&self) -> SimpleSpan<usize, u64> {
        self.1
    }
}

impl<V: Variable> Expr<V> {
    pub fn get_num(&self, span: SimpleSpan<usize, u64>) -> CompResult<OrderedFloat<f32>> {
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
