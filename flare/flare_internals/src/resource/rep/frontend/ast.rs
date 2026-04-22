use std::{
    fmt::{self, Debug, Display},
    hash::Hash,
};

use crate::{
    passes::frontend::typing::TypeScheme,
    resource::{
        errors::CompResult,
        rep::common::{FlareSpan, HasSpan, Spanned, Syntax, Variable},
    },
};

use internment::Intern;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UntypedAst;

impl Syntax for UntypedAst {
    type Expr = Spanned<Intern<Expr<Self::Variable>>>;
    type Type = Intern<TypeScheme>;
    type Pattern = ();
    type Variable = Untyped;
    type Name = Spanned<Intern<String>>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Untyped(pub Spanned<Intern<String>>);

impl Variable for Untyped {}

impl HasSpan for Untyped {
    fn span(&self) -> FlareSpan {
        self.0.1
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum Expr<V>
where
    V: Variable,
{
    Ident(V),
    // Param(V),
    Number(ordered_float::OrderedFloat<f32>),
    String(Spanned<Intern<String>>),
    Bool(bool),
    #[default]
    Unit,
    Particle(Spanned<Intern<String>>),
    Hole(V),

    Item(ItemId),

    Concat(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Project(Direction, Spanned<Intern<Self>>),
    Inject(Direction, Spanned<Intern<Self>>),
    Branch(Spanned<Intern<Self>>, Spanned<Intern<Self>>),

    Label(Label, Spanned<Intern<Self>>),
    Unlabel(Spanned<Intern<Self>>, Label),

    // ProductConstructor {
    // fields: Intern<[(Label, Spanned<Intern<Self>>)]>,
    // },
    Bin(Spanned<Intern<Self>>, BinOp, Spanned<Intern<Self>>),

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
    pub fn id(&self) -> FlareSpan {
        self.1
    }

    pub fn collect_labels(self) -> Vec<(Label, Spanned<Intern<Expr<V>>>)> {
        let mut v = vec![];
        fn collect_labels<V: Variable>(
            row: Spanned<Intern<Expr<V>>>,
            out: &mut Vec<(Label, Spanned<Intern<Expr<V>>>)>,
        ) {
            match *row.0 {
                Expr::Label(label, v) => out.push((label, v)),
                Expr::Concat(r1, r2) => {
                    collect_labels(r1, out);
                    collect_labels(r2, out);
                }
                _ => panic!("Not an expr"),
            }
        }
        collect_labels(self, &mut v);
        v
    }
}
