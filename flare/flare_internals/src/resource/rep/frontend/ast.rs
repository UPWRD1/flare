use std::{
    fmt::{self, Debug, Display},
    hash::Hash,
};

use crate::{
    passes::frontend::typing::Type,
    resource::{
        errors::{CompResult, DynamicErr},
        rep::{common::Spanned, frontend::files::FileID},
    },
};

use chumsky::span::SimpleSpan;
use internment::Intern;
use ordered_float::OrderedFloat;

// pub trait Variable:
//     Clone + PartialEq + Debug + Eq + Hash + Copy + Sync + Send + 'static + Display
// {
// }
#[salsa::interned(debug)]
pub struct Identifier<'db> {
    #[returns(ref)]
    pub name: String,
    pub ty: Option<Type<'db>>,
}

// #[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
// #[repr(transparent)]
// pub struct Untyped(pub Spanned<Intern<String>>);

// impl Variable for Untyped {}

// impl Ident for Untyped {
//     fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
//         Ok(self.0)
//     }
// }

// impl Display for Untyped {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "{}", self.0.0)
//     }
// }

//GENERATED: Claude

/// Type representing a Pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, salsa::Update)]
pub enum Pattern<'db> {
    Wildcard,

    // Variable pattern: matches anything, binds to variable
    Var(Identifier<'db>),

    // Literal patterns: match exact values
    Number(ordered_float::OrderedFloat<f64>),
    String(Spanned<String>),
    Particle(Spanned<String>),
    Bool(bool),
    Unit,

    // Constructor pattern: matches labeled variant
    // Pattern::Ctor(label, inner_pattern)
    // Examples: Some(x), None, Ok(y), Err(msg)
    Ctor(Label, Spanned<&'db Self>),

    // Record pattern: matches record fields
    // Pattern::Record(fields, is_open)
    // Closed: {x, y} matches exactly x and y fields
    // Open: {x, y, ..} matches at least x and y fields
    Record {
        fields: &'static [(Label, Spanned<Self>)],
        open: bool, // true for {x, ..}, false for {x}
    },

    // Tuple pattern: matches fixed-size products
    Tuple(&'static [Spanned<Self>]),

    // As pattern: matches and binds to variable
    // x as Some(y) matches Some variant, binds whole to x, inner to y
    As(Identifier<'db>, Spanned<&'db Self>),

    // Or pattern: matches if any sub-pattern matches
    // Some(0) | None matches either
    Or(&'static [Self]),

    // Guard pattern: pattern with boolean condition
    // x when x > 0
    Guard(Spanned<&'db Self>, Spanned<&'db Expr<'db>>),
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, salsa::Update)]
// #[salsa::interned(debug)]
pub enum Expr<'db> {
    Ident(Identifier<'db>),
    // Param(V),
    Number(ordered_float::OrderedFloat<f32>),
    String(Spanned<Intern<String>>),
    Bool(bool),
    Unit,
    Particle(Spanned<Intern<String>>),

    Hole(Identifier<'db>),

    Item(ItemId, Kind),

    Concat(Spanned<&'db Self>, Spanned<&'db Self>),
    Project(Direction, Spanned<&'db Self>),

    Inject(Direction, Spanned<&'db Self>),
    Branch(Spanned<&'db Self>, Spanned<&'db Self>),

    Label(Label, Spanned<&'db Self>),
    Unlabel(Spanned<&'db Self>, Label),

    Pat(Spanned<Pattern<'db>>),

    Mul(Spanned<&'db Self>, Spanned<&'db Self>),
    Div(Spanned<&'db Self>, Spanned<&'db Self>),
    Add(Spanned<&'db Self>, Spanned<&'db Self>),
    Sub(Spanned<&'db Self>, Spanned<&'db Self>),
    Comparison(Spanned<&'db Self>, BinOp, Spanned<&'db Self>),

    // Access(Spanned<Intern<Self>>),
    Call(Spanned<&'db Self>, Spanned<&'db Self>),
    FieldAccess(Spanned<&'db Self>, Spanned<&'db Self>),
    MethodAccess {
        obj: Spanned<&'db Self>,
        prop: Option<Identifier<'db>>,
        method: Spanned<&'db Self>,
    },
    Myself,

    If(Spanned<&'db Self>, Spanned<&'db Self>, Spanned<&'db Self>),
    Match(Spanned<&'db Self>, &'db [MatchArm<'db>]),
    Lambda(Identifier<'db>, Spanned<&'db Self>, LambdaInfo),
    Let(Identifier<'db>, Spanned<&'db Self>, Spanned<&'db Self>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MatchArm<'db> {
    pub pat: Spanned<Pattern<'db>>,
    pub body: Spanned<Expr<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LambdaInfo {
    Anon,
    Curried,
}

impl Spanned<Expr<'_>> {
    pub fn id(&self) -> SimpleSpan<usize, u64> {
        self.1
    }
}

impl Expr<'_> {
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

impl fmt::Display for Expr<'_> {
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

#[derive(Debug, PartialEq, salsa::Update)]
pub struct ImportItem<'db> {
    pub items: Vec<Spanned<Intern<Expr<'db>>>>,
}

#[derive(Debug, Clone, Copy, PartialEq, salsa::Update)]
pub struct ImplDef<'db> {
    pub the_ty: Spanned<Intern<String>>,
    pub methods: &'db [(Identifier<'db>, Spanned<Expr<'db>>, Spanned<Type>)],
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Definition<'db> {
    Import(Spanned<Expr<'db>>),
    Type(
        Identifier<'db>,
        &'db [Spanned<Intern<Type>>],
        Spanned<Intern<Type>>,
    ),
    Let(
        Identifier<'db>,
        Spanned<Intern<Expr<'db>>>,
        Spanned<Intern<Type>>,
    ),
    Extern(
        Spanned<Intern<String>>,
        &'db [Identifier<'db>],
        Spanned<Intern<Type>>,
    ),
    ImplDef(ImplDef<'db>),
}
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ItemDefinition<'db> {
    pub def: Definition<'db>,
    pub is_pub: bool,
}
#[derive(Debug, Clone, PartialEq)]
pub struct Package<'db> {
    pub name: Spanned<Intern<String>>,
    pub items: Vec<ItemDefinition<'db>>,
}

#[salsa::tracked(debug)]
pub struct Program<'db> {
    pub packages: Vec<(Package<'db>, FileID)>,
}
