use internment::Intern;

use crate::resource::{
    errors::CompResult,
    rep::{
        common::{Ident, Spanned, Syntax, Variable},
        frontend::{
            ast::{BinOp, Direction, ItemId, Kind, Label, Untyped},
            csttypes::CstType,
            files::FileID,
        },
    },
};

/// Type representing a Pattern.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Pattern<V: Variable> {
    Any,

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
    Variant(Label, Spanned<Intern<Self>>),

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
    Guard(Spanned<Intern<Self>>, Spanned<Intern<CstExpr<V>>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MatchArm<V: Variable> {
    pub pat: Spanned<Intern<Pattern<V>>>,
    pub body: Spanned<Intern<CstExpr<V>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum CstExpr<V>
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

    Pat(Spanned<Pattern<V>>),

    Mul(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Div(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Add(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Sub(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Comparison(Spanned<Intern<Self>>, BinOp, Spanned<Intern<Self>>),

    Call(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    FieldAccess(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Myself,
    MethodAccess {
        obj: Spanned<Intern<Self>>,
        prop: Option<Spanned<Intern<String>>>,
        method: Spanned<Intern<Self>>,
    },
    If(
        Spanned<Intern<Self>>,
        Spanned<Intern<Self>>,
        Spanned<Intern<Self>>,
    ),
    Match(Spanned<Intern<Self>>, &'static [MatchArm<V>]),
    Lambda(V, Spanned<Intern<Self>>),
    Let(
        V,
        // Spanned<Intern<Pattern<V>>>,
        Spanned<Intern<Self>>,
        Spanned<Intern<Self>>,
    ),
}

impl<V: Variable> Ident for Spanned<Intern<CstExpr<V>>> {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        match *self.0 {
            CstExpr::Ident(v) => v.ident(),
            _ => panic!("Cannot Ident: {self:?}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ImportItem<V: Variable> {
    pub items: Vec<Spanned<Intern<CstExpr<V>>>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ImplDef<S: Syntax> {
    pub the_ty: S::Name,
    pub methods: &'static [(S::Name, S::Expr, S::Type)],
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Definition<S: Syntax> {
    Import(S::Expr),
    Type(S::Name, &'static [S::Type], S::Type),
    Let(S::Name, S::Expr, S::Type),
    Extern(S::Name, &'static [S::Variable], S::Type),
    ImplDef(ImplDef<S>),
}
#[derive(Debug, Clone, PartialEq)]
pub struct ItemDefinition<S: Syntax> {
    pub def: Definition<S>,
    pub is_pub: bool,
}

#[derive(Debug, PartialEq)]
pub struct Package<S: Syntax> {
    pub name: S::Name,
    pub items: Vec<ItemDefinition<S>>,
}

#[derive(Debug, PartialEq)]
pub struct Program<S: Syntax> {
    pub packages: Vec<(Package<S>, FileID)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UntypedCst;

impl Syntax for UntypedCst {
    type Expr = Spanned<Intern<CstExpr<Self::Variable>>>;
    type Type = Spanned<Intern<CstType>>;
    type Variable = Untyped;
    type Name = Spanned<Intern<String>>;
}
