use internment::Intern;
use rustc_hash::FxHashMap;

use crate::resource::rep::{
    common::{Spanned, Syntax},
    frontend::{
        ast::{BinOp, ItemId, Kind, Label, Untyped},
        csttypes::CstType,
        files::FileID,
    },
};

/// Type representing a Pattern.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Pattern<S: Syntax> {
    Hole(S::Variable),

    Any,

    // Variable pattern: matches anything, binds to variable
    Var(S::Variable),

    // Literal patterns: match exact values
    Number(ordered_float::OrderedFloat<f32>),
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

    // At pattern: matches and binds to variable
    // x @ Some(y) matches Some variant, binds whole to x, inner to y
    At(S::Variable, Spanned<Intern<Self>>),

    // Or pattern: matches if any sub-pattern matches
    // Some(0) | None matches either
    Or(&'static [Pattern<S>]),

    // Guard pattern: pattern with boolean condition
    // x when x > 0
    Guard(Spanned<Intern<Self>>, Spanned<Intern<CstExpr<S>>>),
}
impl<S: Syntax> Pattern<S> {
    pub fn bindings(&self) -> Vec<S::Variable> {
        fn bindings<S: Syntax>(p: &Pattern<S>, vars: &mut Vec<S::Variable>) {
            match p {
                Pattern::Hole(_) | Pattern::Any => (),
                Pattern::Var(v) => vars.push(*v),
                Pattern::Number(_)
                | Pattern::String(_)
                | Pattern::Particle(_)
                | Pattern::Bool(_)
                | Pattern::Unit => (),
                Pattern::Variant(_, subpat) => bindings(&subpat.0, vars),
                Pattern::Record { fields, open } => {
                    fields.iter().for_each(|(_, f)| bindings(&f.0, vars))
                }
                Pattern::Tuple(fields) => fields.iter().for_each(|f| bindings(&f.0, vars)),
                Pattern::At(v, _) => vars.push(*v),
                Pattern::Or(patterns) => todo!(),
                Pattern::Guard(spanned, spanned1) => todo!(),
            }
        }
        let mut v = vec![];
        bindings(self, &mut v);
        v
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MatchArm<S: Syntax> {
    pub pat: Spanned<Intern<Pattern<S>>>,
    pub body: Spanned<Intern<CstExpr<S>>>,
}

/// One field_assignment at the top level or inside a fielded_constructor.
/// All three surface forms collapse here.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldDef<S: Syntax> {
    pub name: S::Name,
    // pub params: Intern<[S::Name]>, // non-empty = syntactic function sugar
    pub ty: Option<S::Type>,                // the annotation, if present
    pub value: Spanned<Intern<CstExpr<S>>>, // absent = abstract / extern decl
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CstExpr<S: Syntax> {
    Ident(S::Variable),
    Number(ordered_float::OrderedFloat<f32>),
    String(Spanned<Intern<String>>),
    Bool(bool),
    Unit,
    Particle(Spanned<Intern<String>>),

    Hole(S::Variable),

    Item(ItemId, Kind),

    ProductConstructor {
        fields: Intern<[FieldDef<S>]>,
    },

    Label(Label, Spanned<Intern<Self>>),
    Unlabel(Spanned<Intern<Self>>, Label),

    Pat(Spanned<Pattern<S>>),
    Mul(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Div(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Add(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Sub(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    Comparison(Spanned<Intern<Self>>, BinOp, Spanned<Intern<Self>>),

    Call(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    FieldAccess(Spanned<Intern<Self>>, Label),
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
    Match(Spanned<Intern<Self>>, &'static [MatchArm<S>]),
    Lambda(S::Variable, Spanned<Intern<Self>>),
    Let(
        // S::Variable,
        Spanned<Intern<Pattern<S>>>,
        Spanned<Intern<Self>>,
        Spanned<Intern<Self>>,
    ),
    // Type(S::Type),
}

#[derive(Debug, PartialEq)]
pub struct ImportItem<S: Syntax> {
    pub items: Vec<Spanned<Intern<CstExpr<S>>>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ImplDef<S: Syntax> {
    pub the_ty: S::Name,
    pub methods: &'static [(S::Name, S::Expr, S::Type)],
}

#[derive(Debug, Clone, PartialEq)]
pub enum Macro<S: Syntax> {
    Import(S::Pattern),
    Type(S::Type, S::Type),
    Extern(S::Name, &'static [S::Variable], S::Type),
    ImplDef(ImplDef<S>),
    Name(S::Name),
    Pub(&'static Self),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug)]
pub struct Package<S: Syntax> {
    pub macros: FxHashMap<CstExpr<S>, Vec<Macro<S>>>,
    pub root_node: FieldDef<S>,
}

#[derive(Default, Debug)]
pub struct PackageCollection<S: Syntax> {
    pub packages: Vec<(Package<S>, FileID)>,
}

impl<S: Syntax> PackageCollection<S> {
    pub fn merge(mut self, rhs: Self) -> Self {
        self.packages.extend(rhs.packages);
        self
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UntypedCst;

impl Syntax for UntypedCst {
    type Expr = Spanned<Intern<CstExpr<UntypedCst>>>;
    type Type = Spanned<Intern<CstType>>;
    type Pattern = Spanned<Intern<Pattern<UntypedCst>>>;
    type Variable = Untyped;
    type Name = Spanned<Intern<String>>;
}
