use std::fmt::Display;

use internment::Intern;
use petgraph::stable_graph::NodeIndex;

use crate::{
    passes::frontend::typing::PrimitiveType,
    resource::rep::{
        common::{Spanned, Syntax},
        frontend::{
            ast::{BinOp, ExprLit, ItemId, Label, Untyped},
            csttypes::CstType,
            files::FileID,
        },
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
    Lit(ExprLit),

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
                Pattern::Lit(_) => (),
                Pattern::Variant(_, subpat) => bindings(&subpat.0, vars),
                Pattern::Record { fields, open: _ } => {
                    fields.iter().for_each(|(_, f)| bindings(&f.0, vars))
                }
                Pattern::Tuple(fields) => fields.iter().for_each(|f| bindings(&f.0, vars)),
                Pattern::At(v, _) => vars.push(*v),
                Pattern::Or(_) => todo!(),
                Pattern::Guard(_, _) => todo!(),
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
    pub is_pub: bool,
    // pub params: Intern<[S::Name]>, // non-empty = syntactic function sugar
    pub ty: Option<S::Type>,                // the annotation, if present
    pub value: Spanned<Intern<CstExpr<S>>>, // absent = abstract / extern decl
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Field<S: Syntax> {
    Def(FieldDef<S>),
    Macro(FieldMacro<S>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CstExpr<S: Syntax> {
    Ident(S::Variable),
    Lit(ExprLit),
    Hole(S::Variable),

    Item(ItemId),

    ProductConstructor {
        fields: Intern<[Field<S>]>,
    },
    VariantConstructor {
        name: S::Name,
        value: Option<Spanned<Intern<Self>>>,
    },

    Bin(Spanned<Intern<Self>>, BinOp, Spanned<Intern<Self>>),

    Call(Spanned<Intern<Self>>, Spanned<Intern<Self>>),
    FieldAccess(Spanned<Intern<Self>>, Label),
    // Myself,
    // MethodAccess {
    //     obj: Spanned<Intern<Self>>,
    //     prop: Option<Spanned<Intern<String>>>,
    //     method: Spanned<Intern<Self>>,
    // },
    Match(Spanned<Intern<Self>>, &'static [MatchArm<S>]),
    Lambda(S::Variable, Spanned<Intern<Self>>),
    Let(
        Spanned<Intern<Pattern<S>>>,
        Spanned<Intern<Self>>,
        Spanned<Intern<Self>>,
    ),
    Type(S::Type),
}

impl<S: Syntax> Default for CstExpr<S> {
    fn default() -> Self {
        Self::Lit(ExprLit::default())
    }
}

#[derive(Debug, PartialEq)]
pub struct ImportItem<S: Syntax> {
    pub items: Vec<Spanned<Intern<CstExpr<S>>>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Extend<S: Syntax> {
    pub the_ty: S::Expr,
    pub with: S::Expr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FieldMacro<S: Syntax> {
    Import(S::Pattern),
    Extend(Extend<S>),
    Ret(S::Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug)]
pub struct Package<S: Syntax> {
    pub root_node: Field<S>,
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

#[derive(Debug, Clone)]
pub enum NodeKind {
    /// Input 0: its value.
    /// Output 0: itself (for references).
    Def {
        name: Intern<String>,
    },

    /// A reference to a Def. Output: the Def's value.
    /// During reduction this edge is simply short-circuited.
    Ref,

    /// λ x. body
    /// Input 0: the parameter binding (a Def node for x)
    /// Input 1: the body expression
    /// Output 0: the lambda value
    Lam,

    Bin(BinOp),

    /// f a
    /// Input 0: function
    /// Input 1: argument
    /// Output 0: result
    App,

    /// { ℓ₁ = e₁, ℓ₂ = e₂, … }
    /// Input i: the i-th field value
    /// Output 0: the record
    Record,

    /// e.ℓ
    /// Input 0: the record
    /// Output 0: the field value
    Project {
        label: Intern<String>,
    },

    /// e ⊕ { ℓ = e' }
    /// Input 0: base record, Input 1: extension value
    /// Output 0: extended record
    Extend {
        label: Intern<String>,
    },

    /// Tag a value with a label: ℓ(e)
    /// Input 0: payload
    /// Output 0: tagged value
    Inject {
        label: Intern<String>,
    },

    /// Input 0: left branch
    /// Input 1: right branch
    /// Output 0: the branch node
    Branch,

    /// Input 0: The tagged value
    /// Output 0: The untagged value
    Unlabel {
        label: Intern<String>,
    },

    /// Input 0: condition term
    /// Input 1: "then" arm
    /// Input 2: "else" arm
    If,

    Lit(ExprLit),

    PrimitiveTy(PrimitiveType),

    /// ★ — the universe
    Universe {
        level: u32,
    },

    /// Π (x : A). B
    /// Input 0: domain type, Input 1: codomain (a Lam over x)
    /// Output 0: the Pi type (itself a type)
    Pi,

    /// A row type: { ℓ₁ : T₁, ℓ₂ : T₂ | ρ }
    RowTy {
        labels: Vec<Intern<String>>,
        open: bool,
    },

    /// μ self. body — recursive type/value
    /// Input 0: body (a Lam with self bound)
    /// Output 0: the fixed point
    Mu,

    // ── Core (post-reduction only) ────────────────────────────────────────
    /// A fully reduced function item (like your current IR::Fun).
    /// No inputs during emission; just carried as a node.
    CoreLam {
        arity: usize,
    },

    /// Unreachable / erased node
    Erased,

    Hole {
        name: Intern<String>,
    },
}

impl std::fmt::Display for NodeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                NodeKind::Def { name } => name.to_string(),
                Self::Hole { name } => format!("Hole {name}"),
                _ => format!("{:?}", self),
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
}

/// An edge connects one output port to one output port.
/// In an interaction net, edges are symmetric — neither end is "from" or "to".
/// We use directed edges for clarity: producer → consumer.
#[derive(Debug, Clone, Copy)]
pub enum PortKind {
    Input(usize),
    Output,
    Reference,
    Type,
}
