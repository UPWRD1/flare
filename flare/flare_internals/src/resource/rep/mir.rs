use ordered_float::OrderedFloat;

// A normalized function body
pub struct ANF {
    pub params: Vec<Param>,
    pub body: ANFExpr,
    pub return_type: TypeId,
}

pub struct Param {
    pub id: VarId,
    pub ty: TypeId,
}

// The core ANF expression type
pub enum ANFExpr {
    // Bind a complex expression to a name, then continue
    Let {
        var: VarId,
        ty: TypeId,
        value: ComplexExpr,
        body: Box<ANFExpr>,
    },

    // Terminal expression - must be simple
    Simple(SimpleExpr),
}

// Complex expressions that produce values (need binding)
pub enum ComplexExpr {
    // Function application
    Call {
        func: SimpleExpr,
        args: Vec<SimpleExpr>,
    },

    // Data construction
    Constructor {
        tag: ConstructorId,
        fields: Vec<SimpleExpr>,
    },

    // Pattern matching
    Match {
        scrutinee: SimpleExpr,
        arms: Vec<MatchArm>,
    },

    // Primitive operations
    PrimOp {
        op: PrimOp,
        args: Vec<SimpleExpr>,
    },

    // Simple expression (could be optimized away, but useful for uniformity)
    Simple(SimpleExpr),
}

// Simple expressions - just values, no computation
pub enum SimpleExpr {
    Var(VarId),
    Literal(Literal),
    // Function references (for higher-order functions)
    FuncRef(FuncId),
}

pub struct MatchArm {
    pub pattern: Pattern,
    pub body: ANFExpr,
}

pub enum Pattern {
    Wildcard,
    Var(VarId),
    Constructor {
        tag: ConstructorId,
        fields: Vec<Pattern>,
    },
    Literal(Literal),
}

pub enum PrimOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    Gt,
    And,
    Or,
    Not,
    // etc.
}

pub enum Literal {
    Num(OrderedFloat<f64>),
    Bool(bool),
    String(String),
    Unit,
}

// Identifiers
pub type VarId = u32;
pub type TypeId = u32;
pub type FuncId = u32;
pub type ConstructorId = u32;
