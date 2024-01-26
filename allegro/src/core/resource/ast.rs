use super::tokens::Token;
use super::environment::AKind;

#[derive(Clone, Debug, PartialEq)]
pub struct AssignExpr {
    pub name: Token,
    pub kind: AKind,
    pub value: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub paren: Token,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct GroupExpr {
    pub expression: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LiteralExpr {
    pub value: Token,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LogicalExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ValueExpr {
    pub name: Token,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Expr {
    Assign(AssignExpr),
    Binary(BinExpr),
    Call(CallExpr),
    Grouping(GroupExpr),
    Literal(LiteralExpr),
    Logical(LogicalExpr),
    Unary(UnaryExpr),
    Value(ValueExpr),
    Empty,
}

impl Expr {
    pub fn get_expr_value(&mut self) -> Token {
        match self {
            Self::Assign(a) => a.name.clone(),
            Self::Binary(b) => b.operator.clone(),
            Self::Call(c) => c.paren.clone(),
            Self::Empty => {
                panic!("Cannot get value of empty expression!")
            }
            Self::Grouping(g) => g.expression.get_expr_value().clone(),
            Self::Literal(l) => l.value.clone(),
            Self::Logical(l) => l.operator.clone(),
            Self::Unary(u) => u.operator.clone(),
            Self::Value(v) => v.name.clone(),
        }
    }

    pub fn get_expr_type(&mut self) -> Option<AKind> {
        match self {
            Self::Assign(a) => Some(a.kind.clone()),
            Self::Binary(b) => b.left.clone().get_expr_type(),
            Self::Call(c) => c.callee.get_expr_type(),
            Self::Empty => {
                panic!("Cannot get value of empty expression!")
            }
            Self::Grouping(g) => g.expression.get_expr_type().clone(),
            Self::Literal(l) => l.value.kind.clone(),
            Self::Logical(l) => l.left.get_expr_type().clone(),
            Self::Unary(u) => u.right.get_expr_type().clone(),
            Self::Value(v) => v.name.kind.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BlockStmt {
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExpressionStmt {
    pub expression: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct OpDecl {
    pub name: Token,
    pub params: Vec<ValDecl>,
    pub kind: AKind,
    pub body: BlockStmt,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Statement>,
    pub else_branch: Option<Box<Statement>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrintStmt {
    pub expression: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnStmt {
    pub value: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ValDecl {
    pub name: Token,
    pub kind: AKind,
    pub initializer: Expr,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Statement {
    Block(BlockStmt),
    Expression(ExpressionStmt),
    Operation(OpDecl),
    If(IfStmt),
    Print(PrintStmt),
    Return(ReturnStmt),
    Val(ValDecl),
    Empty,
}

impl Statement {
    pub fn get_token_value(&mut self) -> Token {
        match self {
            Self::Val(vd) => vd.name.clone(),
            Self::Operation(op) => op.name.clone(),
            Self::Expression(ex) => ex.expression.get_expr_value(),
            Self::Print(p) => p.expression.get_expr_value(),
            Self::Return(r) => r.value.get_expr_value(),
            _ => panic!("Unkown statemnet kind"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum SymbolValue {
    Str(String),
    Int(i32),
    Float(f32),
    Bool(bool),
    Identity(Ident), //name value
    Unknown,
    Nothing,
}

impl SymbolValue {
    pub fn to_akind(self) -> AKind {
        match self {
            Self::Bool(b) => AKind::TyBool,
            Self::Float(f) => AKind::TyFlt,
            Self::Int(i) => AKind::TyInt,
            Self::Str(s)=> AKind::TyStr,
            Self::Identity(i) => {if i.kind.is_some() {
                return *i.kind.unwrap()
            } else {
                return AKind::TyUnknown
            }
        },
            Self::Nothing => AKind::TyMute,
            _ => panic!("Unknown type!")
        }
    }

    pub fn get_string(self) -> Option<String> {
        match self {
            Self::Identity(i) => {
                return i.name
            },
            _ => panic!("Cannot get string name of value {:?}", self)
        }
    }
}


#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Ident {
    pub name: Option<String>,
    pub kind: Option<Box<AKind>>,
    pub value: Box<SymbolValue>,
}