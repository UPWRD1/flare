use super::{environment::AKind, tokens::Token};

#[derive(Clone, Debug, PartialEq)]
pub struct AssignExpr {
    pub name: Token,
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
pub struct ScalarExpr {
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
    ScalarEx(ScalarExpr),
    Logical(LogicalExpr),
    Unary(UnaryExpr),
    Value(ValueExpr),
    Empty,
}

impl Expr {
    pub fn get_expr_value(&self) -> Token {
        match self {
            Self::Assign(a) => a.name.clone(),
            Self::Binary(b) => b.left.clone().get_expr_value(),
            Self::Call(c) => c.paren.clone(),
            Self::Empty => {
                panic!("Cannot get value of empty expression!")
            }
            Self::Grouping(g) => g.expression.get_expr_value().clone(),
            Self::ScalarEx(l) => l.value.clone(),
            Self::Logical(l) => l.operator.clone(),
            Self::Unary(u) => u.operator.clone(),
            Self::Value(v) => v.name.clone(),
        }
    }
    /*
    pub fn get_expr_type(&mut self) -> Option<AKind> {
        match self {
            Self::Assign(a) => Some(a.kind.clone()),
            Self::Binary(b) => b.left.clone().get_expr_type(),
            Self::Call(c) => c.callee.get_expr_type(),
            Self::Empty => {
                panic!("Cannot get value of empty expression!")
            }
            Self::Grouping(g) => g.expression.get_expr_type().clone(),
            Self::Literal(l) => l.value.value.clone(),
            Self::Logical(l) => l.left.get_expr_type().clone(),
            Self::Unary(u) => u.right.get_expr_type().clone(),
            Self::Value(v) => v.name.kind.clone(),
        }
    }
     */
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
    pub returnval: AKind,
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
    pub returntype: AKind,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ValDecl {
    pub name: Ident,
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

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Scalar {
    Str(String),
    Int(i32),
    Float(f32),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum SymbolValue {
    Scalar(Scalar),
    Identity(Ident), //name value
    Unknown,
    Mute,
}

impl SymbolValue {
    pub fn to_akind(self) -> AKind {
        match self {
            Self::Scalar(s) => match s {
                Scalar::Bool(_) => AKind::TyBool,
                Scalar::Float(_) => AKind::TyFlt,
                Scalar::Int(_) => AKind::TyInt,
                Scalar::Str(_) => AKind::TyStr,
            },
            Self::Identity(_) => {
                //if i.kind.is_some() {
                //    return *i.kind.unwrap();
                //} else {
                return AKind::TyUnknown;
                //}
            }
            Self::Mute => AKind::TyMute,
            _ => panic!("Unknown type! {:?}", self),
        }
    }

    pub fn get_string(self) -> Option<String> {
        match self {
            Self::Identity(i) => return Some(i.name),
            Self::Scalar(s) => match s {
                Scalar::Int(v) => format!("{v}").into(),
                Scalar::Float(v) => format!("{v}").into(),
                Scalar::Str(v) => format!("{v}").into(),
                Scalar::Bool(v) => format!("{v}").into(),
            },
            _ => panic!("Cannot get string name of value {:?}", self),
        }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Ident {
    pub name: String,
    //pub kind: Option<Box<AKind>>,
    //pub value: Box<SymbolValue>,
}
