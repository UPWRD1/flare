use super::tokens::Token;
use super::tokens::TokenKind;

#[derive(Clone, Debug, PartialEq)]
pub struct AssignExpr {
    pub name: Token,
    pub kind: SymbolKind,
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
    pub parent: Token,
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
            Self::Assign(a) => {
                a.name
            }
            Self::Binary(b) => {
                b.operator
            }
            Self::Call(c) => {
                c.parent
            }
            Self::Empty => {
                panic!("Cannot get value of empty expression!")
            }
            Self::Grouping(g) => {
                g.expression.get_expr_value()
            }
            Self::Literal(l) => {
                l.value
            }
            Self::Logical(l) => {
                l.operator
            }
            Self::Unary(u) => {
                u.operator
            }
            Self::Value(v) => {
                v.name
            }
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
    pub kind: SymbolKind,
    pub body: BlockStmt,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Statement>,
    pub else_branch: Box<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrintStmt {
    pub expression: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnStmt {
    pub keyword: TokenKind,
    pub value: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ValDecl {
    pub name: Token,
    pub kind: SymbolKind,
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
            Self::Val(vd) => {
                vd.name
            }
            &mut Self::Operation(op) => {
                op.name
            }
            Self::Expression(ex) => {
                ex.expression.get_expr_value()
            }
            Self::Print(p) => {
                p.expression.get_expr_value()
            }
            Self::Return(r) => {
                r.value.get_expr_value()
            }
            _ => panic!("Unkown statemnet kind")
        }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum SymbolKind {
    Str(String),
    TyStr,
    Int(i32),
    TyInt,
    Float(f32),
    TyFlt,
    Bool(bool),
    TyBool,
    TyMute,
    Identity(String, Box<SymbolKind>), //name value
    Nothing,
    Unknown,
}
