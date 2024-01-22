use super::tokens::TokenKind;
use super::tokens::Token;

#[derive(Clone, Debug)]
pub struct AssignExpr {
    pub name: Token,
    pub kind: SymbolKind,
    pub value: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct BinExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub paren: TokenKind,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct GroupExpr {
    pub expression: Box<Expr>
}

#[derive(Clone, Debug)]
pub struct LiteralExpr {
    pub value: Token,
}

impl LiteralExpr {
    pub fn get_literal_value(&mut self) -> SymbolKind {
        self.value.get_value()
    }
}

#[derive(Clone, Debug)]
pub struct LogicalExpr {
    pub left: Box<Expr>,
    pub operator: TokenKind,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct VariableExpr {
    pub name: Token
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub enum Expr {
    Assign(AssignExpr),
    Binary(BinExpr),
    Call(CallExpr),
    Grouping(GroupExpr),
    Literal(LiteralExpr),
    Logical(LogicalExpr),
    Unary(UnaryExpr),
    Variable(VariableExpr),
    Empty
}

#[derive(Clone, Debug)]
pub struct BlockStmt {
    pub statements: Vec<Statement>
}

#[derive(Clone, Debug)]
pub struct ExpressionStmt {
    pub expression: Expr
}

#[derive(Clone, Debug)]
pub struct FunctionStmt {
    pub name: Token,
    pub params: Vec<Statement>,
    pub kind: SymbolKind,
    pub body: BlockStmt,
}

#[derive(Clone, Debug)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Statement>,
    pub else_branch: Box<Statement>,
}

#[derive(Clone, Debug)]
pub struct PrintStmt {
    pub expression: Expr,
}

#[derive(Clone, Debug)]
pub struct ReturnStmt {
    pub keyword: TokenKind,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub struct ValDecl {
    pub name: Token,
    pub kind: SymbolKind,
    pub initializer: Expr,
}


#[derive(Clone, Debug)]
#[allow(dead_code)]
pub enum Statement {
    Block(BlockStmt),
    Expression(ExpressionStmt),
    Function(FunctionStmt),
    If(IfStmt),
    Print(PrintStmt),
    Return(ReturnStmt),
    Val(ValDecl),
    Empty,

}

#[derive(Clone, Debug, PartialEq, PartialOrd,)]
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