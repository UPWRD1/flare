use std::clone;

use super::tokens::TokenKind;
use super::tokens::Token;

pub struct AssignExpr {
    pub name: Token,
    pub kind: SymbolKind,
    pub value: Box<Expr>,
}

pub struct BinExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

pub struct CallExpr {
    pub callee: Box<Expr>,
    pub paren: TokenKind,
    pub args: Vec<Expr>,
}

pub struct GroupExpr {
    pub expression: Box<Expr>
}

pub struct LiteralExpr {
    pub value: SymbolKind,
}

pub struct LogicalExpr {
    pub left: Box<Expr>,
    pub operator: TokenKind,
    pub right: Box<Expr>,
}

pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

pub struct VariableExpr {
    pub name: TokenKind
}

pub enum Expr {
    Assign(AssignExpr),
    Binary(BinExpr),
    Call(CallExpr),
    Grouping(GroupExpr),
    Literal(LiteralExpr),
    Logical(LogicalExpr),
    Unary(UnaryExpr),
    Variable(VariableExpr),
}

pub struct BlockStmt {
    pub statements: Vec<Statement>
}

pub struct ExpressionStmt {
    pub expression: Expr
}

pub struct FunctionStmt {
    pub name: TokenKind,
    pub params: Vec<TokenKind>,
    pub body: Vec<Statement>,
}

pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Statement>,
    pub else_branch: Box<Statement>,
}

pub struct PrintStmt {
    pub expression: Expr,
}

pub struct ReturnStmt {
    pub keyword: TokenKind,
    pub value: Expr,
}

pub struct ValStmt {
    pub name: TokenKind,
    pub initializer: Expr,
}

pub enum Statement {
    Block(BlockStmt),
    Expression(ExpressionStmt),
    Function(FunctionStmt),
    If(IfStmt),
    Print(PrintStmt),
    Return(ReturnStmt),
    Val(ValStmt),
}

#[derive(Clone, Debug)]
pub enum SymbolKind {
    Str(String),
    Int(i32),
    Bool(bool),

}