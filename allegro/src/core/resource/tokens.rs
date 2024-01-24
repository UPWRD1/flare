use super::ast::SymbolKind;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    TkKWOp,
    TkKWVal,
    TkKwPrint,
    TkKwIs,
    TkKwIf,
    TkKwElse,
    TkKwMatch,
    TkKwWhile,
    TkKwFor,
    TkKwIn,
    TkKwTask,
    TkKwInvoke,
    TkKwTrue,
    TkKwFalse,
    TkSymbol,
    TkLiteral,
    TkNumeric,
    TkTyInt,
    TkTyFlt,
    TkTyStr,
    TkTyBool,
    TkTyMute,
    TkPlus,
    TkMinus,
    TkStar,
    TkSlash,
    TkLparen,
    TkRparen,
    TkSmallArr,
    TkBigArr,
    TkPipe,
    TkPercent,
    TkLBrace,
    TkRBrace,
    TkStatementEnd,
    TkEqual,
    TkCEQ,
    TkCNE,
    TkCLT,
    TkCLE,
    TkCGT,
    TkCGE,
    TkAnd,
    TkOr,
    TkComma,
    TkColon,
    TkDot,
    TEof,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    //pub lexeme: Lexeme,
    pub literal: SymbolKind,
    pub location: usize,
}

#[macro_export]
macro_rules! create_token {
    ($el: tt, $kind: tt) => {
        Token {
            kind: $kind,
            //lexeme: $el.clone(),
            literal: $el.value.clone(),
            location: $el.location,
        }
    };
}
