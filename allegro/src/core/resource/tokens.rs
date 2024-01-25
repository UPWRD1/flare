use super::ast::SymbolKind;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenType {
    TkKWOp,
    TkKwVal,
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
    TkKwReturn,
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
    pub tokentype: TokenType,
    //pub lexeme: Lexeme,
    pub kind: SymbolKind,
    pub location: usize,
}

#[macro_export]
macro_rules! create_token {
    ($el: tt, $kind: tt) => {
        Token {
            tokentype: $kind,
            //lexeme: $el.clone(),
            kind: $el.value.clone(),
            location: $el.location,
        }
    };
}
