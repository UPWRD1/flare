use super::{ast::SymbolValue, environment::AKind};

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
    TkType(AKind),
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
pub struct TokenValue {
    value: SymbolValue,
    kind: Option<AKind>
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub tokentype: TokenType,
    pub value: SymbolValue,
    pub kind: Option<AKind>,
    pub location: usize,
}

#[macro_export]
macro_rules! create_token {
    ($el: tt, $kind: tt) => {
        Token {
            tokentype: $kind,
            value: $el.value.clone(),
            kind: None,
            location: $el.location,
        }
    };

    ($el: tt, $kind: tt($a: expr)) => {
        Token {
            tokentype: $kind,
            value: $el.value.clone(),
            kind: $a,
            location: $el.location,
        }
    };
}
