use super::{ast::SymbolValue, environment::AKind};

///Enum for type of token
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenType {
    TkKwUse,
    TkKWOp,
    TkKwLet,
    TkKwPrint,
    TkKwOf,
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
    TkScalar,
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
    TkAssign,
    TkAssignInfer,
    TkOpMuteShortHand,
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
    TkEof,
}

///Level 2 abstraction, represents a "cleaned up" version of the lexemes, eg. keywords are now seperated from symbols
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub tokentype: TokenType,
    pub value: Option<SymbolValue>,
    pub location: usize,
    // pub lit: String,
}

impl Token {
    pub fn is_keyword(&self) -> bool {
        match self.tokentype {
            TokenType::TkKwUse
            | TokenType::TkKWOp
            | TokenType::TkKwLet
            | TokenType::TkKwPrint
            | TokenType::TkKwOf
            | TokenType::TkKwIf
            | TokenType::TkKwElse
            | TokenType::TkKwMatch
            | TokenType::TkKwWhile
            | TokenType::TkKwFor
            | TokenType::TkKwIn
            | TokenType::TkKwTask
            | TokenType::TkKwInvoke
            | TokenType::TkKwReturn
            | TokenType::TkKwTrue
            | TokenType::TkKwFalse => true,
            _ => false,
        }
    }

    pub fn to_string(&self) -> String {
        let x: String = match self.tokentype {
            TokenType::TkKwUse => "use",
            TokenType::TkKWOp => "op",
            TokenType::TkKwLet => "let",
            TokenType::TkKwPrint => "print",
            TokenType::TkKwOf => "of",
            TokenType::TkKwIf => "if",
            TokenType::TkKwElse => "else",
            TokenType::TkKwMatch => "match",
            TokenType::TkKwWhile => "while",
            TokenType::TkKwFor => "for",
            TokenType::TkKwIn => "in",
            TokenType::TkKwTask => "task",
            TokenType::TkKwInvoke => "invoke",
            TokenType::TkKwReturn => "return",
            TokenType::TkKwTrue => "true",
            TokenType::TkKwFalse => "false",
            TokenType::TkSymbol => "",
            TokenType::TkScalar => {
                let x = self.value.clone();
                let y = x.clone().unwrap().get_string();
                let z = y.clone().unwrap().clone();
                let o = z.clone().as_str();
                o
            }
            TokenType::TkType(_) => todo!(),
            TokenType::TkPlus => "+",
            TokenType::TkMinus => "-",
            TokenType::TkStar => "*",
            TokenType::TkSlash => "/",
            TokenType::TkLparen => "(",
            TokenType::TkRparen => ")",
            TokenType::TkSmallArr => "->",
            TokenType::TkBigArr => "=>",
            TokenType::TkPipe => "|",
            TokenType::TkPercent => "%",
            TokenType::TkLBrace => "{",
            TokenType::TkRBrace => "}",
            TokenType::TkStatementEnd => "",
            TokenType::TkAssign => "=",
            TokenType::TkAssignInfer => ":=",
            TokenType::TkOpMuteShortHand => ":->",
            TokenType::TkCEQ => "==",
            TokenType::TkCNE => "!=",
            TokenType::TkCLT => "<",
            TokenType::TkCLE => "<=",
            TokenType::TkCGT => ">",
            TokenType::TkCGE => ">=",
            TokenType::TkAnd => "&&",
            TokenType::TkOr => "||",
            TokenType::TkComma => ",",
            TokenType::TkColon => ":",
            TokenType::TkDot => ".",
            TokenType::TkEof => todo!(),
        }
        .to_string();
        x
    }
}
