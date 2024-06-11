use super::{ast::SymbolValue, environment::AKind};

///Enum for type of token
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenType {
    TkKwUse,
    TkKwLet,
    TkKwEnd,
    TkKwVal,
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
            | TokenType::TkKwLet
            | TokenType::TkKwEnd
            | TokenType::TkKwVal
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
            TokenType::TkKwUse => "use".to_string(),
            TokenType::TkKwLet => "let".to_string(),
            TokenType::TkKwEnd => "end".to_string(),
            TokenType::TkKwVal => "val".to_string(),
            TokenType::TkKwPrint => "print".to_string(),
            TokenType::TkKwOf => "of".to_string(),
            TokenType::TkKwIf => "if".to_string(),
            TokenType::TkKwElse => "else".to_string(),
            TokenType::TkKwMatch => "match".to_string(),
            TokenType::TkKwWhile => "while".to_string(),
            TokenType::TkKwFor => "for".to_string(),
            TokenType::TkKwIn => "in".to_string(),
            TokenType::TkKwTask => "task".to_string(),
            TokenType::TkKwInvoke => "invoke".to_string(),
            TokenType::TkKwReturn => "return".to_string(),
            TokenType::TkKwTrue => "true".to_string(),
            TokenType::TkKwFalse => "false".to_string(),
            TokenType::TkSymbol => self.value.clone().unwrap().get_string().unwrap(),
            TokenType::TkScalar => {
                self.value.clone().unwrap().get_string().unwrap()
            }
            TokenType::TkType(ref t) => t.to_string(),
            TokenType::TkPlus => "+".to_string(),
            TokenType::TkMinus => "-".to_string(),
            TokenType::TkStar => "*".to_string(),
            TokenType::TkSlash => "/".to_string(),
            TokenType::TkLparen => "(".to_string(),
            TokenType::TkRparen => ")".to_string(),
            TokenType::TkSmallArr => "->".to_string(),
            TokenType::TkBigArr => "=>".to_string(),
            TokenType::TkPipe => "|".to_string(),
            TokenType::TkPercent => "%".to_string(),
            TokenType::TkLBrace => "{".to_string(),
            TokenType::TkRBrace => "}".to_string(),
            TokenType::TkStatementEnd => ";".to_string(),
            TokenType::TkAssign => "=".to_string(),
            TokenType::TkAssignInfer => ":=".to_string(),
            TokenType::TkCEQ => "==".to_string(),
            TokenType::TkCNE => "!=".to_string(),
            TokenType::TkCLT => "<".to_string(),
            TokenType::TkCLE => "<=".to_string(),
            TokenType::TkCGT => ">".to_string(),
            TokenType::TkCGE => ">=".to_string(),
            TokenType::TkAnd => "&&".to_string(),
            TokenType::TkOr => "||".to_string(),
            TokenType::TkComma => ",".to_string(),
            TokenType::TkColon => ":".to_string(),
            TokenType::TkDot => ".".to_string(),
            TokenType::TkEof => "".to_string(),
        }
        ;
        x
    }
}
