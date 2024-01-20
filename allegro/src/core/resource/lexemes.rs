use crate::core::resource::ast::SymbolKind;

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LexemeKind {
    LxSymbol,
    LxLiteral,
    LxNumeric,
    LxPlus,
    LxMinus,
    LxStar,
    LxSlash,
    LxLparen,
    LxRparen,
    LxSmallArr,
    LxBigArr,
    LxPipe,
    LxPercent,
    LxDoubleDot,
    LxLBrace,
    LxRBrace,
    LxStatementEnd,
    LxEqual,
    LxCEQ,
    LxCNE,
    LxCLT,
    LxCLE,
    LxCGT,
    LxCGE,
    LxAnd,
    LxOr,
    LxComma,
    LxColon,
    LxDot,
    Err,
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lexeme {
    pub kind: LexemeKind,
    pub value: SymbolKind,
    pub location: usize,
}
