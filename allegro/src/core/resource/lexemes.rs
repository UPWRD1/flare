#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LexemeKind {
    LxSymbol(String),
    LxLiteral(String),
    LxNumeric(i32),
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
    pub character: char,
    pub location: usize,
}