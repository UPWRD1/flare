use crate::core::resource::ast::SymbolValue;

use super::ast::Scalar;


///Enum for different types of lexemes
#[allow(dead_code)]
//#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum LexemeKind {
    LxSymbol(SymbolValue),
    LxScalar(Scalar),
    //LxNumeric(SymbolValue),
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
    LxAssignInfer,
    LxOpMuteShorthand,
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

///Level 1 Abstraction, represents individual chars as enum
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Lexeme {
    pub kind: LexemeKind,
    //pub value: SymbolValue,
    pub location: usize,
}
