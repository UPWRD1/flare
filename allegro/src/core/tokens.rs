
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Atype {
    Int(i32),
    AString(String),
}

impl std::fmt::Display for Atype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atype::Int(i) => {
                write!(f, "{}", i)
            }
            Atype::AString(i) => {
                write!(f, "{}", i)
            }

        }
      }
}

pub type Ident = String;

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CSToken {
    Comment,
    KwVal, // name type value
    KwOperation, // name
    TkIdent(Ident),
    TkLiteral(Atype),
    TkPlus, TkMinus, TkStar, TkSlash,
    TkLparen, TkRparen,
    TkSmallArr, TkBigArr,
    TkPipe,
    TkLBrace, TkRBrace,
    TyFloat,
    TyInt,
    TyMute,
    TyDerive,
    TyString,
    TkStatementEnd,
    KwDo, KwIs, KwEnd,
    TkEqual, TkCEQ, TkCNE, TkCLT, TkCLE, TkCGT, TkCGE, TkAnd, TkOr,
    TkComma, TkColon, TkDot,
    Eof
}