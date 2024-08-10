use logos::Logos;
use serde::{Deserialize, Serialize};

///Enum for type of token
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Logos, Serialize, Deserialize)]
#[logos(skip r"[\t\f\n]+")] // Ignore this regex pattern between tokens
pub enum Tk {
    #[regex(" ", logos::skip)]
    TkSpace,
    #[token(";")]
    TkSemicolon,
    // #[token("\n")]
    // TkStatementEnd,
    #[regex("--.*", logos::skip)]
    TkComment,
    #[token("with")]
    TkKwWith,
    #[token("let")]
    TkKwLet,
    #[token("do")]
    TkKwDo,
    #[token("end")]
    TkKwEnd,
    #[token("print")]
    TkKwPrint,
    #[token("of")]
    TkKwOf,
    #[token("fn")]
    TkKwFn,
    #[token("mut")]
    TkKwMut,
    #[token("if")]
    TkKwIf,
    #[token("else")]
    TkKwElse,
    #[token("while")]
    TkKwWhile,
    #[token("for")]
    TkKwFor,
    #[token("in")]
    TkKwIn,
    #[token("thru")]
    TkKwThru,
    #[token("return")]
    TkKwReturn,
    #[token("and")]
    TkKwAnd,
    #[token("or")]
    TkKwOr,
    #[token("not")]
    TkKwNot,
    #[token("int")]
    TkKwInt,
    #[token("flt")]
    TkKwFlt,
    #[token("str")]
    TkKwStr,
    #[token("Fn")]
    TkKwFnTy,
    #[token("bool")]
    TkKwBool,
    #[regex("([a-zA-Z]|_)+[a-zA-Z0-9]*", priority=2)]
    TkSymbol,
    //|true|false|\"[a-zA-Z0-9]*\"
    #[regex("[0-9]+", priority=4)]
    TkInt,
    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", priority=2)]
    TkFlt,
    #[regex("true", priority=3)]
    TkTrue,
    #[regex("false", priority=3)]
    TkFalse,
    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#)]
    TkStrLit,
    #[token("->")]
    TkArr,
    #[token("!")]
    TkBang,
    #[token("?")]
    TkQuestion,
    #[token("&")]
    TkFuncComp,
    #[token("+")]
    TkPlus,
    #[token("-")]
    TkMinus,
    #[token("*")]
    TkStar,
    #[token("/")]
    TkSlash,
    #[token("(")]
    TkLparen,
    #[token(")")]
    TkRparen,
    #[token("[")]
    TkLbracket,
    #[token("]")]
    TkRbracket,
    #[token("{")]
    TkLbrace,
    #[token("}")]
    TkRbrace,
    #[token("=")]
    TkAssign,
    #[token("==")]
    TkCEQ,
    #[token("<")]
    TkCLT,
    #[token("<=")]
    TkCLE,
    #[token(">")]
    TkCGT,
    #[token(">=")]
    TkCGE,
    #[token(",")]
    TkComma,
    #[token(":")]
    TkColon,
    #[token(".")]
    TkDot,
    #[token("@")]
    TkAt,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: Tk,
    pub lit: String,
}

impl Token {
    pub fn new(kind: Tk, lit: String) -> Self {
        Token {kind, lit}
    }
}