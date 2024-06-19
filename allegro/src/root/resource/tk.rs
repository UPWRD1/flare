use logos::Logos;

use super::ast::Itype;

///Enum for type of token
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Logos)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
pub enum Tk<'a> {
    #[regex("--.*\n", logos::skip)]
    TkComment,
    #[token("use")]
    TkKwUse,
    #[token("let")]
    TkKwLet,
    #[token("var")]
    TkKwVar,
    #[token("print")]
    TkKwPrint,
    #[token("of")]
    TkKwOf,
    #[token("fn")]
    TkKwFn,
    #[token("if")]
    TkKwIf,
    #[token("else")]
    TkKwElse,
    #[token("match")]
    TkKwMatch,
    #[token("while")]
    TkKwWhile,
    #[token("for")]
    TkKwFor,
    #[token("in")]
    TkKwIn,
    #[token("return")]
    TkKwReturn,
    #[token("and")]
    TkKwAnd,
    #[token("or")]
    TkKwOr,
    #[token("not")]
    TkKwNot,
    #[regex("([a-zA-Z]|_)+[a-zA-Z0-9]*", priority=3)]
    TkSymbol(&'a str),
    #[regex("[0-9]+|true|false|\"*\"", priority=2)]
    TkScalar(&'a str),
    #[token("->")]
    TkArr,
    #[token("!")]
    TkBang,
    #[token("?")]
    TkQuestion,
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
}