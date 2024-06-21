use std::str::FromStr;

use logos::Logos;

use super::{ast::VTypeKind, itypes::Itype};

use crate::root::passes::parser::*;

///Enum for type of token
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Logos)]
#[logos(skip r"[ \t\f]+")] // Ignore this regex pattern between tokens
pub enum Tk<'a> {
    #[token(";")]
    TkSemicolon,
    #[token("\n")]
    TkStatementEnd,

    #[regex("--.*\n", logos::skip)]
    TkComment,
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
    #[token("int")]
    TkKwInt,
    #[token("flt")]
    TkKwFlt,
    #[token("str")]
    TkKwStr,
    #[token("bool")]
    TkKwBool,
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

impl Tk<'_> {
    pub fn translate(&self) -> Token {
        match self {
            Tk::TkKwLet => Token::Let,
            Tk::TkKwOf => Token::Of,
            Tk::TkKwFn => Token::Fn,
            Tk::TkKwIf => Token::If,
            Tk::TkKwElse => Token::Else,
            Tk::TkKwWhile => Token::While,
            Tk::TkKwFor => Token::For,
            Tk::TkKwIn => Token::In,
            Tk::TkKwReturn => Token::Return,
            Tk::TkKwAnd => Token::And,
            Tk::TkKwOr => Token::Or,
            Tk::TkKwNot => Token::Not,
            Tk::TkSymbol(s) => Token::Ident(s.to_string()),
            Tk::TkScalar(v) => Token::Scalar(Itype::from_str(v).unwrap()),
            Tk::TkArr => Token::Arrow,
            Tk::TkBang => Token::Bang,
            Tk::TkQuestion => Token::Question,
            Tk::TkPlus => Token::Plus,
            Tk::TkMinus => Token::Minus,
            Tk::TkStar => Token::Mult,
            Tk::TkSlash => Token::Div,
            Tk::TkLparen => Token::LParen,
            Tk::TkRparen => Token::RParen,
            Tk::TkLbracket => todo!(),
            Tk::TkRbracket => todo!(),
            Tk::TkAssign => Token::Assign,
            Tk::TkCEQ => Token::Equal,
            Tk::TkCLT => Token::Less,
            Tk::TkCLE => Token::LessEq,
            Tk::TkCGT => Token::Greater,
            Tk::TkCGE => Token::GreaterEq,
            Tk::TkComma => Token::Comma,
            Tk::TkColon => Token::Colon,
            Tk::TkComment => todo!(),
            Tk::TkKwDo => Token::Do,
            Tk::TkKwEnd => Token::End,
            Tk::TkSemicolon | Tk::TkStatementEnd => Token::StatementEnd,
            Tk::TkKwInt => Token::Vtype(VTypeKind::Int),
            Tk::TkKwFlt => Token::Vtype(VTypeKind::Flt),
            Tk::TkKwStr => Token::Vtype(VTypeKind::Str),
            Tk::TkKwBool => Token::Vtype(VTypeKind::Bool),

            _ => panic!(),

        }
        //todo!()
    }
}