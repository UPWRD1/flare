use std::str::FromStr;
use logos::{Lexer, Logos};
use logos::Skip;
use super::{ast::VTypeKind, itypes::Itype};

use crate::error_nocode;
use crate::root::passes::parser::*;

fn newline_callback(lex: &mut Lexer<Tk>) -> (usize, usize) {
    lex.extras.0 += 1;
    lex.extras.1 = lex.span().start + 1;
    (lex.extras.0, lex.extras.1)
}

/// Compute the line and column position for the current word.
fn word_callback(lex: &mut Lexer<Tk>) -> (usize, usize) {
    let line = lex.extras.0 + 1;
    let column = lex.span().start - lex.extras.1;
    (line, column)
}


///Enum for type of token
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Logos)]
#[logos(skip r"[\t\f]+")] // Ignore this regex pattern between tokens
#[logos(extras = (usize, usize))]
pub enum Tk {
    #[regex(" ", logos::skip)]
    TkSpace,
    #[token(";", newline_callback)]
    TkSemicolon((usize, usize)),
    #[token("\n", newline_callback)]
    TkStatementEnd((usize, usize)),
    #[regex("--.*", logos::skip)]
    TkComment,
    #[token("let", word_callback)]
    TkKwLet((usize, usize)),
    #[token("do", word_callback)]
    TkKwDo((usize, usize)),
    #[token("end", word_callback)]
    TkKwEnd((usize, usize)),
    #[token("print", word_callback)]
    TkKwPrint((usize, usize)),
    #[token("of", word_callback)]
    TkKwOf((usize, usize)),
    #[token("fn", word_callback)]
    TkKwFn((usize, usize)),
    #[token("if", word_callback)]
    TkKwIf((usize, usize)),
    #[token("else", word_callback)]
    TkKwElse((usize, usize)),
    #[token("match", word_callback)]
    TkKwMatch((usize, usize)),
    #[token("while", word_callback)]
    TkKwWhile((usize, usize)),
    #[token("for", word_callback)]
    TkKwFor((usize, usize)),
    #[token("in", word_callback)]
    TkKwIn((usize, usize)),
    #[token("return", word_callback)]
    TkKwReturn((usize, usize)),
    #[token("and", word_callback)]
    TkKwAnd((usize, usize)),
    #[token("or", word_callback)]
    TkKwOr((usize, usize)),
    #[token("not", word_callback)]
    TkKwNot((usize, usize)),
    #[token("int", word_callback)]
    TkKwInt((usize, usize)),
    #[token("flt", word_callback)]
    TkKwFlt((usize, usize)),
    
    #[token("str", word_callback)]
    TkKwStr((usize, usize)),
    #[token("bool", word_callback)]
    TkKwBool((usize, usize)),
    #[regex("([a-zA-Z]|_)+[a-zA-Z0-9]*", priority=3, callback=word_callback)]
    TkSymbol((usize, usize)),
    #[regex("[0-9]+|true|false|\"*\"", priority=2, callback=word_callback)]
    TkScalar((usize, usize)),
    #[token("->", word_callback)]
    TkArr((usize, usize)),
    #[token("!", word_callback)]
    TkBang((usize, usize)),
    #[token("?", word_callback)]
    TkQuestion((usize, usize)),
    #[token("+", word_callback)]
    TkPlus((usize, usize)),
    #[token("-", word_callback)]
    TkMinus((usize, usize)),
    #[token("*", word_callback)]
    TkStar((usize, usize)),
    #[token("/", word_callback)]
    TkSlash((usize, usize)),
    #[token("(", word_callback)]
    TkLparen((usize, usize)),
    #[token(")", word_callback)]
    TkRparen((usize, usize)),
    #[token("[", word_callback)]
    TkLbracket((usize, usize)),
    #[token("]", word_callback)]
    TkRbracket((usize, usize)),
    #[token("=", word_callback)]
    TkAssign((usize, usize)),
    #[token("==", word_callback)]
    TkCEQ((usize, usize)),
    #[token("<", word_callback)]
    TkCLT((usize, usize)),
    #[token("<=", word_callback)]
    TkCLE((usize, usize)),
    #[token(">", word_callback)]
    TkCGT((usize, usize)),
    #[token(">=", word_callback)]
    TkCGE((usize, usize)),
    #[token(",", word_callback)]
    TkComma((usize, usize)),
    #[token(":", word_callback)]
    TkColon((usize, usize)),
    #[token(".", word_callback)]
    TkDot((usize, usize)),
}

impl Tk {
    pub fn translate(&self, lex: &mut Lexer<Tk>) -> Token {
        match self {
            Tk::TkKwLet(_) => Token::Let,
            Tk::TkKwOf(_) => Token::Of,
            Tk::TkKwFn(_) => Token::Fn,
            Tk::TkKwIf(_) => Token::If,
            Tk::TkKwElse(_) => Token::Else,
            Tk::TkKwWhile (_) => Token::While,
            Tk::TkKwFor(_) => Token::For,
            Tk::TkKwIn(_) => Token::In,
            Tk::TkKwReturn(_) => Token::Return,
            Tk::TkKwAnd(_) => Token::And,
            Tk::TkKwOr(_) => Token::Or,
            Tk::TkKwNot(_) => Token::Not,
            Tk::TkSymbol(_) => Token::Ident(lex.slice().to_string()),
            Tk::TkScalar(_) => Token::Scalar(Itype::from_str(lex.slice()).unwrap()),
            Tk::TkArr(_) => Token::Arrow,
            Tk::TkBang(_) => Token::Bang,
            Tk::TkQuestion(_) => Token::Question,
            Tk::TkPlus(_) => Token::Plus,
            Tk::TkMinus(_) => Token::Minus,
            Tk::TkStar(_) => Token::Mult,
            Tk::TkSlash(_) => Token::Div,
            Tk::TkLparen(_) => Token::LParen,
            Tk::TkRparen(_) => Token::RParen,
            Tk::TkLbracket(_) => Token::LBracket,
            Tk::TkRbracket(_) => Token::RBracket,
            Tk::TkAssign(_) => Token::Assign,
            Tk::TkCEQ(_) => Token::Equal,
            Tk::TkCLT(_) => Token::Less,
            Tk::TkCLE(_) => Token::LessEq,
            Tk::TkCGT(_) => Token::Greater,
            Tk::TkCGE(_) => Token::GreaterEq,
            Tk::TkComma(_) => Token::Comma,
            Tk::TkDot(_) => Token::Dot,
            Tk::TkColon(_) => Token::Colon,
            Tk::TkComment => todo!(),
            Tk::TkKwDo(_) => Token::Do,
            Tk::TkKwEnd(_) => Token::End,
            Tk::TkSemicolon(_) | Tk::TkStatementEnd(_) => Token::StatementEnd,
            Tk::TkKwInt(_) => Token::Vtype(VTypeKind::Int),
            Tk::TkKwFlt(_) => Token::Vtype(VTypeKind::Flt),
            Tk::TkKwStr(_) => Token::Vtype(VTypeKind::Str),
            Tk::TkKwBool(_) => Token::Vtype(VTypeKind::Bool),
            _ => panic!("{:?}", self),

        }
        //todo!()
    }

    pub fn syntax_error(self, lex: logos::Lexer<Tk>, e: &String) {
        match self {
            Tk::TkComment | Tk::TkSpace
            => todo!(),
            Tk::TkSemicolon((line, column))
            | Tk::TkStatementEnd((line, column))
            | Tk::TkKwLet((line, column))
            | Tk::TkKwDo((line, column))
            | Tk::TkKwEnd((line, column))
            | Tk::TkKwPrint((line, column))
            | Tk::TkKwOf((line, column))
            | Tk::TkKwFn((line, column))
            | Tk::TkKwIf((line, column))
            | Tk::TkKwElse((line, column))
            | Tk::TkKwMatch((line, column))
            | Tk::TkKwWhile((line, column))
            | Tk::TkKwFor((line, column))
            | Tk::TkKwIn((line, column))
            | Tk::TkKwReturn((line, column))
            | Tk::TkKwAnd((line, column))
            | Tk::TkKwOr((line, column))
            | Tk::TkKwNot((line, column))
            | Tk::TkKwInt((line, column))
            | Tk::TkKwFlt((line, column))
            | Tk::TkKwStr((line, column))
            | Tk::TkKwBool((line, column))
            | Tk::TkSymbol((line, column))
            | Tk::TkScalar((line, column))
            | Tk::TkArr((line, column))
            | Tk::TkBang((line, column))
            | Tk::TkQuestion((line, column))
            | Tk::TkPlus((line, column))
            | Tk::TkMinus((line, column))
            | Tk::TkStar((line, column))
            | Tk::TkSlash((line, column))
            | Tk::TkLparen((line, column))
            | Tk::TkRparen((line, column))
            | Tk::TkLbracket((line, column))
            | Tk::TkRbracket((line, column))
            | Tk::TkAssign((line, column))
            | Tk::TkCEQ((line, column))
            | Tk::TkCLT((line, column))
            | Tk::TkCLE((line, column))
            | Tk::TkCGT((line, column))
            | Tk::TkCGE((line, column))
            | Tk::TkComma((line, column))
            | Tk::TkColon((line, column))
            | Tk::TkDot((line, column)) => {
                error_nocode!("Syntax error at {line}:{:?} with token {e}", lex.source().char_indices().collect::<Vec<(usize, char)>>()[column - 1]);
            }
        }
    }
}