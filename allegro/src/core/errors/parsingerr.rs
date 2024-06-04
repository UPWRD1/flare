use crate::core::resource::tokens::{Token, TokenType};

#[derive(Debug)]
pub struct ParseError {
    pub token: Token,
    pub msg: String,
}

impl ParseError {
    pub fn _report(&mut self) {
        if self.token.tokentype == TokenType::TkEof {}
    }
}
