use crate::core::resource::tokens::{Token, TokenKind};

#[derive(Debug)]
pub struct ParseError {
    pub token: Token,
    pub msg: String,
}

impl ParseError {
    pub fn _report(&mut self) {
        if self.token.kind == TokenKind::TEof {

        }
    }
}