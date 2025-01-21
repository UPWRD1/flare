use std::collections::HashSet;

use super::tk::Token;

#[derive(Debug, Clone)]
pub struct LexRes {
    pub tokens: Vec<Token>,
    pub filename: String,
    pub symbols: HashSet<String>
}

