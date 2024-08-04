use crate::Tk;

pub struct Parser {
    loc: usize, 
    tokens: Vec<Tk>,
}

impl Parser {
    pub fn new(tokens: Vec<Tk>) -> Self {
        Parser {loc: 0, tokens }
    }

    fn check(&mut self, tocheck: Vec<Tk>) -> bool {
        let newitems = &self.tokens.clone()[self.loc..tocheck.len()];
        newitems == tocheck
    }

    pub fn parse() {
        
    }
}