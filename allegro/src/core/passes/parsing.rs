use crate::core::{resource::{tokens::{Token, TokenKind, TokenKind::*}, ast::*}, errors::parsingerr::ParseError};

pub struct Parser {
    pub tkvec: Vec<Token>,
    pub curr: usize,
    pub ast: Vec<Statement>,
}

impl Parser {
    pub fn new(tkvec: Vec<Token>) -> Self {
        Parser { tkvec, curr: 0, ast: vec![]}
    }

    fn peek(&mut self) -> Token {
        self.tkvec.get(self.curr).unwrap().clone()
    }

    fn previous(&mut self) -> Token {
        self.tkvec.get(self.curr - 1).unwrap().clone()
    }

    fn is_at_end(&mut self) -> bool {
        self.peek().kind == TokenKind::TEof
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.curr += 1;
        }
        return self.previous()
    }

    fn check(&mut self, kind: TokenKind) -> bool {
        if self.is_at_end() { return false };
        return self.peek().kind == kind
    }

    fn search(&mut self, kinds: Vec<TokenKind>) -> bool {
        for kind in kinds {
            if self.check(kind) {
                self.advance();
                return true
            }
        }

        return false;
    }

    fn error(&mut self, token: Token, msg: String) {
        let pe = ParseError {
            token,
            msg,
        };
        panic!("Error! {:?}", pe);
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> Token {
        if self.check(kind) {
            return self.advance();
        } else {
            let tk = self.peek();
            self.error(tk, message.to_string());
            panic!()
        }
    }

    fn primary(&mut self) -> Expr {
        if self.search(vec![TkFalse]) {
            return Expr::Literal(LiteralExpr {value: TkSymbol(SymbolKind::Bool(false))})
        } else if self.search(vec![TkTrue]) {
            return Expr::Literal(LiteralExpr {value: TkSymbol(SymbolKind::Bool(true))})
        } else if self.search(vec![TkLiteral("dummy?".to_string())]) {
            return Expr::Literal(LiteralExpr {value: TkSymbol(self.previous().literal)})
        } else if self.search(vec![TkLparen]) {
            let expr: Expr = self.expression();
            self.consume(TkRparen, "Expected ')' after expression");
            return Expr::Grouping(GroupExpr { expression: Box::new(expr) })
        } else {
            return Expr::Empty
        }
    }

    fn unary(&mut self) -> Expr {
        if self.search(vec![TkMinus]) {
            let operator: Token = self.previous();
            let right = self.unary();
            return Expr::Unary(UnaryExpr { operator, right: Box::new(right) })
        }

        return self.primary()
    }

    fn factor(&mut self) -> Expr {
        let mut expr: Expr = self.unary();

        while self.search(vec![TkStar, TkSlash]) {
            let operator: Token = self.previous();
            let right: Expr = self.unary();
            expr = Expr::Binary(BinExpr { left: Box::new(expr), operator, right: Box::new(right) })
        }

        return expr
    }

    fn term(&mut self) -> Expr {
        let mut expr: Expr = self.factor();

        while self.search(vec![TkMinus, TkPlus]) {
            let operator: Token = self.previous();
            let right: Expr = self.factor();
            expr = Expr::Binary(BinExpr { left: Box::new(expr), operator, right: Box::new(right)})
        }

        return expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();

        while self.search(vec![TkCGT, TkCGE, TkCLT, TkCLE]) {
            let operator: Token = self.previous();
            let right: Expr = self.term();
            expr = Expr::Binary(BinExpr { left: Box::new(expr), operator, right: Box::new(right) })
        }

        return expr;
    }

    fn equality(&mut self) -> Expr {
        let mut expr: Expr = self.comparison();

        while self.search(vec![TkCNE, TkCEQ]) {
            let operator: Token = self.previous();
            let right: Expr = self.comparison();
            expr = Expr::Binary(BinExpr { left: Box::new(expr), operator: operator, right: Box::new(right) });
        }

        return expr
    }

    fn expression(&mut self) -> Expr {
        return self.equality();
    }

    pub fn parse(&mut self) -> Expr {
            return self.expression();
    }
}