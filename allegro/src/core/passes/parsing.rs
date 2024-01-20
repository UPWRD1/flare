use crate::core::{
    errors::parsingerr::ParseError,
    resource::{
        ast::*,
        tokens::{Token, TokenKind, TokenKind::*},
    },
};

pub struct Parser {
    pub tkvec: Vec<Token>,
    pub curr: usize,
    pub ast: Vec<Statement>,
}

impl Parser {
    pub fn new(tkvec: Vec<Token>) -> Self {
        Parser {
            tkvec,
            curr: 0,
            ast: vec![],
        }
    }

    fn peek(&mut self) -> Token {
        if self.curr < self.tkvec.len() {
            self.tkvec.get(self.curr).unwrap().clone()
        } else {
            Token {
                kind: TEof,
                literal: SymbolKind::Nothing,
                location: self.curr,
            }
        }
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
        return self.previous();
    }

    fn check(&mut self, kind: TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        };
        return self.peek().kind == kind;
    }

    fn grab(&mut self) -> Option<Token> {
        if self.is_at_end() {
            return None;
        };
        Some(self.peek())
    }

    fn search(&mut self, kinds: Vec<TokenKind>) -> bool {
        for kind in kinds {
            if self.check(kind) {
                self.advance();
                return true;
            }
        }

        return false;
    }

    fn search_get(&mut self, kinds: Vec<TokenKind>) -> Option<Token> {
        for _kind in kinds {
            if let Some(x) = self.grab() {
                return Some(x);
            }
        }

        return None;
    }

    fn error(&mut self, token: Token, msg: String) {
        let pe = ParseError { token, msg };
        panic!("Error! {:?}", pe);
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> Token {
        if self.check(kind) {
            return self.advance();
        } else {
            let tk = self.peek();
            println!("{tk:?}");
            self.error(tk.clone(), message.to_string());
            panic!()
        }
    }

    fn consume_options(&mut self, kinds: Vec<TokenKind>, message: &str) -> Token {
        for kind in kinds {
            if self.check(kind) {
                return self.advance();
            }
        }
        let tk = self.peek();
        println!("{tk:?}");
        self.error(tk.clone(), message.to_string());
        panic!()
    }

    fn primary(&mut self) -> Expr {
        if let Some(tk) = self.search_get(vec![TkFalse]) {
            return Expr::Literal(LiteralExpr { value: tk });
        } else if let Some(tk) = self.search_get(vec![TkTrue]) {
            return Expr::Literal(LiteralExpr { value: tk });
        } else if let Some(tk) = self.search_get(vec![TkLiteral]) {
            return Expr::Literal(LiteralExpr { value: tk });
        } else if self.search(vec![TkLparen]) {
            let expr: Expr = self.expression();
            self.consume(TkRparen, "Expected ')' after expression");
            return Expr::Grouping(GroupExpr {
                expression: Box::new(expr),
            });
        } else {
            return Expr::Empty;
        }
    }

    fn unary_expr(&mut self) -> Expr {
        if self.search(vec![TkMinus]) {
            let operator: Token = self.previous();
            let right = self.unary_expr();
            return Expr::Unary(UnaryExpr {
                operator,
                right: Box::new(right),
            });
        }

        return self.primary();
    }

    fn factor_expr(&mut self) -> Expr {
        let mut expr: Expr = self.unary_expr();
        while self.search(vec![TkStar, TkSlash]) {
            let operator: Token = self.previous();
            let right: Expr = self.unary_expr();
            expr = Expr::Binary(BinExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            })
        }

        return expr;
    }

    fn termExpr(&mut self) -> Expr {
        let mut expr: Expr = self.factor_expr();

        while self.search(vec![TkMinus, TkPlus]) {
            let operator: Token = self.previous();
            let right: Expr = self.factor_expr();
            expr = Expr::Binary(BinExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            })
        }

        return expr;
    }

    fn comparisonExpr(&mut self) -> Expr {
        let mut expr = self.termExpr();
        while self.search(vec![TkCGT, TkCGE, TkCLT, TkCLE]) {
            let operator: Token = self.previous();
            let right: Expr = self.termExpr();
            expr = Expr::Binary(BinExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            })
        }

        return expr;
    }

    fn equality_expr(&mut self) -> Expr {
        let mut expr: Expr = self.comparisonExpr();

        while self.search(vec![TkCNE, TkCEQ]) {
            let operator: Token = self.previous();
            let right: Expr = self.comparisonExpr();
            expr = Expr::Binary(BinExpr {
                left: Box::new(expr),
                operator: operator,
                right: Box::new(right),
            });
        }

        return expr;
    }

    fn expression(&mut self) -> Expr {
        return self.equality_expr();
    }

    fn printStmt(&mut self) -> Statement {
        let value: Expr = self.expression();
        self.consume_options(vec![TkStatementEnd, TkLparen], "Unexpected end of print statement!");
        return Statement::Print(PrintStmt { expression: value });
    }

    fn exprStmt(&mut self) -> Statement {
        let expr: Expr = self.expression();
        self.consume(TkStatementEnd, "Unexpected end of expression!");
        return Statement::Expression(ExpressionStmt { expression: expr });
    }

    fn statement(&mut self) -> Statement {
        if self.search(vec![TkKwPrint]) {
            return self.printStmt();
        } else {
            return self.exprStmt();
        }
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut statements: Vec<Statement> = vec![];
        while !self.is_at_end() {
            statements.push(self.statement());
        }
        return statements;
    }
}
