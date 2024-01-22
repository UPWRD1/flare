use crate::core::resource::{
    ast::*,
    tokens::{Token, TokenKind, TokenKind::*},
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

    fn search(&mut self, kinds: Vec<TokenKind>) -> bool {
        for kind in kinds {
            if self.check(kind) {
                self.advance();
                return true;
            }
        }
        return false;
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> Token {
        if self.check(kind) {
            return self.advance();
        } else {
            //let tk = self.peek();
            //println!("{:?}", self.tkvec);
            println!("{message}");
            //panic!();
            return self.advance();
        }
    }

    fn primary(&mut self) -> Expr {
        let tk = self.peek();
        if self.search(vec![TkFalse]) {
            return Expr::Literal(LiteralExpr { value: tk });
        } else if self.search(vec![TkTrue]) {
            return Expr::Literal(LiteralExpr { value: tk });
        } else if self.search(vec![TkLiteral, TkNumeric]) {
            return Expr::Literal(LiteralExpr { value: tk });
        } else if self.search(vec![TkSymbol]) {
            return Expr::Variable(VariableExpr {
                name: self.previous(),
            });
        } else if self.search(vec![TkLparen]) {
            let expr: Expr = self.expression();
            self.consume(TkRparen, "Expected ')' after expression");
            return Expr::Grouping(GroupExpr {
                expression: Box::new(expr),
            });
        } else {
            panic!("An error occured!!");
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

    fn term_expr(&mut self) -> Expr {
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

    fn comparison_expr(&mut self) -> Expr {
        let mut expr = self.term_expr();
        while self.search(vec![TkCGT, TkCGE, TkCLT, TkCLE]) {
            let operator: Token = self.previous();
            let right: Expr = self.term_expr();
            expr = Expr::Binary(BinExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            })
        }

        return expr;
    }

    fn equality_expr(&mut self) -> Expr {
        let mut expr: Expr = self.comparison_expr();

        while self.search(vec![TkCNE, TkCEQ]) {
            let operator: Token = self.previous();
            let right: Expr = self.comparison_expr();
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

    fn print_stmt(&mut self) -> Statement {
        let expr: Expr = self.expression();
        self.consume(TkStatementEnd, "Unexpected end of print statement!");
        return Statement::Print(PrintStmt { expression: expr });
    }

    fn expr_stmt(&mut self) -> Statement {
        let expr: Expr = self.expression();
        self.consume(TkStatementEnd, "Unexpected end of expression!");
        return Statement::Expression(ExpressionStmt { expression: expr });
    }

    fn statement(&mut self) -> Statement {
        if self.search(vec![TkKwPrint]) {
            return self.print_stmt();
        } else {
            return self.expr_stmt();
        }
    }

    fn val_decl(&mut self) -> Statement {
        let name: Token = self.consume(TkSymbol, "Expected value name");
        
        let kind: SymbolKind = self.advance().literal.clone();
        
        let mut initializer: Expr = Expr::Empty;
        if self.search(vec![TkEqual]) {
            initializer = self.expression()
        }
        self.consume(TkStatementEnd, "Unexpected end of value declaration!");
        return Statement::Val(ValDecl {
            name,
            kind,
            initializer,
        });
    }

    fn op_decl(&mut self) -> Statement {
        let name: Token = self.consume(TkSymbol, "Expected operation name");
        println!("Creating opdecl");
        let mut initializer: Expr = Expr::Empty;
        self.consume(TkEqual, "Expected '=' after operation name!");
        self.consume(TkLparen, "Expected '=' after operation name!");
        let mut params: Vec<Expr> = vec![];
        while self.search(vec![TkRparen]) {
            //params.push(self.val_decl())
        }
        self.advance();
        self.advance();
        self.advance();
        self.advance();
        self.advance();
        
        //println!("{:?}", self.tkvec[self.curr]);
        return self.statement();
    }

    fn declaration(&mut self) -> Statement {
        if self.search(vec![TkKWVal]) {
            return self.val_decl();
        } else if self.search(vec![TkKWOp]) {
            return self.op_decl();
        }
        return self.statement();
    }

    pub fn parse(&mut self) {
        let mut statements: Vec<Statement> = vec![];
        while !self.is_at_end() {
            statements.push(self.declaration());
        }
        self.ast = statements;
    }

    pub fn supply(&mut self) -> Vec<Statement> {
        return self.ast.clone();
    }
}
