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
            panic!();
            //return self.advance();
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
            Expr::Empty
            //panic!("An error occured!!");
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
        let (name, kind) = self.val_signiture();
        self.consume(TkEqual, "Expected '='");
        println!("{:?}", self.tkvec[self.curr].clone());

        let initializer: Expr = self.expression();

        if !self.search(vec![TkStatementEnd, TkRBrace]) {
            println!("{:?}", self.tkvec[self.curr].clone());
            panic!("Unexpected end of value declaration!")
        }
        return Statement::Val(ValDecl {
            name,
            kind,
            initializer,
        });
    }

    fn val_signiture(&mut self) -> (Token, SymbolKind) {
        let name: Token = self.consume(TkSymbol, "Expected value name");

        let kind: SymbolKind = self.advance().literal.clone();
        (name, kind)
    }

    fn op_decl(&mut self) -> Statement {
        let name: Token = self.consume(TkSymbol, "Expected operation name");
        println!("Creating opdecl");
        let mut params: Vec<Statement> = vec![];

        self.consume(TkEqual, "Expected '='");
        self.consume(TkLparen, "Expected '('");
        while self.tkvec[self.curr].kind == TkSymbol {
            //println!("{:?}", self.tkvec[self.curr].clone());

            let np = self.val_signiture();
            params.push(Statement::Val(ValDecl {
                name: np.0,
                kind: np.1,
                initializer: Expr::Empty,
            }));
            self.curr += 1;
        }
        //self.advance();
        self.consume(TkSmallArr, "Expected '->'");
        let opreturnkind = self.tkvec[self.curr].clone().literal;
        self.advance();
        self.consume(TkPipe, "Expected '|'");

        // println!("Operation {:?} of type {:?} with params: {:?}", name, opreturnkind, params);
        return Statement::Function(FunctionStmt {
            name,
            params,
            kind: opreturnkind,
            body: BlockStmt {
                statements: self.block(),
            },
        });
    }

    fn block(&mut self) -> Vec<Statement> {
        let mut collector: Vec<Statement> = vec![];
        self.consume(TkLBrace, "Expected '{'");
        while !self.is_at_end() && self.tkvec[self.curr].kind != TkRBrace {
            collector.push(self.declaration());
            self.curr += 1;
        }
        self.advance();
        collector
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
