use crate::core::resource::{
    ast::*,
    table::*,
    tokens::{Token, TokenKind, TokenKind::*},
};

use std::hash::*;
use std::collections::hash_map::DefaultHasher;

pub struct Parser {
    pub tkvec: Vec<Token>,
    pub curr: usize,
    pub symboltable: GlobalTable,
    pub ast: Vec<Statement>,
}

impl Parser {
    pub fn new(tkvec: Vec<Token>, sy: GlobalTable) -> Self {
        Parser {
            tkvec,
            curr: 0,
            symboltable: sy,
            ast: vec![],
        }
    }

    fn init_val(&mut self, n: &Token, sk: &SymbolKind) -> String {
        let mut s = DefaultHasher::new();
        let collected: String = format!("{:?}", n.kind);
        collected.hash(&mut s);
        let res = format!("{}", s.finish());
        self.symboltable.values.entries.push(Entry { hash: res.clone(), value: None });
        return res
    }

    fn assign_val(&mut self, vd: ValDecl, h: String) {
        let index = self.symboltable.values.entries.iter().position(|s| s.hash == h).unwrap();
        self.symboltable.values.entries.insert(index, Entry { hash: h, value: Some(vd) });
    }

    fn add_val(&mut self, vd: ValDecl) {
        let hash = self.init_val(&vd.name.clone(), &vd.kind.clone());
        self.assign_val(vd, hash);
    }

    fn init_operation(&mut self, n: &Token, sk: &SymbolKind) -> String {
        let mut s = DefaultHasher::new();
        let collected: String = format!("{:?}{:?}", n.literal, n.location);
        collected.hash(&mut s);
        let res = format!("{}", s.finish());
        self.symboltable.values.entries.push(Entry { hash: res.clone(), value: None });
        return res
    }

    fn assign_operation(&mut self, vd: ValDecl, h: String) {
        let index = self.symboltable.values.entries.iter().position(|s| s.hash == h).unwrap();
        self.symboltable.values.entries.insert(index, Entry { hash: h, value: Some(vd) });
    }

    fn add_operation(&mut self, vd: ValDecl, n: &Token, sk: &SymbolKind) {
        let hash = self.init_operation(n, sk);
        self.assign_operation(vd, hash);
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
            println!("{message}");
            panic!();
        }
    }

    fn consume_vec(&mut self, kinds: Vec<TokenKind>, message: &str) -> Token {
        for kind in kinds {
            if self.check(kind) {
                return self.advance();
            } else {
                continue;
            }
        }
        println!("{message}");
        panic!()
    }

    fn primary(&mut self) -> Expr {
        let tk = self.peek();
        if self.search(vec![TkKwFalse]) {
            return Expr::Literal(LiteralExpr { value: tk });
        } else if self.search(vec![TkKwTrue]) {
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
        } else if self.search(vec![TkKWVal]) {
            return self.val_decl();
        } else if self.search(vec![TkKwIf]) {
            return self.val_decl();
        }else {
            return self.expr_stmt();
        }
    }

    fn val_decl(&mut self) -> Statement {
        let (name, kind) = self.val_signiture();
        self.consume(TkEqual, "Expected '='");

        let initializer: Expr = self.expression();

        if !self.search(vec![TkStatementEnd]) {
            panic!("Unexpected end of value declaration!")
        }
        let res = ValDecl {
            name,
            kind,
            initializer,
        };

        self.add_val(res.clone());
        
        return Statement::Val(res)
    }

    fn init_param(&mut self) -> (Token, SymbolKind, String) {
        let name: Token = self.consume(TkSymbol, "Expected value name");

        let kind: SymbolKind = self
            .consume_vec(vec![TkTyFlt, TkTyInt, TkTyStr, TkTyMute, TkTyBool], "Invalid type")
            .literal;

        let hash = self.init_val(&name, &kind);

        (name, kind, hash)
    }

    fn val_signiture(&mut self) -> (Token, SymbolKind) {
        let name: Token = self.consume(TkSymbol, "Expected value name");

        let kind: SymbolKind = self
            .consume_vec(vec![TkTyFlt, TkTyInt, TkTyStr, TkTyMute, TkTyBool], "Invalid type")
            .literal;

        (name, kind)
    }

    

    fn op_decl(&mut self) -> Statement {
        let name: Token = self.consume(TkSymbol, "Expected operation name");
        let mut params: Vec<Statement> = vec![];
        let opreturnkind: SymbolKind;
        self.consume(TkEqual, "Expected '='");
        self.consume(TkLparen, "Expected '('");
        //println!("{:?}", self.tkvec[self.curr]);

        if self.tkvec[self.curr].kind != TkRparen {
            while self.tkvec[self.curr].kind == TkSymbol
                || self.tkvec[self.curr].kind == TkTyStr
                || self.tkvec[self.curr].kind == TkTyInt
                || self.tkvec[self.curr].kind == TkTyFlt
            {
                let nv = self.init_param();
                params.push(Statement::Val(ValDecl {
                    name: nv.0,
                    kind: nv.1,
                    initializer: Expr::Empty,
                }));
                //self.curr += 2;
            }
        } else {
            self.consume(TkRparen, "Expected ')'");
        }
        //println!("{:?}", self.tkvec[self.curr]);

        self.consume(TkSmallArr, "Expected '->'");
        opreturnkind = self.advance().literal;
        self.consume(TkKwIs, "Expected keyword 'is'");

        return Statement::Operation(OpDecl {
            name,
            params,
            kind: opreturnkind,
            body: BlockStmt {
                statements: self.opblock(),
            },
        });
    }

    fn opblock(&mut self) -> Vec<Statement> {
        let mut collector: Vec<Statement> = vec![];
        self.consume(TkLBrace, "Expected '{'");
        //sprintln!("{:?}", self.tkvec[self.curr].kind);
        while self.tkvec[self.curr].kind != TkRBrace && self.tkvec[self.curr + 1].kind != TEof {
            collector.push(self.statement());
        }
        self.consume(TkRBrace, "Expected '}'");
        collector
    }

    fn declaration(&mut self) -> Statement {
        if self.search(vec![TkKWOp]) {
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

    pub fn supply(&mut self) -> (Vec<Statement>, GlobalTable) {
        return (self.ast.clone(), self.symboltable.clone());
    }
}
