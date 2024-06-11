use crate::root::resource::errors::Errors;
use crate::root::resource::output::draw_error;
use crate::{error, error_noquit, quit};

use crate::{
    info,
    root::resource::{
        ast::*,
        environment::AKind,
        tokens::{
            Token,
            TokenType::{self, *},
        },
    },
};

pub struct Parser {
    pub tkvec: Vec<Token>,
    pub curr: usize,
    pub ast: Vec<Statement>,
    pub new_stmts: Vec<Statement>,
}

impl Parser {
    pub fn new(tkvec: Vec<Token>) -> Self {
        Parser {
            tkvec,
            curr: 0,
            ast: vec![],
            new_stmts: vec![],
        }
    }

    fn peek(&mut self) -> Token {
        if self.curr < self.tkvec.len() {
            self.tkvec.get(self.curr).unwrap().clone()
        } else {
            Token {
                tokentype: TkEof,
                value: None,
                location: self.curr,
                //lit: "".to_string()
            }
        }
    }

    #[allow(dead_code)]
    fn inspect(&mut self) {
        println!("{:?}", self.tkvec[self.curr].tokentype)
    }

    fn previous(&mut self) -> Token {
        self.tkvec.get(self.curr - 1).unwrap().clone()
    }

    fn current(&self) -> Token {
        self.tkvec.get(self.curr).unwrap().clone()
    }

    fn is_at_end(&mut self) -> bool {
        self.peek().tokentype == TokenType::TkEof
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.curr += 1;
        }
        self.previous()
    }

    fn check(&mut self, kind: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        };
        self.peek().tokentype == kind
    }

    fn search(&mut self, kinds: Vec<TokenType>) -> bool {
        for kind in kinds {
            if self.check(kind) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, kind: TokenType) -> Token {
        if self.check(kind.clone()) {
            self.advance()
        } else {
            let dummy: Token = Token {
                tokentype: kind.clone(),
                value: None,
                location: self.curr,
            };
            if dummy.is_keyword() {
                error_noquit!(Errors::SyntaxMissingKeyword, (dummy.to_string()));
                draw_error(&self.tkvec, self.curr);
                quit!();

                //dbg!(self.tkvec.clone());
            } else {
                error_noquit!(Errors::SyntaxMissingChar, (dummy.to_string()));
                draw_error(&self.tkvec, self.curr);
                panic!();
            }
            //error_nocode!("Error at {}: {message}", self.current().location);
            //panic!();
        }
    }

    fn consume_vec(&mut self, kinds: Vec<TokenType>) -> Token {
        for kind in kinds.clone() {
            if self.check(kind) {
                return self.advance();
            } else {
                continue;
            }
        }
        self.advance();
        error_noquit!(
            Errors::SyntaxMissingType,
            (self.peek().value.unwrap().to_akind().to_string())
        );
        draw_error(&self.tkvec, self.curr);
        quit!(); //panic!()
    }

    fn primary(&mut self) -> Expr {
        let tk = self.peek();
        if self.search(vec![TkSymbol]) {
            Expr::Modify(ModifyExpr { name: tk, val: Box::new(self.expression())})
        } else if self.search(vec![TkKwFalse, TkKwTrue]) {
            return Expr::ScalarEx(ScalarExpr { value: tk });
        } else if self.search(vec![TkScalar]) {
            return Expr::ScalarEx(ScalarExpr { value: tk });
            //value: Token
            //tokentype: tk.tokentype,
            //value: SymbolValue::Identity(
            //Ident { name: None, kind: Some(AKind::TyStr), value: SymbolValue::Str())
            //),
            //kind:
            //location: tk.location,
        } else if self.search(vec![TkLparen]) {
            let expr: Expr = self.expression();
            self.consume(TkRparen);
            return Expr::Grouping(GroupExpr {
                expression: Box::new(expr),
            });
        } else {
            return Expr::Empty;
        }
    }

    fn secondary(&mut self) -> Expr {
        let expr = self.primary();
        loop {
            if self.search(vec![TkLparen]) {
                let mut args: Vec<Expr> = vec![];
                let paren: Token = if self.search(vec![TkRparen]) {
                    self.previous()
                } else {
                    loop {
                        let argument = self.expression();
                        args.push(argument);
                        if !self.search(vec![TkComma]) {
                            break;
                        }
                    }
                    self.consume(TkRparen)
                };

                return Expr::Call(CallExpr {
                    callee: expr.get_expr_value(),
                    paren,
                    args,
                });
            } else {
                break;
            }
        }
        expr
    }

    fn unary_expr(&mut self) -> Expr {
        if self.search(vec![TkMinus]) {
            let operator: Token = self.previous();
            //println!("{:?}", operator);
            self.advance();
            //println!("{:?}", self.previous());
            let right = self.secondary();
            return Expr::Unary(UnaryExpr {
                operator,
                right: Box::new(right),
            });
        }

        self.secondary()
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

        expr
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

        expr
    }

    fn comparison_expr(&mut self) -> Expr {
        let mut expr = self.term_expr();
        while self.search(vec![TkCGT, TkCGE, TkCLT, TkCLE, TkCEQ, TkCNE]) {
            let operator: Token = self.previous();
            let right: Expr = self.term_expr();
            expr = Expr::Logical(LogicalExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            })
        }

        expr
    }

    fn equality_expr(&mut self) -> Expr {
        let mut expr: Expr = self.comparison_expr();

        while self.search(vec![TkCNE, TkCEQ]) {
            let operator: Token = self.previous();
            let right: Expr = self.comparison_expr();
            expr = Expr::Binary(BinExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        expr
    }

    fn expression(&mut self) -> Expr {
        self.equality_expr()
    }

    fn print_stmt(&mut self) -> Statement {
        let expr: Expr = self.expression();
        //self.consume(TkStatementEnd, "Unexpected end of print statement!");
        Statement::Print(PrintStmt { expression: expr })
    }

    fn expr_stmt(&mut self) -> Statement {
        let expr: Expr = self.expression();
        self.consume(TkStatementEnd);
        Statement::Expression(ExpressionStmt { expression: expr })
    }

    fn return_stmt(&mut self) -> Statement {
        let value: Expr = self.expression();
        Statement::Return(ReturnStmt {
            value: value.clone(),
            returntype: match value.get_expr_value().value {
                Some(v) => v.to_akind(),
                None => {
                    //println!("{:?}", value);
                    todo!()
                }
            },
        })
    }

    fn if_stmt(&mut self) -> Statement {
        let cond = self.expression();
        let ib = self.block();
        let mut eb: Option<Box<BlockStmt>> = None;
        if self.check(TkKwElse) {
            self.advance();
            eb = Some(Box::new(BlockStmt {
                statements: self.block(),
            }));
        }
        Statement::If(IfStmt {
            condition: cond,
            then_branch: Box::new(BlockStmt { statements: ib }),
            else_branch: eb,
        })
    }

    fn statement(&mut self) -> Statement {
        if self.search(vec![TkKwPrint]) {
            self.print_stmt()
        } else if self.search(vec![TkKwVal]) {
            return self.val_decl();
        } else if self.search(vec![TkKwReturn]) {
            return self.return_stmt();
        } else if self.search(vec![TkKwUse]) {
            self.use_statement();
            return self.expr_stmt();
        } else if self.search(vec![TkKwIf]) {
            return self.if_stmt();
        } else {
            return self.expr_stmt();
        }
    }

    fn use_statement(&mut self) {
        //self.advance();
        info!(
            "Found dependancy {:?}",
            self.current().value.unwrap().get_string().unwrap()
        );
    }

    fn val_decl(&mut self) -> Statement {
        let (name, kind) = self.val_signiture();
        if kind.is_unknown() {
            self.consume(TkAssignInfer);
            let initializer: Expr = self.expression();
            if !self.search(vec![TkStatementEnd]) {
                error!(
                    Errors::SyntaxUnexpectedEnd,
                    ("Unexpected end of value declaration".to_string())
                );
            }
            let res = BindingDecl {
                name: Pair {
                    name: name.value.unwrap().get_string().unwrap(),
                    value: Box::new(initializer.get_expr_value().value.unwrap()),
                    kind: match initializer {
                        Expr::Assign(_)
                        | Expr::Binary(_)
                        | Expr::Call(_)
                        | Expr::Grouping(_)
                        | Expr::ScalarEx(_)
                        | Expr::Unary(_)
                        | Expr::Modify(_)
                        | Expr::Empty => initializer.get_expr_value().value.unwrap().to_akind(),

                        Expr::Logical(_) => AKind::TyBool,
                    },
                },
                initializer,
            };

            Statement::Bind(res)
        } else {
            self.consume(TkAssign);

            let initializer: Expr = self.expression();

            if !self.search(vec![TkStatementEnd]) {
                panic!("Unexpected end of value declaration!")
            }

            let res = BindingDecl {
                name: Pair {
                    name: name.value.unwrap().get_string().unwrap(),
                    value: Box::new(initializer.get_expr_value().value.unwrap()),
                    kind,
                },
                initializer,
            };

            Statement::Bind(res)
        }
    }

    // fn init_param(&mut self) -> Token {
    //     let (name, kind) = self.val_signiture();

    //     Token {
    //         tokentype: TkSymbol,
    //         value: Some(SymbolValue::Pair(Pair {
    //             name: name
    //                 .clone()
    //                 .value
    //                 .unwrap()
    //                 .get_string()
    //                 .expect("Expected param name"),
    //             kind,
    //             value: Box::new(SymbolValue::Unknown),
    //         })),
    //         location: name.location,
    //     }
    // }

    fn val_signiture(&mut self) -> (Token, AKind) {
        let name: Token = self.consume(TkSymbol);

        let kind: AKind = if self.check(TkAssignInfer) {
            AKind::TyUnknown
        } else {
            self.consume(TkColon);
            self.inspect();

            let tty = self.consume_vec(vec![
                TkType(AKind::TyInt),
                TkType(AKind::TyFlt),
                TkType(AKind::TyStr),
                TkType(AKind::TyBool),
                TkType(AKind::TyEof),
                TkType(AKind::TyMute),
            ]);
            //dbg!(tty.clone());
            let k = match tty.tokentype {
                TkType(t) => t,
                _ => panic!("Token cannot be type"),
            };
            k
        };

        (
            Token {
                tokentype: name.clone().tokentype,
                value: Some(SymbolValue::Pair(Pair {
                    name: name
                        .clone()
                        .value
                        .unwrap()
                        .get_string()
                        .expect("Expected param name"),
                    kind: kind.clone(),
                    value: Box::new(SymbolValue::Unknown),
                })),
                location: name.location,
            },
            kind,
        )
    }

    fn func_decl(&mut self) -> Statement {
        let name: Token = self.consume(TkSymbol);

        self.consume(TkColon);

        let opreturnkind_tk = self.advance().tokentype;
        let opreturnkind = AKind::TyOp(match opreturnkind_tk {
            TkType(t) => Box::new(t),
            _ => panic!("invalid type {opreturnkind_tk:?}"),
        });

        //let opreturnkind = AKind::TyOp(Box::new(AKind::TyUnknown));

        self.consume(TkKwOf);

        self.consume(TkLparen);

        let mut params: Vec<BindingDecl> = vec![];
        if self.current().tokentype != TkRparen {
            while self.current().tokentype != TkRparen {
                let nv = self.val_signiture();
                params.push(BindingDecl {
                    name: Pair {
                        name: nv.0.value.clone().unwrap().get_string().unwrap(),
                        kind: nv.1,
                        value: Box::new(nv.0.value.unwrap()),
                    },
                    initializer: Expr::Empty,
                });

                if self.check(TkRparen) {
                    self.advance();

                    break;
                } else {
                    self.advance();
                }
            }
        }
        //self.advance();

        //dbg!(params.clone());

        self.consume(TkRparen);

        self.consume(TkSmallArr);

        // Fix return type of op name's identity
        //match name.value.clone().unwrap() {
        //    SymbolValue::Identity(ref mut i) => i.kind = Some(Box::new(opreturnkind.clone())),
        //    _ => panic!("Invalid function name"),
        //}

        Statement::Function(FuncDecl {
            name,
            params,
            returnval: opreturnkind,
            body: BlockStmt {
                statements: self.funcblock(),
            },
        })
    }

    fn funcblock(&mut self) -> Vec<Statement> {
        let mut collector: Vec<Statement> = vec![];
        //self.inspect();
        if self.check(TkKwReturn) {
            collector.push(self.statement());
        } else {
            self.consume(TkLBrace);
            while self.tkvec[self.curr].tokentype != TkRBrace
                && self.tkvec[self.curr + 1].tokentype != TkEof
            {
                collector.push(self.statement());
            }

            self.consume(TkRBrace);
        }
        collector
    }

    fn block(&mut self) -> Vec<Statement> {
        let mut collector: Vec<Statement> = vec![];

        self.consume(TkLBrace);
        while self.tkvec[self.curr].tokentype != TkRBrace
            && self.tkvec[self.curr + 1].tokentype != TkEof
        {
            collector.push(self.statement());
        }
        self.consume(TkRBrace);

        collector
    }

    fn declaration(&mut self) -> Statement {
        if self.search(vec![TkKwLet]) {
            return self.func_decl();
        }
        self.statement()
    }

    pub fn parse(&mut self) {
        let mut statements: Vec<Statement> = vec![];
        while !self.is_at_end() {
            statements.push(self.declaration());
        }
        for el in statements.clone() {
            match el {
                Statement::Expression(ref es) => match es.expression {
                    Expr::Empty => {}
                    _ => {
                        self.new_stmts.push(el.to_owned());
                    }
                },
                _ => self.new_stmts.push(el.to_owned()),
            }
        }
    }

    pub fn supply(&mut self) -> Vec<Statement> {
        self.new_stmts.clone()
    }
}
