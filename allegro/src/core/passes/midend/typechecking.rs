use crate::core::resource;
use crate::core::resource::ast::Expr;
use crate::core::resource::ast::Statement;
use crate::core::resource::environment::AKind;
use crate::core::resource::environment::Environment;
use crate::core::resource::errors::Errors::*;
use crate::core::resource::tokens::TokenType;
use crate::error;
use crate::error_nocode;

pub struct Typechecker {
    ast: Vec<Statement>,
    loc: usize,
    env: Environment,
    has_current_op_returned: bool,
    current_op_kind: Option<AKind>,
    checked: Vec<Statement>,
}

impl Typechecker {
    pub fn new(ast: Vec<Statement>) -> Self {
        Typechecker {
            ast,
            loc: 0,
            env: Environment::new(),
            has_current_op_returned: false,
            current_op_kind: None,
            checked: vec![],
        }
    }

    fn check_statement(&mut self, stmt: Statement) {
        match stmt {
            Statement::Expression(e) => {
                self.resolve_expr(e.expression);
            }
            Statement::Bind(mut bd) => {
                let mut declared_type = bd.name.kind;

                let mut resolved_type = self.resolve_expr(bd.initializer.clone());

                if declared_type.is_unknown() {
                    let x: AKind = self
                        .env
                        .get_akind_scoped(bd.name.value.get_string().unwrap());
                    bd.name.kind = x.clone();
                    declared_type = x;
                }

                if declared_type.is_op() {
                    declared_type = declared_type.extract_op_type();
                }
                if resolved_type.clone().unwrap().is_op() {
                    resolved_type = Some(resolved_type.unwrap().extract_op_type())
                }

                self.expect_expr(
                    bd.initializer,
                    resolved_type.expect("Expected type"),
                    declared_type.clone(),
                );
                self.env.define(bd.name.name, declared_type, -1)

            }
            Statement::Block(b) => {
                for statement in b.statements {
                    self.check_statement(statement)
                }

                //self.env.scope.drop_enclosing();
            }

            Statement::Operation(o) => {
                let op_type = o.returnval;
                
                self.env.define(
                    o.name.value.clone().unwrap().get_string().unwrap(),
                    op_type.clone(),
                    o.params.len().try_into().unwrap(),
                );

                self.has_current_op_returned = false;
                self.current_op_kind = Some(op_type);

                self.env.scope.new_parent();

                for param in o.params {
                    //println!("{}", param.name.name);
                    //self.env.scope.define(Entry { name: param.name.name, arity: -1, value: param.name.kind })
                    self.env.define(param.name.name, param.name.kind, -1)
                }

                self.check_statement(Statement::Block(o.body));

                if self.current_op_kind != Some(AKind::TyOp(Box::new(AKind::TyMute)))
                    && !self.has_current_op_returned
                {
                    panic!(
                        "Expected operation '{:?}' to return a value of type {:?}",
                        o.name.value.clone().unwrap().get_string().unwrap(),
                        self.current_op_kind,
                    )
                }

                self.env.scope.drop_enclosing()
            }

            Statement::Print(p) => {
                self.resolve_expr(p.expression);
            }

            Statement::If(i) => {
                let condition_type = self.resolve_expr(i.condition.clone());
                self.expect_expr(
                    i.condition,
                    condition_type.expect("Expected Type"),
                    AKind::TyBool,
                );
                self.check_statement(*i.then_branch.clone());
                if i.else_branch.is_some() {
                    self.check_statement(*i.else_branch.unwrap().clone())
                }
            }

            Statement::Return(r) => {
                if self.current_op_kind == Some(AKind::TyMute) {
                    panic!("This operation does not return any value")
                }
                let value_kind = self.resolve_expr(r.value.clone());
                self.expect_expr(
                    r.value,
                    value_kind.expect("Expected Type"),
                    self.current_op_kind
                        .clone()
                        .expect("Expected return type")
                        .extract_op_type(),
                );
                self.has_current_op_returned = true;
            }

            _ => panic!("Unknown Statement type: {:?}", stmt),
        }
    }

    fn expect_expr(&mut self, expr: Expr, exprkind: AKind, expected_kind: AKind) {
        let nk: AKind = exprkind.clone();

        match nk {
            AKind::TyUnknown => {
                return;
            }
            AKind::TyOp(ref t) => {
                if **t == expected_kind {
                    return;
                }
            }

            _ => {
                if nk == expected_kind {
                    return;
                }
            }
        }

        panic!(
            "Expected {:?} to be of type {:?}, but found {:?}",
            expr, expected_kind, exprkind
        );
        //std::process::exit(1);
    }

    fn resolve_expr(&mut self, expr: Expr) -> Option<AKind> {
        match expr {
            Expr::ScalarEx(l) => Some(l.value.value.unwrap().to_akind()),
            Expr::Binary(ref b) => {
                let lhstype = self.resolve_expr(*b.left.clone());
                let rhstype = self.resolve_expr(*b.right.clone());

                match b.operator.tokentype {
                    TokenType::TkPlus => {
                        if !lhstype.clone().unwrap().is_numeric_type()
                            && lhstype.clone().unwrap() != AKind::TyStr
                        {
                            error_nocode!(
                                "Expected {:?} to be of type Int, Flt, or Str, but found {:?}",
                                expr,
                                lhstype
                            );
                        }
                    }
                    TokenType::TkMinus
                    | TokenType::TkStar
                    | TokenType::TkSlash
                    | TokenType::TkCLE
                    | TokenType::TkCLT
                    | TokenType::TkCGE
                    | TokenType::TkCGT => {
                        if !lhstype.clone().unwrap().is_numeric_type() {
                            error_nocode!(
                                "Expected {:?} to be of type Int or Flt, but found {:?}",
                                expr,
                                lhstype
                            );
                        }
                    }
                    _ => panic!("Invalid operator {:?}", b.operator),
                }
                if lhstype.clone().unwrap() != AKind::TyUnknown
                    && lhstype.clone() != rhstype.clone()
                {
                    panic!("{:?} and {:?} are of differing types!", lhstype, rhstype);
                }

                match b.operator.tokentype {
                    TokenType::TkCLE
                    | TokenType::TkCLT
                    | TokenType::TkCGE
                    | TokenType::TkCGT
                    | TokenType::TkCEQ
                    | TokenType::TkCNE => Some(AKind::TyBool),
                    _ => Some(AKind::TyInt),
                }
            }
            Expr::Unary(ref u) => {
                let rhstype = self.resolve_expr(*u.right.clone());
                match u.operator.tokentype {
                    TokenType::TkMinus => {
                        if !rhstype.clone().unwrap().is_numeric_type() {
                            error_nocode!(
                                "Expected {:?} to be of type Int or Flt, but found {:?}",
                                expr,
                                rhstype
                            );
                        }
                        Some(AKind::TyInt)
                    }
                    _ => panic!("Invalid unary operation {:?}", u.operator),
                }
            }
            Expr::Value(v) => Some(
                self.env
                    .get_akind_scoped(v.name.value?.get_string().unwrap()),
            ),
            Expr::Assign(a) => {
                let value_type = self.resolve_expr(*a.value.clone());
                let bind_type = self
                    .env
                    .get_akind_symbol(&a.name.value.clone()?.get_string().unwrap());
                self.expect_expr(*a.value.clone(), value_type.clone()?, bind_type);
                self.env.define(
                    a.name.value.unwrap().get_string().unwrap(),
                    value_type.clone().unwrap(),
                    -1,
                );
                value_type
            }
            Expr::Call(c) => {
                let retval = self
                    .env
                    .get_akind_scoped(c.callee.value.clone().unwrap().get_string().unwrap());
                //let callee_type = c.callee.value.clone().unwrap().to_akind();
                for (i, arg) in c.args.iter().enumerate() {
                    let arg_type = self.resolve_expr(arg.clone());
                    let expected_type =
                        c.clone().args[i].get_expr_value().value.unwrap().to_akind();
                    //println!("{:?}", expected_type.clone());
                    self.expect_expr(arg.clone(), arg_type?, expected_type);
                    return Some(retval);
                }
                let call_arity = self
                    .env
                    .get_arity(c.callee.value.clone().unwrap().get_string().unwrap());
                if call_arity != c.args.len().try_into().unwrap() {
                    error!(
                        SyntaxBadArguments,
                        (
                            c.callee.value.clone().unwrap().get_string().unwrap(),
                            call_arity as usize,
                            c.args.len()
                        )
                    );
                }

                Some(retval)
            }

            Expr::Grouping(g) => self.resolve_expr(*g.expression),

            Expr::Empty => None,

            _ => panic!("Unknown expression type {:?}", expr),
        }
    }

    pub fn check(&mut self) {
        let mut nast = self.ast.clone();
        for s in nast.len()..0 {
            let c = &nast[s];
            if *c
                == Statement::Expression(resource::ast::ExpressionStmt {
                    expression: Expr::Empty,
                })
            {
                nast.remove(s);
            }
        }
        /*while self.loc < nast.len() {
            let stmt: Statement = nast[self.loc].clone();
            self.build_environment(stmt.clone());
            self.loc += 1;
        }
        */
        self.loc = 0;
        while self.loc < nast.len() {
            //dbg!(self.env.clone());
            let stmt: Statement = nast[self.loc].clone();
            self.check_statement(stmt.clone());
            self.checked.push(stmt.clone());
            //dbg!(stmt);
            self.loc += 1
        }
    }

    pub fn supply(&mut self) -> (Vec<Statement>, Environment) {
        (self.checked.clone(), self.env.clone())
    }
}
