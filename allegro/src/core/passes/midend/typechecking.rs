use crate::core::resource;
use crate::core::resource::ast::Expr;
use crate::core::resource::ast::Statement;
use crate::core::resource::environment::AKind;
use crate::core::resource::environment::Environment;
use crate::core::resource::tokens::TokenType;

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
        return Typechecker {
            ast,
            loc: 0,
            env: Environment::new(),
            has_current_op_returned: false,
            current_op_kind: None,
            checked: vec![],
        };
    }

    fn check_statement(&mut self, stmt: Statement) {
        match stmt {
            Statement::Expression(e) => {
                self.resolve_expr(e.expression);
            }
            Statement::Val(vd) => {
                //println!("{:?}", vd);
                let declared_type = vd.name.value.clone().to_akind();
                let resolved_type = self.resolve_expr(vd.initializer.clone());
                println!("Declared: {:?}", declared_type);
                println!("Resolved: {:?}", resolved_type);

                self.expect_expr(
                    vd.initializer,
                    resolved_type.expect("Expected type"),
                    vec![declared_type.clone()],
                );
                self.env
                    .define(vd.name.value.get_string().unwrap(), declared_type)
            }
            Statement::Block(b) => {
                let previous = self.env.clone();
                self.env = Environment::new_with_previous(previous.clone());

                for statement in b.statements {
                    self.check_statement(statement)
                }

                self.env = previous.clone()
            }

            Statement::Operation(o) => {
                let op_type = o.returnval.to_akind();
                self.env
                    .define(o.name.value.clone().get_string().unwrap(), op_type.clone());

                let previous = self.env.clone();
                self.env = Environment::new_with_previous(previous.clone());

                self.has_current_op_returned = false;
                self.current_op_kind = Some(op_type);

                for param in o.params {
                    //println!("{:?}", param);
                    self.env
                        .define(param.name.value.clone().get_string().unwrap(), param.name.value.to_akind())
                }

                self.check_statement(Statement::Block(o.body));

                if self.current_op_kind != Some(AKind::TyMute) && !self.has_current_op_returned {
                    panic!(
                        "Expected operation '{:?}' to return a value of type {:?}",
                        o.name.value.clone().get_string().unwrap(),
                        self.current_op_kind,
                    )
                }

                self.env = previous.clone();
            }

            Statement::Print(p) => {
                self.resolve_expr(p.expression);
            }

            Statement::If(i) => {
                let condition_type = self.resolve_expr(i.condition.clone());
                self.expect_expr(
                    i.condition,
                    condition_type.expect("Expected Type"),
                    vec![AKind::TyBool],
                );
                self.check_statement(*i.then_branch.clone());
                if i.else_branch.is_some() {
                    self.check_statement(*i.else_branch.unwrap().clone())
                }
            }

            Statement::Return(r) => {
                if self.current_op_kind == Some(AKind::TyMute) {
                    println!("{:?}", self.current_op_kind);
                    panic!("This operation does not return any value")
                }
                let value_kind = self.resolve_expr(r.value.clone());
                self.expect_expr(
                    r.value,
                    value_kind.expect("Expected Type"),
                    vec![self.current_op_kind.clone().expect("Expected return type")],
                );
                self.has_current_op_returned = true;
            }

            _ => panic!("Unknown Statement type: {:?}", stmt),
        }
    }

    fn expect_expr(&mut self, expr: Expr, exprkind: AKind, expected_kinds: Vec<AKind>) {
        for kind in &expected_kinds {
            let nk: AKind = exprkind.clone();

            if expected_kinds.contains(&AKind::TyUnknown){
                return;
            } else if nk == *kind {
                return;
            }
        }

        println!(
            "Error: Expected {:?} to be of type {:?}, but found {:?}",
            expr, expected_kinds, exprkind
        );
        std::process::exit(1);
    }

    fn resolve_expr(&mut self, expr: Expr) -> Option<AKind> {
        match expr {
            Expr::Literal(l) => Some(l.value.value.to_akind()),
            Expr::Binary(b) => {
                let lhstype = self.resolve_expr(*b.left.clone());
                let rhstype = self.resolve_expr(*b.right.clone());

                match b.operator.tokentype {
                    TokenType::TkPlus => self.expect_expr(
                        *b.left.clone(),
                        lhstype.clone()?,
                        vec![AKind::TyInt, AKind::TyFlt, AKind::TyStr],
                    ),
                    TokenType::TkMinus
                    | TokenType::TkStar
                    | TokenType::TkSlash
                    | TokenType::TkCLE
                    | TokenType::TkCLT
                    | TokenType::TkCGE
                    | TokenType::TkCGT => self.expect_expr(
                        *b.left.clone(),
                        lhstype.clone()?,
                        vec![AKind::TyInt, AKind::TyFlt],
                    ),
                    _ => panic!("Invalid operator {:?}", b.operator),
                }
                if lhstype.clone().unwrap() != AKind::TyUnknown {
                    if lhstype.clone() != rhstype.clone() {
                        panic!("{:?} and {:?} are of differing types!", lhstype, rhstype);
                    }
                }

                match b.operator.tokentype {
                    TokenType::TkCLE
                    | TokenType::TkCLT
                    | TokenType::TkCGE
                    | TokenType::TkCGT
                    | TokenType::TkCEQ
                    | TokenType::TkCNE => return Some(AKind::TyBool),
                    _ => return Some(AKind::TyInt),
                }
            }
            Expr::Unary(u) => {
                let rhstype = self.resolve_expr(*u.right.clone());
                match u.operator.tokentype {
                    TokenType::TkMinus => {
                        self.expect_expr(*u.right, rhstype?, vec![AKind::TyInt, AKind::TyFlt]);
                        return Some(AKind::TyInt);
                    }
                    _ => panic!("Invalid unary operation {:?}", u.operator),
                }
            }
            Expr::Value(mut v) => return Some(self.env.get(v.name.value.get_string().unwrap())),
            Expr::Assign(a) => {
                let value_type = self.resolve_expr(*a.value.clone());
                let bind_type = self.env.get(a.name.value.get_string().unwrap());
                self.expect_expr(*a.value.clone(), value_type.clone()?, vec![bind_type]);
                return value_type;
            }
            Expr::Call(c) => {
                let callee_type = self.resolve_expr(*c.callee.clone());
                for (i, arg) in c.args.iter().enumerate() {
                    let arg_type = self.resolve_expr(arg.clone());
                    let expected_type = c.clone().args[i].get_expr_value().value.to_akind();
                    println!("{:?}", expected_type.clone());
                    self.expect_expr(arg.clone(), arg_type?, vec![expected_type])
                }
                return callee_type;
            }

            Expr::Grouping(g) => {
                return self.resolve_expr(*g.expression)
            }

            Expr::Empty => return None,

            _ => panic!("Unknown expression type {:?}", expr),
        }
    }

    pub fn check(&mut self) {
        let mut nast = self.ast.clone();
        for s in nast.len()..0 {
            let c = &nast[s];
            if *c == Statement::Expression(resource::ast::ExpressionStmt { expression: Expr::Empty }) {
                nast.remove(self.loc);
            }
        }
        while self.loc < nast.len() {
            //dbg!(self.env.clone());
            let stmt: Statement = nast[self.loc].clone();
            self.check_statement(stmt.clone());
            self.checked.push(stmt);
            self.loc += 1
        }
    }

    pub fn supply(&mut self) -> Vec<Statement> {
        return self.checked.clone();
    }
}
