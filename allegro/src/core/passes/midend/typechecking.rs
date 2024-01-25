use crate::core::resource::ast::Expr;
use crate::core::resource::ast::Statement;
use crate::core::resource::ast::SymbolKind;
use crate::core::resource::ast::SymbolKind::*;
use crate::core::resource::environment::Environment;
use crate::core::resource::tokens::TokenType;

pub struct Typechecker {
    ast: Vec<Statement>,
    loc: usize,
    env: Environment,
    has_current_op_returned: bool,
    current_op_kind: Option<SymbolKind>,
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
                let declared_type = vd.kind;
                let resolved_type = self.resolve_expr(vd.initializer.clone());
                self.expect_expr(vd.initializer, resolved_type, vec![declared_type])
            }
            Statement::Block(b) => {
                let previous = self.env.clone();
                self.env = Environment::new_with_previous(previous.clone());

                for statement in b.statements {
                    self.check_statement(statement)
                }

                self.env = previous.clone()
            }

            Statement::Operation(mut o) => {
                let op_type = o.kind;
                self.env.define(o.name.kind.get_identity_string(), op_type.clone());

                let previous = self.env.clone();
                self.env = Environment::new_with_previous(previous.clone());

                self.has_current_op_returned = false;
                self.current_op_kind = Some(op_type);

                for param in o.params {
                    self.env.define(param.name.kind.clone().get_identity_string(), param.kind)
                }

                self.check_statement(Statement::Block(o.body));

                if self.current_op_kind != Some(TyMute) && !self.has_current_op_returned {
                    panic!("Expected operation '{}' to return a value of type {:?}", o.name.kind.get_identity_string(), self.current_op_kind, )
                }

                self.env = previous.clone();
            }

            Statement::Print(p) => {
                self.resolve_expr(p.expression);
            }

            Statement::If(i) => {
                let condition_type = self.resolve_expr(i.condition.clone());
                self.expect_expr(i.condition, condition_type, vec![TyBool]);
                self.check_statement(*i.then_branch.clone());
                if i.else_branch.is_some() {
                    self.check_statement(*i.else_branch.unwrap().clone())
                }
            }

            Statement::Return(r) => {
                if self.current_op_kind == Some(TyMute) {
                    panic!("This operation does not return any value")
                }
                let value_kind = self.resolve_expr(r.value.clone());
                self.expect_expr(r.value, value_kind, vec![self.current_op_kind.clone().expect("Expected return type")]);
                self.has_current_op_returned = true;
            }

            _ => panic!("Unknown Statement type: {:?}", stmt),
        }
    }

    fn expect_expr(&mut self, expr: Expr, exprkind: SymbolKind, expected_kinds: Vec<SymbolKind>) {
        for kind in &expected_kinds {
            let nk = exprkind.clone().translate_kind();
            if &nk == kind {
                return;
            }
        }

        panic!(
            "Expected {:?} to be of type {:?}, but found {:?}",
            expr, expected_kinds, exprkind
        )
    }

    fn resolve_expr(&mut self, expr: Expr) -> SymbolKind {
        match expr {
            Expr::Literal(l) => l.value.kind,
            Expr::Binary(b) => {
                let lhstype = self.resolve_expr(*b.left.clone());
                let rhstype = self.resolve_expr(*b.right.clone());

                match b.operator.tokentype {
                    TokenType::TkPlus => {
                        self.expect_expr(*b.left.clone(), lhstype.clone(), vec![TyBool, TyFlt])
                    }
                    TokenType::TkMinus
                    | TokenType::TkStar
                    | TokenType::TkSlash
                    | TokenType::TkCLE
                    | TokenType::TkCLT
                    | TokenType::TkCGE
                    | TokenType::TkCGT => {
                        self.expect_expr(*b.left.clone(), lhstype.clone(), vec![TyInt])
                    }
                    _ => panic!("Invalid operator {:?}", b.operator),
                }

                if lhstype != rhstype {
                    panic!("{:?} and {:?} are of differing types!", b.left, b.right);
                }

                match b.operator.tokentype {
                    TokenType::TkCLE
                    | TokenType::TkCLT
                    | TokenType::TkCGE
                    | TokenType::TkCGT
                    | TokenType::TkCEQ
                    | TokenType::TkCNE => return SymbolKind::TyBool,
                    _ => return SymbolKind::TyInt,
                }
            }
            Expr::Unary(u) => {
                let rhstype = self.resolve_expr(*u.right.clone());
                match u.operator.tokentype {
                    TokenType::TkMinus => {
                        self.expect_expr(*u.right, rhstype, vec![TyInt]);
                        return TyInt;
                    }
                    _ => panic!("Invalid unary operation {:?}", u.operator),
                }
            }
            Expr::Value(mut v) => {
                return self.env.get(v.name.kind.get_identity_string())
            }
            Expr::Assign(a) => {
                let value_type = self.resolve_expr(*a.value.clone());
                let bind_type = self.env.get(a.name.kind.clone().get_identity_string());
                self.expect_expr(*a.value.clone(), value_type.clone(), vec![bind_type]);
                return value_type
            }
            Expr::Empty => {
                return SymbolKind::Nothing
            },

            _ => panic!("Unknown expression type {:?}", expr),
        }
    }

    pub fn check(&mut self) {
        while self.loc < self.ast.len() {
            let stmt: Statement = self.ast[self.loc].clone();
            self.check_statement(stmt);
            self.loc += 1
        }
    }

    pub fn supply(&mut self) -> Vec<Statement> {
        return self.checked.clone();
    }
}
