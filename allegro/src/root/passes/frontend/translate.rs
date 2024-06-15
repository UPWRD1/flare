use crate::{lang::Actions, root::legacy_resource::{ast::{ExpressionStmt, Statement}, tokens::Token}};
use crate::root::legacy_resource::ast::*;
pub struct Translator {
    statements: Vec<Statement>
}

impl Translator {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }
}

impl Actions for Translator {
    fn on_number(&mut self, head: hime_redist::symbols::Symbol, body: &dyn hime_redist::symbols::SemanticBody) {
        
    }

    fn on_string(&mut self, head: hime_redist::symbols::Symbol, body: &dyn hime_redist::symbols::SemanticBody) {
        todo!()
    }

    fn on_identifier(&mut self, head: hime_redist::symbols::Symbol, body: &dyn hime_redist::symbols::SemanticBody) {
        self.statements.push(Statement::Expression(ExpressionStmt { expression: Expr::Value(ValueExpr {}) }))
    }

    fn on_bin(&mut self, head: hime_redist::symbols::Symbol, body: &dyn hime_redist::symbols::SemanticBody) {
        todo!()
    }

    fn on_bind(&mut self, head: hime_redist::symbols::Symbol, body: &dyn hime_redist::symbols::SemanticBody) {
        todo!()
    }
}