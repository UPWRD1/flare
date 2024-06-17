use crate::{lang::Actions, root::resource::ast::Statement};

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
        dbg!(head);
        //dbg!(body);
        panic!()
    }

    fn on_string(&mut self, head: hime_redist::symbols::Symbol, body: &dyn hime_redist::symbols::SemanticBody) {
        todo!()
    }

    fn on_identifier(&mut self, head: hime_redist::symbols::Symbol, body: &dyn hime_redist::symbols::SemanticBody) {
        todo!()
    }

    fn on_bin(&mut self, head: hime_redist::symbols::Symbol, body: &dyn hime_redist::symbols::SemanticBody) {
        todo!()
    }

    fn on_bind(&mut self, head: hime_redist::symbols::Symbol, body: &dyn hime_redist::symbols::SemanticBody) {
        todo!()
    }
    
    fn on_range(&mut self, head: hime_redist::symbols::Symbol, body: &dyn hime_redist::symbols::SemanticBody) {
        todo!()
    }
    
    fn on_for(&mut self, head: hime_redist::symbols::Symbol, body: &dyn hime_redist::symbols::SemanticBody) {
        todo!()
    }
    
    fn on_while(&mut self, head: hime_redist::symbols::Symbol, body: &dyn hime_redist::symbols::SemanticBody) {
        todo!()
    }
    
    fn on_if(&mut self, head: hime_redist::symbols::Symbol, body: &dyn hime_redist::symbols::SemanticBody) {
        todo!()
    }
    
    fn on_print(&mut self, head: hime_redist::symbols::Symbol, body: &dyn hime_redist::symbols::SemanticBody) {
        todo!()
    }
    
    fn on_func(&mut self, head: hime_redist::symbols::Symbol, body: &dyn hime_redist::symbols::SemanticBody) {
        todo!()
    }
}