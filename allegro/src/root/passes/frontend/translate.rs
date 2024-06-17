use hime_redist::symbols::{SemanticElement, SemanticElementTrait};

use crate::root::resource::ast::Expr;
use crate::{
    lang::Actions,
    root::resource::{
        self,
        ast::{BinOp, Itype, Statement, ToItype},
    },
};

pub struct Translator {
    pub temp_statements: Vec<Statement>,
    pub funcdecl: Vec<Statement>,
    pub valuestack: Vec<Expr>,
}

impl Translator {
    pub fn new() -> Self {
        Self {
            temp_statements: vec![],
            funcdecl: vec![],
            valuestack: vec![],
        }
    }
}

impl Actions for Translator {
    fn on_number(
        &mut self,
        _head: hime_redist::symbols::Symbol,
        body: &dyn hime_redist::symbols::SemanticBody,
    ) {
        let num = body.get_element_at(0);
        //dbg!(num.clone());

        let val = num.get_value();

        let sc = val.unwrap().to_string().to_itype();
        //dbg!(sc.clone());
        self.valuestack.push(Expr::Scalar(sc))
        //panic!()
    }

    fn on_string(
        &mut self,
        _head: hime_redist::symbols::Symbol,
        body: &dyn hime_redist::symbols::SemanticBody,
    ) {
        todo!()
    }

    fn on_identifier(
        &mut self,
        _head: hime_redist::symbols::Symbol,
        body: &dyn hime_redist::symbols::SemanticBody,
    ) {
        let name = body.get_element_at(0).get_value();
        //dbg!(name.clone());

        //dbg!(name.clone());
        self.valuestack
            .push(Expr::Variable(name.unwrap().to_string()));
        //panic!()
    }

    fn on_bin(
        &mut self,
        _head: hime_redist::symbols::Symbol,
        body: &dyn hime_redist::symbols::SemanticBody,
    ) {
        //dbg!(&self.valuestack);
        let right = self.valuestack.pop().unwrap();
        let left = self.valuestack.pop().unwrap();
        let op = body.get_element_at(1).get_symbol().name;
        dbg!(left.clone(), right.clone());
        dbg!(op);

        self.valuestack.push(Expr::Binary {
            lhs: Box::new(left),
            op: BinOp::from_str(op),
            rhs: Box::new(right),
        })
    }

    fn on_bind(
        &mut self,
        _head: hime_redist::symbols::Symbol,
        body: &dyn hime_redist::symbols::SemanticBody,
    ) {
        let name = body.get_element_at(0).get_value();

        self.temp_statements.push(Statement::Bind {
            name: name.unwrap().to_string(),
            value: self.valuestack.pop().unwrap(),
        });
        //panic!()
    }

    fn on_range(
        &mut self,
        _head: hime_redist::symbols::Symbol,
        body: &dyn hime_redist::symbols::SemanticBody,
    ) {
        todo!()
    }

    fn on_for(
        &mut self,
        _head: hime_redist::symbols::Symbol,
        body: &dyn hime_redist::symbols::SemanticBody,
    ) {
        todo!()
    }

    fn on_while(
        &mut self,
        _head: hime_redist::symbols::Symbol,
        body: &dyn hime_redist::symbols::SemanticBody,
    ) {
        todo!()
    }

    fn on_if(
        &mut self,
        _head: hime_redist::symbols::Symbol,
        body: &dyn hime_redist::symbols::SemanticBody,
    ) {
        todo!()
    }

    fn on_print(
        &mut self,
        _head: hime_redist::symbols::Symbol,
        body: &dyn hime_redist::symbols::SemanticBody,
    ) {
        self.temp_statements.push(Statement::Print {
            value: self.valuestack.pop().unwrap(),
        });
    }

    fn on_func(
        &mut self,
        _head: hime_redist::symbols::Symbol,
        body: &dyn hime_redist::symbols::SemanticBody,
    ) {
        let name = body.get_element_at(0).get_value().unwrap().to_string();
        let rt = match body.get_element_at(2).get_value() {
            Some(s) => s.to_string().to_itype(),
            None => Itype::Mute,
        };

        let params: Vec<String> = vec![];
        let t = body.get_element_at(2).get_value();

        //dbg!(t);
        //panic!()
        self.funcdecl.push(Statement::Func {
            name,
            rt,
            params,
            body: self.temp_statements.clone(),
        });
        self.temp_statements.clear();
    }

    fn on_param(
        &mut self,
        head: hime_redist::symbols::Symbol,
        body: &dyn hime_redist::symbols::SemanticBody,
    ) {
        let name = body.get_element_at(0).get_value();

        self.valuestack
            .push(Expr::Variable(name.unwrap().to_string()));
    }

    fn on_return(
        &mut self,
        head: hime_redist::symbols::Symbol,
        body: &dyn hime_redist::symbols::SemanticBody,
    ) {
        self.temp_statements.push(Statement::Return {
            value: self.valuestack.pop().unwrap(),
        });
    }

    fn on_call(
        &mut self,
        head: hime_redist::symbols::Symbol,
        body: &dyn hime_redist::symbols::SemanticBody,
    ) {
        let name = body.get_element_at(0).get_value();
        dbg!(name);
    }
}
