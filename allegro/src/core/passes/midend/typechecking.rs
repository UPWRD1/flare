use core::panic;

use crate::core::resource::{ast::*, states::{Appearances, StateTable}};

pub struct Typechecker {
    ast: Vec<Statement>,
    loc: usize,
    tbl: StateTable,
    checked: Vec<Statement>,
}

impl Typechecker {
    pub fn new(ast: Vec<Statement>) -> Self {
        return Typechecker {
            ast,
            loc: 0,
            tbl: StateTable::new(),
            checked: vec![],
        };
    }

    fn add(&mut self, st: Statement) {
        self.checked.push(st);
    }

    fn check_var_in_expr(&mut self, tbl: StateTable, depth: usize, name: Statement, expr: Expr) {
    }

    fn check_expr(&mut self, tbl: StateTable, depth: usize, expr: Expr) -> StateTable {
        let names: Vec<Statement> = tbl.clone().entries.iter().map(|e| e.name.clone()).collect();
        for entry in tbl.clone().entries {
            self.check_var_in_expr(tbl.clone(), depth, entry.name, expr.clone())
        }
        tbl
    }

    fn check_statement(&mut self, tbl: StateTable, depth: usize, stmt: Statement) {
        match stmt {
            Statement::Val(vd) => {
                let tbl: StateTable = self.check_expr(tbl, depth, vd.initializer);
            }
            _ => panic!("Unkown statemnet")
        }
    }

    fn linearity_check(&mut self, params: Vec<Statement>, body: BlockStmt) {
        let depth = 0;

        let mut ntbl: StateTable = StateTable::new();

        ntbl.init_table(params)
    }

    pub fn check(&mut self) {
        while self.loc < self.ast.len() {
            let mut stmt: Statement = self.ast[self.loc].clone();

            self.loc += 1
        }
    }

    pub fn supply(&mut self) -> Vec<Statement> {
        return self.checked.clone();
    }
}