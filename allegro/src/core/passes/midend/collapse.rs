use crate::core::resource::ast::*;

pub struct Collapser {
    ast: Vec<Statement>,
    loc: usize,
    collapsed: Vec<Statement>,
}

impl Collapser {
    pub fn new(ast: Vec<Statement>) -> Self {
        return Collapser {
            ast,
            loc: 0,
            collapsed: vec![],
        };
    }

    fn add(&mut self, st: Statement) {
        self.collapsed.push(st);
    }

    pub fn collapse(&mut self) {
        while self.loc < self.ast.len() {
            let stmt = self.ast[self.loc].clone();
            let _to_create: Statement;
            match stmt {
                Statement::Expression(ref e) => match &e.expression {
                    Expr::Binary(b) => match b.operator.kind {
                        crate::core::resource::tokens::TokenKind::TkPlus => {
                            let new_val = &b.left;
                            
                        }
                        _ => {}
                    },
                    _ => {}
                },
                _ => {}
            }
            self.add(stmt.clone());
            self.loc += 1
        }
    }

    pub fn supply(&mut self) -> Vec<Statement> {
        return self.collapsed.clone();
    }
}
