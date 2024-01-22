use crate::core::resource::ast::*;

pub struct Typechecker {
    ast: Vec<Statement>,
    loc: usize,
    checked: Vec<Statement>,
}

impl Typechecker {
    pub fn new(ast: Vec<Statement>) -> Self {
        return Typechecker {
            ast,
            loc: 0,
            checked: vec![],
        };
    }

    fn add(&mut self, st: Statement) {
        self.checked.push(st);
    }

    pub fn check(&mut self) {
        while self.loc < self.ast.len() {
            let mut stmt = self.ast[self.loc].clone();
            //println!("{:?}", stmt);
            self.add(do_check(&mut stmt));
            self.loc += 1
        }
    }

    pub fn supply(&mut self) -> Vec<Statement> {
        return self.checked.clone();
    }
}

fn do_check(stmt: &mut Statement) -> Statement {
    let mut to_kind: SymbolKind = SymbolKind::Unknown;
    match *stmt {
        Statement::Val(ref mut vd) => {
            let init = &vd.initializer;
            match init {
                Expr::Literal(le) => {
                    to_kind = le.value.literal.clone();
                }
                Expr::Binary(be) => {
                    let mut ns = Statement::Expression(ExpressionStmt { expression: *be.left.clone() });
                    let ds = do_check(&mut ns);
                    //dbg!(ds.clone());
                }
                _ => panic!("Invalid value declaration"),
            }
            vd.kind = to_kind;
            return Statement::Val(vd.clone())
        }
        _ => {
            return stmt.clone();
        }
    }
}
