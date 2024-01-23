use core::panic;

use crate::core::resource::ast::*;

#[derive(Debug, Clone)]
pub struct GenLine {
    i: usize,
    c: String,
}

pub struct Generator {
    ast: Vec<Statement>,
    loc: usize,
    output: Vec<GenLine>,
}

#[allow(missing_fragment_specifier)]
macro_rules! line {
    ($i:expr, $($arg:tt)*) => {
        GenLine {
            i: $i,
            c: format!("{}", format_args!($($arg)*))
        }
    };
}

impl Generator {
    pub fn new(ast: Vec<Statement>) -> Self {
        return Generator {
            ast,
            loc: 0,
            output: vec![],
        };
    }

    fn add(&mut self, st: GenLine) {
        self.output.push(st);
    }

    fn get_identity(&mut self, sk: &SymbolKind) -> String {
        match sk {
            SymbolKind::Identity(n, _) => {
                return n.to_string()
            }
            _ => panic!("Expected Identity, found {:?}", sk)
        }
    }

    fn get_cval(&mut self, sk: SymbolKind) -> String {
        match sk {
            SymbolKind::Int(i) => i.to_string(),
            SymbolKind::Str(s) => s.to_string(),
            SymbolKind::Float(f) => f.to_string(),
            _ => {
                panic!("Unkown type!")
            }
        }
    }

    fn get_ctype(&mut self, vd: ValDecl) -> String {
        //println!("{vd:?}");
        match vd.kind {
            SymbolKind::TyStr => return "char*".to_string(),

            SymbolKind::TyInt => return "int".to_string(),

            _ => panic!("Unsupported type!"),
        }
    }

    fn get_oprtkind(&mut self, op: OpDecl) -> String {
        //println!("{vd:?}");
        match op.kind {
            SymbolKind::TyStr => return "char*".to_string(),

            SymbolKind::TyInt => return "int".to_string(),

            SymbolKind::Nothing => return "void".to_string(),

            _ => panic!("Unsupported type! {:?}", op.kind),
        }
    }

    pub fn generate(&mut self) {
        while self.loc < self.ast.len() {
            let el = self.ast[self.loc].clone();
            match el {
                Statement::Val(vd) => {
                    let cname: String;
                    let ckind = self.get_ctype(vd.clone());
                    let cval: String = self.get_cval(match vd.initializer {
                        Expr::Literal(mut le) => le.get_literal_value(),
                        _ => panic!("Unkown type!"),
                    });
                    cname = self.get_identity(&vd.name.literal);
                    self.add(line!(0, "{} {} = {}", ckind, cname, cval));
                }

                Statement::Operation(op) => {
                    let opname: String = self.get_identity(&op.name.literal);
                    let oprtkind: String = self.get_oprtkind(op.clone());

                    for stmt in op.body.statements {
                        self.generate()
                    }

                    println!("{} {}()", oprtkind, opname);
                }

                _ => {
                    panic!("Unsuportted ast Token {:?}", el);
                }
            };

            self.loc += 1
        }
    }

    pub fn supply(&mut self) -> String {
        let mut accum: String = "void main() {\n".to_string();
        for i in self.output.clone() {
            accum = format!("{accum}{};\n", i.c)
        }
        accum = format!("{accum}}}");
        return accum;
    }
}
