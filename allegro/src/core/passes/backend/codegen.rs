use core::panic;

use crate::core::resource::{ast::{self, *}, environment::AKind};

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

    fn get_cval(&mut self, sk: SymbolValue) -> String {
        match sk {
            SymbolValue::Int(i) => i.to_string(),
            SymbolValue::Str(s) => format!("\"{}\"", s.to_string()),
            SymbolValue::Float(f) => f.to_string(),
            _ => {
                panic!("Unkown type!")
            }
        }
    }

    fn get_ctype(&mut self, s: Statement) -> String {
        //println!("{s:?}");
        match s {
            Statement::Val(vd) => vd.name.value.to_akind().to_ctype(),
            Statement::Operation(o) => o.name.value.to_akind().to_ctype(),
            _ => panic!("Unsupported statement {s:?}")
        }
       
    }

    fn gen_cparams(&mut self, s: Statement) -> String {
        match s {
            Statement::Operation(o) => {
                let p = o.params;
                let mut accum: String = "".to_string();
                let mut count = 0;
                for v in &p {
                    let n = v.name.value.clone().get_string().unwrap();
                    let t = <ast::SymbolValue as Clone>::clone(&v.name.value).to_akind().to_ctype();
                    if count == &p.len() -1 {
                        accum = format!("{accum}{t} {n}");
                    } else {
                        accum = format!("{accum}{t} {n}, ");
                    }
                    count += 1;
                }
                return accum
            }
            _ => panic!(
                "{:?} is not an operation!", s
            )
        }
    }

    pub fn generate(&mut self) {
        while self.loc < self.ast.len() {
            let el = self.ast[self.loc].clone();
            self.gen_code(el);

            self.loc += 1
        }
    }

    fn gen_code(&mut self, el: Statement) {
        match el.clone() {
            Statement::Val(vd) => {
                let cname: String;
                let ckind = self.get_ctype(el);
                let cval: String = self.get_cval(match vd.initializer {
                    Expr::Literal(mut le) => le.value.value,
                    _ => panic!("Unknown type!"),
                });
                match vd.name.value {
                    SymbolValue::Identity(n) => cname = n.name.unwrap(),
                    _ => panic!("Unknown name!"),
                }
                self.add(line!(0, "{} {} = {};", ckind, cname, cval));
            }
    
            Statement::Operation(o) => {
                let cname: String = o.name.value.get_string().unwrap();
                let ckind = self.get_ctype(el.clone());
                let cparams = self.gen_cparams(el);
                self.add(line!(0, "{ckind} {cname}({cparams}) {{"));
                self.gen_code(Statement::Block(o.body))
        
            }
    
            Statement::Block(b) => {
                for s in b.statements {
                    self.gen_code(s)
                }
            }

            Statement::Expression(e) => {
                if e.expression == Expr::Empty {
                //do nothing
                }
            }
            _ => {
                panic!("Unsupported ast Token: {:?}", el);
            }
        };
    }
    
    pub fn supply(&mut self) -> String {
        let mut accum: String = "".to_string();
        for i in self.output.clone() {
            accum = format!("{accum}{}\n", i.c)
        }
        accum = format!("{accum}}}");
        return accum;
    }
}