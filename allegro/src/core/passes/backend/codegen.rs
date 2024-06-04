use core::panic;

use crate::core::resource::{
    ast::{self, *},
    environment::Environment,
    tokens::{Token, TokenType},
};

#[derive(Debug, Clone)]
pub struct GenLine {
    c: String,
}

pub struct Generator {
    ast: Vec<Statement>,
    env: Environment,
    loc: usize,
    output: Vec<GenLine>,
}

#[allow(missing_fragment_specifier)]
macro_rules! line {
    ($i:expr, $($arg:tt)*) => {
        GenLine {
            c: format!("{}", format_args!($($arg)*))
        }
    };
}

impl Generator {
    pub fn new(ast: Vec<Statement>, e: Environment) -> Self {
        Generator {
            ast,
            env: e,
            loc: 0,
            output: vec![],
        }
    }

    fn add(&mut self, st: GenLine) {
        self.output.push(st);
    }

    fn get_cval(&mut self, sk: SymbolValue) -> String {
        match sk {
            SymbolValue::Scalar(s) => match s {
                Scalar::Int(i) => i.to_string(),
                Scalar::Str(s) => format!("\"{}\"", s),
                Scalar::Float(f) => f.to_string(),
                _ => {
                    panic!("Unkown type!")
                }
            },
            _ => {
                panic!("Unkown type!")
            }
        }
    }

    fn get_ctype(&mut self, s: Statement) -> String {
        //println!("{s:?}");
        match s {
            Statement::Val(vd) => vd
                .initializer
                .get_expr_value()
                .value
                .unwrap()
                .to_akind()
                .to_ctype(),
            Statement::Operation(o) => o.returnval.to_ctype(),
            _ => panic!("Unsupported statement {s:?}"),
        }
    }

    fn gen_cparams(&mut self, s: Statement) -> String {
        match s {
            Statement::Operation(o) => {
                let p = o.params;
                let mut accum: String = "".to_string();
                let mut count = 0;
                for v in &p {
                    let n = &v.name.name;
                    let t = <ast::SymbolValue as Clone>::clone(
                        &<Option<ast::SymbolValue> as Clone>::clone(
                            &v.initializer.get_expr_value().value,
                        )
                        .unwrap(),
                    )
                    .to_akind()
                    .to_ctype();
                    if count == &p.len() - 1 {
                        accum = format!("{accum}{t} {n}");
                    } else {
                        accum = format!("{accum}{t} {n}, ");
                    }
                    count += 1;
                }
                accum
            }
            _ => panic!("{:?} is not an operation!", s),
        }
    }

    fn gen_operator(&mut self, o: Token) -> String {
        match o.tokentype {
            TokenType::TkPlus => "+".to_string(),
            TokenType::TkMinus => "-".to_string(),
            TokenType::TkStar => "*".to_string(),
            TokenType::TkSlash => "/".to_string(),
            TokenType::TkPercent => "%".to_string(),
            TokenType::TkCEQ => "==".to_string(),
            TokenType::TkCNE => "!=".to_string(),
            TokenType::TkCLT => "<".to_string(),
            TokenType::TkCLE => "<=".to_string(),
            TokenType::TkCGT => ">".to_string(),
            TokenType::TkCGE => ">=".to_string(),
            TokenType::TkAnd => "&&".to_string(),
            TokenType::TkOr => "||".to_string(),
            _ => panic!("{:?} is not an operator", o),
        }
    }

    pub fn generate(&mut self) {
        while self.loc < self.ast.len() {
            let el = self.ast[self.loc].clone();
            let res = self.gen_code(el);
            //println!("{res}");
            self.add(line!(0, "{res}"));
            self.loc += 1
        }
    }

    fn gen_code(&mut self, el: Statement) -> String {
        match el.clone() {
            Statement::Val(vd) => {
                
                let ckind = self.get_ctype(el);
                let cval: String = self.get_cval(match vd.initializer {
                    Expr::ScalarEx(le) => le.value.value.unwrap(),
                    _ => panic!("Unknown type!"),
                });
                let cname: String = vd.name.name;
                let x = format!("{} {} = {};", ckind, cname, cval);
                x
            }

            Statement::Operation(o) => {
                let cname: String = o.name.value.unwrap().get_string().unwrap();
                let ckind = self.get_ctype(el.clone());
                let cparams = self.gen_cparams(el);
                let b = self.gen_code(Statement::Block(o.body));
                let x = format!("{ckind} {cname}({cparams}) {b}");
                x
            }

            Statement::Block(b) => {
                let mut accum: String = "{".to_string();
                for s in b.statements {
                    if s != Statement::Empty
                        && s != Statement::Expression(ExpressionStmt {
                            expression: Expr::Empty,
                        })
                    {
                        accum = format!("{accum}\n    {}", self.gen_code(s))
                    }
                }
                accum = format!("{accum}\n}}\n");
                accum
            }

            Statement::Expression(e) => match e.expression {
                Expr::Assign(a) => {
                    let cvname = a.name.value.clone().unwrap().get_string().unwrap();
                    let cvtype = self
                        .env
                        .get(a.name.value.unwrap().get_string().unwrap())
                        .to_ctype();
                    let cvval = self.gen_code(Statement::Expression(ExpressionStmt {
                        expression: *a.value,
                    }));
                    format!("{cvtype} {cvname} = {cvval}")
                }
                Expr::Binary(b) => {
                    let cl = self.gen_code(Statement::Expression(ExpressionStmt {
                        expression: *b.left,
                    }));
                    let cr = self.gen_code(Statement::Expression(ExpressionStmt {
                        expression: *b.right,
                    }));
                    let o = self.gen_operator(b.operator);

                    format!("{cl} {o} {cr}")
                }
                Expr::Call(c) => todo!(),
                Expr::Grouping(g) => {
                    let x = format!(
                        "({})",
                        self.gen_code(Statement::Expression(ExpressionStmt {
                            expression: *g.expression
                        }))
                    );
                    x
                }
                Expr::ScalarEx(l) => {
                    
                    l.value.value.unwrap().get_string().unwrap()
                }
                Expr::Logical(l) => {
                    let cl = self.gen_code(Statement::Expression(ExpressionStmt {
                        expression: *l.left,
                    }));
                    let cr = self.gen_code(Statement::Expression(ExpressionStmt {
                        expression: *l.right,
                    }));
                    let o = self.gen_operator(l.operator);

                    format!("{cl} {o} {cr}")
                }
                Expr::Unary(u) => todo!(),
                Expr::Value(v) => {
                    let x = v.name.value.unwrap().get_string().unwrap();
                    x
                }
                Expr::Empty => "".to_string(),
            },

            Statement::Return(r) => {
                let mut accum = "return".to_string();
                accum = format!(
                    "{accum} {};",
                    self.gen_code(Statement::Expression(ExpressionStmt {
                        expression: r.value
                    }))
                );
                accum
            }

            Statement::Print(p) => match p.expression.get_expr_value().tokentype {
                TokenType::TkKwTrue => todo!(),
                TokenType::TkKwFalse => todo!(),
                TokenType::TkSymbol => {
                    println!("{:?}", p.expression.get_expr_value().value);
                    "//printf".to_string()
                    //return format!("printf({}", key,)
                }
                TokenType::TkScalar => match p.expression.get_expr_value().value.unwrap() {
                    SymbolValue::Scalar(s) => match s {
                        Scalar::Str(_st) => {
                            let accum = format!(
                                "printf(\"{}\");",
                                self.gen_code(Statement::Expression(ExpressionStmt {
                                    expression: p.expression
                                }))
                            );
                            accum
                        }
                        Scalar::Float(f) => {
                            let accum = format!("printf(\"%d\", {});", f);
                            accum
                        }
                        Scalar::Int(i) => {
                            let accum = format!("printf(\"%d\", {});", i);
                            accum
                        }
                        Scalar::Bool(b) => {
                            let accum = format!("printf({});", b);
                            accum
                        }
                    },
                    _ => panic!("{:?} cannot be printed!", p.expression),
                },
                _ => panic!("{:?} cannot be printed!", p.expression),
            },

            _ => {
                panic!("Unsupported ast Token: {:?}", el);
            }
        }
    }

    pub fn supply(&mut self) -> String {
        let mut accum: String = "#include <stdio.h>\n\n".to_string();
        for i in self.output.clone() {
            accum = format!("{accum}{}", i.c)
        }
        accum = accum.to_string();
        accum
    }
}
