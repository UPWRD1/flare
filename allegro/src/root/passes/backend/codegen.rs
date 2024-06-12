use core::panic;

use crate::root::resource::{
    ast::*,
    environment::{AKind, Environment},
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
    currentindent: usize,
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
            currentindent: 0,
        }
    }

    fn add(&mut self, st: GenLine) {
        self.output.push(st);
    }

    fn gen_cformat_specifier(&self, t: AKind) -> String {
        match t {
            AKind::TyStr => "%s".to_string(),
            AKind::TyInt => "%d".to_string(),
            AKind::TyFlt => "%f".to_string(),
            AKind::TyOp(a) => self.gen_cformat_specifier(a.extract_op_type()),
            AKind::TyBool => "%d".to_string(),
            _ => panic!("Cannot convert {t:?} to c representation"),
        }
    }

    fn get_cval(&mut self, sk: SymbolValue) -> String {
        match sk {
            SymbolValue::Scalar(s) => match s {
                Scalar::Int(i) => i.to_string(),
                Scalar::Str(s) => format!("\"{}\"", s),
                Scalar::Float(f) => f.to_string(),
                Scalar::Bool(b) => {
                    if b {
                        "true".to_string()
                    } else {
                        "false".to_string()
                    }
                }
            },
            // SymbolValue::Pair(p) => {
            //     match p.kind {
            //         AKind::TyStr |
            //         AKind::TyInt |
            //         AKind::TyFlt |
            //         AKind::TyBool => p.kind.to_ctype(),
            //         _ => {dbg!(p); panic!()}
            //     }
            // }
            SymbolValue::Pair(i) => {
                let name = format!("{}", i.name);
                name
            }
            _ => {
                panic!("Unknown type {:?}!", sk)
            }
        }
    }

    fn get_ctype(&mut self, s: Statement) -> String {
        //println!("{s:?}");
        match s {
            Statement::Bind(vd) => self.env.get_akind_symbol(&vd.name.name).to_ctype(),
            Statement::Function(o) => self.env.get_akind_symbol(&o.name.name).to_ctype(),

            _ => panic!("Unsupported statement {s:?}"),
        }
    }

    fn gen_cparams(&mut self, s: Statement) -> String {
        match s {
            Statement::Function(o) => {
                let p = o.params;
                let mut accum: String = "".to_string();
                for (count, v) in p.iter().enumerate() {
                    let n = v.name.name.clone();
                    let t = v.name.kind.to_ctype();
                    if count == &p.len() - 1 {
                        accum = format!("{accum}{t} {n}");
                    } else {
                        accum = format!("{accum}{t} {n}, ");
                    }
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
        //dbg!(self.currentindent);
        match el.clone() {
            Statement::Bind(vd) | Statement::MutBind(vd) => {
                let ckind = self.get_ctype(Statement::Bind(vd.clone()));
                let cval: String = match vd.initializer.clone() {
                    Expr::ScalarEx(le) => self.get_cval(SymbolValue::Scalar(le.value.clone())),
                    Expr::Call(c) => self.gen_code(Statement::Expression(ExpressionStmt {
                        expression: Expr::Call(c),
                    })),
                    Expr::Binary(b) => self.gen_code(Statement::Expression(ExpressionStmt {
                        expression: Expr::Binary(b),
                    })),
                    Expr::Grouping(g) => self.gen_code(Statement::Expression(ExpressionStmt {
                        expression: Expr::Grouping(g),
                    })),
                    Expr::Value(v) => self.gen_code(Statement::Expression(ExpressionStmt {
                        expression: Expr::Value(v),
                    })),
                    Expr::Logical(l) => self.gen_code(Statement::Expression(ExpressionStmt {
                        expression: Expr::Logical(l),
                    })),
                    _ => panic!("Unknown value {:?}!", vd.initializer),
                };
                let cname: String = vd.name.name;
                let x = format!("{} {} = {};", ckind, cname, cval);
                x
            }

            Statement::Function(o) => {
                let cname: String = o.name.name;
                let ckind: String = self.get_ctype(el.clone());
                let cparams = self.gen_cparams(el.clone());
                let b = self.gen_code(Statement::Block(o.name.value.extract_block().unwrap()));
                let x: String = format!("{ckind} {cname}({cparams}) {b}\n\n");

                x
            }

            Statement::Block(b) => {
                let mut accum: String = "{".to_string(); //format!("{space:>width$}{{", space=" ", width=self.currentindent * 4);
                self.currentindent += 1;
                for s in b.statements {
                    if s != Statement::Empty
                        && s != Statement::Expression(ExpressionStmt {
                            expression: Expr::Empty,
                        })
                    {
                        //println!("{space:0>width$}", space=" ", width=self.currentindent * 4);
                        accum = format!(
                            "{accum}\n{space:>width$}{}",
                            self.gen_code(s),
                            space = " ",
                            width = self.currentindent * 4
                        )
                    }
                }
                self.currentindent -= 1;
                accum = format!(
                    "{accum}\n{space:>width$}}}",
                    space = " ",
                    width = self.currentindent * 4
                );
                accum
            }

            Statement::Expression(e) => {
                let x = match e.expression {
                    Expr::Assign(a) => {
                        let cvname = a.name.value.clone().unwrap().get_string().unwrap();
                        let cvtype = self
                            .env
                            .get_akind_symbol(&a.name.value.unwrap().get_string().unwrap())
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
                    Expr::Call(c) => {
                        let name = c.callee.name;
                        let mut argstring = "".to_string();
                        let mut argcount = 0;
                        for arg in c.args {
                            argcount += 1;
                            if argcount > 1 {
                                argstring = format!(
                                    "{}, {}",
                                    argstring,
                                    self.gen_code(Statement::Expression(ExpressionStmt {
                                        expression: arg
                                    }))
                                )
                            } else {
                                argstring = format!(
                                    "{}{}",
                                    argstring,
                                    self.gen_code(Statement::Expression(ExpressionStmt {
                                        expression: arg
                                    }))
                                )
                            }
                        }
                        format!("{}({});", name, argstring)
                        //todo!()
                    }
                    Expr::Grouping(g) => {
                        let x = format!(
                            "({})",
                            self.gen_code(Statement::Expression(ExpressionStmt {
                                expression: *g.expression
                            }))
                        );
                        x
                    }
                    Expr::ScalarEx(l) => self.get_cval(SymbolValue::Scalar(l.value)),
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
                    Expr::Unary(u) => {
                        format!(
                            "{}{}",
                            self.gen_operator(u.operator),
                            self.gen_code(Statement::Expression(ExpressionStmt {
                                expression: *u.right,
                            }))
                        )
                    }
                    Expr::Value(v) => self.get_cval(v.name.value.unwrap()),
                    Expr::Empty => "".to_string(),
                };
                x
            }

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
                //TokenType::TkKwTrue => todo!(),
                //TokenType::TkKwFalse => todo!(),
                TokenType::TkSymbol => {
                    let v = p.expression.get_expr_value();
                    //dbg!(v.clone());
                    let name = v.clone().to_string();
                    let ty = self.gen_cformat_specifier(self.env.get_akind_symbol(&name));
                    //println!("{:?}", p.expression.get_expr_value().value.unwrap().get_string().unwrap());
                    //panic!();
                    format!("printf(\"{}\\n\", {});", ty, name)
                    //return format!("printf({}", key,)
                }
                TokenType::TkScalar => match p.expression.get_expr_value().value.unwrap() {
                    SymbolValue::Scalar(s) => {
                        let val = match s.clone() {
                            Scalar::Str(str) => format!("\"{}\"", str),
                            Scalar::Int(i) => i.to_string(),
                            Scalar::Float(f) => f.to_string(),
                            Scalar::Bool(b) => b.to_string(),
                        };
                        let ty = self.gen_cformat_specifier(s.to_akind());
                        //println!("{:?}", p.expression.get_expr_value().value.unwrap().get_string().unwrap());
                        //panic!();
                        format!("printf(\"{}\\n\", {});", ty, val)
                    }
                    _ => panic!("{:?} cannot be printed!", p.expression),
                },
                _ => panic!("{:?} cannot be printed!", p.expression),
            },

            Statement::If(i) => {
                let ifex = self.gen_code(Statement::Expression(ExpressionStmt {
                    expression: i.condition,
                }));
                let tb = self.gen_code(Statement::Block(*i.then_branch));
                if let Some(ref ebs) = i.else_branch {
                    let eb = self.gen_code(Statement::Block(*ebs.clone()));
                    return format!("if ({}) {} else {}", ifex, tb, eb);
                } else {
                    return format!("if ({}) {}", ifex, tb);
                }
            }

            Statement::While(w) => {
                let c = self.gen_code(Statement::Expression(ExpressionStmt {
                    expression: w.condition,
                }));
                let b = self.gen_code(Statement::Block(*w.block));
                return format!("while ({}) {}", c, b);
            }

            Statement::ReAssign(r) => {
                format!(
                    "{} = {};",
                    r.name.name,
                    self.gen_code(Statement::Expression(ExpressionStmt {
                        expression: r.newval
                    }))
                )
            }

            _ => {
                panic!("Unsupported ast Token: {:?}", el);
            }
        }
    }

    pub fn supply(&mut self) -> String {
        let mut accum: String =
            "//This file was automatically generated by allegro.\n//Submit bug reports to https://github.com/UPWRD1/allegro\n\n#include <stdio.h>\n#include <stdbool.h>\n\n".to_string();
        for i in self.output.clone() {
            accum = format!("{accum}{}", i.c)
        }
        accum = accum.to_string();
        accum
    }
}
