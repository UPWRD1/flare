pub mod passes;
use std::fs;

use logos::Logos;
use resource::ast::{Module, Program};
pub mod resource;

use crate::root::resource::tk::{Tk, Token};

pub fn compile_filename(filename: &String) -> Module {
    let src = fs::read_to_string(filename).unwrap();
    let mut lex = Tk::lexer(&src);

    let mut tokens: Vec<Token> = vec![];
    for _i in 0..lex.clone().collect::<Vec<Result<Tk, ()>>>().len() {
        let a = lex.next().unwrap().unwrap();
        //println!("{i} {a:?} '{}'", lex.slice());
        tokens.push(Token::new(a, lex.slice().to_string()));
    }

    use passes::parser::parse;
    parse(&tokens)   
}

pub fn get_dependencies(mut p: Program) -> Program {
    let m = p.modules.first().unwrap();
    for a in m.body.clone() {
        match a {
            resource::ast::Ast::WithClause { include } => {
                let to_compile = include.first().unwrap().get_symbol_name();
                let tc = format!(
                    "{}/{}.alg",
                    std::env::current_dir().unwrap().to_string_lossy(),
                    to_compile
                );
                let dep_ast = compile_filename(&tc);
                let mut dep_p = get_dependencies(Program { modules: vec![dep_ast], dependencies: vec![] });
                p.dependencies.push(tc);
                p.dependencies.append(&mut dep_p.dependencies);
                p.modules.append(&mut dep_p.modules);
                dbg!(p.dependencies.clone());

            }
            _ => continue,
        }
    }
    p
}