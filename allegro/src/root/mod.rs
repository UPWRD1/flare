pub mod passes;
use std::{collections::HashSet, fs};

use logos::Logos;
use resource::ast::{Expr, Module, Program, SymbolTableEntry};
pub mod resource;

use crate::root::resource::tk::{Tk, Token};

pub fn compile_filename(filename: &String) -> Module {
    let src = fs::read_to_string(filename).unwrap();
    let mut lex = Tk::lexer(&src);
    // if lex.clone().last()
    //     != Some(Ok(Tk::TkStatementEnd((
    //         src.lines().collect::<Vec<&str>>().len(),
    //         0,
    //     ))))
    // {
    //     error_nocode!("Missing last newline in file: '{filename}'");
    // }
    let mut tokens: Vec<Token> = vec![];
    for i in 0..lex.clone().collect::<Vec<Result<Tk, ()>>>().len() {
        let a = lex.next().unwrap().unwrap();
        println!("{i} {a:?} '{}'", lex.slice());
        tokens.push(Token::new(a, lex.slice().to_string()))
    }

    use passes::parser::parse;
    parse(tokens)
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
                dbg!(p.dependencies.clone());

            }
            _ => continue,
        }
    }
    p
}

pub fn gen_table(
    p: Module,
    mut table: HashSet<SymbolTableEntry>,
) -> (Module, HashSet<SymbolTableEntry>) {
    for c in p.body {
        match c {
            resource::ast::Ast::FnDef { name, args, body } => {
                table.insert(SymbolTableEntry {
                    rawname: name,
                    value: resource::ast::ASTType::Fn,
                    version: 1,
                });
                for e in body {}
            }
            resource::ast::Ast::WithClause { include } => todo!(),
        }
    }
    todo!()
}

fn table_expr(e: Expr, mut table: HashSet<SymbolTableEntry>) {
    match e {
        Expr::Assignment { name, value } => todo!(),
        Expr::MutableAssignment { name, value } => todo!(),
        Expr::Closure { args, body } => todo!(),
        Expr::Return { value } => todo!(),
        Expr::Int(_) => todo!(),
        Expr::Flt(_) => todo!(),
        Expr::Str(_) => todo!(),
        Expr::Bool(_) => todo!(),
        Expr::Symbol(_) => todo!(),
        Expr::Call {
            name,
            args,
            namespace,
        } => todo!(),
        _ => todo!(),
    }
}
