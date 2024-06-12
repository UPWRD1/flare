mod passes;
mod resource;

use std::io::Write;
use std::time::Instant;


use passes::frontend::analyze;
use passes::frontend::lexing;
use passes::frontend::parsing;
use passes::midend::typechecking;

use passes::backend::codegen;
use resource::errors::Errors;
use resource::tokens::Token;

use crate::error;
use crate::info;


pub fn full_compile(filename: &String) {
    let now = Instant::now();

    let cstvec = compile_lex(filename);

    let analyzed: Vec<Token> = compile_analyze(cstvec);

    let ast = compile_parse(analyzed);

    let (checked, e) = compile_typecheck(ast);

    let generated = compile_codegen(checked, e);
    
    let mut file = std::fs::File::create(format!("{}.c", filename)).expect("Could not create file");
    let _ = file.write_all(generated.to_string().as_bytes());

    let elapsed = now.elapsed();
    info!("Compiled {} in {:.2?}", filename, elapsed);

}

fn compile_codegen(checked: Vec<resource::ast::Statement>, e: resource::environment::Environment) -> String {
    let mut generator = codegen::Generator::new(checked.clone(), e);
    generator.generate();
    let generated = generator.supply();
    //println!("{}", generated.clone());
    //info!("Generation: OK");
    generated
}

fn compile_typecheck(ast: Vec<resource::ast::Statement>) -> (Vec<resource::ast::Statement>, resource::environment::Environment) {
    let mut checker = typechecking::Typechecker::new(ast.clone());
    checker.check();
    let (checked, e) = checker.supply();
    //dbg!(e.clone());
    //info!("Checking: OK");
    (checked, e)
}

fn compile_parse(analyzed: Vec<Token>) -> Vec<resource::ast::Statement> {
    let mut parser = parsing::Parser::new(analyzed);
    parser.parse();
    let ast: Vec<resource::ast::Statement> = parser.supply();
    //dbg!(ast.clone());
    //info!("Parsing: OK");
    ast
}

fn compile_analyze(cstvec: Vec<resource::lexemes::Lexeme>) -> Vec<resource::tokens::Token> {
    let mut analyzer = analyze::Analyzer::new(cstvec);
    analyzer.analyze();
    let analyzed = analyzer.supply();
    //dbg!(analyzed.clone());
    //info!("Analyzing: OK");
    analyzed
}

fn compile_lex(filename: &String) -> Vec<resource::lexemes::Lexeme> {
    let contents_unsure = &std::fs::read_to_string(filename);
    if contents_unsure.is_err() {
        error!(Errors::MissingFile, (filename.to_string()));
    };
    let contents = contents_unsure.as_ref().unwrap();
    let mut lxr = lexing::Lexer::new(contents.to_string());
    lxr.lex();
    let cstvec: Vec<resource::lexemes::Lexeme> = lxr.supply();
    //dbg!(cstvec.clone());
    //info!("Lexing: OK");
    cstvec
}

pub fn compile_import(filename: &String) -> Vec<Token> {
    let cstvec = compile_lex(filename);
    let analyzed: Vec<Token> = compile_analyze(cstvec);
    analyzed
}