mod passes;
mod resource;
mod errors;

use passes::analyze;
use passes::lexing;
use passes::parsing;

pub fn start(filename: &String) {
    let contents = &std::fs::read_to_string(filename).unwrap();
    let mut lxr = lexing::Lexer::new(contents.to_string());
    lxr.lex();
    let cstvec: Vec<resource::lexemes::Lexeme> = lxr.supply();
    //println!("{:?}", cstvec);
    println!("[i] Lexing: OK");
    
    let mut analyzer = analyze::Analyzer::new(cstvec);
    analyzer.analyze();
    let analyzed = analyzer.supply();
    //println!("{:?}", analyzed);
    println!("[i] Analyzing: OK");

    let mut parser = parsing::Parser::new(analyzed);
    
    let parsed = parser.parse();
    println!("{:?}", parsed);
    println!("[i] Parsing: OK");


}
