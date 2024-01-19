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
    let cstvec = lxr.supply();
    println!("{:?}", cstvec);
    println!("[i] Lexing: OK");
    
    let mut proc = analyze::Analyzer::new(cstvec.clone());
    proc.analyze();
    let processed = proc.supply();
    println!("{:?}", processed);
    println!("[i] Processing: OK");

    let mut parser = parsing::Parser::new(processed);
    
    let parsed = parser.parse();
    println!("{:?}", parsed);
    println!("[i] Parsing: OK");


}
