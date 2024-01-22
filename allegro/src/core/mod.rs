mod errors;
mod passes;
mod resource;

use passes::frontend::analyze;
use passes::frontend::lexing;
use passes::frontend::parsing;
use passes::midend::collapse;
use passes::midend::typechecking;

pub fn start(filename: &String) {
    let contents = &std::fs::read_to_string(filename).unwrap();
    let mut lxr = lexing::Lexer::new(contents.to_string());
    lxr.lex();
    let cstvec: Vec<resource::lexemes::Lexeme> = lxr.supply();
    //dbg!(cstvec.clone());
    println!("[i] Lexing: OK");

    let mut analyzer = analyze::Analyzer::new(cstvec);
    analyzer.analyze();
    let analyzed = analyzer.supply();
    //dbg!(analyzed.clone());
    println!("[i] Analyzing: OK");

    let mut parser = parsing::Parser::new(analyzed);
    parser.parse();
    let ast = parser.supply();
    //dbg!(ast.clone());
    println!("[i] Parsing: OK");

    let mut collapser = collapse::Collapser::new(ast);
    collapser.collapse();
    let collapsed = collapser.supply();
    //dbg!(collapsed.clone());
    println!("[i] Collapsing: OK");

    let mut checker = typechecking::Typechecker::new(collapsed.clone());
    checker.check();
    let checked = checker.supply();
    //dbg!(checked.clone());
    println!("[i] Checking: OK");
}
