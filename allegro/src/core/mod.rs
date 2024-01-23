mod errors;
mod passes;
mod resource;

use passes::frontend::analyze;
use passes::frontend::lexing;
use passes::frontend::parsing;
//use passes::midend::collapse;
//use passes::midend::typechecking;

//use passes::backend::codegen;

//use std::io::Write;

use resource::table::*;

pub fn start(filename: &String) {
    let mut global_context: GlobalTable = GlobalTable::new();

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
    dbg!(ast.clone());
    println!("[i] Parsing: OK");

    //let mut collapser = collapse::Collapser::new(ast);
    //collapser.collapse();
    //let collapsed = collapser.supply();
    //dbg!(collapsed.clone());
    //println!("[i] Collapsing: OK");

    //let mut checker = typechecking::Typechecker::new(collapsed.clone());
    //checker.check();
    //let checked = checker.supply();
    //dbg!(checked.clone());
    //println!("[i] Checking: OK");

    //let mut generator = codegen::Generator::new(collapsed.clone());
    //generator.generate();
    //let generated = generator.supply();
    //println!("{}", generated.clone());
    //println!("[i] Generation: OK");

    //let mut file = std::fs::File::create(format!("{}.c", filename)).expect("Could not create file");
    //let _ = file.write_all(format!("{}", generated).as_bytes());
}
