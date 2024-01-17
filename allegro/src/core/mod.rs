mod lexing;
mod processing;
mod tokens;
mod ast;

pub fn start(filename: &String) {
    let contents = &std::fs::read_to_string(filename).unwrap();
    let mut lxr = lexing::Lexer::new(contents.to_string());
    lxr.lex();
    let cstvec = lxr.supply();
    println!("[i] Lexing: OK");
    println!("{:?}", cstvec);
    let mut proc = processing::Processor::new(cstvec.clone());
    proc.process();
    let ast = proc.supply();
    println!("{:?}", ast);
}