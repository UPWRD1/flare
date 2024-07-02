pub mod passes;
// pub mod legacy_resource;
pub mod resource;

// use std::io::Write;
// use std::time::Instant;

// use passes::legacy_frontend::analyze;
// use passes::legacy_frontend::lexing;
// //use passes::legacy_frontend::parsing;
// use passes::legacy_midend::typechecking;

// use passes::legacy_backend::codegen;
// use legacy_resource::errors::Errors;
// use legacy_resource::tokens::LegacyToken;


// use crate::error;
// use crate::info;

// pub fn legacy_compile(filename: &String) {
//     let now = Instant::now();

//     let cstvec = legacy_compile_lex(filename);

//     let analyzed: Vec<LegacyToken> = legacy_compile_analyze(cstvec);

//     let ast = legacy_compile_parse(analyzed);

//     let (checked, e) = compile_typecheck(ast);

//     let generated = legacy_compile_codegen(checked, e);
    
//     let mut file = std::fs::File::create(format!("{}.c", filename)).expect("Could not create file");
//     let _ = file.write_all(generated.to_string().as_bytes());

//     let elapsed = now.elapsed();
//     info!("Compiled {} in {:.2?}", filename, elapsed);

// }

// fn legacy_compile_codegen(checked: Vec<legacy_resource::ast::Statement>, e: legacy_resource::environment::Environment) -> String {
//     let mut generator = codegen::Generator::new(checked.clone(), e);
//     generator.generate();
//     let generated = generator.supply();
//     //println!("{}", generated.clone());
//     //info!("Generation: OK");
//     generated
// }

// fn compile_typecheck(ast: Vec<legacy_resource::ast::Statement>) -> (Vec<legacy_resource::ast::Statement>, legacy_resource::environment::Environment) {
//     let mut checker = typechecking::Typechecker::new(ast.clone());
//     checker.check();
//     let (checked, e) = checker.supply();
//     //dbg!(e.clone());
//     //info!("Checking: OK");
//     (checked, e)
// }

// fn legacy_compile_parse(analyzed: Vec<LegacyToken>) -> Vec<legacy_resource::ast::Statement> {
//     let mut parser = passes::legacy_frontend::parsing::Parser::new(analyzed);
//     parser.parse();
//     let ast: Vec<legacy_resource::ast::Statement> = parser.supply();
//     //dbg!(ast.clone());
//     //info!("Parsing: OK");
//     ast
// }

// fn legacy_compile_analyze(cstvec: Vec<legacy_resource::lexemes::Lexeme>) -> Vec<legacy_resource::tokens::LegacyToken> {
//     let mut analyzer = analyze::Analyzer::new(cstvec);
//     analyzer.analyze();
//     return analyzer.supply()
//     //dbg!(analyzed.clone());
//     //info!("Analyzing: OK");
// }

// fn legacy_compile_lex(filename: &String) -> Vec<legacy_resource::lexemes::Lexeme> {
//     let contents_unsure = &std::fs::read_to_string(filename);
//     if contents_unsure.is_err() {
//         error!(Errors::MissingFile, (filename.to_string()));
//     };
//     let contents = contents_unsure.as_ref().unwrap();
//     let mut lxr = lexing::Lexer::new(contents.to_string());
//     lxr.lex();
//     let cstvec: Vec<legacy_resource::lexemes::Lexeme> = lxr.supply();
//     //dbg!(cstvec.clone());
//     //info!("Lexing: OK");
//     cstvec
// }

// pub fn legacy_compile_import(filename: &String) -> Vec<LegacyToken> {
//     let cstvec = legacy_compile_lex(filename);
//     let analyzed: Vec<LegacyToken> = legacy_compile_analyze(cstvec);
//     analyzed
// }