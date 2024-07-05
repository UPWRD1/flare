extern crate colored;
extern crate lazy_static;
use std::{env, fs, time::Instant};

use logos::Logos;
use root::{
    passes::{midend::typechecking::{infer_types, Substitution}, parser},
    resource::{ast, tk::Tk},
};

extern crate pomelo;

extern crate logos;
//use parser::*;

mod root;

fn main() {
    const VERSION: &str = "0.0.1";
    let prog_args: Vec<String> = env::args().collect();

    match prog_args.len() {
        3 => match prog_args[1].as_str() {
            "-lc" | "--lcompile" => {
                let filename: &String = &prog_args[2];
                println!("Compiling {} to {}.c", filename, filename);
                //root::legacy_compile(filename);
            }
            "-c" | "--compile" => {
                let now = Instant::now();

                let filename: &String = &prog_args[2];
                //info!("Compiling {} to {}.c", filename, filename);
                let src = fs::read_to_string(filename).unwrap();
                let mut lex = Tk::lexer(&src);
                let mut parser = parser::Parser::new(ast::Program::new());
                // if lex.clone().last()
                //     != Some(Ok(Tk::TkStatementEnd((
                //         src.lines().collect::<Vec<&str>>().len(),
                //         0,
                //     ))))
                // {
                //     error_nocode!("Missing last newline in file: '{filename}'");
                // }
                for _ in 0..lex.clone().collect::<Vec<Result<Tk, ()>>>().len() {
                    let a = lex.next().unwrap().unwrap();
                    let token = a.translate(&mut lex.clone());
                    //println!("{a:?} '{}'", lex.slice());
                    let _ = parser.parse(token.clone()).inspect_err(|e| a.syntax_error(lex.clone(), e));
                }
                let prg = parser
                    .end_of_input()
                    .unwrap().1;
                //println!("{:#?}", prg);

               let mut master_subs: Vec<Substitution> = vec![];
                for f in prg.funcs {
                    for s in f.code {
                        let mut subs = match s {
                            ast::Stmt::Expr(e) => infer_types(&e),
                            _ => todo!(),
                        };
                        master_subs.append(&mut subs)
                    }
                }

                dbg!(master_subs);

                //let nend = end.strip_prefix("\"").unwrap().strip_suffix('\"').unwrap().replace("\\n", "\n");
                //let mut file = std::fs::File::create(format!("{}.ll", filename)).expect("Could not create file");
                //let _ = file.write_all(nend.as_bytes());
            
                let elapsed = now.elapsed();
                println!("Compiled {} in {:.2?}", filename, elapsed);
                
                //println!("{}", end);
                //dbg!(lex);
            }

            &_ => todo!(),
        },
        2 => match prog_args[1].as_str() {
            "--help" | "-h" => {
                println!("Allegro v{}", VERSION)
            }
            &_ => todo!(),
        },

        _ => {
            panic!("Invalid length of arguments!")
        }
    }
}



