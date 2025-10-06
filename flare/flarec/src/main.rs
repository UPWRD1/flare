//#![warn(clippy::pedantic)]
//#![deny(elided_lifetimes_in_paths)]

use std::{env, path::PathBuf, time::Instant};

use flare_internals::{
    compile_program,
    /*passes::midend::environment::Environment*/
    resource::errors::{CompResult, ReportableError},
};

fn main() -> CompResult<()> {
    const VERSION: &str = "0.0.1";
    let prog_args: Vec<String> = env::args().collect();

    match prog_args.len() {
        3 => match prog_args[1].as_str() {
            "-c" | "--compile" => {
                let filename: PathBuf = PathBuf::from(&prog_args[2]).canonicalize()?;
                let now: Instant = Instant::now();
                let _code = compile_program(&filename).inspect_err(|e| e.report()); //compile_typecheck(&mut root::Context { env: Environment::new() }, &filename).inspect_err(|e| {e.report(); exit(1)}).unwrap();
                let elapsed = now.elapsed();
                //fs::write(format!("{}.ssa", &filename.display()), code).expect("Unable to write file");

                println!("Compiled {} in {elapsed:.2?}", filename.display());
                Ok(())
            }

            &_ => todo!(),
        },
        2 => match prog_args[1].as_str() {
            "--help" | "-h" => {
                println!("Flare v{VERSION}");
                Ok(())
            }
            "--generate" | "-g" => {
                use bnf::Grammar;

                let input = include_str!("/workspaces/allegro/flare/grammar.bnf");
                let grammar: Grammar = input.parse().unwrap();
                let sentence = grammar.generate();
                match sentence {
                    Ok(s) => println!("random sentence: {}", s),
                    Err(e) => println!("something went wrong: {}!", e),
                }
                Ok(())
            }
            &_ => todo!(),
        },
        _ => {
            panic!("Invalid length of arguments!")
        }
    }
}
