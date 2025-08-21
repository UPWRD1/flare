//#![warn(clippy::pedantic)]
//#![deny(elided_lifetimes_in_paths)]
extern crate colored;
extern crate lazy_static;
pub mod root;

use std::{env, fs, path::PathBuf, process::exit, time::Instant};

use root::{parse_program, /*passes::midend::environment::Environment*/ resource::errors::{CompResult, ReportableError}};

use crate::root::passes::midend::environment::Environment;

fn main() -> CompResult<()>{
    const VERSION: &str = "0.0.1";
    let prog_args: Vec<String> = env::args().collect();

    match prog_args.len() {
        3 => match prog_args[1].as_str() {
            "-c" | "--compile" => {
                let filename: PathBuf = PathBuf::from(&prog_args[2]).canonicalize()?;
                let now: Instant = Instant::now();
                let code = parse_program(&filename).inspect_err(|e| e.report());//compile_typecheck(&mut root::Context { env: Environment::new() }, &filename).inspect_err(|e| {e.report(); exit(1)}).unwrap();
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
            &_ => todo!(),
        },

        _ => {
            panic!("Invalid length of arguments!")
        }
    }
}
