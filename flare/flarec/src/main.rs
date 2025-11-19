//#![warn(clippy::pedantic)]
//#![deny(elided_lifetimes_in_paths)]

use std::{env, io::Write, path::PathBuf};

use flare_internals::{
    Context, convert_path_to_id,
    passes::backend::c::C,
    resource::errors::{CompResult, ReportableError},
};
fn enable_loggin() {
    unsafe {
        if cfg!(debug_assertions) {
            std::env::set_var("RUST_LOG", "trace");
            pretty_env_logger::formatted_builder()
                .filter_level(log::LevelFilter::Trace)
                //.format_module_path(false)
                .format(|f, r| writeln!(f, "{}", r.args()))
                .init();
        } else {
            std::env::set_var("RUST_LOG", "off");
        }
    }
}

fn main() -> CompResult<()> {
    const VERSION: &str = "0.0.1";
    let prog_args: Vec<String> = env::args().collect();
    enable_loggin();

    match prog_args.len() {
        3 => match prog_args[1].as_str() {
            "-c" | "--compile" => {
                let filename = PathBuf::from(&prog_args[2]).canonicalize()?.leak();
                let id = convert_path_to_id(filename);

                let mut ctx = Context::new(filename, id, C);
                match ctx.compile_program(id) {
                    //.inspect_err(|e| e.report()); //compile_typecheck(&mut root::Context { env: Environment::new() }, &filename).inspect_err(|e| {e.report(); exit(1)}).unwrap();
                    //fs::write(format!("{}.ssa", &filename.display()), code).expect("Unable to write file");
                    Ok((code, elapsed)) => {
                        println!("Compiled {} in {elapsed:.2?}", filename.display());
                        let f =
                            PathBuf::from(filename.to_str().unwrap().split(".").next().unwrap())
                                .with_extension(".c");
                        dbg!(f);

                        Ok(())
                    }

                    Err(e) => {
                        e.report(&ctx.filectx);
                        std::process::exit(1)
                    }
                }
            }

            &_ => todo!(),
        },
        2 => match prog_args[1].as_str() {
            "--help" | "-h" => {
                println!("Flare v{VERSION}");
                Ok(())
            }
            "--generate" | "-g" => {
                // use bnf::Grammar;

                // let input = include_str!("../../grammar.bnf");
                // let grammar: Grammar = input.parse().unwrap();
                // let sentence = grammar.generate();
                // match sentence {
                //     Ok(s) => println!("random sentence: {}", s),
                //     Err(e) => println!("something went wrong: {}!", e),
                // }
                Ok(())
            }
            &_ => todo!(),
        },
        _ => {
            panic!("Invalid length of arguments!")
        }
    }
}
