//#![warn(clippy::pedantic)]
//#![deny(elided_lifetimes_in_paths)]

use std::{env, fs::File, io::Write, panic, path::PathBuf};

use flare_internals::{
    Context, convert_path_to_id,
    passes::backend::{c::C, lowering::ir::IRTarget, target::Target},
    resource::errors::CompResult,
};
fn enable_loggin() {
    if cfg!(debug_assertions) {
        pretty_env_logger::formatted_builder()
            .filter_level(log::LevelFilter::Info)
            .target(pretty_env_logger::env_logger::Target::Stdout)
            //.format_module_path(false)
            .format(|f, r| writeln!(f, "{}", r.args(),))
            .init();
    }
}

fn panic_hook() {
    panic::set_hook(Box::new(|panic_info| {
        let location = panic_info.location().unwrap();
        let file = location.file();
        let line = location.line();

        // The payload can be either &str or String
        let payload = panic_info.payload();
        let message = if let Some(s) = payload.downcast_ref::<&str>() {
            s
        } else if let Some(s) = payload.downcast_ref::<String>() {
            s
        } else {
            "Unknown panic type"
        };

        eprintln!("Brrrrrrrr!");
        eprintln!("Your code was so cool, flarec ICE-d out!\n");
        eprintln!("flarec encountered a fatal internal compiler error during compilation.");
        eprintln!("This is a bug within flarec.");
        eprintln!("This may be caused by a bug in your code, or an issue with your environment.\n");
        eprintln!("Please file an issue here:");
        eprintln!("\thttps://github.com/UPWRD1/flare/issues/new/choose");
        eprintln!("\nError details:");
        eprintln!("ICE at {}:{}", file, line);
        eprintln!("\t{message}\n");

        // Custom message format

        // Optionally, print a backtrace programmatically (requires Rust 1.65+)
        let backtrace = std::backtrace::Backtrace::capture();
        eprintln!("stack backtrace:\n{}", backtrace);
        eprintln!("flarec will now panic. Goodbye.");
    }));
}

fn main() -> CompResult<()> {
    const VERSION: &str = "0.0.1";
    let prog_args: Vec<String> = env::args().collect();
    enable_loggin();

    panic_hook();

    match prog_args.len() {
        3 => match prog_args[1].as_str() {
            "-c" | "--compile" => {
                let filename = PathBuf::from(&prog_args[2]).canonicalize()?.leak();
                let id = convert_path_to_id(filename);
                let target = IRTarget;
                let mut ctx = Context::new(filename, id, target);
                match ctx.compile_program(id) {
                    //.inspect_err(|e| e.report()); //compile_typecheck(&mut root::Context { env: Environment::new() }, &filename).inspect_err(|e| {e.report(); exit(1)}).unwrap();
                    //fs::write(format!("{}.ssa", &filename.display()), code).expect("Unable to write file");
                    Ok((code, elapsed)) => {
                        println!("Compiled {} in {elapsed:.2?}", filename.display());
                        let f =
                            PathBuf::from(filename.to_str().unwrap().split(".").next().unwrap())
                                .with_extension(target.ext().into());
                        let mut f = File::create(f).unwrap();
                        f.write_all(code.as_bytes())?;

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
