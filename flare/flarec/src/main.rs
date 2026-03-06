// Copyright 2025 Luke Davis

//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at

//        http://www.apache.org/licenses/LICENSE-2.0

//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
#![allow(clippy::upper_case_acronyms)]
use std::{fs::File, io::Write, panic, path::PathBuf, time::Instant};

use clap::{Parser, ValueEnum, crate_description, crate_version};
use flare_internals::{
    build, convert, generate, lower, make_filectx, monomorph, parse,
    passes::backend::target::{Target, irtarget::IRTarget, lirtarget::LIRTarget, llvm::LLVM},
    reduce, resolve,
    resource::errors::CompResult,
    simplify, typecheck,
};
fn enable_loggin() {
    if cfg!(debug_assertions) {
        pretty_env_logger::formatted_builder()
            // .filter_level(log::LevelFilter::Error)
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
        eprintln!("Your code was too cool for flarec!\n");
        eprintln!("flarec encountered a fatal, internal compiler error (ICE) during compilation.");
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

#[derive(Parser)]
#[command(name = "flarec")]
#[command(version = crate_version!())]
#[command(about = crate_description!(), long_about = None)]
struct Cli {
    input_files: Vec<PathBuf>,

    #[arg(short = 'o', long = "output")]
    output_file: PathBuf,

    #[arg(short = 'e', long = "emit", default_value_t = EmitOptions::default(), value_enum)]
    emit: EmitOptions,
}

#[derive(Copy, Clone, ValueEnum, Default)]
enum EmitOptions {
    LIR,
    IR,
    #[default]
    LLVM,
}

macro_rules! make_target {
    ($target:tt, $cli:ident) => {{
        let files = $cli.input_files;
        let target = $target;
        let ctx = make_filectx(files);
        let now: Instant = Instant::now();
        parse(&ctx)
            .and_then(build)
            .and_then(resolve)
            .and_then(typecheck)
            .and_then(lower)
            .and_then(simplify)
            .and_then(monomorph)
            .and_then(reduce)
            .and_then(convert)
            .and_then(|cc| generate(cc, target))
            .and_then(|res| {
                let output: Vec<u8> = res.into();
                let elapsed = now.elapsed();
                println!("Compiled  in {elapsed:.2?}",);
                let f = $cli.output_file.with_extension(target.ext());
                let mut f = File::create(f).unwrap();
                f.write_all(&output)?;

                Ok(())
            })
            .inspect_err(|e| e.report(&ctx))
    }};
}

fn main() {
    enable_loggin();
    panic_hook();

    let cli = Cli::parse(); // unsafe { backtrace_on_stack_overflow::enable() };

    match cli.emit {
        EmitOptions::IR => {
            let ctx = make_filectx(cli.input_files);
            let now: Instant = Instant::now();
            parse(&ctx)
                .and_then(build)
                .and_then(resolve)
                .and_then(typecheck)
                .and_then(lower)
                .and_then(simplify)
                .and_then(monomorph)
                .and_then(reduce)
                .and_then(|res| {
                    let ir = res.ir;
                    // dbg!(&ir);

                    let output = ir
                        .into_iter()
                        .enumerate()
                        .map(|(i, x)| format!("item #{i}: is\n{x}\nend item #{i}"))
                        .collect::<Vec<String>>()
                        .join("\n\n");
                    let elapsed = now.elapsed();
                    println!("Compiled  in {elapsed:.2?}",);
                    let f = cli.output_file.with_extension(IRTarget.ext());
                    let mut f = File::create(f).unwrap();
                    f.write_all(output.as_bytes())?;

                    Ok(())
                })
                .inspect_err(|e| e.report(&ctx))
        }
        EmitOptions::LIR => make_target!(LIRTarget, cli),
        EmitOptions::LLVM => make_target!(LLVM, cli),
    }
    .unwrap_or_else(|_| std::process::exit(1))
}
