extern crate colored;
extern crate lazy_static;
use std::{env, fs};

use logos::Logos;
use root::resource::tk::Tk;


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
                info!("Compiling {} to {}.c", filename, filename);
                root::legacy_compile(filename);
            }
            "-c" | "--compile" => {
                let filename: &String = &prog_args[2];
                info!("Compiling {} to {}.c", filename, filename);
                let src = fs::read_to_string(filename).unwrap();
                let mut lex = Tk::lexer(&src);
                for i in 0..lex.clone().collect::<Vec<Result<Tk, ()>>>().len() {
                    let a = lex.next().unwrap().unwrap();
                    println!("{a:?}");
                }
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
