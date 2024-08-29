extern crate colored;
extern crate lazy_static;
extern crate peg;
extern crate serde_json;

use std::{env, time::Instant};
//use std::collections::{HashMap, HashSet}

use root::{passes::midend::typechecking::Typechecker, resource::ast::Program,};

extern crate logos;
//use parser::*;

pub mod root;

fn main() {
    const VERSION: &str = "0.0.1";
    let prog_args: Vec<String> = env::args().collect();

    match prog_args.len() {
        3 => match prog_args[1].as_str() {
            "-c" | "--compile" => {
                let now = Instant::now();

                let filename: &String = &prog_args[2];
                let mut p = Program { modules: vec![], dependencies: vec![] };
                let root_ast = root::compile_filename(filename);
                p.modules.push(root_ast.clone());
                
                let new_p = root::get_dependencies(p.clone());
                dbg!(new_p.clone());
                let mut t = Typechecker::new();
                t.check(new_p);
                //dbg!(t);
                //let mut table: SymbolTable = SymbolTable::new();
                let elapsed = now.elapsed();
                println!("Compiled {} in {:.2?}", filename, elapsed);
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
