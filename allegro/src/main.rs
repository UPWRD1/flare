extern crate colored;

use std::env;

mod core;

fn main() {
    const VERSION: &str = "0.0.1";
    let prog_args: Vec<String> = env::args().collect();

    match prog_args.len() {
        3 => match prog_args[1].as_str() {
            "-c" | "--compile"=> {
                let filename: &String = &prog_args[2];
                info!("Compiling {} to {}.c", filename, filename);
                core::full_compile(filename);
            }

            &_ => todo!(),
        },
        2 => match prog_args[1].as_str() {
            "--help" | "-h" => {
                println!("Allegro v{}", VERSION)
            }
            &_ => todo!()
        },

        _ => {
            panic!("Invalid length of arguments!")
        }
    }
}
