use std::env;

mod core;

fn main() {
    let prog_args: Vec<String> = env::args().collect();

    match prog_args.len() {
        3 => {
            let filename = &prog_args[1];
            let output_filename = &prog_args[2];
            println!("[i] Compiling {} to {}", filename, output_filename);
            core::start(filename);
        }
        _ => {
            panic!("Invalid length of arguments!")
        }
    }
}
