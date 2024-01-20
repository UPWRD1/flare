use std::env;

mod core;

fn main() {
    let prog_args: Vec<String> = env::args().collect();

    match prog_args.len() {
        3 => {
            let filename = &prog_args[1];
            let filename_llvm = &prog_args[2];
            println!("[i] Compiling {} to {}", filename, filename_llvm);
            core::start(filename);
        }
        _ => {
            panic!("Invalid length of arguments!")
        }
    }
}
