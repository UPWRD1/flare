use std::env;

mod core;

fn main() {
    const VERSION: &str = "0.0.1";
    let prog_args: Vec<String> = env::args().collect();

    match prog_args.len() {
        4 => match prog_args[1].as_str() {
            "-c" => {
                let filename = &prog_args[2];
                let output_filename = &prog_args[3];
                println!("[i] Compiling {} to {}", filename, output_filename);
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
