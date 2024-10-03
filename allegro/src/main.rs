//#![warn(clippy::pedantic)]
#![deny(elided_lifetimes_in_paths)]
extern crate colored;
extern crate lazy_static;
extern crate peg;
extern crate logos;

pub mod root;

use std::{env, fs, time::Instant};

use root::compile_typecheck;


fn main() {
    const VERSION: &str = "0.0.1";
    let prog_args: Vec<String> = env::args().collect();

    match prog_args.len() {
        3 => match prog_args[1].as_str() {
            "-c" | "--compile" => {

                let filename: &String = &prog_args[2];
                let now: Instant = Instant::now();
                let res = compile_typecheck(filename);
                let elapsed = now.elapsed();
                //println!("{:#?}", res.clone());
                let serialized = serde_json::to_string_pretty(&res).unwrap();
                fs::write(format!("{}.json", filename), serialized).expect("Unable to write file");

                println!("Compiled {filename} in {elapsed:.2?}");
            }

            &_ => todo!(),
        },
        2 => match prog_args[1].as_str() {
            "--help" | "-h" => {
                println!("Allegro v{VERSION}");
            }
            &_ => todo!(),
        },

        _ => {
            panic!("Invalid length of arguments!")
        }
    }
}





#[cfg(test)]
pub mod tests {

    use root::compile_json;

    use crate::root;

    #[test]
    fn primatives() {
        let res = compile_json(&"/workspaces/allegro/allegro/tests/typechecking/primatives/primativetest.alg".to_string());
        let correct = "{\"modules\":[{\"body\":[{\"FnDef\":{\"name\":\"main\",\"rettype\":\"Bool\",\"args\":[],\"limits\":[],\"body\":[{\"e\":{\"Int\":3},\"exprtype\":\"Int\"},{\"e\":{\"Flt\":3.0},\"exprtype\":\"Flt\"},{\"e\":{\"Str\":\"\\\"three\\\"\"},\"exprtype\":\"Str\"},{\"e\":{\"Bool\":true},\"exprtype\":\"Bool\"}]}}]}],\"dependencies\":[]}";
        assert_eq!(res, correct);
    }
}