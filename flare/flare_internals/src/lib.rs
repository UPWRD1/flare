#[warn(clippy::pedantic)]
pub mod passes;
pub mod resource;

use std::{fs::File, io::Read, path::{Path, PathBuf}};

use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::{
    passes::{
        //backend::{flatten::Flattener, gen::Generator},
        midend::environment::Environment,
        parser,
    },
    resource::{
        errors::CompResult,
        rep::{Package, Program},
    },
};

//use crate::root::resource::tk::{Tk, Token};

pub struct Context {
    pub env: Environment,
}

pub fn parse_file(src_path: &PathBuf) -> CompResult<(Package, String)> {
    let mut src_string = String::new();

    let mut src = File::open(src_path)?;
    src.read_to_string(&mut src_string)?;
    let res = parser::parse(&src_string).map_err(|e| e.get_dyn().src(&src_string))?; //TODO: handle errors properly

    Ok((res, src_string))
}

pub fn parse_program(src_path: &Path) -> CompResult<Program> {
    let path = src_path.canonicalize().unwrap();
    let parent_dir = path.parent().unwrap();
    let dir_contents = std::fs::read_dir(parent_dir)?
        .filter_map(Result::ok)
        .filter(|entry| entry.path().extension().is_some_and(|ext| ext == "flr"))
        .collect::<Vec<_>>();

    let processed = dir_contents.par_iter().map(|entry| {
        let file_path = entry.path();
        let (pack, str) = parse_file(&file_path)
            .map_err(|e| {
                e.get_dyn()
                    .filename(file_path.file_name().unwrap().to_str().unwrap())
            })
            .unwrap();
        (pack, file_path, str)
    });
    Ok(
    Program {
        packages: processed.collect(),
    })
}

pub fn compile_program(src_path: &Path) -> CompResult<Program> {
    let program = parse_program(src_path)?;
    //dbg!(program.clone());
    //dbg!(program.clone());
    let e = Environment::build(program.clone())?;
    e.check()?;
    //dbg!(&e);

    Ok(program)
}


// pub fn compile_typecheck(ctx: &mut Context, filename: &std::path::Path) -> CompResult<String> {
    // todo!()
    // let mut p = Program {
    //     modules: vec![],
    //     dependencies: HashSet::new(),
    // };

    // let root_ast = parse_file(ctx, filename.clone())?;
    // p.modules.push(root_ast.clone());

    // match root_ast {
    //     Cst::Module { name: _, body } => {
    //         for c in body {
    //             match c {
    //                 Cst::WithClause { include } => {
    //                     let parent_path = PathBuf::from_iter(
    //                         filename
    //                             .canonicalize()?
    //                             .components()
    //                             .clone()
    //                             .into_iter()
    //                             .take(filename.canonicalize()?.components().count() - 1)
    //                             .collect::<Vec<std::path::Component>>()
    //                             .iter()
    //                             .map(|x| x.as_os_str()),
    //                     );
    //                     let include_path =
    //                         parent_path.join(format!("{}.flr", include.get_symbol_name().unwrap()));

    //                     let include_ast = parse_file(ctx, include_path)?;
    //                     p.modules.push(include_ast.clone());
    //                 }
    //                 _ => {}
    //             }
    //         }
    //     }
    //     _ => panic!("Should be a module"),
    // }

    // //println!("{:#?}", p.clone());

    // ctx.env.build(p.clone())?;
    // //dbg!(ctx.env.clone());

    // // let mut tc = Typechecker::new(ctx.env.clone());
    // // let res = tc.check()?;
    // // dbg!(res.clone());

    // // let mut flattener = Flattener::new(res.clone());
    // // let flat = flattener.flatten();
    // // let main_func: FunctionTableEntry = flat.items.get(&quantifier!(Root, Func("main"), End)).cloned().unwrap().into();
    // // dbg!(&main_func);

    // //let mut g = Generator::new(res);
    // //let code = g.generate().unwrap();
    // //println!("Output: \n{}", code);
    // //todo!();
    // Ok("".to_string())
// }
