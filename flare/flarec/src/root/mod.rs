pub mod passes;
use std::{collections::HashSet, fs::File, io::Read, path::PathBuf};
//use logos::Logos;
use passes::{
    //backend::{flatten::Flattener, gen::Generator},
    midend::environment::{Environment, Quantifier},
    parser,
};
//use passes::midend::typechecking::Typechecker;
use resource::errors::CompResult;

use crate::root::{
    passes::midend::typechecking::Solver,
    resource::{
        errors::CompilerErr,
        rep::{Package, Program},
    },
};

pub mod resource;

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

    // let filename = src_path.file_name().unwrap().to_str().unwrap().to_string();
    // let mut error_stream: Vec<ParseErr> = vec![];
    // let mut lex = Tk::lexer(&src_string);

    // let mut tokens: Vec<Token> = vec![];

    // for _i in 0..lex.clone().collect::<Vec<Result<Tk, LexingError>>>().len() {
    //     match lex.next().unwrap() {
    //         Ok(a) => tokens.push(Token::new(a.clone(), lex.slice().to_string())),
    //         Err(_) => bail!("Unidentified character '{}'", lex.slice())
    //     }
    //    // println!("{_i} {a:?} '{}'", lex.slice());
    // }

    // use passes::parser::parse;
    // Ok(parse(&tokens, &module_name.to_str().unwrap().to_string())?)
}

pub fn parse_program(src_path: &PathBuf) -> CompResult<Program> {
    let path = src_path.canonicalize().unwrap();
    let parent_dir = path.parent().unwrap();
    let dir_contents = std::fs::read_dir(parent_dir)?
        .filter_map(Result::ok)
        .filter(|entry| entry.path().extension().map_or(false, |ext| ext == "flr"))
        .collect::<Vec<_>>();

    let mut program = Program { packages: vec![] };

    for entry in dir_contents {
        let file_path = entry.path();
        let new_prog = parse_file(&file_path).map_err(|e| {
            e.get_dyn()
                .filename(file_path.file_name().unwrap().to_str().unwrap())
        })?;
        program.packages.push((new_prog.0, file_path, new_prog.1));
    }

    //dbg!(program.clone());
    //dbg!(program.clone());
    let mut e = Environment::build(program.clone())?;
    e.check()?;
    //dbg!(&e);

    // for (_name, entry) in e.items.iter_mut() {
    //     //println!("{:?} => {:?}", item.0, item.1);
    //     match entry {

    //         passes::midend::environment::Entry::Let { ref mut sig, body, .. } => {
    //             let mut tc = Solver::new(&mut e);
    //             let tv = tc.check_expr(body)?;
    //             let fn_sig = tc.solve(tv)?;
    //             *sig = Some(fn_sig);
    //         },
    //     _=> todo!(),

    //     }
    // }

    Ok(program)
}

pub fn compile_typecheck(ctx: &mut Context, filename: &std::path::Path) -> CompResult<String> {
    todo!()
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
}
