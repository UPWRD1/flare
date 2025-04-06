pub mod passes;
use std::{collections::HashSet, fs::File, io::Read, path::PathBuf};
use logos::Logos;
use passes::{backend::{flatten::Flattener, gen::Generator}, midend::{environment::{Environment, FunctionTableEntry, Quantifier}, typechecking::Typechecker}};
//use passes::midend::typechecking::Typechecker;
use resource::{cst::{Cst, Program}, errors::LexingError};
pub mod resource;
use anyhow::{bail, Result};
use crate::{quantifier, root::resource::tk::{Tk, Token}};


pub struct Context {
    pub env: Environment,
}

pub fn compile_module(ctx: &mut Context, src_path: PathBuf) -> anyhow::Result<Cst> {
    let mut src_string = String::new();

    let mut src = File::open(src_path.clone())?;
    let module_name = src_path.file_stem().unwrap();
    src.read_to_string(&mut src_string)?;
    let mut lex = Tk::lexer(&src_string);

    let mut tokens: Vec<Token> = vec![];


    for _i in 0..lex.clone().collect::<Vec<Result<Tk, LexingError>>>().len() {
        match lex.next().unwrap() {
            Ok(a) => tokens.push(Token::new(a.clone(), lex.slice().to_string())),
            Err(e) => bail!("Unidentified character '{}'", lex.slice())
        }
       // println!("{_i} {a:?} '{}'", lex.slice());
    }

    use passes::parser::parse;
    Ok(parse(&tokens, &module_name.to_str().unwrap().to_string())?)
}


pub fn compile_typecheck(ctx: &mut Context, filename: &PathBuf) -> Result<String>{
    let mut p = Program {
        modules: vec![],
        dependencies: HashSet::new(),
    };

    let root_ast = compile_module(ctx, filename.clone())?;
    p.modules.push(root_ast.clone());

    match root_ast {
        Cst::Module { name, body } => {
            for c in body {
                match c {
                    Cst::WithClause { include } => {
                        let parent_path = PathBuf::from_iter(filename.canonicalize()?.components().clone().into_iter().take(filename.canonicalize()?.components().count() - 1).collect::<Vec<std::path::Component>>().iter().map(|x| x.as_os_str()));
                        let include_path = parent_path.join(format!("{}.flr", include.get_symbol_name().unwrap()));

                        let include_ast = compile_module(ctx, include_path)?;
                        p.modules.push(include_ast.clone());
                    
                    },
                    _ => {}
                }
            }
        }
        _ => panic!("Should be a module")
    }

    //println!("{:#?}", p.clone());

    ctx.env.build(p.clone())?;
    //dbg!(ctx.env.clone());


    let mut tc = Typechecker::new(ctx.env.clone());
    let res = tc.check(filename.file_stem().unwrap().to_str().unwrap().to_string())?;
    dbg!(res.clone());
 
    // let mut flattener = Flattener::new(res.clone());
    // let flat = flattener.flatten();
    // let main_func: FunctionTableEntry = flat.items.get(&quantifier!(Root, Func("main"), End)).cloned().unwrap().into();
    // dbg!(&main_func);

    //let mut g = Generator::new(res);
    //let code = g.generate().unwrap();
    //println!("Output: \n{}", code);
    //todo!();
    Ok("".to_string())
}
