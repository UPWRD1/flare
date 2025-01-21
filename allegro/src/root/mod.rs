pub mod passes;
use std::{collections::HashSet, fs, path:: PathBuf};
use itertools::Itertools;
use logos::Logos;
use passes::midend::{environment::Environment, typechecking::Typechecker};
//use passes::midend::typechecking::Typechecker;
use resource::{ast::{FileModule, Program}, errors::{CompilerError, ParsingError}, lex::LexRes};
pub mod resource;
use anyhow::Result;
use crate::root::resource::tk::{Tk, Token};


pub struct Context {
    pub env: Environment,
}

fn query_lex(ctx: &mut Context, filename: &String) -> LexRes {
    let src = fs::read_to_string(filename.clone()).unwrap();
    ctx.env.add_file(filename, PathBuf::from(filename), &src);
    let mut lex = Tk::lexer(&src);

    let mut tokens: Vec<Token> = vec![];
    let mut symbols: HashSet<String> = HashSet::new();


    for _i in 0..lex.clone().collect::<Vec<Result<Tk, ()>>>().len() {
        let a: Tk = lex.next().unwrap().unwrap();
        //println!("{_i} {a:?} '{}'", lex.slice());
        tokens.push(Token::new(a, lex.slice().to_string()));
        if matches!(a, Tk::TkSymbol(_)) {
            symbols.insert(lex.slice().to_owned());
        }
    }

    LexRes {tokens, filename: filename.clone(), symbols}
}

pub fn compile_filename(ctx: &mut Context, filename: &String) -> Result<FileModule, ParsingError> {
    let res: LexRes = query_lex(ctx, filename);
    ///dbg!(res.symbols);
    use passes::parser::parse;
    let m = parse(&res.tokens)?;

    Ok(FileModule {
        name: PathBuf::from(filename.clone().as_str()).file_stem().unwrap().to_owned().into_string().unwrap(),
        body: m.body.clone(),
    })
}
pub fn get_dependencies(ctx: &mut Context, mut p: Program) -> Result<Program> {
    let m = p.modules.iter().nth(0).unwrap();
    for a in m.body.clone() {
        match a {
            resource::ast::Ast::WithClause { include } => {
                let to_compile = include.first().unwrap().get_lit();
                let tc = format!(
                    "{}/{}.alg",
                    std::env::current_dir().unwrap().to_string_lossy(),
                    to_compile
                );
                let dep_ast = compile_filename(ctx, &tc)?;
                let mut dep_p = get_dependencies(ctx, Program {
                    modules: vec![dep_ast],
                    dependencies: HashSet::new(),
                }).unwrap();
                p.dependencies.insert(tc);
                p.dependencies = p.dependencies.union(&dep_p.dependencies.iter().map(|e| e.clone()).collect()).map(|e| e.clone()).collect();
                p.modules.append(&mut dep_p.modules);
                p.modules = p.modules.into_iter().dedup().collect();
                //dbg!(p.dependencies.clone());
            }
            _ => continue,
        }
    }
    p.modules.dedup();

    Ok(p)
}

pub fn compile_typecheck(ctx: &mut Context, filename: &String) -> Result<()>{
    let mut p = Program {
        modules: vec![],
        dependencies: HashSet::new(),
    };
    let root_ast = compile_filename(ctx, filename)?;
    p.modules.push(root_ast.clone());

    let np = get_dependencies(ctx, p)?;
    println!("{:#?}", np.clone());

    ctx.env.build(np);
    dbg!(ctx.env.clone());


    let mut tc = Typechecker::new(ctx.env.clone());
    let res = tc.check()?;
    dbg!(res);
    //let new_p: TypedProgram = np.clone().into();
    //new_p
    Ok(())
}
