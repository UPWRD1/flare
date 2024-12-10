pub mod passes;
use std::{collections::HashSet, fs, path:: PathBuf};

use logos::Logos;
use passes::midend::environment::{self, Environment};
//use passes::midend::typechecking::Typechecker;
use resource::{ast::{Module, Program}, lex::LexRes};
pub mod resource;

use crate::root::resource::tk::{Tk, Token};


pub struct Context {
    pub env: Environment,
}

fn query_lex(filename: &String) -> LexRes {
    let src = fs::read_to_string(filename.clone()).unwrap();
    let mut lex = Tk::lexer(&src);

    let mut tokens: Vec<Token> = vec![];
    let mut symbols: HashSet<String> = HashSet::new();


    for _i in 0..lex.clone().collect::<Vec<Result<Tk, ()>>>().len() {
        let a: Tk = lex.next().unwrap().unwrap();
        println!("{_i} {a:?} '{}'", lex.slice());
        tokens.push(Token::new(a, lex.slice().to_string(), 0, 0));
        if matches!(a, Tk::TkSymbol) {
            symbols.insert(lex.slice().to_owned());

        }
    }

    LexRes {tokens, filename: filename.clone(), symbols}
}

pub fn compile_filename(ctx: &Context, filename: &String) -> Module {
    let res: LexRes = query_lex(filename);
    dbg!(res.symbols);
    use passes::parser::parse;
    let m = parse(&res.tokens);

    Module {
        name: PathBuf::from(filename.clone().as_str()).file_stem().unwrap().to_owned().into_string().unwrap(),
        body: m.body.clone(),
    }
}

pub fn get_dependencies(ctx: &Context,mut p: Program) -> Program {
    let m = p.modules.first().unwrap();
    for a in m.body.clone() {
        match a {
            resource::ast::Ast::WithClause { include } => {
                let to_compile = include.first().unwrap().get_lit();
                let tc = format!(
                    "{}/{}.alg",
                    std::env::current_dir().unwrap().to_string_lossy(),
                    to_compile
                );
                let dep_ast = compile_filename(ctx, &tc);
                let mut dep_p = get_dependencies(ctx, Program {
                    modules: vec![dep_ast],
                    dependencies: vec![],
                });
                p.dependencies.push(tc);
                p.dependencies.append(&mut dep_p.dependencies);
                p.modules.append(&mut dep_p.modules);
                //dbg!(p.dependencies.clone());
            }
            _ => continue,
        }
    }
    p
}

pub fn compile_typecheck(ctx: &Context, filename: &String) {
    let mut p = Program {
        modules: vec![],
        dependencies: vec![],
    };
    let root_ast = compile_filename(ctx, filename);
    p.modules.push(root_ast.clone());
    let np = get_dependencies(ctx, p.clone());
    dbg!(np.clone());

    let mut env = environment::Environment::new();
    env.build(np);

    // let mut tc = Typechecker::new();
    // tc.check(Box::new(np));
    //let new_p: TypedProgram = np.clone().into();
    //new_p
}

pub fn compile_json(filename: &String) -> String {
    //compile_typecheck(filename);
    //dbg!(new_p.clone());

    serde_json::to_string(&()).unwrap()
}
