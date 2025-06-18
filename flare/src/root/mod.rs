pub mod passes;
use lrlex::DefaultLexerTypes;
use std::{collections::HashSet, fs::File, io::Read, path::PathBuf};
//use logos::Logos;
use passes::
    //backend::{flatten::Flattener, gen::Generator},
    midend::{
        environment::{Environment, Quantifier},
        typechecking::Typechecker,
    };
//use passes::midend::typechecking::Typechecker;
use resource::{cst::{Cst, Program}, errors::{CompResult, ParseErr, ParseErrorCollection}};
pub mod resource;

//use crate::root::resource::tk::{Tk, Token};

pub struct Context {
    pub env: Environment,
}

pub fn compile_module(ctx: &mut Context, src_path: PathBuf) -> CompResult<Cst> {
    let mut src_string = String::new();

    let mut src = File::open(src_path.clone())?;
    let module_name = src_path.file_stem().unwrap();
    src.read_to_string(&mut src_string)?;

    let lexerdef = crate::flare_l::lexerdef();
    let lexer: lrlex::LRNonStreamingLexer<'_, '_, DefaultLexerTypes> = lexerdef.lexer(&src_string);
    // Pass the lexer to the parser and lex and parse the input.
    let (res, errs) = crate::flare_y::parse(&lexer);
    let filename = src_path.file_name().unwrap().to_str().unwrap().to_string();
    let mut error_stream: Vec<ParseErr> = vec![];
    for e in errs {
        error_stream.push(ParseErr::new_from_lrpar_err(e, &filename, &src_string))
    }
    if !error_stream.is_empty() {
        
        return Err(ParseErrorCollection::from(error_stream).into())
    
    }

    match res {
        Some(r) => Ok(r.unwrap()),
        _ => {
            Err(ParseErr::new("Could not parse file", None).into())
        }
    }
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

pub fn compile_typecheck(ctx: &mut Context, filename: &PathBuf) -> CompResult<String> {
    let mut p = Program {
        modules: vec![],
        dependencies: HashSet::new(),
    };

    let root_ast = compile_module(ctx, filename.clone())?;
    p.modules.push(root_ast.clone());

    match root_ast {
        Cst::Module { name: _, body } => {
            for c in body {
                match c {
                    Cst::WithClause { include } => {
                        let parent_path = PathBuf::from_iter(
                            filename
                                .canonicalize()?
                                .components()
                                .clone()
                                .into_iter()
                                .take(filename.canonicalize()?.components().count() - 1)
                                .collect::<Vec<std::path::Component>>()
                                .iter()
                                .map(|x| x.as_os_str()),
                        );
                        let include_path =
                            parent_path.join(format!("{}.flr", include.get_symbol_name().unwrap()));

                        let include_ast = compile_module(ctx, include_path)?;
                        p.modules.push(include_ast.clone());
                    }
                    _ => {}
                }
            }
        }
        _ => panic!("Should be a module"),
    }

    //println!("{:#?}", p.clone());

    ctx.env.build(p.clone())?;
    //dbg!(ctx.env.clone());

    let mut tc = Typechecker::new(ctx.env.clone());
    let res = tc.check()?;
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
