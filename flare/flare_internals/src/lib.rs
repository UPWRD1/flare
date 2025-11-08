#[warn(clippy::pedantic)]
#[forbid(unused_unsafe, clippy::fallible_impl_from)]
#[deny(
    clippy::perf,
    clippy::correctness,
    clippy::suspicious,
    clippy::complexity,
    clippy::style,
    clippy::branches_sharing_code,
    clippy::use_self,
    clippy::box_collection,
    clippy::boxed_local,
    clippy::redundant_allocation,
    clippy::deref_by_slicing,
    clippy::cloned_instead_of_copied
)]
#[warn(clippy::large_stack_frames, clippy::panic, clippy::dbg_macro)]
#[allow(
    clippy::type_complexity,
    clippy::diverging_sub_expression,
    clippy::missing_panics_doc
)]
pub mod passes;
pub mod resource;

use std::{
    hash::{Hash, Hasher},
    path::Path,
    time::{Duration, Instant},
};

use rustc_hash::{FxHashMap, FxHasher};

use crate::{
    passes::{
        //backend::{flatten::Flattener, gen::Generator},
        midend::{environment::Environment, typechecking::Solver},
        parser,
    },
    resource::{
        errors::{CompResult, CompilerErr, ErrorCollection},
        rep::{
            ast::{Package, Program},
            files::{FileID, FileSource},
        },
    },
};

//use crate::root::resource::tk::{Tk, Token};
#[derive(Debug)]
pub struct Context {
    pub filectx: FxHashMap<FileID, FileSource<'static>>,
}

impl Context {
    pub fn new(src_path: &'static Path, id: FileID) -> Self {
        let src_text = std::fs::read_to_string(src_path).unwrap();

        // Leak the string to get a 'static lifetime, then cast to 'src
        let src_text: &'static str = Box::leak(src_text.into_boxed_str());
        let source = FileSource {
            filename: src_path,
            src_text,
        };
        Context {
            filectx: vec![(id, source)].into_iter().collect::<FxHashMap<_, _>>(),
        }
    }

    // pub fn to_static(self) -> Context {
    //     Context {
    //         filectx: self
    //             .filectx
    //             .into_iter()
    //             .map(|(k, v)| {
    //                 let k: (u64, FileSource<'static>) = (
    //                     k,
    //                     FileSource {
    //                         filename: v.filename.to_path_buf().leak(),
    //                         src_text: v.src_text.to_string().leak(),
    //                     },
    //                 );
    //                 k
    //             })
    //             .collect(),
    //     }
    // }

    pub fn parse_file(&mut self, id: FileID) -> CompResult<Package> {
        parser::parse(self, id)
    }

    pub fn parse_program(&mut self, id: FileID) -> CompResult<Program> {
        let src_path = self.filectx.get(&id).unwrap().filename;
        let path = src_path.canonicalize().unwrap();
        let parent_dir = path.parent().unwrap();
        let dir_contents: Vec<FileSource<'static>> = std::fs::read_dir(parent_dir)?
            .filter_map(Result::ok)
            .filter(|entry| entry.path().extension().is_some_and(|ext| ext == "flr"))
            .map(|x| {
                let src_text = std::fs::read_to_string(x.path()).unwrap();
                // Leak the string to get a 'static lifetime, then cast to 'src
                let src_text = Box::leak(src_text.into_boxed_str());

                FileSource {
                    filename: x.path().leak(),
                    src_text,
                }
            })
            .collect::<Vec<_>>();
        let mut processed: Vec<Result<(Package, FileID), CompilerErr>> = vec![];
        // dir_contents
        //     .iter()
        //     .map(|entry: &FileSource<'src>| {
        //         let converted_id = convert_path_to_id(entry.filename);
        //         self.filectx.insert(converted_id, entry.clone());
        //         let (pack, str): (Package<'src>, &str) = self.parse_file(converted_id)?;
        //         Ok((pack, entry.filename, str))
        //     })
        //     .collect();
        for entry in dir_contents {
            let converted_id = convert_path_to_id(entry.filename);
            self.filectx.insert(converted_id, entry.clone());
            let pack = self.parse_file(converted_id)?;
            // dbg!(&pack.name);
            processed.push(Ok((pack, converted_id)))
        }
        // dbg!(&processed
        //     .iter()
        //     .filter(|x| x.is_ok())
        //     .map(|x| x.as_ref().unwrap().1));

        let (v, errors): (Vec<_>, Vec<_>) = processed.into_iter().partition(|x| x.is_ok());
        let v: Vec<_> = v.into_iter().map(Result::unwrap).collect();
        let errors: Vec<_> = errors.into_iter().map(Result::unwrap_err).collect();

        if errors.is_empty() {
            Ok(Program { packages: v })
        } else {
            Err(ErrorCollection::new(errors).into())
        }
    }

    pub fn compile_program(&mut self, id: FileID) -> CompResult<((), Duration)> {
        let now: Instant = Instant::now();
        let program = self.parse_program(id)?;
        // dbg!(&program);

        let e = Environment::build(&program)?;
        //dbg!(&e);
        let mut s = Solver::new(&e, resource::rep::quantifier::QualifierFragment::Root);
        s.check()?;
        //dbg!(&e);
        let elapsed = now.elapsed();

        Ok(((), elapsed))
    }
}

pub fn convert_path_to_id(path: &Path) -> FileID {
    let mut hasher = FxHasher::default();
    path.hash(&mut hasher);
    //path.canonicalize().unwrap().hash(&mut hasher);
    hasher.finish()
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
