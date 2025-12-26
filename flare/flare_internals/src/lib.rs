// #[warn(clippy::pedantic)]
#[forbid(
    unused_unsafe,
    clippy::fallible_impl_from,
    // clippy::undocumented_unsafe_blocks
)]
#[deny(
    // clippy::pedantic,
    // clippy::nursery,
    clippy::perf,
    clippy::correctness,
    // clippy::suspicious,
    clippy::complexity,
    clippy::style,
    clippy::branches_sharing_code,
    clippy::use_self,
    clippy::box_collection,
    clippy::boxed_local,
    clippy::redundant_allocation,
    clippy::deref_by_slicing,
    clippy::cloned_instead_of_copied,
    clippy::used_underscore_binding,
    // clippy::unwrap_in_result,
    // clippy::min_ident_chars,
    )]
#[warn(
    clippy::large_stack_frames,
    clippy::panic,
    clippy::dbg_macro,
    clippy::unwrap_used,
    // clippy::restriction
)]
#[allow(
    clippy::must_use_candidate,
    clippy::return_self_not_must_use,
    clippy::type_complexity,
    clippy::diverging_sub_expression,
    clippy::missing_panics_doc
)]
pub mod passes;
pub mod resource;

use core::iter::Iterator;
use std::{
    hash::{Hash, Hasher},
    path::Path,
    time::{Duration, Instant},
};

use chumsky::span::SimpleSpan;
use rustc_hash::{FxHashMap, FxHasher};

use crate::{
    passes::{
        //backend::{flatten::Flattener, gen::Generator},
        backend::{
            lowering::Lowerer,
            monomorph, simplify,
            target::{Generator, Target},
        },
        midend::{
            environment::Environment, resolution::Resolver, typechecker::Typechecker, typing::Type,
        },
        parser,
    },
    resource::{
        errors::{CompResult, CompilerErr},
        rep::{
            Spanned,
            ast::{
                Package,
                Program,
                Untyped,
                // Untyped
            },
            files::{FileID, FileSource},
        },
    },
};

pub type FileCtx = FxHashMap<FileID, FileSource<'static>>;
#[derive(Debug)]
/// The context for a Flare bundle.
pub struct Context<T: Target> {
    pub filectx: FileCtx,
    pub target: T,
}

impl<T: Target> Context<T> {
    pub fn new(src_path: &'static Path, id: FileID, target: T) -> Self {
        let src_text = std::fs::read_to_string(src_path).unwrap();

        // Leak the string to get a 'static lifetime, then cast to 'src
        let src_text: &'static str = Box::leak(src_text.into_boxed_str());
        let source = FileSource {
            filename: src_path,
            src_text,
        };
        Context {
            filectx: vec![(id, source)].into_iter().collect::<FxHashMap<_, _>>(),
            target,
        }
    }

    pub fn parse_file(&self, id: FileID) -> CompResult<Vec<Package<Untyped>>> {
        parser::parse(&self.filectx, id)
    }

    pub fn parse_program(&mut self, id: FileID) -> CompResult<Program<Untyped>> {
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
        let mut processed: Vec<(Vec<Package<Untyped>>, FileID)> = vec![];
        for entry in dir_contents {
            let converted_id = convert_path_to_id(entry.filename);
            self.filectx.insert(converted_id, entry.clone());
            let pack = self.parse_file(converted_id)?;
            processed.push((pack, converted_id))
        }

        let v: Vec<_> = processed
            .into_iter()
            .flat_map(|(packages, id)| {
                packages
                    .into_iter()
                    .map(|p| (p, id))
                    .collect::<Vec<(Package<_>, FileID)>>()
            })
            .collect::<Vec<_>>();

        Ok(Program { packages: v })
    }

    pub fn compile_program(&mut self, id: FileID) -> CompResult<(T::Output, Duration)> {
        // use internment::Intern;
        // use resource::rep::quantifier::QualifierFragment::*;
        let now: Instant = Instant::now();
        let program = self.parse_program(id)?;

        let e = Environment::build(&program)?;
        let default_span = Type::Infer.to_default_span();
        let intrinsics: [(&str, &'static [Untyped], Type); 10] = [
            (
                "intrinsic_arith_add",
                vec![
                    Untyped(Spanned("l".to_string().into(), SimpleSpan::default())),
                    Untyped(Spanned("r".to_string().into(), SimpleSpan::default())),
                ]
                .leak(),
                Type::Func(
                    Type::Num.to_default_span(),
                    Type::Func(Type::Num.to_default_span(), Type::Num.to_default_span())
                        .to_default_span(),
                ),
            ),
            (
                "intrinsic_arith_sub",
                vec![
                    Untyped(Spanned("l".to_string().into(), SimpleSpan::default())),
                    Untyped(Spanned("r".to_string().into(), SimpleSpan::default())),
                ]
                .leak(),
                Type::Func(
                    Type::Num.to_default_span(),
                    Type::Func(Type::Num.to_default_span(), Type::Num.to_default_span())
                        .to_default_span(),
                ),
            ),
            (
                "intrinsic_arith_mul",
                vec![
                    Untyped(Spanned("l".to_string().into(), SimpleSpan::default())),
                    Untyped(Spanned("r".to_string().into(), SimpleSpan::default())),
                ]
                .leak(),
                Type::Func(
                    Type::Num.to_default_span(),
                    Type::Func(Type::Num.to_default_span(), Type::Num.to_default_span())
                        .to_default_span(),
                ),
            ),
            (
                "intrinsic_arith_div",
                vec![
                    Untyped(Spanned("l".to_string().into(), SimpleSpan::default())),
                    Untyped(Spanned("r".to_string().into(), SimpleSpan::default())),
                ]
                .leak(),
                Type::Func(
                    Type::Num.to_default_span(),
                    Type::Func(Type::Num.to_default_span(), Type::Num.to_default_span())
                        .to_default_span(),
                ),
            ),
            (
                "intrinsic_compare_eq",
                vec![
                    Untyped(Spanned("l".to_string().into(), SimpleSpan::default())),
                    Untyped(Spanned("r".to_string().into(), SimpleSpan::default())),
                ]
                .leak(),
                Type::Func(
                    Type::Generic(default_span.convert("?T".to_string())).to_default_span(),
                    Type::Func(
                        Type::Generic(default_span.convert("?T".to_string())).to_default_span(),
                        Type::Bool.to_default_span(),
                    )
                    .to_default_span(),
                ),
            ),
            (
                "intrinsic_compare_neq",
                vec![
                    Untyped(Spanned("l".to_string().into(), SimpleSpan::default())),
                    Untyped(Spanned("r".to_string().into(), SimpleSpan::default())),
                ]
                .leak(),
                Type::Func(
                    Type::Generic(default_span.convert("?T".to_string())).to_default_span(),
                    Type::Func(
                        Type::Generic(default_span.convert("?T".to_string())).to_default_span(),
                        Type::Bool.to_default_span(),
                    )
                    .to_default_span(),
                ),
            ),
            (
                "intrinsic_compare_clt",
                vec![
                    Untyped(Spanned("l".to_string().into(), SimpleSpan::default())),
                    Untyped(Spanned("r".to_string().into(), SimpleSpan::default())),
                ]
                .leak(),
                Type::Func(
                    Type::Num.to_default_span(),
                    Type::Func(Type::Num.to_default_span(), Type::Bool.to_default_span())
                        .to_default_span(),
                ),
            ),
            (
                "intrinsic_compare_cle",
                vec![
                    Untyped(Spanned("l".to_string().into(), SimpleSpan::default())),
                    Untyped(Spanned("r".to_string().into(), SimpleSpan::default())),
                ]
                .leak(),
                Type::Func(
                    Type::Num.to_default_span(),
                    Type::Func(Type::Num.to_default_span(), Type::Bool.to_default_span())
                        .to_default_span(),
                ),
            ),
            (
                "intrinsic_compare_cgt",
                vec![
                    Untyped(Spanned("l".to_string().into(), SimpleSpan::default())),
                    Untyped(Spanned("r".to_string().into(), SimpleSpan::default())),
                ]
                .leak(),
                Type::Func(
                    Type::Num.to_default_span(),
                    Type::Func(Type::Num.to_default_span(), Type::Bool.to_default_span())
                        .to_default_span(),
                ),
            ),
            (
                "intrinsic_compare_cge",
                vec![
                    Untyped(Spanned("l".to_string().into(), SimpleSpan::default())),
                    Untyped(Spanned("r".to_string().into(), SimpleSpan::default())),
                ]
                .leak(),
                Type::Func(
                    Type::Num.to_default_span(),
                    Type::Func(Type::Num.to_default_span(), Type::Bool.to_default_span())
                        .to_default_span(),
                ),
            ),
        ];
        let mut resolver = Resolver::new(e, intrinsics);
        let order = resolver.build()?;
        let resolved_e = resolver.finish();

        let tc = Typechecker::new(order.leak(), resolved_e);
        let (items, source) = tc.check()?;
        // dbg!(&items);
        let lowerer = Lowerer::new();
        let ir = lowerer.lower(source, &items);
        let ir = simplify::simplify(&ir);
        let ir = monomorph::monomorph(ir);
        let ir = simplify::simplify(&ir);

        let g = Generator::new(self.target, ir);

        let out = g.generate();
        // dbg!(&out);
        let elapsed = now.elapsed();

        Ok((out, elapsed))
    }
}

pub fn convert_path_to_id(path: &Path) -> FileID {
    let mut hasher = FxHasher::default();
    path.hash(&mut hasher);
    //path.canonicalize().unwrap().hash(&mut hasher);
    hasher.finish()
}
