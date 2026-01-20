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
    warnings,
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
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

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

pub trait Operation {}
#[derive(Debug)]
struct Init;
impl Operation for Init {}

pub type FileCtx = FxHashMap<FileID, FileSource>;
// #[derive(Debug)]
/// The context for a Flare bundle.
pub struct Context<const N: usize, T> {
    pub filectx: FileCtx,
    pub target: T,
    pub intrinsics: [(&'static str, &'static [Untyped], Type); N],
}

impl<const N: usize, T: Target> Context<N, T> {
    // pub fn new(src_paths: &'static Path, id: FileID, target: T) -> Self {
    //     let src_text = std::fs::read_to_string(src_path).unwrap();

    //     // Leak the string to get a 'static lifetime, then cast to 'src
    //     let src_text: &'static str = Box::leak(src_text.into_boxed_str());
    //     let source = FileSource {
    //         filename: src_path,
    //         src_text,
    //     };
    //     Context {
    //         filectx: vec![(id, source)].into_iter().collect::<FxHashMap<_, _>>(),
    //         target,
    //         // intrinsics: Default::default(),
    //         // raw_ast: Default::default(),
    //         // typed_ast: Default::default(),
    //         // env: Default::default(),
    //         // ir: Default::default(),
    //         // lir: Default::default(),
    //     }
    // }

    pub fn new(
        src_paths: Vec<PathBuf>,
        target: T,
        intrinsics: [(&'static str, &'static [Untyped], Type); N],
    ) -> Self {
        let filectx = src_paths
            .into_iter()
            .map(|filepath| {
                let id = convert_path_to_id(&filepath);

                let src_text = std::fs::read_to_string(&filepath).unwrap();

                let source = FileSource {
                    filepath,
                    source: src_text,
                };
                (id, source)
            })
            .collect();
        Self {
            filectx,
            target,
            intrinsics,
        }
    }

    pub fn parse_file(&self, id: FileID) -> CompResult<Vec<Package<Untyped>>> {
        parser::parse(&self.filectx, id)
    }

    pub fn parse_program(&mut self) -> CompResult<Program<Untyped>> {
        let mut processed: Vec<(Vec<Package<Untyped>>, FileID)> = vec![];
        for id in self.filectx.keys() {
            let pack = self.parse_file(*id)?;
            processed.push((pack, *id))
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

    pub fn compile_program(&mut self) -> CompResult<(T::Output, Duration)> {
        // use internment::Intern;
        // use resource::rep::quantifier::QualifierFragment::*;
        let now: Instant = Instant::now();
        let program = self.parse_program()?;

        let e = Environment::build(&program)?;

        let mut resolver = Resolver::new(e, self.intrinsics);
        let order = resolver.build()?;
        let resolved_e = resolver.finish()?;

        let tc = Typechecker::new(order.leak(), resolved_e);
        let (items, source) = tc.check()?;
        // dbg!(&source);
        let lowerer = Lowerer::new();
        let ir = lowerer.lower(source, &items);
        let ir = simplify::simplify(&ir);
        let ir = monomorph::monomorph(ir);
        // let ir = simplify::simplify(&ir);

        let final_ir = self.target.convert(ir);

        let g = Generator::new(self.target, final_ir);

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
