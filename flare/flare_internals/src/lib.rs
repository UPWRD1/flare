// Copyright 2025 Luke Davis

//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at

//        http://www.apache.org/licenses/LICENSE-2.0

//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.

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
    clippy::suspicious,
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
    clippy::unwrap_in_result,
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
    cell::OnceCell,
    hash::{Hash, Hasher},
    mem::MaybeUninit,
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use petgraph::graph::NodeIndex;
use rustc_hash::{FxHashMap, FxHasher};

use crate::{
    passes::{
        //backend::{flatten::Flattener, gen::Generator},
        backend::{
            lir::LIR,
            lowering::{Lowerer, ir::IR},
            monomorph, simplify,
            target::{Generator, Target},
        },
        midend::{
            environment::Environment,
            resolution::Resolver,
            typechecker::Typechecker,
            typing::{ItemSource, Type, Typed, TypesOutput},
        },
        parser,
    },
    resource::{
        errors::{CompResult, CompilerErr},
        rep::{
            ast::{
                ItemId,
                Package,
                Program,
                Untyped, // Untyped
            },
            files::{FileID, FileSource},
        },
    },
};

// #[derive(Debug)]
struct Init;

struct Parse {
    program: Program<Untyped>,
}

struct Build {
    env: Environment,
}

struct Resolve {
    order: Vec<NodeIndex>,
    env: Environment,
}

struct Typecheck {
    items: Vec<(ItemId, TypesOutput)>,
    source: ItemSource,
}

struct Lower {
    ir: Vec<IR>,
}
struct Simplify {
    ir: Vec<IR>,
}
struct Monomorph {
    ir: Vec<IR>,
}
struct Convert<T: Target> {
    converted: Vec<T::Input>,
}
struct Generate<T: Target> {
    output: T::Output,
}

pub trait Operation {}
impl Operation for Init {}
impl Operation for Parse {}
impl Operation for Build {}
impl Operation for Resolve {}
impl Operation for Typecheck {}
impl Operation for Lower {}
impl Operation for Simplify {}
impl Operation for Monomorph {}

pub type FileCtx = FxHashMap<FileID, FileSource>;
// #[derive(Debug)]
/// The context for a Flare bundle.
pub struct Context<const N: usize, T, O> {
    pub filectx: FileCtx,
    pub target: T,
    pub intrinsics: [(&'static str, &'static [Untyped], Type); N],
    pub op: O,
}

impl<const N: usize, T: Target> Context<N, T, Init> {
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
            op: Init,
        }
    }

    fn parse_file(&self, id: FileID) -> CompResult<Vec<Package<Untyped>>> {
        parser::parse(&self.filectx, id)
    }

    pub fn parse(self) -> CompResult<Context<N, T, Parse>> {
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

        let program = Program { packages: v };
        Ok(Context {
            op: Parse { program },
            filectx: self.filectx,
            target: self.target,
            intrinsics: self.intrinsics,
        })
    }
}

impl<const N: usize, T: Target> Context<N, T, Parse> {
    pub fn build(self) -> CompResult<Context<N, T, Build>> {
        let env = Environment::build(&self.op.program)?;
        Ok(Context {
            op: Build { env },
            filectx: self.filectx,
            target: self.target,
            intrinsics: self.intrinsics,
        })
    }
}

impl<const N: usize, T: Target> Context<N, T, Build> {
    pub fn resolve(self) -> CompResult<Context<N, T, Resolve>> {
        let mut resolver = Resolver::new(self.op.env, self.intrinsics);
        let order = resolver.build()?;
        let env = resolver.finish()?;
        Ok(Context {
            op: Resolve { order, env },
            filectx: self.filectx,
            target: self.target,
            intrinsics: self.intrinsics,
        })
    }
}

impl<const N: usize, T: Target> Context<N, T, Resolve> {
    pub fn typecheck(self) -> CompResult<Context<N, T, Typecheck>> {
        let tc = Typechecker::new(self.op.order.leak(), self.op.env);
        let (items, source) = tc.check()?;
        Ok(Context {
            op: Typecheck { items, source },
            filectx: self.filectx,
            target: self.target,
            intrinsics: self.intrinsics,
        })
    }
}

impl<const N: usize, T: Target> Context<N, T, Typecheck> {
    pub fn lower(self) -> CompResult<Context<N, T, Lower>> {
        let lowerer = Lowerer::new();
        let ir = lowerer.lower(self.op.source, &self.op.items);
        Ok(Context {
            op: Lower { ir },
            filectx: self.filectx,
            target: self.target,
            intrinsics: self.intrinsics,
        })
    }
}

impl<const N: usize, T: Target> Context<N, T, Lower> {
    pub fn simplify(self) -> CompResult<Context<N, T, Simplify>> {
        let ir = simplify::simplify(&self.op.ir);
        Ok(Context {
            op: Simplify { ir },
            filectx: self.filectx,
            target: self.target,
            intrinsics: self.intrinsics,
        })
    }
}

impl<const N: usize, T: Target> Context<N, T, Simplify> {
    pub fn monomorph(self) -> CompResult<Context<N, T, Monomorph>> {
        let ir = monomorph::monomorph(self.op.ir);
        Ok(Context {
            op: Monomorph { ir },
            filectx: self.filectx,
            target: self.target,
            intrinsics: self.intrinsics,
        })
    }
}

impl<const N: usize, T: Target> Context<N, T, Monomorph> {
    pub fn convert(self) -> CompResult<Context<N, T, Convert<T>>> {
        let converted = self.target.convert(self.op.ir);
        Ok(Context {
            op: Convert { converted },
            filectx: self.filectx,
            target: self.target,
            intrinsics: self.intrinsics,
        })
    }
}

impl<const N: usize, T: Target> Context<N, T, Convert<T>> {
    pub fn generate(self) -> CompResult<Context<N, T, Generate<T>>> {
        let g = Generator::new(self.target, self.op.converted);

        let output = g.generate();
        Ok(Context {
            op: Generate { output },
            filectx: self.filectx,
            target: self.target,
            intrinsics: self.intrinsics,
        })
    }
}

impl<const N: usize, T: Target> Context<N, T, Generate<T>> {
    pub fn finish(self) -> T::Output {
        self.op.output
    }
}

pub fn compile_program(&mut self) -> CompResult<(T::Output, Duration)> {
    // use internment::Intern;
    // use resource::rep::quantifier::QualifierFragment::*;
    let now: Instant = Instant::now();

    let final_ir = self.target.convert(ir);

    let g = Generator::new(self.target, final_ir);

    let out = g.generate();
    // dbg!(&out);
    let elapsed = now.elapsed();

    Ok((out, elapsed))
}

pub fn convert_path_to_id(path: &Path) -> FileID {
    let mut hasher = FxHasher::default();
    path.hash(&mut hasher);
    //path.canonicalize().unwrap().hash(&mut hasher);
    hasher.finish()
}
