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
    unused_allocation,
    clippy::ptr_arg,
    clippy::needless_pass_by_ref_mut,
    clippy::needless_pass_by_value,
    // clippy::min_ident_chars,
    )]
#[warn(
    clippy::large_stack_frames,
    // clippy::panic,
    clippy::dbg_macro,
    // clippy::unwrap_used,
    // clippy::restriction
)]
#[allow(
    // warnings,
    unused_variables,
    clippy::must_use_candidate,
    clippy::return_self_not_must_use,
    clippy::type_complexity,
    clippy::diverging_sub_expression,
    clippy::missing_panics_doc,
    unstable_name_collisions
)]
pub mod passes;
pub mod resource;

use core::iter::Iterator;
use std::{
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
};

use petgraph::graph::NodeIndex;
use rustc_hash::{FxHashMap, FxHasher};

use crate::{
    passes::{
        backend::{
            lir::{ClosureConvertOut, closure_convert},
            target::{Generator, Target},
        },
        frontend::{
            environment::Environment,
            parser,
            resolution::Resolver,
            typechecker::Typechecker,
            typing::{ItemSource, Type, TypesOutput},
        },
        //backend::{flatten::Flattener, gen::Generator},
        midend::{lowering::Lowerer, monomorph, reduce, simplify},
    },
    resource::{
        errors::{CompResult, CompilerErr},
        rep::{
            frontend::{
                ast::{
                    ItemId,
                    Package,
                    Program,
                    Untyped, // Untyped
                },
                files::{FileID, FileSource},
            },
            midend::ir::IR,
        },
    },
};

// #[derive(Debug)]
pub struct Init;

pub struct Parse {
    program: Program<Untyped>,
}

pub struct Build {
    env: Environment,
}

pub struct Resolve {
    order: Vec<NodeIndex>,
    env: Environment,
}
pub struct Typecheck {
    items: Vec<(ItemId, TypesOutput)>,
    source: ItemSource,
}
pub struct Lower {
    pub ir: Vec<IR>,
}

pub struct Simplify {
    pub ir: Vec<IR>,
}

pub struct Reduce {
    pub ir: Vec<IR>,
}

pub struct Monomorph {
    pub ir: Vec<IR>,
}

pub struct Convert {
    converted: Vec<ClosureConvertOut>,
}

pub struct Generate<T: Target> {
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
impl Operation for Reduce {}
impl Operation for Monomorph {}

pub type FileCtx = FxHashMap<FileID, FileSource>;

// pub type CtxResult<const N: usize, T, O> = Result<Context<N, T, O>, CtxErr>;

/// The context/state machine for compiling a Flare bundle.
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
        let ir = simplify::simplify(self.op.ir);
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
        // Sanity check
        // debug_assert!(ir.iter().all(|ir| matches!(ir.type_of(), _)));
        // let ir = self.op.ir;
        Ok(Context {
            op: Monomorph { ir },
            filectx: self.filectx,
            target: self.target,
            intrinsics: self.intrinsics,
        })
    }
}

impl<const N: usize, T: Target> Context<N, T, Monomorph> {
    pub fn reduce(self) -> CompResult<Context<N, T, Reduce>> {
        let ir = reduce::reduce(self.op.ir);
        // let ir = self.op.ir;
        Ok(Context {
            op: Reduce { ir },
            filectx: self.filectx,
            target: self.target,
            intrinsics: self.intrinsics,
        })
    }
}
impl<const N: usize, T: Target> Context<N, T, Reduce> {
    pub fn convert(self) -> CompResult<Context<N, T, Convert>> {
        let converted = closure_convert(self.op.ir);
        Ok(Context {
            op: Convert { converted },
            filectx: self.filectx,
            target: self.target,
            intrinsics: self.intrinsics,
        })
    }
}

impl<const N: usize, T: Target> Context<N, T, Convert> {
    pub fn generate(self) -> CompResult<Context<N, T, Generate<T>>> {
        let g = Generator::new(self.target.clone(), self.op.converted);

        let output = g.generate();
        Ok(Context {
            op: Generate { output },
            filectx: self.filectx,
            target: self.target.clone(),
            intrinsics: self.intrinsics,
        })
    }
}

impl<const N: usize, T: Target> Context<N, T, Generate<T>> {
    pub fn finish(self) -> CompResult<Vec<u8>> {
        Ok((self.op.output).into())
    }
}

pub fn convert_path_to_id(path: &Path) -> FileID {
    let mut hasher = FxHasher::default();
    path.hash(&mut hasher);
    //path.canonicalize().unwrap().hash(&mut hasher);
    hasher.finish()
}
