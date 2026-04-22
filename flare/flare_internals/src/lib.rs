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
#![feature(control_flow_into_value)]
#[forbid(
    unused_unsafe,
    clippy::fallible_impl_from,
    clippy::used_underscore_binding,
    clippy::used_underscore_items,
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
    unused_allocation,
    clippy::ptr_arg,
    clippy::needless_pass_by_ref_mut,
    clippy::needless_pass_by_value,
    // clippy::min_ident_chars,
    )]
#[warn(
    clippy::unwrap_in_result,
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
use rustc_hash::FxHashMap;
use std::path::PathBuf;

use crate::{
    passes::{
        backend::{
            lir::{ClosureConvertOut, closure_convert},
            target::{Generator, Target},
        },
        frontend::{
            environment::{Environment, EnvironmentBuilder},
            parser,
            resolution::Resolver,
            typechecker::Typechecker,
            typing::{ItemSource, TypesOutput},
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
                    UntypedAst,
                    // Untyped
                },
                cst::{PackageCollection, UntypedCst},
                files::{FileID, FileSource},
            },
            midend::ir::IR,
        },
    },
};

// #[derive(Debug)]
pub struct Init;

pub struct Parse {
    program: PackageCollection<UntypedCst>,
}

pub struct Build {
    env: Environment<UntypedCst>,
}
#[derive(Debug)]
pub struct Resolve {
    env: Environment<UntypedAst>,
}
#[derive(Debug)]
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

pub struct Monomorph {
    pub ir: Vec<IR>,
}

pub struct Reduce {
    pub ir: Vec<IR>,
}

pub struct Convert {
    pub cc: Vec<ClosureConvertOut>,
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
pub struct Context<T, O> {
    pub filectx: FileCtx,
    pub target: T,
    pub op: O,
}

pub fn make_filectx(src_paths: &[PathBuf]) -> FileCtx {
    src_paths
        .iter()
        .enumerate()
        .map(|(id, filepath)| {
            let id = id as u64;
            let src_text = std::fs::read_to_string(filepath).unwrap();

            let source = FileSource {
                filepath: filepath.to_path_buf(),
                source: src_text,
                id,
            };
            (id, source)
        })
        .collect()
}

fn parse_file(file: &FileSource) -> CompResult<PackageCollection<UntypedCst>> {
    parser::parse(file)
}

pub fn parse(filectx: &FileCtx) -> CompResult<Parse> {
    let mut program = PackageCollection::default();
    for file in filectx.values() {
        let collection = parse_file(file)?;
        program = program.merge(collection)
    }

    Ok(Parse { program })
}

pub fn build(parse: Parse) -> CompResult<Build> {
    let env = EnvironmentBuilder::<UntypedCst>::build(parse.program)?;
    Ok(Build { env })
}

pub fn resolve(build: Build) -> CompResult<Resolve> {
    let resolver = Resolver::new(build.env);
    let env = resolver.analyze()?;
    Ok(Resolve { env })
}

pub fn typecheck(resolve: Resolve) -> CompResult<Typecheck> {
    let tc = Typechecker::new(resolve.env);
    let (items, source) = tc.check()?;
    // for item in &items {
    //     println!("#{}:\n{}\n------------", item.0.0, item.1.typed_ast)
    // }
    Ok(Typecheck { items, source })
}

pub fn lower(tc: Typecheck) -> CompResult<Lower> {
    let lowerer = Lowerer::new();
    let ir = lowerer.lower(tc.source, &tc.items);
    // Sanity check
    Ok(Lower { ir })
}

pub fn simplify(lower: Lower) -> CompResult<Simplify> {
    let ir = simplify::simplify(lower.ir);
    Ok(Simplify { ir })
}

pub fn monomorph(simplify: Simplify) -> CompResult<Monomorph> {
    let ir = monomorph::monomorph(simplify.ir);
    // Sanity check
    // debug_assert!(ir.iter().all(|ir| matches!(ir.type_of(), _)));
    // let ir = self.op.ir;
    Ok(Monomorph { ir })
}

pub fn reduce(ir: Monomorph) -> CompResult<Reduce> {
    let ir = reduce::reduce(ir.ir);
    Ok(Reduce { ir })
}

pub fn convert(ir: Reduce) -> CompResult<Convert> {
    let cc = closure_convert(ir.ir);
    Ok(Convert { cc })
}

pub fn generate<T: Target>(converted: Convert, target: T) -> CompResult<T::Output> {
    let g = Generator::new(target, converted.cc);
    let output = g.generate();
    Ok(output)
}
