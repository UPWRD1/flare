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
    // clippy::min_ident_chars,
    )]
#[warn(
    clippy::large_stack_frames,
    // clippy::panic,
    clippy::dbg_macro,
    clippy::unwrap_used,
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
use salsa::{Accumulator, tracked};

use crate::{
    passes::{
        backend::{
            lir::{ClosureConvertOut, closure_convert},
            target::{Generator, Target},
        },
        frontend::{
            environment::{self, Environment},
            parser,
            resolution::Resolver,
            typechecker::Typechecker,
            typing::{ItemSource, Type, TypesOutput},
        },
        //backend::{flatten::Flattener, gen::Generator},
        midend::{lowering::Lowerer, monomorph, reduce, simplify},
    },
    resource::{
        errors::CompResult,
        rep::{
            frontend::{
                ast::{
                    ItemId,
                    Package,
                    Program,
                    // Untyped, // Untyped
                },
                files::{FileID, FileSource},
            },
            midend::ir::IR,
        },
    },
};

// #[derive(Debug)]
pub struct Init;

#[tracked]
pub struct Parse<'db> {
    program: Program<'db>,
}
#[tracked]
pub struct Build<'db> {
    env: Environment<'db>,
}
#[tracked]
pub struct Resolve<'db> {
    order: Vec<NodeIndex>,
    env: Environment<'db>,
}
#[tracked]
pub struct Typecheck<'db> {
    items: Vec<(ItemId, TypesOutput<'db>)>,
    source: ItemSource,
}
#[tracked]
pub struct Lower<'db> {
    ir: Vec<IR>,
}
#[tracked]
pub struct Simplify<'db> {
    ir: Vec<IR>,
}
#[tracked]
pub struct Reduce<'db> {
    ir: Vec<IR>,
}
#[tracked]
pub struct Monomorph<'db> {
    ir: Vec<IR>,
}
#[tracked]
pub struct Convert<'db> {
    converted: Vec<ClosureConvertOut>,
}

pub struct Generate<T: Target> {
    output: T::Output,
}

#[salsa::tracked]
pub struct FileCtx<'db> {
    cache: FxHashMap<FileID, FileSource>,
}

#[salsa::db]
struct Db {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for Db {}

// pub type CtxResult<const N: usize, T, O> = Result<Context<N, T, O>, CtxErr>;

/// The context/state machine for compiling a Flare bundle.
// pub struct Context<const N: usize, T, O> {
//     pub filectx: FileCtx<'db>,
//     pub target: T,
//     pub intrinsics: [(&'static str, &'static [Untyped], Type); N],
//     pub op: O,
// }

pub fn make_filectx<'db>(db: &'db dyn salsa::Database, src_paths: Vec<PathBuf>) -> FileCtx<'db> {
    FileCtx::new(
        db,
        src_paths
            .into_iter()
            .map(|filepath| {
                let id = convert_path_to_id(&filepath);

                let src_text = std::fs::read_to_string(&filepath).unwrap();

                let source = FileSource::new(db, filepath, src_text);
                (id, source)
            })
            .collect(),
    )
}

fn parse_file<'db>(
    db: &'db dyn salsa::Database,
    ctx: FileCtx<'db>,
    id: FileID,
) -> Vec<Package<'db>> {
    match parser::parse(db, ctx, id) {
        Ok(v) => v,
        Err(de) => {
            de.accumulate(db);
            panic!("")
        }
    }
}

#[tracked]
pub fn parse<'db>(db: &'db dyn salsa::Database, filectx: FileCtx<'db>) -> Parse<'db> {
    let mut processed: Vec<(Vec<Package>, FileID)> = vec![];
    for id in filectx.cache(db).keys() {
        let pack = parse_file(db, filectx, *id);
        processed.push((pack, *id))
    }

    let packages: Vec<_> = processed
        .into_iter()
        .flat_map(|(packages, id)| {
            packages
                .into_iter()
                .map(|p| (p, id))
                .collect::<Vec<(Package, FileID)>>()
        })
        .collect::<Vec<_>>();

    let program = Program::new(db, packages);
    Parse::new(db, program)
}

#[tracked]
pub fn build<'db>(db: &'db dyn salsa::Database, filectx: FileCtx<'db>) -> Build<'db> {
    let program = parse(db, filectx);
    let env = environment::build(db, program.program(db));
    Build::new(db, env)
}

#[tracked]
pub fn resolve<'db>(db: &'db dyn salsa::Database, filectx: FileCtx<'db>) -> Resolve<'db> {
    let build = build(db, filectx);
    let env = build.env(db);
    let mut resolver = Resolver::new(db, env);
    let order = resolver.build()?;
    let env = resolver.finish()?;
    Resolve::new(db, order, env)
}

#[tracked]
pub fn typecheck<'db>(db: &'db dyn salsa::Database, filectx: FileCtx<'db>) -> Typecheck<'db> {
    let resolved = resolve(db, filectx);
    let order = resolved.order(db);
    let env = resolved.env(db);
    let tc = Typechecker::new(order.leak(), env);
    let (items, source) = tc.check()?;
    Typecheck::new(db, items, source)
}

#[tracked]
pub fn lower<'db>(db: &'db dyn salsa::Database) -> Lower {
    let lowerer = Lowerer::new();
    let ir = lowerer.lower(self.op.source, &self.op.items);
    Lower { ir }
}
#[tracked]
pub fn simplify<'db>(db: &'db dyn salsa::Database) -> Simplify {
    let ir = simplify::simplify(self.op.ir);
    Simplify { ir }
}
#[tracked]
pub fn monomorph<'db>(db: &'db dyn salsa::Database) -> Monomorph {
    let ir = monomorph::monomorph(self.op.ir);
    // Sanity check
    debug_assert!(ir.iter().all(|ir| matches!(ir.type_of(), _)));

    Monomorph { ir }
}
#[tracked]
pub fn reduce<'db>(db: &'db dyn salsa::Database) -> Reduce {
    let ir = reduce::reduce(self.op.ir);
    Reduce { ir }
}
#[tracked]
pub fn convert(db: &'db dyn salsa::Database) -> Convert {
    let converted = closure_convert(self.op.ir);
    Convert { converted }
}
pub fn generate(self) -> Generate<T> {
    let g = Generator::new(self.target.clone(), self.op.converted);

    let output = g.generate();
    Generate { output }
}

pub fn finish(self) -> CompResult<Vec<u8>> {
    Ok((self.op.output).into())
}

pub fn convert_path_to_id(path: &Path) -> FileID {
    let mut hasher = FxHasher::default();
    path.hash(&mut hasher);
    //path.canonicalize().unwrap().hash(&mut hasher);
    hasher.finish()
}
