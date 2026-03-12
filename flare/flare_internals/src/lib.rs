pub mod passes;
pub mod resource;
use rustc_hash::FxHashMap;

use crate::{
    passes::{
        frontend::resolution::Resolver,
        //backend::{flatten::Flattener, gen::Generator},
    },
    resource::{
        errors::{CompResult, CompilerErr},
        rep::frontend::files::{FileID, FileSource},
    },
};

pub struct Resolve {}
pub struct Typecheck {}

pub type FileCtx = FxHashMap<FileID, FileSource>;

// pub type CtxResult<const N: usize, T, O> = Result<Context<N, T, O>, CtxErr>;

/// The context/state machine for compiling a Flare bundle.
pub struct Context<T, O> {
    pub filectx: FileCtx,
    pub target: T,
    pub op: O,
}

pub fn resolve() -> CompResult<Resolve> {
    let resolver = Resolver::new();
    let (_env, _order) = resolver.analyze()?;
    Ok(Resolve {})
}
