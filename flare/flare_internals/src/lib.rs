pub mod passes;
pub mod resource;

use crate::passes::frontend::resolution::{Container, Inspector};

pub struct Resolve {}
pub struct Typecheck {}

// pub type FileCtx = FxHashMap<FileID, FileSource>;

// pub type CtxResult<const N: usize, T, O> = Result<Context<N, T, O>, CtxErr>;

/// The context/state machine for compiling a Flare bundle.
pub struct Context<T, O> {
    // pub filectx: FileCtx,
    pub target: T,
    pub op: O,
}

pub fn resolve() -> Result<Resolve, ()> {
    let resolver = Inspector {};
    resolver.inspect_container(Container { f1: (), f2: () });
    Ok(Resolve {})
}
