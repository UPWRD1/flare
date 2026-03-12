use crate::resource::{
    errors::CompResult,
    rep::frontend::cst::{Program, UntypedCst},
};

#[derive(Debug)]
/// The main environment graph structure. Holds all the objects produced by
/// the  parser, and the index of the root.
///
/// Obviously, `Environment` does not implement `Copy`, but it also does not
/// implement `Clone`, since it is ridiculously expensive, and there is no
/// real reason to clone the environment.
#[non_exhaustive]
pub struct Environment {}

impl Environment {
    /// Build the environment from a given `Program`
    /// # Errors
    /// - on invalid names,
    ///
    pub fn build(_: &Program<UntypedCst>) -> CompResult<Self> {
        loop {}
    }
}
