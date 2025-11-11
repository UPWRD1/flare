use chumsky::span::SimpleSpan;
use internment::Intern;

use crate::resource::{
    errors::{self, CompResult},
    rep::{ast::Expr, Spanned},
};

pub trait SpanWrapped {
    fn get_span(&self) -> SimpleSpan<usize, u64>
    where
        Self: SpanWrapped;
}

pub trait Ident: SpanWrapped + Named {
    fn ident(&self) -> CompResult<Intern<String>>;
}

/// Trait for entities that have Names. Implementing this trait is preferred
/// over a custom name implementation. Currently the only major type that
/// implements its own name getter is `QualifierFragment`, since it doesn't
/// carry span information (since it is statically created within the compiler)
pub trait Named {
    // #[clippy::deny()]
    /// Internal get_name that returns a name or `None`. Users should implement this function, but shouldn't call it.
    fn get_name(&self) -> Option<Spanned<Intern<Expr>>>;

    fn name(&self) -> CompResult<Spanned<Intern<Expr>>>
    where
        Self: std::fmt::Debug + SpanWrapped,
    {
        // SAFETY: This is perfectly safe, users shouldn't call it though.
        let n = self.get_name();
        match n {
            Some(d) => Ok(d),
            None => Err(errors::bad_ident(self, self.get_span())),
        }
    }
}
