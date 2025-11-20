use chumsky::span::SimpleSpan;
use internment::Intern;

use crate::resource::{
    errors::CompResult,
    rep::{
        ast::{Expr, Variable},
        Spanned,
    },
};

pub trait SpanWrapped {
    fn get_span(&self) -> SimpleSpan<usize, u64>;
    // where
    // Self: SpanWrapped;
}

pub trait Ident {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>>;
}

/// Trait for entities that have Names. Implementing this trait is preferred
/// over a custom name implementation. Currently the only major type that
/// implements its own name getter is `QualifierFragment`, since it doesn't
/// carry span information (since it is statically created within the compiler)
pub trait Named<V: Variable> {
    // #[clippy::deny()]
    /// Internal get_name that returns a name or `None`. Users should implement this function, but shouldn't call it.
    fn get_name(&self) -> Option<Spanned<Intern<Expr<V>>>>;

    fn name(&self) -> CompResult<Spanned<Intern<Expr<V>>>>
// where
        // Self: std::fmt::Debug,
    {
        let n = self.get_name();
        match n {
            Some(d) => Ok(d),
            None => todo!("Cannot get name"),
            // None => DynamicErr::new(format!("Cannot get name of {:?}", self))
            // .label("here", self.to_owned()),
        }
    }
}

// impl fmt::Display for dyn Named {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "{}", self.name().unwrap().ident().unwrap())
//     }
// }
