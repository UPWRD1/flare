mod helper {
    use chumsky::prelude::*;
    use flare_internals::resource::rep::{Spanned, ast::NodeId};
    use internment::Intern;
    pub struct SpanGenerator {
        next: usize,
    }
    impl SpanGenerator {
        pub fn new() -> Self {
            Self { next: 0 }
        }
        pub fn fresh(&mut self) -> NodeId {
            // self.next += 1;
            SimpleSpan::new(0u64, self.next..self.next)
        }

        pub fn with<T>(&mut self, item: T) -> Spanned<Intern<T>>
        where
            T: Into<Intern<T>>,
        {
            Spanned(item.into(), self.fresh())
        }

        pub fn build<T, U>(&mut self, f: impl Fn(&mut Self) -> U) -> Spanned<Intern<T>>
        where
            U: Into<Intern<T>>,
        {
            Spanned(f(self).into(), self.fresh())
        }
    }
}

#[cfg(test)]
mod tests {
    use flare_internals::passes::midend::{resolution::subst_generic_type, typing::Type};

    use crate::helper::SpanGenerator;

    use super::*;

    #[test]
    fn subst_ty() {
        let mut g = SpanGenerator::new();
        let t = g.build(|g| {
            Type::Func(
                g.build(|g| Type::Generic(g.with("T".to_string()))),
                g.build(|g| Type::Generic(g.with("T".to_string()))),
            )
        });

        let comp = g.build(|g| Type::Func(g.with(Type::String), g.with(Type::String)));
        let sub = subst_generic_type(
            t,
            Type::Generic(g.with("T".to_string())).into(),
            Type::String.into(),
        );
        assert_eq!(sub, comp)
    }
}
