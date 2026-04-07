mod helper {
    use chumsky::prelude::*;
    use flare_internals::resource::rep::common::{FlareSpan, Spanned};
    use internment::Intern;
    pub struct SpanGenerator {
        next: usize,
    }
    impl SpanGenerator {
        pub fn new() -> Self {
            Self { next: 0 }
        }
        pub fn fresh(&mut self) -> FlareSpan {
            // self.next += 1;
            FlareSpan(self.next, self.next, 0u64)
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
    use flare_internals::{
        passes::frontend::resolution::subst_generic_type,
        resource::rep::frontend::csttypes::CstType,
    };

    use crate::helper::SpanGenerator;

    use super::*;

    #[test]
    fn subst_ty() {
        let mut g = SpanGenerator::new();
        let t = g.build(|g| {
            CstType::Func(
                g.build(|g| CstType::Generic(g.with("T".to_string()))),
                g.build(|g| CstType::Generic(g.with("T".to_string()))),
            )
        });

        let comp = g.build(|g| CstType::Func(g.with(CstType::String), g.with(CstType::String)));
        let sub = subst_generic_type(
            t,
            CstType::Generic(g.with("T".to_string())).into(),
            CstType::String.into(),
        );
        assert_eq!(sub, comp)
    }
}
