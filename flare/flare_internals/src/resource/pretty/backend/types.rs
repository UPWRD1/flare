use itertools::Itertools;
use tiny_pretty::Doc;

use crate::resource::{
    pretty::{DocExt, Render},
    rep::backend::types::LIRType,
};

impl Render for LIRType {
    fn render(self) -> Doc<'static> {
        // dbg!(&self);
        match self {
            Self::Int => Doc::text("i32"),
            Self::Float => Doc::text("f64"),
            Self::String => Doc::text("str"),
            Self::Unit => Doc::text("unit"),
            Self::Closure(l, r) => l.render().space().text("->").space().render(*r).brackets(),
            Self::ClosureEnv(c, params) => Doc::text("|")
                .append(
                    Doc::list(
                        params
                            .iter()
                            .map(|p| p.render())
                            .intersperse(Doc::text(", "))
                            .collect(),
                    )
                    .text("|"),
                )
                .render(*c),
            Self::Array(v) => Doc::list(
                v.iter()
                    .map(|el| el.render())
                    .intersperse(Doc::text(","))
                    .collect(),
            )
            .braces(),

            Self::Union(v) => Doc::list(
                v.iter()
                    .map(|p| p.render())
                    .intersperse(Doc::text("|"))
                    .collect(),
            )
            .braces(),
        }
    }
}
