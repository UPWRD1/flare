use std::fmt::Display;

use itertools::Itertools;
use tiny_pretty::Doc;

use crate::resource::{
    pretty::{DocExt, INC, Render},
    rep::midend::irtype::{IRType, Row, TyApp},
};

impl Render for IRType {
    fn render(self) -> Doc<'static> {
        match self {
            Self::Num => Doc::text("num"),
            Self::Unit => Doc::text("unit"),
            Self::Str => Doc::text("str"),
            Self::Bool => Doc::text("bool"),
            Self::Particle(intern) => Doc::text(format!("@{intern}")),
            Self::Var(type_var) => Doc::text(format!("?{}", type_var.0)),
            Self::Fun(l, r) => l
                .render()
                .append(Doc::space().append(Doc::text("->").append(Doc::space())))
                .append(r.render()),
            Self::TyFun(kind, t) => Doc::text(format!("TyFunc {kind:?} ")).append(t.render()),
            Self::Prod(row) => row.render("*"),
            Self::Sum(row) => row.render("|"),
            Self::Volatile(v) => v.render().text("!"),
        }
    }
}

impl Display for IRType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let doc = self.clone().render();
        write!(
            f,
            "{}",
            tiny_pretty::print(
                &doc,
                &tiny_pretty::PrintOptions {
                    width: 80,
                    ..Default::default()
                }
            )
        )
    }
}

impl Render for TyApp {
    fn render(self) -> Doc<'static> {
        match self {
            Self::Ty(t) => t.render(),
            Self::Row(row) => row.render(", "),
        }
    }
}

impl Row {
    pub fn render(self, infix: impl Into<String>) -> Doc<'static> {
        let infix = infix.into();
        match self {
            Self::Open(o) => Doc::text(format!("%{}", o.0)),
            Self::Closed(c) => {
                if c.is_empty() {
                    Doc::text("{}")
                } else if c.len() <= 3 {
                    Doc::nil()
                        .append(Doc::text("{"))
                        .append(
                            Doc::list(
                                Itertools::intersperse(
                                    c.into_iter().map(|x| x.render().nest(INC)),
                                    Doc::space()
                                        .append(Doc::text(infix).append(Doc::line_or_space())),
                                )
                                .collect(),
                            )
                            .group()
                            .nest(INC),
                        )
                        .append(Doc::text("}").nest(INC))
                } else {
                    Doc::nil()
                        .append(Doc::text("{"))
                        .append(
                            Doc::hard_line()
                                .append(Doc::list(
                                    Itertools::intersperse(
                                        c.into_iter().map(|x| x.render().nest(INC)),
                                        Doc::space()
                                            .append(Doc::text(infix).append(Doc::line_or_space())),
                                    )
                                    .collect(),
                                ))
                                .group()
                                .nest(INC),
                        )
                        .append(Doc::hard_line().append(Doc::text("}")))
                }
            }
        }
    }
}

impl std::fmt::Display for TyApp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ty(t) => write!(f, "{}", t),
            Self::Row(row) => todo!(),
        }
    }
}
