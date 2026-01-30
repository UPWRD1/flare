use std::fmt::Display;

use ::itertools::Itertools;

use tiny_pretty::Doc;

use crate::resource::{
    pretty::{DocExt, Render},
    rep::backend::lir::{LIR, Var},
};

impl Render for Var {
    fn render(self) -> Doc<'static> {
        Doc::text(format!("${}", self.id.0))
    }
}

impl Render for LIR {
    fn render(self) -> Doc<'static> {
        match self {
            Self::Var(var) => Doc::text(format!("${}", var.id.0)),
            Self::Int(i) => Doc::text(format!("{i}i")),
            Self::Str(s) => Doc::text(format!("{s}")),
            Self::Unit => Doc::text("unit".to_string()),
            Self::Float(f) => Doc::text(format!("{f}f")),
            Self::Closure(t, item_id, vars) => Doc::text("closure")
                .space()
                .text(format!("{}", item_id.0))
                .append(
                    Doc::list(
                        vars.iter()
                            .map(|x| {
                                Doc::text(format!("${}: ", x.id.0,)).append(x.ty.clone().render())
                            })
                            .intersperse(Doc::text(", "))
                            .collect(),
                    )
                    .brackets(),
                ),
            Self::Apply(ir, ir1) => ir.render().append(ir1.render().parens()),
            Self::BulkApply(fun, args) => fun.render().append(
                Doc::list(
                    args.into_iter()
                        .map(|arg| arg.render())
                        .intersperse(Doc::text(","))
                        .collect(),
                )
                .parens(),
            ),
            Self::Local(var, d, b) => Doc::text("let")
                .space()
                .render(var)
                .space()
                .text("=")
                .space()
                .render(*d)
                .space()
                .text("in")
                .space()
                // .hard_line()
                .render(*b),
            Self::Access(ir, i) => ir.render().text(format!("^{i}")),
            Self::Struct(v) => Doc::list(
                v.into_iter()
                    .map(Render::render)
                    .intersperse(Doc::text(","))
                    .collect(),
            )
            .braces(),
            Self::Field(ir, u) => ir.render().append(Doc::text(u.to_string()).brackets()),
            Self::Item(id) => Doc::text(format!("#{}", id.0)),
            Self::Extern(n) => Doc::text(format!("extern_{}", n)),
            Self::BinOp(l, op, r) => l.render().space().text(format!("{op}")).space().render(*r),
            Self::Case(scrutinee, branches) => Doc::text("case").space().render(*scrutinee).append(
                Doc::list(
                    branches
                        .into_iter()
                        .map(Render::render)
                        .intersperse(Doc::text(","))
                        .collect(),
                )
                .nest(2)
                .braces(),
            ),
        }
    }
}
impl Display for LIR {
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
