use itertools::Itertools;
use tiny_pretty::Doc;

use crate::{
    passes::backend::lowering::ir::{IR, TyApp, Var},
    resource::pretty::{DocExt, INC, Render},
};

impl IR {
    fn collect_fun_vars(&self, vars: &mut Vec<Var>) -> Self {
        if let Self::Fun(var, body) = self {
            vars.push(var.clone());
            body.collect_fun_vars(vars)
        } else {
            self.clone()
        }
    }

    fn collect_app_args(self, args: &mut Vec<Self>) -> Self {
        // dbg!(&self);
        if let Self::App(fun, arg) = self {
            args.push(*arg);
            fun.collect_app_args(args)
        } else {
            args.reverse();
            self
        }
    }
}

impl Var {
    fn render_n(self) -> Doc<'static> {
        Doc::text(format!("${}", self.id.0))
    }
}

impl Render for Var {
    fn render(self) -> Doc<'static> {
        Doc::text(format!("${}:", self.id.0))
            .append(Doc::space())
            .append(self.ty.render())
    }
}

impl Render for IR {
    fn render(self) -> Doc<'static> {
        // dbg!(level);
        match self {
            Self::Var(var) => var.render_n(),
            Self::Num(ordered_float) => Doc::text(format!("{ordered_float}")),
            Self::Str(intern) => Doc::text(format!("\"{intern}\"")),
            Self::Bool(b) => Doc::text(format!("{b}")),
            Self::Unit => Doc::text("unit"),
            Self::Fun(v, b) => {
                let mut vars = vec![v.clone()];
                let ir = b.clone().collect_fun_vars(&mut vars);
                Doc::text("fn")
                    .append(Doc::space())
                    .append(
                        Doc::list(
                            Itertools::intersperse(
                                vars.into_iter().map(Render::render),
                                Doc::text(",").space(),
                            )
                            .collect(),
                        )
                        .brackets(),
                    )
                    .space()
                    .text("=>") // .append(Doc::)
                    .group()
                    .append(Doc::line_or_space().append(ir.render()).group().nest(INC))
            }

            Self::App(fun, r) => {
                let mut v = vec![*r];
                let fun = fun.collect_app_args(&mut v);

                fun.render().append(
                    Doc::line_or_nil()
                        .append(Doc::list(
                            Itertools::intersperse(
                                v.into_iter().map(Render::render),
                                Doc::text(",").space(),
                            )
                            .collect(),
                        ))
                        .parens(),
                )
            }
            Self::TyApp(t, k) => Doc::nil()
                .append(match k {
                    TyApp::Ty(t) => t.render(),
                    TyApp::Row(row) => row.render(","),
                })
                .brackets()
                .append(Doc::text("::"))
                .render(*t),

            Self::TyFun(k, b) => Doc::text("tyfn")
                .space()
                .text(format!("{k:?}"))
                .space()
                .text("=>")
                .group()
                .append(Doc::line_or_space().append(b.render()).group().nest(INC)),

            Self::Local(v, b, i) => Doc::nil()
                .append(
                    Doc::text("let")
                        .space()
                        .render(v)
                        .space()
                        .append(Doc::text("="))
                        .group(),
                )
                .append(
                    Doc::soft_line().append(b.render()).nest(INC), // .nest(level),
                )
                .append(Doc::line_or_space().append(Doc::text("in")))
                .append(Doc::hard_line().append(i.render()).nest(INC)),

            Self::If(cond, then, other) => Doc::text("if")
                .space()
                .render(*cond)
                .space()
                .text("then")
                .append(Doc::nil().hard_line().render(*then).nest(INC).group())
                .hard_line()
                .text("else")
                .append(Doc::nil().hard_line().render(*other).nest(INC)),

            Self::Bin(l, op, r) => l.render().text(format!("{op}")).render(*r),

            Self::Tuple(v) => {
                if v.is_empty() {
                    Doc::nil().braces()
                } else {
                    Doc::list(
                        Itertools::intersperse(
                            v.into_iter()
                                .map(|x| Doc::hard_line().append(x.render()).nest(INC)),
                            Doc::text(","),
                        )
                        .collect(),
                    )
                    .group()
                    .braces()
                }
            }
            Self::Case(_, b, v) => Doc::text("match")
                .space()
                .append(b.render())
                .text(":")
                .append(
                    Doc::list(
                        Itertools::intersperse(
                            v.into_iter()
                                .map(|x| Doc::hard_line().append(x.render()).nest(INC)),
                            Doc::text(","),
                        )
                        .collect(),
                    )
                    .group(),
                ),
            Self::Field(k, s) => Doc::nil()
                .append(k.render())
                .append(Doc::text("."))
                // .append(Doc::space())
                .append(Doc::text(format!("{s}"))),
            Self::Tag(t, i, b) => Doc::nil()
                .append(b.render())
                .space()
                .append(Doc::text("as"))
                .space()
                .append(t.render())
                .append(Doc::text(" variant "))
                .append(Doc::text(format!("{i}"))),
            Self::Particle(p) => Doc::text(format!("@{p}")),
            Self::Item(_, id) => Doc::text(format!("#{}", id.0)),
            // .append(Doc::space())
            // .append(t.render()),
            Self::Extern(n, _) => Doc::text(format!("extern_{n}")),
        }
    }
}
