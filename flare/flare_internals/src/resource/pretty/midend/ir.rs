use std::fmt::Display;

use itertools::Itertools;
use tiny_pretty::Doc;

use crate::resource::{
    pretty::{DocExt, INC, Render}, rep::midend::{ir::{Branch, IR, Var}, irtype::{IRType, Row, TyApp}},
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
        Doc::text(format!("${}:", self.id.0)).append(self.ty.render())
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
            // Self::Comment(s, r) => Doc::text(format!("//{s}")).hard_line().render(*r),
            Self::App(fun, r) => {
                let mut v = vec![*r];
                let fun = fun.collect_app_args(&mut v);

                fun.render().brackets().append(
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
            Self::Case(_, scrutinee, branches) => Doc::text("match")
                .space()
                .append(scrutinee.render())
                .text(":")
                .append(
                    Doc::list(
                        Itertools::intersperse(
                            branches
                                .into_iter()
                                .map(|branch| Doc::hard_line().append(branch.render()).nest(INC)),
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
                .append(Doc::text("become"))
                .space()
                .append({
                    if let IRType::Sum(Row::Closed(ref r)) = t {
                        r[i].clone().render()
                    } else {
                        panic!("non-row")
                    }
                })
                .space()
                .text("in")
                .space()
                .render(t),
            Self::Particle(p) => Doc::text(format!("@{p}")),
            Self::Item(_t, id) => Doc::text(format!("#{}", id.0)),
            // .append(Doc::space())
            // .append(t.render()),
            Self::Extern(n, _) => Doc::text(format!("extern_{n}")),
        }
    }
}
impl Render for Branch {
    fn render(self) -> Doc<'static> {
        Doc::text("|")
            .append(Doc::space())
            .append(self.param.render())
            .append(Doc::space())
            .append(Doc::text("then"))
            .append(Doc::soft_line())
            .append(self.body.render())
            .nest(INC)
    }
}

impl Display for IR {
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
