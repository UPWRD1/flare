use std::fmt::Display;

use internment::Intern;
use tiny_pretty::Doc;

use crate::resource::{
    pretty::{DocExt, Render},
    rep::{
        common::{Spanned, Variable},
        frontend::ast::{Expr, Label, Untyped},
    },
};

impl Render for Untyped {
    fn render(self) -> Doc<'static> {
        Doc::text(self.0.0.to_string())
    }
}

impl Render for Label {
    fn render(self) -> Doc<'static> {
        Doc::text(self.0.0.to_string())
    }
}
impl<V: Variable + Render> Render for Spanned<Intern<Expr<V>>> {
    fn render(self) -> tiny_pretty::Doc<'static> {
        match *self.0 {
            Expr::Ident(v) => v.render(),
            Expr::Number(n) => Doc::text(format!("{n}")),
            Expr::String(s) => Doc::text(format!("\"{}\"", s.0)),
            Expr::Bool(b) => Doc::text(format!("{b}")),
            Expr::Unit => Doc::text("Unit"),
            Expr::Particle(p) => Doc::text(format!("@{}", p.0)),
            Expr::Hole(_) => Doc::text("HOLE"),
            Expr::Item(item_id, _) => Doc::text(format!("#{}", item_id.0)),
            Expr::Concat(l, r) => l.render().text(" <> ").render(r).brackets(),
            Expr::Project(d, v) => v.render().text(" ~> ").text(format!("{d:?}")).brackets(),
            Expr::Inject(d, v) => v.render().text(" <~ ").text(format!("{d:?}")).brackets(),
            Expr::Branch(l, r) => l.render().text(" ?? ").render(r),
            Expr::Label(label, v) => label.render().text(":").space().render(v).braces(),
            Expr::Unlabel(v, label) => label.render().text("^").render(v),
            Expr::Mul(l, r) => l.render().space().text("*").space().render(r),
            Expr::Div(l, r) => l.render().space().text("/").space().render(r),
            Expr::Add(l, r) => l.render().space().text("+").space().render(r),
            Expr::Sub(l, r) => l.render().space().text("-").space().render(r),
            Expr::Comparison(l, bin_op, spanned1) => todo!(),
            Expr::Call(f, e) => f.render().append(e.render().parens()),
            Expr::If(cond, then, other) => Doc::text("if")
                .space()
                .render(cond)
                .text("then")
                .hard_line()
                .nest(4)
                .append(then.render())
                .hard_line()
                .text("else")
                .hard_line()
                .nest(4)
                .append(other.render()),
            Expr::Lambda(v, body) => Doc::text("fn")
                .space()
                .render(v)
                .space()
                .text("=>")
                .hard_line()
                .nest(4)
                .render(body),
            Expr::Let(v, def, body) => Doc::text("let")
                .space()
                .render(v)
                .space()
                .text("=")
                .hard_line()
                .nest(4)
                .append(def.render())
                .hard_line()
                .text("in")
                .hard_line()
                .nest(4)
                .append(body.render().hard_line().nest(4)),
            Expr::Access(l, r) => l.render().text(".").text(r.0.0),
        }
    }
}

impl<V: Variable + Render> Display for Spanned<Intern<Expr<V>>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let doc = self.render();
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
