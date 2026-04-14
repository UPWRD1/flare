use std::fmt::{self, Display};

use internment::Intern;
use itertools::Itertools;
use tiny_pretty::Doc;

use crate::{
    passes::frontend::typing::{Row, Type, Typed},
    resource::{
        pretty::{DocExt, Render},
        rep::{
            common::{Spanned, Variable},
            frontend::ast::{BinOp, Expr, Label, Untyped},
        },
    },
};

impl Render for Untyped {
    fn render(self) -> Doc<'static> {
        Doc::text(self.0.0.to_string())
    }
}

impl Render for Typed {
    fn render(self) -> Doc<'static> {
        Doc::text(self.0.0.0.to_string())
        // Doc::text(self.0.0.0.to_string())
        //     .text(":")
        //     .space()
        //     .render(self.1)
    }
}

impl Render for Label {
    fn render(self) -> Doc<'static> {
        Doc::text(self.0.0.to_string())
    }
}

impl Render for Spanned<Intern<Row>> {
    fn render(self) -> Doc<'static> {
        match *self.0 {
            Row::Closed(c) => Doc::list(
                c.fields
                    .iter()
                    .zip(c.values)
                    .map(|(f, v)| f.render().text(":").space().render(*v))
                    .intersperse(Doc::text(", "))
                    .collect(),
            ),

            _ => todo!(),
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Eq => write!(f, "=="),
            BinOp::Neq => write!(f, "!="),
            BinOp::Gt => write!(f, ">"),
            BinOp::Lt => write!(f, "<"),
            BinOp::Gte => write!(f, ">="),
            BinOp::Lte => write!(f, "<="),
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
        }
    }
}

impl Render for BinOp {
    fn render(self) -> Doc<'static> {
        Doc::text(format!("{}", self))
    }
}

impl Render for Spanned<Intern<Type>> {
    fn render(self) -> Doc<'static> {
        match *self.0 {
            Type::Unifier(ty_uni_var) => todo!(),
            Type::Var(v) => Doc::text(format!("?{}", v.0)),
            Type::Num => Doc::text("num"),
            Type::Unit => Doc::text("unit"),
            Type::String => Doc::text("str"),
            Type::Bool => Doc::text("bool"),
            Type::Particle(p) => Doc::text(format!("@{}", p.0)),
            Type::Func(l, r) => l
                .render()
                .append(Doc::space().append(Doc::text("->").append(Doc::space())))
                .append(r.render()),
            Type::TypeFun(_, _) => todo!(),
            Type::Prod(r) => r.render().braces(),
            Type::Sum(r) => Doc::text("|").render(r).text("|"),
            Type::Label(_, _) => todo!(),
            Type::Recursive(v, p, t) => {
                todo!()
            }
            Type::Hole => todo!(),
        }
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
            Expr::Comparison(l, bin_op, r) => l.render().space().render(bin_op).space().render(r),
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
