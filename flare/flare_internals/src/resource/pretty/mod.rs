use std::fmt::Display;

use tiny_pretty::Doc;

pub mod backend;
pub mod frontend;
pub mod midend;

pub const INC: usize = 2;

pub trait DocExt<'a> {
    fn parens(self) -> Self;
    fn brackets(self) -> Self;
    fn braces(self) -> Self;
    fn space(self) -> Self;
    fn text(self, t: impl Display) -> Self;
    fn hard_line(self) -> Self;
    fn render(self, r: impl Render) -> Self;
    fn comma(self) -> Self;
}

impl<'a> DocExt<'a> for Doc<'a> {
    fn parens(self) -> Self {
        Self::text("(").append(self).text(")")
    }

    fn brackets(self) -> Self {
        Self::text("[").append(self).append(Self::text("]"))
    }

    fn braces(self) -> Self {
        Self::text("{").append(self).append(Self::text("}"))
    }

    fn space(self) -> Self {
        self.append(Self::space())
    }

    fn text(self, t: impl std::fmt::Display) -> Self {
        use std::borrow::Cow;
        let t: String = t.to_string();
        self.append(Self::text(Cow::from(t)))
    }
    fn hard_line(self) -> Self {
        self.append(Self::hard_line())
    }

    fn render(self, r: impl Render) -> Self {
        self.append(r.render())
    }

    fn comma(self) -> Self {
        self.text(",")
    }
}

pub trait Render {
    fn render(self) -> Doc<'static>;
}
