use tiny_pretty::Doc;


pub mod frontend;
pub mod backend;
pub mod midend;

pub const INC: usize = 2;

pub trait DocExt<'a> {
    fn parens(self) -> Self;
    fn brackets(self) -> Self;
    fn braces(self) -> Self;
    fn space(self) -> Self;
    fn text(self, t: impl Into<std::borrow::Cow<'a, str>>) -> Self;
    fn hard_line(self) -> Self;
    fn render(self, r: impl Render) -> Self;
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

    fn text(self, t: impl Into<std::borrow::Cow<'a, str>>) -> Self {
        self.append(Self::text(t.into()))
    }
    fn hard_line(self) -> Self {
        self.append(Self::hard_line())
    }

    fn render(self, r: impl Render) -> Self {
        self.append(r.render())
    }
}

pub trait Render {
    fn render(self) -> Doc<'static>;
}
