use std::fmt::Display;

use internment::Intern;
use ordered_float::OrderedFloat;

use crate::{passes::backend::target::Target, resource::rep::ast::BinOp};
use itertools::Itertools;
use tiny_pretty::Doc;

const INC: usize = 2;

#[derive(Clone, Copy)]
pub struct IRTarget;

impl Target for IRTarget {
    type Partial = IR;

    type Output = String;
    type Input = IR;

    fn generate(&mut self, ir: IR) -> Self::Partial {
        ir
    }

    fn finish(self, p: Vec<Self::Partial>) -> Self::Output {
        p.into_iter()
            .enumerate()
            .map(|(i, x)| format!("item #{i}: is\n{x}\nend item #{i}"))
            .collect::<Vec<String>>()
            .join("\n\n")
    }
    fn ext(&self) -> impl Into<String> {
        "ir"
    }
    fn convert(ir: Vec<(IR, Type)>) -> Vec<Self::Input> {
        ir.into_iter().map(|(ir, _)| ir).collect()
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub struct TypeVar(pub usize);

#[derive(PartialEq, Eq, PartialOrd, Clone, Debug, Hash, Default)]
pub enum Type {
    Num,
    #[default]
    Unit,
    Str,
    Bool,
    Particle(Intern<String>),

    Var(TypeVar),

    Fun(Box<Self>, Box<Self>),
    TyFun(Kind, Box<Self>),

    Prod(Row),
    Sum(Row),
}

impl Type {
    #[must_use]
    pub fn is_cheap_alloc(&self) -> bool {
        // Technically, unit is zero-sized, so it doesn't alloc
        matches!(self, Self::Num | Self::Bool | Self::Str | Self::Unit)
    }
}

impl Render for Type {
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
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Clone, Debug, Hash)]
pub enum Row {
    Open(TypeVar),
    Closed(Vec<Type>),
}

impl Row {
    fn render(self, infix: impl Into<String>) -> Doc<'static> {
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

impl Type {
    #[must_use]
    pub fn fun(l: Self, r: Self) -> Self {
        Self::Fun(Box::new(l), Box::new(r))
    }
    #[must_use]
    pub fn funs(args: impl Into<Vec<Self>>, ret: Self) -> Self {
        args.into()
            .into_iter()
            .rfold(ret, |ret, arg| Self::Fun(Box::new(arg), Box::new(ret)))
    }

    #[must_use]
    pub fn ty_fun(k: Kind, t: Self) -> Self {
        Self::TyFun(k, Box::new(t))
    }

    #[must_use]
    pub fn prod(row: Row) -> Self {
        match row {
            Row::Closed(elems) if elems.len() == 1 => unsafe {
                elems.into_iter().next().unwrap_unchecked()
            },
            row => Self::Prod(row),
        }
    }

    #[must_use]
    pub fn sum(row: Row) -> Self {
        match row {
            Row::Closed(elems) if elems.len() == 1 => unsafe {
                elems.into_iter().next().unwrap_unchecked()
            },
            row => Self::Sum(row),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash, Default)]
#[repr(transparent)]
pub struct VarId(pub usize);

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub struct ItemId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub id: VarId,
    pub ty: Type,
}

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.id.0)
    }
}

impl Var {
    #[must_use]
    pub fn new(id: VarId, ty: Type) -> Self {
        Self { id, ty }
    }

    #[must_use]
    pub fn map_ty(self, f: impl FnOnce(Type) -> Type) -> Self {
        Self {
            ty: f(self.ty),
            ..self
        }
    }

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kind {
    Type,
    Row,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Branch {
    pub param: Var,
    pub body: IR,
}

impl Branch {
    #[must_use]
    pub fn as_fun(&self) -> IR {
        IR::fun(self.param.clone(), self.body.clone())
    }
}

impl Render for Branch {
    fn render(self) -> Doc<'static> {
        Doc::text("|")
            .append(Doc::space())
            .append(Doc::text(format!("{}", self.param)))
            .append(Doc::space())
            .append(Doc::text("then"))
            .append(Doc::soft_line())
            .append(self.body.render())
            .nest(INC)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyApp {
    Ty(Type),
    Row(Row),
}

impl TyApp {
    pub fn subst_tyapp(self, f: Self) -> Self {
        f
    }
    pub fn is_cheap_alloc(&self) -> bool {
        match self {
            Self::Ty(t) => t.is_cheap_alloc(),
            Self::Row(_) => false,
        }
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

#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub enum IR {
    Var(Var),
    Num(OrderedFloat<f64>),
    Str(Intern<String>),
    Bool(bool),
    #[default]
    Unit,

    Particle(Intern<String>),

    Fun(Var, Box<Self>),
    App(Box<Self>, Box<Self>),

    TyFun(Kind, Box<Self>),
    TyApp(Box<Self>, TyApp),
    Local(Var, Box<Self>, Box<Self>),

    If(Box<Self>, Box<Self>, Box<Self>),

    Bin(Box<Self>, BinOp, Box<Self>),

    Tuple(Vec<Self>),
    Field(Box<Self>, usize),
    Tag(Type, usize, Box<Self>),
    Case(Type, Box<Self>, Vec<Branch>),

    Item(Type, ItemId),
    Extern(&'static str, Type),
}

#[allow(clippy::should_implement_trait)]
impl IR {
    pub fn fun(v: Var, b: Self) -> Self {
        Self::Fun(v, Box::new(b))
    }

    pub fn funs<I>(vars: I, body: Self) -> Self
    where
        I: IntoIterator<Item = Var>,
        I::IntoIter: DoubleEndedIterator,
    {
        vars.into_iter()
            .rfold(body, |body, var| Self::fun(var, body))
    }

    pub fn app(l: Self, r: Self) -> Self {
        Self::App(Box::new(l), Box::new(r))
    }

    pub fn ty_fun(k: Kind, b: Self) -> Self {
        Self::TyFun(k, Box::new(b))
    }

    pub fn ty_app(b: Self, t: TyApp) -> Self {
        Self::TyApp(Box::new(b), t)
    }

    pub fn field(f: Self, i: usize) -> Self {
        Self::Field(Box::new(f), i)
    }

    pub fn tuple(v: impl IntoIterator<Item = Self>) -> Self {
        Self::Tuple(v.into_iter().collect())
    }

    pub fn tag(ty: Type, tag: usize, body: Self) -> Self {
        Self::Tag(ty, tag, Box::new(body))
    }

    pub fn case(ty: Type, scrutinee: Self, branch: impl IntoIterator<Item = Branch>) -> Self {
        Self::Case(ty, Box::new(scrutinee), branch.into_iter().collect())
    }

    pub fn local(var: Var, defn: Self, body: Self) -> Self {
        Self::Local(var, Box::new(defn), Box::new(body))
    }

    pub fn branch(param: Var, body: Self) -> Branch {
        Branch { param, body }
    }

    pub fn r#if(c: Self, t: Self, o: Self) -> Self {
        Self::If(c.into(), t.into(), o.into())
    }

    pub fn add(l: Self, r: Self) -> Self {
        Self::Bin(Box::new(l), BinOp::Add, Box::new(r))
    }

    pub fn sub(l: Self, r: Self) -> Self {
        Self::Bin(Box::new(l), BinOp::Sub, Box::new(r))
    }
    pub fn mul(l: Self, r: Self) -> Self {
        Self::Bin(Box::new(l), BinOp::Mul, Box::new(r))
    }
    pub fn div(l: Self, r: Self) -> Self {
        Self::Bin(Box::new(l), BinOp::Div, Box::new(r))
    }

    pub fn bin(l: Self, op: BinOp, r: Self) -> Self {
        Self::Bin(Box::new(l), op, Box::new(r))
    }

    pub fn type_of(&self) -> Type {
        match self {
            Self::Var(v) => v.ty.clone(),
            Self::Num(_) => Type::Num,
            Self::Str(_) => Type::Str,
            Self::Bool(_) => Type::Bool,
            Self::Unit => Type::Unit,

            Self::Particle(p) => Type::Particle(*p),

            Self::Fun(arg, body) => Type::fun(arg.ty.clone(), body.type_of()),

            Self::App(fun, arg) => {
                let Type::Fun(fun_arg_ty, ret_ty) = fun.type_of() else {
                    unreachable!("IR used non-function type as a function")
                };
                if arg.type_of() != *fun_arg_ty {
                    unreachable!("Function applied to wrong argument type");
                }
                *ret_ty
            }

            // These should all be numbers
            Self::Bin(l, op, r) => match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                    let lty = l.type_of();
                    let rty = r.type_of();
                    if lty != rty || lty != Type::Num {
                        unreachable!(
                            "Expected number type in arithmatic operation while generating IR",
                        )
                    }
                    Type::Num
                }
                BinOp::And | BinOp::Or => {
                    let lty = l.type_of();
                    let rty = r.type_of();
                    if lty != rty || lty != Type::Bool {
                        unreachable!(
                            "Expected number type in arithmatic operation while generating IR",
                        )
                    }
                    Type::Bool
                }
                BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte => {
                    let lty = l.type_of();
                    let rty = r.type_of();
                    if lty != rty {
                        unreachable!("Expected comparison operands to be equal",)
                    }
                    Type::Bool
                }
            },

            Self::TyFun(kind, body) => Type::ty_fun(*kind, body.type_of()),
            Self::TyApp(ty_fun, ty_app) => {
                let Type::TyFun(kind, ret_ty) = ty_fun.type_of() else {
                    unreachable!("Type applied to non-forall IR term");
                };
                match (kind, ty_app) {
                    (Kind::Type, TyApp::Ty(ty)) => ret_ty.subst_ty(ty.clone()),
                    (Kind::Row, TyApp::Row(row)) => ret_ty.subst_row(row.clone()),
                    (Kind::Type, TyApp::Row(_)) => {
                        unreachable!("Kind mismatch. Type applied a Row to variable of kind Type",)
                    }
                    (Kind::Row, TyApp::Ty(_)) => {
                        unreachable!("Type applied a Type to variable of kind Row",)
                    }
                }
            }
            Self::Local(v, defn, body) => {
                // dbg!(v, defn.type_of());
                if v.ty != defn.type_of() {
                    unreachable!(
                        "Type mismatch local variable has different type from its definition",
                    )
                }
                body.type_of()
            }

            Self::If(cond, then, other) => {
                if cond.type_of() != Type::Bool {
                    unreachable!("Cannot evaluate if expression with a non-bool condition")
                }

                let then_ty = then.type_of();

                if then_ty != other.type_of() {
                    unreachable!("If arms have differing types")
                }
                then_ty
            }

            Self::Tuple(elems) => {
                Type::Prod(Row::Closed(elems.iter().map(Self::type_of).collect()))
            }
            Self::Field(body, field) => {
                let Type::Prod(Row::Closed(elems)) = body.type_of() else {
                    unreachable!("IR accessed field of a non product type");
                };
                elems[*field].clone()
            }
            Self::Tag(ty, tag, body) => {
                let Type::Sum(Row::Closed(elems)) = ty else {
                    unreachable!("ICE: Tagged value with non sum type");
                };

                if body.type_of() != elems[*tag] {
                    unreachable!("Tagged value has element with the wrong type")
                }

                ty.clone()
            }
            Self::Case(ty, elem, branches) => {
                let Type::Sum(Row::Closed(elems)) = elem.type_of() else {
                    unreachable!("Case scrutinee does not have sum type")
                };

                for (branch, elem) in branches.iter().zip(elems.iter()) {
                    if elem != &branch.param.ty {
                        unreachable!(
                            "Branch has unexpected parameter type {:?} != {:?}",
                            elem.clone(),
                            branch.param.ty.clone()
                        )
                    }

                    if ty != &branch.body.type_of() {
                        unreachable!("ICE: Branch body has unexpected type")
                    }
                }

                ty.clone()
            }
            Self::Item(t, _) | Self::Extern(_, t) => t.clone(),
        }
    }

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
