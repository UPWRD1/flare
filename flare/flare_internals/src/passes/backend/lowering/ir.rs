use std::fmt::Display;

use internment::Intern;
use ordered_float::OrderedFloat;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    passes::backend::target::Target,
    resource::{
        pretty::{INC, Render},
        rep::ast::BinOp,
    },
};
use itertools::Itertools;
use tiny_pretty::Doc;

#[derive(Clone, Copy, Default)]
pub struct IRTarget;

impl Target for IRTarget {
    type Partial = IR;

    type Output = String;
    type Input = IR;

    fn generate(&mut self, ir: IR) -> Self::Partial {
        ir
    }

    fn finish(&self, p: Vec<Self::Partial>) -> Self::Output {
        p.into_iter()
            .enumerate()
            .map(|(i, x)| format!("item #{i}: is\n{x}\nend item #{i}"))
            .collect::<Vec<String>>()
            .join("\n\n")
    }
    fn ext(&self) -> &str {
        "ir"
    }
    fn convert(&self, ir: Vec<IR>) -> Vec<Self::Input> {
        ir
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub struct TypeVar(pub usize);

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash, Default)]
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

    pub fn into_ret_ty(self) -> Self {
        match self {
            Self::Fun(_, r) => r.into_ret_ty(),
            Self::TyFun(kind, r) => r.into_ret_ty(),
            _ => self,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash)]
pub enum Row {
    Open(TypeVar),
    Closed(Vec<Type>),
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
            .append(self.param.render())
            .append(Doc::space())
            .append(Doc::text("then"))
            .append(Doc::soft_line())
            .append(self.body.render())
            .nest(INC)
    }
}

#[allow(dead_code)]
pub enum Param {
    Ty(Kind),
    Val(Var),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TyApp {
    Ty(Type),
    Row(Row),
}

impl std::fmt::Display for TyApp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ty(t) => write!(f, "{}", t),
            Self::Row(row) => todo!(),
        }
    }
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

#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub enum IR {
    Var(Var),
    Num(OrderedFloat<f64>),
    Str(Intern<String>),
    Bool(bool),
    #[default]
    Unit,
    // Comment(String, Box<Self>),
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
    Extern(Intern<String>, Type),
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

            // Self::Comment(_, r) => r.type_of(),
            Self::Particle(p) => Type::Particle(*p),

            Self::Fun(arg, body) => Type::fun(arg.ty.clone(), body.type_of()),

            Self::App(fun, arg) => {
                if let Type::Fun(fun_arg_ty, ret_ty) = fun.type_of() {
                    if arg.type_of() != *fun_arg_ty {
                        unreachable!(
                            "Function applied to wrong argument type. Expected {}, found {}",
                            fun_arg_ty,
                            arg.type_of()
                        );
                    }
                    *ret_ty
                } else if let Self::Item(t, id) = &**fun {
                    t.clone().into_ret_ty()
                } else {
                    unreachable!("IR used non-function type as a function: {fun}")
                }
            }

            // These should all be numbers
            Self::Bin(l, op, r) => Self::type_of_binop(l, *op, r),

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
                if v.ty != defn.type_of() {
                    unreachable!(
                        "Type mismatch local variable ${} has different type from its definition:  {} vs {}, \ndefn = \n{}",
                        v.id.0,
                        v.ty,
                        defn.type_of(),
                        defn
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
                    unreachable!(
                        "Tagged value has element with the wrong type {} vs {}, \nir = {}",
                        body.type_of(),
                        elems[*tag],
                        self,
                    )
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
                        unreachable!(
                            "ICE: Branch body has unexpected type: ty {ty:?} vs branch: {:?}",
                            branch.body.type_of()
                        )
                    }
                }

                ty.clone()
            }
            Self::Item(t, _) | Self::Extern(_, t) => t.clone(),
        }
    }

    fn type_of_binop(l: &Self, op: BinOp, r: &Self) -> Type {
        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                let lty = l.type_of();
                let rty = r.type_of();
                if lty != rty || lty != Type::Num {
                    unreachable!("Expected number type in arithmatic operation while generating IR",)
                }
                Type::Num
            }
            BinOp::And | BinOp::Or => {
                let lty = l.type_of();
                let rty = r.type_of();
                if lty != rty || lty != Type::Bool {
                    unreachable!("Expected number type in arithmatic operation while generating IR",)
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
        }
    }

    pub fn size(&self) -> usize {
        fn size_app(ir: &IR, args: usize) -> usize {
            match ir {
                IR::App(fun, arg) => arg.size() + size_app(fun, args + 1),
                IR::TyApp(ir, _) => size_app(ir, args),
                ir => ir.size() + 10 * (1 + args),
            }
        }
        match self {
            Self::Var(_)
            | Self::Num(_)
            | Self::Str(_)
            | Self::Bool(_)
            | Self::Unit
            | Self::Particle(_) => 0,

            // Self::Comment(_, r) => r.size(),
            Self::Bin(l, _, r) => l.size() + r.size(),

            Self::Fun(_, body) => 10 + body.size(),
            Self::App(fun, arg) => arg.size() + size_app(fun, 1),
            Self::TyFun(_, ir) | Self::TyApp(ir, _) | Self::Field(ir, _) | Self::Tag(_, _, ir) => {
                ir.size()
            }
            Self::Local(var, defn, body) => {
                defn.size() + body.size() + (if var.ty.is_cheap_alloc() { 0 } else { 10 })
            }
            Self::If(c, t, o) => c.size().max(t.size().max(o.size())),
            Self::Tuple(v) => v.iter().map(Self::size).sum::<usize>(),
            Self::Case(_, s, b) => s.size() + b.iter().map(|x| x.as_fun().size()).sum::<usize>(),
            Self::Item(_, _) | Self::Extern(_, _) => 0,
            // _ => todo!(),
        }
    }

    pub fn is_trivial(&self) -> bool {
        matches!(
            self,
            // Self::Var(_) |
            Self::Num(_) | Self::Str(_) | Self::Unit | Self::Bool(_) | Self::Particle(_)
        )
    }
    pub fn is_value(&self) -> bool {
        match self {
            Self::Bin(l, _, r) => l.is_value() && r.is_value(),

            Self::TyFun(_, ir) => ir.is_value(),
            Self::Local(_, defn, body) => defn.is_value() && body.is_value(),
            Self::Tuple(s) => s.iter().all(Self::is_value),
            Self::Var(_) | Self::App(_, _) | Self::TyApp(_, _) => false,
            Self::Num(_)
            | Self::Str(_)
            | Self::Unit
            | Self::Bool(_)
            | Self::Particle(_)
            | Self::Fun(_, _)
            | Self::Tag(_, _, _)
            | Self::Case(_, _, _) => true,
            _ => todo!("{self:?}"),
        }
    }

    pub fn who_do_i_call(&self) -> Vec<ItemId> {
        self.iter()
            .filter_map(|ir| {
                if let Self::Item(_, id) = ir {
                    Some(*id)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn split_funs(self) -> (Vec<Param>, Self) {
        fn split_funs(ir: IR, params: &mut Vec<Param>) -> IR {
            match ir {
                IR::TyFun(kind, ir) => {
                    params.push(Param::Ty(kind));
                    split_funs(*ir, params)
                }
                IR::Fun(var, ir) => {
                    params.push(Param::Val(var));
                    split_funs(*ir, params)
                }
                ir => ir,
            }
        }
        let mut params = vec![];
        let body = split_funs(self, &mut params);
        (params, body)
    }

    pub fn iter<'a>(&'a self) -> IrIterator<'a> {
        IrIterator { stack: vec![self] }
    }
}

// GENERATED: Google AI Overview 😭
#[derive(Debug)]
pub struct IrIterator<'a> {
    stack: Vec<&'a IR>,
}

impl<'a> Iterator for IrIterator<'a> {
    type Item = &'a IR;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(node) = self.stack.pop() {
            // Push child nodes onto the stack (e.g., for depth-first traversal)
            // Note: the order of pushing determines the traversal order
            match node {
                IR::Var(_)
                | IR::Num(_)
                | IR::Str(_)
                | IR::Bool(_)
                | IR::Unit
                | IR::Particle(_)
                | IR::Item(_, _)
                | IR::Extern(_, _) => {

                    // Do nothing
                }
                // IR::Comment(_, ir)
                IR::Fun(_, ir)
                | IR::TyFun(_, ir)
                | IR::TyApp(ir, _)
                | IR::Field(ir, _)
                | IR::Tag(_, _, ir) => self.stack.push(ir),
                IR::App(l, r) => {
                    self.stack.push(r); // push right first so left is processed next (LIFO)
                    self.stack.push(l);
                }
                IR::Local(var, defn, body) => {
                    self.stack.push(body);
                    self.stack.push(defn);
                }
                IR::If(cond, then, other) => {
                    self.stack.push(other);
                    self.stack.push(then);
                    self.stack.push(cond);
                }
                IR::Bin(l, _, r) => {
                    self.stack.push(r);
                    self.stack.push(l);
                }

                IR::Tuple(irs) => {
                    for ir in irs.iter().rev() {
                        self.stack.push(ir);
                    }
                }
                IR::Case(_, scrutinee, branches) => {
                    for branch in branches.iter().rev() {
                        self.stack.push(&branch.body);
                    }
                    self.stack.push(scrutinee)
                }
            }
            Some(node)
        } else {
            None // Stack is empty, iteration is complete
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

impl Display for Type {
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

struct IRModule {
    externs: FxHashMap<ItemId, Intern<String>>,
    items: FxHashMap<ItemId, IR>,
}
