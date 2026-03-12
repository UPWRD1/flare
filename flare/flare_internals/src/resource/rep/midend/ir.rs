use std::fmt::Display;

use internment::Intern;
use ordered_float::OrderedFloat;

use crate::resource::rep::{
    frontend::ast::BinOp,
    midend::irtype::{IRType, Kind, Row, TyApp},
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash, Default)]
#[repr(transparent)]
pub struct VarId(pub usize);

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub struct ItemId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub id: VarId,
    pub ty: IRType,
}

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.id.0)
    }
}

impl Var {
    #[must_use]
    pub fn new(id: VarId, ty: IRType) -> Self {
        Self { id, ty }
    }

    #[must_use]
    pub fn map_ty(self, f: impl FnOnce(IRType) -> IRType) -> Self {
        let ty = f(self.ty);
        Self { ty, ..self }
    }
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

#[allow(dead_code)]
pub enum Param {
    Ty(Kind),
    Val(Var),
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub enum IR {
    Var(Var),
    Num(OrderedFloat<f32>),
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
    Tag(IRType, usize, Box<Self>),
    Case(IRType, Box<Self>, Vec<Branch>),

    Item(IRType, ItemId),
    Extern(Intern<String>, IRType),
}

#[allow(clippy::should_implement_trait)]
impl IR {
    pub fn fun(_: Var, _: Self) -> Self {
        loop {}
    }

    pub fn funs<I>(_: I, _: Self) -> Self
    where
        I: IntoIterator<Item = Var>,
        I::IntoIter: DoubleEndedIterator,
    {
        loop {}
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

    pub fn tag(ty: IRType, tag: usize, body: Self) -> Self {
        Self::Tag(ty, tag, Box::new(body))
    }

    pub fn case(ty: IRType, scrutinee: Self, branch: impl IntoIterator<Item = Branch>) -> Self {
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

    pub fn type_of(&self) -> IRType {
        loop {}
    }

    pub fn size(&self) -> usize {
        loop {}
    }

    pub fn is_trivial(&self) -> bool {
        match self {
            Self::Var(_)
            | Self::Num(_)
            | Self::Str(_)
            | Self::Unit
            | Self::Bool(_)
            | Self::Particle(_) => true,
            // Self::Field(x, _) if x.is_trivial() => true,
            _ => false,
        }
    }
    pub fn is_value(&self) -> bool {
        match self {
            Self::Bin(l, _, r) => l.is_value() && r.is_value(),

            Self::TyFun(_, ir) => ir.is_value(),
            Self::TyApp(ir, _) => ir.is_value(),
            Self::Local(_, defn, body) => defn.is_value() && body.is_value(),
            Self::Tuple(s) => s.iter().all(Self::is_value),
            Self::App(f, a) if f.is_value() && a.is_value() => true,
            Self::App(_, _) | Self::Case(..) => false,
            Self::Field(ir, _) => ir.is_value(),
            Self::Fun(..) => true,
            Self::Var(_)
            | Self::Num(_)
            | Self::Str(_)
            | Self::Unit
            | Self::Bool(_)
            | Self::Particle(_)
            | Self::Tag(_, _, _) => true,
            Self::Item(_, _) | Self::Extern(_, _) => true,
            Self::If(..) => false,
            // _ => todo!("{self:?}"),
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

    pub fn children(self) -> Vec<Self> {
        match self {
            IR::Var(_) | IR::Num(_) | IR::Str(_) | IR::Bool(_) | IR::Unit | IR::Particle(_) => {
                vec![]
            }
            IR::Fun(_, ir) => vec![*ir],
            IR::App(l, r) => vec![*l, *r],
            IR::TyFun(kind, ir) => vec![*ir],
            IR::TyApp(ir, ty_app) => vec![*ir],
            IR::Local(var, ir, ir1) => vec![*ir, *ir1],
            IR::If(ir, ir1, ir2) => vec![*ir, *ir1, *ir2],
            IR::Bin(ir, bin_op, ir1) => vec![*ir, *ir1],
            IR::Tuple(irs) => irs,
            IR::Field(ir, _) => vec![*ir],
            IR::Tag(irtype, _, ir) => vec![*ir],
            IR::Case(irtype, ir, branchs) => vec![*ir]
                .into_iter()
                .chain(branchs.into_iter().map(|b| b.body))
                .collect(),
            IR::Item(..) | IR::Extern(..) => vec![],
        }
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
                IR::Local(_, defn, body) => {
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

// struct IRModule {
//     externs: FxHashMap<ItemId, Intern<String>>,
//     items: FxHashMap<ItemId, IR>,
// }
