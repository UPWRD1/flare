use ordered_float::OrderedFloat;

use crate::resource::errors::FatalErr;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub struct TypeVar(pub usize);

#[derive(PartialEq, Eq, PartialOrd, Clone, Debug, Hash)]
pub enum Type {
    Num,
    Unit,
    Str,
    Bool,

    Var(TypeVar),

    Fun(Box<Self>, Box<Self>),
    TyFun(Kind, Box<Self>),

    Prod(Row),
    Sum(Row),
}

#[derive(PartialEq, Eq, PartialOrd, Clone, Debug, Hash)]
pub enum Row {
    Open(TypeVar),
    Closed(Vec<Type>),
}

impl Type {
    pub fn fun(l: Self, r: Self) -> Self {
        Self::Fun(Box::new(l), Box::new(r))
    }

    pub fn funs(args: impl Into<Vec<Self>>, ret: Self) -> Self {
        args.into()
            .into_iter()
            .rfold(ret, |ret, arg| Self::Fun(Box::new(arg), Box::new(ret)))
    }
    pub fn ty_fun(k: Kind, t: Self) -> Self {
        Self::TyFun(k, Box::new(t))
    }

    pub fn prod(row: Row) -> Self {
        match row {
            Row::Closed(elems) if elems.len() == 1 => elems.into_iter().next().unwrap(),
            row => Self::Prod(row),
        }
    }

    pub fn sum(row: Row) -> Self {
        match row {
            Row::Closed(elems) if elems.len() == 1 => elems.into_iter().next().unwrap(),
            row => Self::Sum(row),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
#[repr(transparent)]
pub struct VarId(pub usize);

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub struct ItemId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Var {
    id: VarId,
    ty: Type,
}

impl Var {
    pub fn new(id: VarId, ty: Type) -> Self {
        Self { id, ty }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kind {
    Type,
    Row,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Branch {
    param: Var,
    body: IR,
}

impl Branch {
    pub fn as_fun(&self) -> IR {
        IR::fun(self.param.clone(), self.body.clone())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyApp {
    Ty(Type),
    Row(Row),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IR {
    Var(Var),
    Num(OrderedFloat<f64>),

    Fun(Var, Box<Self>),
    App(Box<Self>, Box<Self>),

    Add(Box<Self>, Box<Self>),
    Sub(Box<Self>, Box<Self>),
    Mul(Box<Self>, Box<Self>),
    Div(Box<Self>, Box<Self>),

    TyFun(Kind, Box<Self>),
    TyApp(Box<Self>, TyApp),
    Local(Var, Box<Self>, Box<Self>),

    Tuple(Vec<Self>),
    Field(Box<Self>, usize),
    Tag(Type, usize, Box<Self>),
    Case(Type, Box<Self>, Vec<Branch>),

    Item(Type, ItemId),
}
#[allow(clippy::should_implement_trait)]
impl IR {
    pub fn add(l: Self, r: Self) -> Self {
        Self::Add(Box::new(l), Box::new(r))
    }

    pub fn sub(l: Self, r: Self) -> Self {
        Self::Sub(Box::new(l), Box::new(r))
    }
    pub fn mul(l: Self, r: Self) -> Self {
        Self::Mul(Box::new(l), Box::new(r))
    }
    pub fn div(l: Self, r: Self) -> Self {
        Self::Div(Box::new(l), Box::new(r))
    }

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
    pub fn type_of(&self) -> Type {
        match self {
            Self::Var(v) => v.ty.clone(),
            Self::Num(_) => Type::Num,
            Self::Fun(arg, body) => Type::fun(arg.ty.clone(), body.type_of()),
            Self::App(fun, arg) => {
                let Type::Fun(fun_arg_ty, ret_ty) = fun.type_of() else {
                    FatalErr::new("IR used non-function type as a function")
                };
                if arg.type_of() != *fun_arg_ty {
                    FatalErr::new("Function applied to wrong argument type");
                }
                *ret_ty
            }
            Self::TyFun(kind, body) => Type::ty_fun(*kind, body.type_of()),
            Self::TyApp(ty_fun, ty_app) => {
                let Type::TyFun(kind, ret_ty) = ty_fun.type_of() else {
                    FatalErr::new("Type applied to non-forall IR term");
                };
                match (kind, ty_app) {
                    (Kind::Type, TyApp::Ty(ty)) => ret_ty.subst_ty(ty.clone()),
                    (Kind::Row, TyApp::Row(row)) => ret_ty.subst_row(row.clone()),
                    (Kind::Type, TyApp::Row(_)) => {
                        panic!("ICE: Kind mismatch. Type applied a Row to variable of kind Type")
                    }
                    (Kind::Row, TyApp::Ty(_)) => {
                        panic!("ICE: Kind mismatch. Type applied a Type to variable of kind Row")
                    }
                }
            }
            Self::Local(v, defn, body) => {
                if v.ty != defn.type_of() {
                    FatalErr::new(
                        "Type mismatch local variable has different type from its definition",
                    )
                }
                body.type_of()
            }

            Self::Tuple(elems) => {
                Type::Prod(Row::Closed(elems.iter().map(|ir| ir.type_of()).collect()))
            }
            Self::Field(body, field) => {
                let Type::Prod(Row::Closed(elems)) = body.type_of() else {
                    panic!("ICE: IR accessed field of a non product type");
                };
                elems[*field].clone()
            }
            Self::Tag(ty, tag, body) => {
                let Type::Sum(Row::Closed(elems)) = ty else {
                    panic!("ICE: Tagged value with non sum type");
                };

                if body.type_of() != elems[*tag] {
                    panic!("ICE: Tagged value has element with the wrong type")
                };

                ty.clone()
            }
            Self::Case(ty, elem, branches) => {
                let Type::Sum(Row::Closed(elems)) = elem.type_of() else {
                    FatalErr::new("Case scrutinee does not have sum type")
                };

                for (branch, elem) in branches.iter().zip(elems.iter()) {
                    if elem != &branch.param.ty {
                        FatalErr::new(format!(
                            "Branch has unexpected parameter type {:?} != {:?}",
                            elem.clone(),
                            branch.param.ty.clone()
                        ))
                    }

                    if ty != &branch.body.type_of() {
                        FatalErr::new("ICE: Branch body has unexpected type")
                    }
                }

                ty.clone()
            }
            Self::Item(t, _) => t.clone(),
            // These should all be numbers
            Self::Add(l, r) | Self::Sub(l, r) | Self::Mul(l, r) | Self::Div(l, r) => {
                let lty = l.type_of();
                let rty = r.type_of();
                if lty != rty || lty != Type::Num {
                    FatalErr::new(
                        "Expected number type in arithmatic operation while generating IR",
                    )
                }
                Type::Num
            }
        }
    }
}
