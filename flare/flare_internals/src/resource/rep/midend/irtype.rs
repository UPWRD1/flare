use internment::Intern;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub struct TypeVar(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kind {
    Type,
    Row,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash)]
pub enum Row {
    Open(TypeVar),
    Closed(Vec<IRType>),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash)]
pub enum Specifier {
    Int,
    Float,
    Unknown,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash, Default)]
pub enum IRType {
    // Num(Specifier),
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

impl IRType {
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

    pub fn is_scalar(&self) -> bool {
        match self {
            IRType::Num | IRType::Unit | IRType::Str | IRType::Bool | IRType::Particle(_) => true,
            IRType::Prod(row) | IRType::Sum(row) => true,
            IRType::Var(type_var) => false,
            IRType::Fun(irtype, irtype1) => false,
            IRType::TyFun(_, t) => t.is_scalar(),
        }
    }
}

impl IRType {
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TyApp {
    Ty(IRType),
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
