#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum LIRType {
    Int,
    Float,
    String,
    Unit,
    Array(Vec<Self>),
    Union(Vec<Self>),
    Closure(Box<Self>, Box<Self>),
    ClosureEnv(Box<Self>, Vec<Self>),
}
impl LIRType {
    pub fn closure(l: Self, r: Self) -> Self {
        Self::Closure(Box::new(l), Box::new(r))
    }

    pub fn closure_env(l: Self, r: Vec<Self>) -> Self {
        Self::ClosureEnv(Box::new(l), r)
    }
}
