use std::{fmt::{Display, Error}, str::FromStr};

#[derive(Debug, Clone, PartialEq, PartialOrd, Default)]
pub enum Itype {
    #[default] Mute,
    Int(Option<i32>),
    Flt(Option<f32>),
    Str(Option<String>),
    Bool(Option<bool>),
}

impl std::ops::Add for Itype {
    type Output = Itype;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Itype::Int(v) => {
                match rhs {
                    Itype::Int(u) => Itype::Int(Some(v.unwrap() + u.unwrap())),
                    _ => todo!(),
                }
            },
            Itype::Flt(v) => {
                match rhs {
                    Itype::Flt(u) => Itype::Flt(Some(v.unwrap() + u.unwrap())),
                    _ => todo!(),
                }
            },

            _ => todo!(),
        }
    }
}

impl std::ops::Sub for Itype {
    type Output = Itype;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Itype::Int(v) => {
                match rhs {
                    Itype::Int(u) => Itype::Int(Some(v.unwrap() - u.unwrap())),
                    _ => todo!(),
                }
            },
            Itype::Flt(v) => {
                match rhs {
                    Itype::Flt(u) => Itype::Flt(Some(v.unwrap() - u.unwrap())),
                    _ => todo!(),
                }
            },

            _ => todo!(),
        }
    }
}

impl std::ops::Mul for Itype {
    type Output = Itype;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Itype::Int(v) => {
                match rhs {
                    Itype::Int(u) => Itype::Int(Some(v.unwrap() * u.unwrap())),
                    _ => todo!(),
                }
            },
            Itype::Flt(v) => {
                match rhs {
                    Itype::Flt(u) => Itype::Flt(Some(v.unwrap() * u.unwrap())),
                    _ => todo!(),
                }
            },

            _ => todo!(),
        }
    }
}

impl std::ops::Div for Itype {
    type Output = Itype;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Itype::Int(v) => {
                match rhs {
                    Itype::Int(u) => Itype::Int(Some(v.unwrap() / u.unwrap())),
                    _ => todo!(),
                }
            },
            Itype::Flt(v) => {
                match rhs {
                    Itype::Flt(u) => Itype::Flt(Some(v.unwrap() / u.unwrap())),
                    _ => todo!(),
                }
            },

            _ => todo!(),
        }
    }
}


impl Display for Itype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

pub trait ToItype {
    fn to_itype(&self) -> Itype;
}

impl ToItype for i32 {
    fn to_itype(&self) -> Itype {
        Itype::Int(Some(*self))
    }
}

impl ToItype for f32 {
    fn to_itype(&self) -> Itype {
        Itype::Flt(Some(*self))
    }
}

impl ToItype for String {
    fn to_itype(&self) -> Itype {
        if self.parse::<i32>().is_ok() {
            return Itype::Int(Some(self.parse::<i32>().unwrap()));
        } else if self.parse::<f32>().is_ok() {
            return Itype::Flt(Some(self.parse::<f32>().unwrap()));
        } else if self.parse::<bool>().is_ok() {
            return Itype::Bool(Some(self.parse::<bool>().unwrap()));
        } else {
            return match self.as_str() {
                "int" => Itype::Int(None),
                "flt" => Itype::Flt(None),
                "str" => Itype::Bool(None),
                "bool" => Itype::Str(None),
                _ => Itype::Str(Some(self.clone())),
            };
        }
    }
}


impl FromStr for Itype {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.parse::<i32>().is_ok() {
            return Ok(Itype::Int(Some(s.parse::<i32>().unwrap())));
        } else if s.parse::<f32>().is_ok() {
            return Ok(Itype::Flt(Some(s.parse::<f32>().unwrap())));
        } else if s.parse::<bool>().is_ok() {
            return Ok(Itype::Bool(Some(s.parse::<bool>().unwrap())));
        } else {
            return match s {
                "int" => Ok(Itype::Int(None)),
                "flt" => Ok(Itype::Flt(None)),
                "str" => Ok(Itype::Bool(None)),
                "bool" => Ok(Itype::Str(None)),
                _ => Ok(Itype::Str(Some(s.to_string()))),
            };
        }
    }
}


impl ToItype for bool {
    fn to_itype(&self) -> Itype {
        Itype::Bool(Some(*self))
    }
}


#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl BinOp {
    pub fn from_str(s: &str) -> Self {
        match s {
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "/" => Self::Div,
            _ => panic!()
        }
    }
}


#[derive(Debug, Clone)]
pub enum UnOp {
    Neg,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
    Unary {
        op: UnOp,
        rhs: Box<Expr>,
    },
    Scalar(Itype),
    Variable(String),
    Call {
        name: String,
        params: Vec<Expr>,
    }
}


#[derive(Debug, Clone)]
pub enum Statement {
    Bind {
        name: String,
        value: Expr,
    },
    Expression(Expr),
    Print {
        value: Expr,
    },
    Return {
        value: Expr,
    },
    Func {
        name: String,
        rt: Itype,
        params: Vec<String>,
        body: Vec<Statement>,
    },
}
