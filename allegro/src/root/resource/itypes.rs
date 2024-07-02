use std::str::FromStr;
use std::fmt::Error;

#[derive(Debug, Clone, PartialEq, PartialOrd, Default)]
pub enum Itype {
    #[default] Mute,
    Int(Option<i32>),
    Flt(Option<f64>),
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

// impl Display for Itype {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         todo!()
//     }
// }

// pub trait ToItype {
//     fn to_itype(&self) -> Itype;
// }

// impl ToItype for i32 {
//     fn to_itype(&self) -> Itype {
//         Itype::Int(Some(*self))
//     }
// }

// impl ToItype for f64 {
//     fn to_itype(&self) -> Itype {
//         Itype::Flt(Some(*self))
//     }
// }

// impl ToItype for String {
//     fn to_itype(&self) -> Itype {
//         if self.parse::<i32>().is_ok() {
//             return Itype::Int(Some(self.parse::<i32>().unwrap()));
//         } else if self.parse::<f64>().is_ok() {
//             return Itype::Flt(Some(self.parse::<f64>().unwrap()));
//         } else if self.parse::<bool>().is_ok() {
//             return Itype::Bool(Some(self.parse::<bool>().unwrap()));
//         } else {
//             return match self.as_str() {
//                 "int" => Itype::Int(None),
//                 "flt" => Itype::Flt(None),
//                 "str" => Itype::Bool(None),
//                 "bool" => Itype::Str(None),
//                 _ => Itype::Str(Some(self.clone())),
//             };
//         }
//     }
// }

impl FromStr for Itype {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.parse::<i32>().is_ok() {
            return Ok(Itype::Int(Some(s.parse::<i32>().unwrap())));
        } else if s.parse::<f64>().is_ok() {
            return Ok(Itype::Flt(Some(s.parse::<f64>().unwrap())));
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

// impl ToItype for bool {
//     fn to_itype(&self) -> Itype {
//         Itype::Bool(Some(*self))
//     }
// }

impl ToString for Itype {
    fn to_string(&self) -> String {
        match self {
            Itype::Mute => todo!(),
            Itype::Int(v) => if v.is_some() {v.unwrap().to_string()} else {"Int".to_string()},
            Itype::Flt(v) => if v.is_some() {v.unwrap().to_string()} else {"Flt".to_string()},
            Itype::Str(v) => if v.is_some() {<Option<String> as Clone>::clone(&v).unwrap().to_string()} else {"Str".to_string()},
            Itype::Bool(v) => if v.is_some() {v.unwrap().to_string()} else {"Bool".to_string()},
        }
    }
}