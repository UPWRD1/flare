#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QualifierFragment {
    #[default]
    Root,
    Package(String),
    Type(String),
    Func(String),
    Method(String),
    Variant(String),
    Field(String),
    Wildcard(String),
    Dummy(&'static str),
}

impl QualifierFragment {
    pub fn name(&self) -> String {
        match self {
            Self::Root => String::from("ROOT"),
            Self::Package(n)
            | Self::Type(n)
            | Self::Func(n)
            | Self::Method(n)
            | Self::Variant(n)
            | Self::Field(n)
            | Self::Wildcard(n) => n.to_owned(),
            Self::Dummy(_) => unreachable!("Should not be used in production"),
        }
    }

    pub fn is(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Self::Wildcard(_), _) | (_, Self::Wildcard(_)) => self.is_unstrict(rhs),
            (_, _) => self == rhs,
        }
    }

    pub fn is_unstrict(&self, rhs: &Self) -> bool {
        self.name() == rhs.name()
    }
}
impl std::fmt::Display for QualifierFragment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Root => write!(f, "Root"),
            Self::Package(n) => write!(f, "Package {n}"),
            Self::Type(n) => write!(f, "Type {n}"),
            Self::Func(n) => write!(f, "Function {n}"),
            Self::Method(n) => write!(f, "Method {n}"),
            Self::Field(n) => write!(f, "Field {n}"),
            Self::Variant(n) => write!(f, "Variant {n}"),
            Self::Wildcard(n) => write!(f, "{n}"),
            Self::Dummy(n) => write!(f, "{n}"),
        }
    }
}
