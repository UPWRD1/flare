trait CardinalOps {
    fn both(&self, rhs: &Self) -> Self;
    fn lower_or(&self, rhs: &Self) -> Self;
}

#[derive(Debug, Clone)]
enum Demand {
    // :waw(usize),
    Call(Multiplicity, Box<Usage>),
    Used,
}

impl CardinalOps for Demand {
    fn both(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (_, Self::Used) | (Self::Used, _) => Self::Used,
            (Self::Call(_, d1), Self::Call(_, d2)) => {
                Self::Call(Multiplicity::Many, Box::new(d1.lower_or(d2)))
            } // _ => panic!("{self:?} & {rhs:?} failed"),
        }
    }

    fn lower_or(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (_, Self::Used) | (Self::Used, _) => Self::Used,
            (Self::Call(n1, d1), Self::Call(n2, d2)) => {
                Self::Call(n1.lower_or(n2), Box::new(d1.lower_or(d2)))
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Multiplicity {
    Once,
    Many,
}

impl CardinalOps for Multiplicity {
    fn both(&self, rhs: &Self) -> Self {
        todo!()
    }

    fn lower_or(&self, rhs: &Self) -> Self {
        todo!()
    }
}

#[derive(Debug, Clone)]
enum Usage {
    Mult(Multiplicity, Demand),
    Absent,
}

impl Usage {
    fn mu(&self) -> usize {
        match self {
            Self::Mult(Multiplicity::Many, _) => usize::MAX,
            Self::Mult(Multiplicity::Once, _) => 1,
            Self::Absent => 0,
        }
    }
    fn mul(&self) -> impl CardinalOps {
        match self {
            Self::Mult(Multiplicity::Once, d) => d.clone(),
            Self::Mult(Multiplicity::Many, d) => d.both(d),
            Self::Absent => todo!(),
        }
    }
}

impl CardinalOps for Usage {
    fn both(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (ddag, Self::Absent) | (Self::Absent, ddag) => ddag.clone(),
            (Self::Mult(_, d1), Self::Mult(_, d2)) => Self::Mult(Multiplicity::Many, d1.both(d2)),
        }
    }

    fn lower_or(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Self::Absent, d) | (d, Self::Absent) => d.clone(),
            (Self::Mult(n1, d1), Self::Mult(n2, d2)) => {
                Self::Mult(n1.lower_or(n2), d1.lower_or(d2))
            }
        }
    }
}

// struct Signature(IR, S)

// type SignatureEnv = FxHashMap<IR, Signature>;
