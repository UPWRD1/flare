use internment::Intern;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum LIRType {
    Int,
    Float,
    String,
    Unit,
    Struct(Intern<[Self]>),
    Union(Intern<[Self]>),
    Closure(Intern<Self>, Intern<Self>),
    ClosureEnv(Intern<Self>, Intern<[Self]>),
}

impl LIRType {
    pub fn closure(l: Self, r: Self) -> Self {
        Self::Closure(l.into(), r.into())
    }

    pub fn closure_env(l: Self, r: Vec<Self>) -> Self {
        Self::ClosureEnv(l.into(), r.as_slice().into())
    }

    pub fn into_struct_fields(&self) -> Vec<Self> {
        match self {
            LIRType::Struct(intern) => intern.to_vec(),
            LIRType::ClosureEnv(_, env) => env.to_vec(),
            _ => unimplemented!("NOt a struct"),
        }
    }

    pub fn destructure_closure(self) -> (Vec<Self>, Self) {
        fn worker(t: &LIRType, v: &mut Vec<LIRType>) {
            match t {
                LIRType::Closure(l, r) => {
                    v.push(**l);
                    worker(r, v);
                }
                _ => v.push(*t),
            }
        }
        let mut v = vec![];
        worker(&self, &mut v);
        let (ret, args) = v.split_last().expect("Could not destructure arrow");
        (args.to_vec(), *ret)
    }
}
