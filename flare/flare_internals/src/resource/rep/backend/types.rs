use internment::Intern;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum LIRType {
    Int,
    Float,
    String,
    Unit,
    Struct(Intern<[Self]>),
    Union(Intern<[Self]>),
    Closure(Intern<[Self]>, Intern<Self>),
    ClosureEnv(Intern<Self>, Intern<[Self]>),
}

impl LIRType {
    pub fn closure(l: &[Self], r: Self) -> Self {
        Self::Closure(l.into(), r.into())
    }

    pub fn closure_env(l: Self, r: Vec<Self>) -> Self {
        Self::ClosureEnv(l.into(), r.as_slice().into())
    }

    pub fn into_struct_fields(&self) -> Vec<Self> {
        match self {
            LIRType::Struct(intern) => intern.to_vec(),
            LIRType::ClosureEnv(..) => self.closure_to_struct_rep().into_struct_fields(),
            // LIRType::ClosureEnv(_, env) => env.to_vec(),
            _ => unimplemented!("Not a struct: {self:?}"),
        }
    }

    pub fn closure_to_struct_rep(self) -> Self {
        match self {
            LIRType::ClosureEnv(f, env) => {
                // let env_struct = LIRType::Struct(env);
                // LIRType::Struct(vec![*f, env_struct].as_slice().into())

                // LIRType::Struct({
                //     let env = env.iter().copied();
                //     vec![*f]
                //         .into_iter()
                //         .chain(env)
                //         .collect::<Vec<_>>()
                //         .as_slice()
                //         .into()
                // })
                LIRType::Struct(env)
            }
            _ => panic!("Not a closure {self:?}"),
        }
    }

    pub fn destructure_closure(self) -> (Vec<Self>, Self) {
        // fn worker(t: &LIRType, v: &mut Vec<LIRType>) {
        //     match t {
        //         LIRType::Closure(l, r) => {
        //             v.push(*l);
        //             worker(r, v);
        //         }
        //         _ => v.push(*t),
        //     }
        // }        let mut v = vec![];
        // worker(&self, &mut v);
        // let (ret, args) = v.split_last().expect("Could not destructure arrow");
        // (args.to_vec(), *ret)
        if let Self::Closure(args, ret) = self {
            (args.to_vec(), *ret)
        } else {
            panic!("Not a closure")
        }
    }

    pub fn variant(self, idx: usize) -> Self {
        if let Self::Union(variants) = self {
            variants[idx]
        } else {
            panic!("Not a union")
        }
    }

    pub fn variants(self) -> Vec<Self> {
        if let Self::Union(variants) = self {
            variants.to_vec()
        } else {
            panic!("Not a union")
        }
    }
}
