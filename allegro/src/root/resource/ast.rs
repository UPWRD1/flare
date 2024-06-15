pub enum Itype {
    Int(Option<i32>),
    Flt(Option<f32>),
    Str(Option<String>),
    Bool(Option<bool>),
}

pub enum Statement {
    Bind {
        name: String,
        value: Itype,
    }
}