use cranelift::{module::FuncId, prelude::*};

use crate::resource::rep::backend::types::LIRType;
#[derive(Debug, Clone)]
pub struct Closure {
    pub ty: LIRType,
    // pub captures: Box<VirtualValue>,
    pub captures: Box<VirtualValue>,
    pub func: Box<VirtualValue>,
    pub sig: Signature,
}

#[derive(Debug, Clone)]
pub enum VirtualValue {
    Scalar(Value),
    StackStruct {
        ty: LIRType,
        ptr: Value,
    },
    UnstableStruct {
        ty: LIRType,
        fields: Vec<Self>,
    },
    Closure(Closure),
    Func(FuncId),
    Pointer(PointeeType, Value),
    TaggedUnion {
        variants: Vec<LIRType>,
        body: Box<Self>,
        idx: usize,
    },
}

#[derive(Debug, Clone)]
pub enum PointeeType {
    Func(Vec<LIRType>, LIRType),
    String,
    Struct,
    Union(Vec<LIRType>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PayloadKind {
    InlineCasted(Type),
    Inline,
    Zero,
    StackPointer,
}

// Whether a struct will be passed as a pointer or as a set of independent values directly
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum StructPassingMode {
    ByScalars,
    ByPointer,
}

impl VirtualValue {
    pub fn as_scalar(&self) -> Value {
        match self {
            VirtualValue::Scalar(value) => *value,
            _ => panic!("not an scalar value"),
        }
    }
}
