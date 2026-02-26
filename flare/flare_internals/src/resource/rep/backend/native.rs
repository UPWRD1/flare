use cranelift::{module::FuncId, prelude::*};
use internment::Intern;

use crate::resource::rep::backend::types::LIRType;
#[derive(Debug, Clone)]
pub struct Closure {
    // pub ty: LIRType,
    // pub captures: Box<VirtualValue>,
    pub captures: Box<VirtualValue>,
    pub func: Box<VirtualValue>,
    pub sig: Signature,
}

#[derive(Debug, Clone)]
pub enum VirtualValue {
    Scalar(Value, LIRType),
    StackStruct {
        ty: LIRType,
        ptr: Value,
    },
    UnstableStruct {
        ty: LIRType,
        fields: Vec<Self>,
    },
    Closure(Closure),
    Func(FuncId, LIRType),
    Pointer(PointeeType, Value),
    TaggedUnion {
        // variants: Vec<LIRType>,
        // body: Vec<Value>,
        body: Value,
        // idx: usize,
        tag: Value,
    },
}

#[derive(Debug, Clone)]
pub enum PointeeType {
    Func(Vec<LIRType>, LIRType),
    String,
    Struct(LIRType),
    Union(Vec<LIRType>, usize),
}

impl PointeeType {
    fn ty(&self) -> LIRType {
        match self {
            PointeeType::Func(args, ret) => {
                LIRType::Closure(args.as_slice().into(), Intern::from(*ret))
            }
            PointeeType::String => LIRType::String,
            PointeeType::Struct(lirtype) => todo!("{lirtype:?}"),
            PointeeType::Union(lirtypes, _) => todo!(),
        }
    }
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
            VirtualValue::Scalar(value, _) => *value,
            _ => panic!("not an scalar value"),
        }
    }

    pub fn ty(&self) -> LIRType {
        match self {
            VirtualValue::Scalar(_, ty)
            | VirtualValue::Func(_, ty)
            | VirtualValue::StackStruct { ty, ptr: _ }
            | VirtualValue::UnstableStruct { ty, fields: _ } => *ty,
            VirtualValue::Closure(closure) => closure.func.ty(),
            VirtualValue::Pointer(pointee_type, value) => pointee_type.ty(),
            VirtualValue::TaggedUnion { body, tag } => todo!(),
        }
    }
}
