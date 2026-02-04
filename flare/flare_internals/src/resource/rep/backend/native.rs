use std::sync::Arc;

use cranelift::{
    codegen::ir::FuncRef,
    module::{FuncId, Module},
    object::ObjectModule,
    prelude::*,
};
use internment::Intern;

use crate::resource::rep::{backend::types::LIRType, midend::ir::ItemId};
#[derive(Debug, Clone)]
pub struct Closure {
    pub captures: Box<VirtualValue>,
    pub func: Box<VirtualValue>,
    pub sig: Signature,
}

#[derive(Debug, Clone)]
pub enum VirtualValue {
    Scalar(Value),
    StackStruct { ty: LIRType, ptr: Value },
    UnstableStruct { ty: LIRType, fields: Vec<Self> },
    Closure(Closure),
    Func(FuncId),
    Pointer(PointeeType, Value),
}

#[derive(Debug, Clone)]
pub enum PointeeType {
    Func(Vec<LIRType>, LIRType),
    String,
    Struct,
}

impl VirtualValue {
    pub fn as_scalar(&self) -> Value {
        match self {
            VirtualValue::Scalar(value) => *value,
            _ => panic!("not an scalar value"),
        }
    }
}
