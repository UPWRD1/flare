use cranelift::{
    module::FuncId,
    prelude::{FunctionBuilder, InstBuilder, Signature, Value},
};
use internment::Intern;

use crate::resource::rep::backend::types::LIRType;
#[derive(Debug, Clone)]
pub struct Closure {
    pub captures: Box<VirtualValue>,
    pub func: Box<VirtualValue>,
    pub sig: Signature,
}

impl Closure {
    pub fn call(
        &self,
        fbuilder: &mut FunctionBuilder<'_>,
        params: &[VirtualValue],
    ) -> VirtualValue {
        let mut real_params = vec![*self.captures.clone()];
        real_params.extend_from_slice(params);
        let sigref = fbuilder.import_signature(self.sig.clone());
        let call = fbuilder.ins().call_indirect(
            sigref,
            self.func.as_scalar(),
            &real_params
                .iter()
                .map(|param| param.as_scalar())
                .collect::<Vec<_>>(),
        );
        match fbuilder.inst_results(call) {
            [res] => VirtualValue::Scalar(*res),
            res => panic!("many ret types"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum VirtualValue {
    Scalar(Value),
    StackStruct {
        ty: Intern<[LIRType]>,
        ptr: Value,
    },
    UnstableStruct {
        ty: Intern<[LIRType]>,
        fields: Vec<Self>,
    },
    Closure(Closure),
    Func(FuncId),
    Pointer(Value),
}

impl VirtualValue {
    pub fn as_scalar(&self) -> Value {
        match self {
            VirtualValue::Scalar(value) => *value,
            _ => panic!("not an scalar value"),
        }
    }
    pub fn as_value(self) -> Value {
        match self {
            VirtualValue::Scalar(value) => value,
            VirtualValue::Pointer(ptr) => ptr,
            VirtualValue::StackStruct { ty: _, ptr } => ptr,
            _ => todo!(),
        }
    }
}
