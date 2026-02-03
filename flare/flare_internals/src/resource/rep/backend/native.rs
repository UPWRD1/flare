use cranelift::prelude::{FunctionBuilder, InstBuilder, Signature, Value};
use internment::Intern;

use crate::resource::rep::backend::types::LIRType;

pub struct Closure {
    pub data: Value,
    pub func: Value,
    pub sig: Signature,
}

impl Closure {
    fn call<'a>(&self, fbuilder: &'a mut FunctionBuilder<'_>, params: &[Value]) -> &'a [Value] {
        let mut real_params = vec![self.data];
        real_params.extend_from_slice(params);
        let sigref = fbuilder.import_signature(self.sig.clone());
        let call = fbuilder
            .ins()
            .call_indirect(sigref, self.func, &real_params);
        fbuilder.inst_results(call)
    }
}
