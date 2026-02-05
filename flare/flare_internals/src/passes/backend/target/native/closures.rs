// A lot of this code was ripped from the cranelift examples page.
use cranelift::{
    module::{FuncId, Linkage, Module},
    prelude::*,
};

use crate::{
    passes::backend::target::native::IRConverter,
    resource::rep::backend::{
        native::{Closure, PointeeType, VirtualValue},
        types::LIRType,
    },
};

impl<'bctx, 'module> IRConverter<'bctx, 'module> {
    pub fn signature_from_decl(&self, func: FuncId) -> Signature {
        self.module
            .declarations()
            .get_function_decl(func)
            .signature
            .clone()
    }

    pub fn call_closure(&mut self, closure: Closure, params: &[VirtualValue]) -> VirtualValue {
        let mut real_params = vec![*closure.captures.clone()];
        real_params.extend_from_slice(params);
        let sigref = self.builder.import_signature(closure.sig.clone());
        let func = self.as_value(&closure.func);
        let new_params = real_params
            .iter()
            .map(|param| self.as_value(param))
            .collect::<Vec<_>>();
        let call = self.builder.ins().call_indirect(sigref, func, &new_params);
        let ty = closure.ty.destructure_closure().1;
        match self.builder.inst_results(call) {
            [res] => VirtualValue::Scalar(*res),
            [] => params[0].clone(),
            res => VirtualValue::UnstableStruct {
                ty,
                fields: res.iter().map(|val| VirtualValue::Scalar(*val)).collect(),
            },
        }
    }

    pub fn construct_closure(&mut self, closure_id: FuncId, captures: &[VirtualValue]) -> Closure {
        let boxed_captures = self.stack_alloc_values(captures.to_vec());
        let (forwarding_func_ref, sig) = {
            let capture_types = captures
                .iter()
                .map(|v| {
                    let v = self.as_value(v);
                    self.type_of_value(v)
                })
                .collect::<Vec<_>>();

            let (func_id, sig) = self.create_forwarding_func(closure_id, &capture_types);

            // let fref = self.module.declare_func_in_func(func_id, self.builder.func);
            // let size_t = self.module.isa().pointer_type();
            //(self.builder.ins().func_addr(size_t, fref), sig) //TODO use function indication for virualvalue

            (func_id, sig)
        };

        let name = self.types.function_names.get_by_left(&closure_id).unwrap();
        let (args, ret) = self.types.function_types.get(name).unwrap();
        let ty = args.iter().fold(*ret, |prev, c| LIRType::closure(*c, prev));

        Closure {
            ty,
            captures: Box::new(boxed_captures),
            func: Box::new(VirtualValue::Func(forwarding_func_ref)),
            sig,
        }
    }

    // If we have a closure with the user-facing signature `(int, int) -> int`
    //
    // Then the closure's actual signature will be `(*void, int, int) -> int`,
    // where `*void` represents a pointer to the captures.
    //
    // We need to dereference those captures and forward them to the real function defined where the
    // closure is created (in this example `f0_real_function` and `f1_real_function`), which this
    // "forwarding function" takes care of.
    //
    // So for the `f1` we'd define.
    //
    // ```
    // fn closure_forward_f1_real_function(captures: *void, x: int) -> int {
    //   let a = *(captures + 0);
    //   let b = *(captures + 4);
    //   return f1_real_function(a, b, x);
    // }
    // ```
    //
    // And then the actual values will be passed around in memory.
    // ```
    // let closure = { data: alloc([1, 2]), func: closure_forward_f1_real_function };
    // ```
    //
    // So that it may be called as
    //
    // ```
    // closure.func(closure.data, 3)
    // ```
    fn create_forwarding_func(&mut self, f: FuncId, captys: &[Type]) -> (FuncId, Signature) {
        // In a real compiler, this symbol needs to be generated in a way that's guaranteed to be
        // unique. You could for example use source code spans, capture type information, or a global counter.
        let symbol = format!("closure_forward_{f}");
        let id = self.types.function_names.get_by_left(&f).unwrap();
        let mut sig = self.types.function_sigs[id].clone();
        // dbg!(&sig);
        // Define the signature of the forwarding function to be that of the closure signature but
        // with the opaque captures pointer added as the first parameter.
        // let sig = {
        //     let mut sig_params = Vec::new();

        //     // The implicit parameters from the capture will be replaced by an opaque pointer instead.
        let voidptr = self.module.isa().pointer_type();
        sig.params.insert(0, AbiParam::new(voidptr));

        //     let real_func_sig = self.signature_from_decl(f);
        //     for &p in real_func_sig.params.iter().skip(captys.len()) {
        //         sig_params.push(p);
        //     }
        //     let returns = real_func_sig.returns.clone();

        //     self.types.make_sig(CallConv::Fast, sig_params, returns);
        //     sig
        // };

        // Declare the closure forwarding function
        let func_id = self
            .module
            .declare_function(&symbol, Linkage::Local, &sig)
            .unwrap();

        // Define the contents of the closure forwarding function
        {
            let mut ctx = codegen::Context::new();
            let mut fctx = FunctionBuilderContext::new();

            let mut closure_builder = FunctionBuilder::new(&mut ctx.func, &mut fctx);
            closure_builder.func.signature = sig.clone();

            let block = closure_builder.create_block();
            closure_builder.append_block_params_for_function_params(block);
            closure_builder.switch_to_block(block);

            let mut real_call_params =
                Vec::with_capacity(captys.len() + closure_builder.func.signature.params.len() - 1);

            // Dereference the captures and add them as implicit parameters
            // let mut offset = 0;
            // for &ty in captys {
            //     let ptr = closure_builder.block_params(block)[0];
            //     let v = closure_builder.ins().load(ty, MemFlags::new(), ptr, offset);
            //     real_call_params.push(v);
            //     offset += ty.bytes() as i32;
            // }

            // Add all other parameters from the forwarding function
            for &v in &closure_builder.block_params(block)[1..] {
                real_call_params.push(v);
            }

            let f_ref = self.module.declare_func_in_func(f, closure_builder.func);

            let call = closure_builder.ins().call(f_ref, &real_call_params);
            let returned = closure_builder.inst_results(call).to_vec();
            closure_builder.ins().return_(&returned);

            self.module
                .define_function(func_id, &mut ctx)
                .expect("Could not define forwarding function");
        };

        (func_id, sig)
    }

    fn alignment_of_scalar_type(of: &Type) -> u32 {
        of.bytes()
    }

    pub fn alignment_of_struct(fields: &[Type]) -> u32 {
        let mut alignment = 0;

        // Since we don't have nested structs, the alignment of a struct is simply its largest field.
        for field in fields {
            let field_alignment = Self::alignment_of_scalar_type(field);
            alignment = alignment.max(field_alignment);
        }

        alignment
    }
    pub fn size_of_struct(fields: &[Type]) -> u32 {
        // let fields = fields.collect();
        let mut size = 0;

        // Go through all fields and increment size by each fields size and padding
        for field in fields {
            size += field.bytes();

            // Add padding to ensure the field is aligned
            let align = Self::alignment_of_scalar_type(field);
            let padding = (align - size % align) % align;
            size += padding;
        }

        // Add padding to the end of the struct to make the struct itself aligned
        let self_align = Self::alignment_of_struct(fields);
        let end_padding = (self_align - size % self_align) % self_align;
        size += end_padding;

        size
    }

    pub fn offset_of_field(field: usize, fields: &[Type]) -> i32 {
        let mut offset = 0;

        // Go through all fields prior to this one and increment offset by their size and padding
        for prior in fields.iter().take(field) {
            offset += prior.bytes() as i32;

            // Add padding to ensure the field is aligned
            let align = Self::alignment_of_scalar_type(prior) as i32;
            let padding = (align - offset % align) % align;
            offset += padding;
        }

        offset
    }

    pub fn stack_alloc_values(&mut self, captures: Vec<VirtualValue>) -> VirtualValue {
        let size_t = self.module.isa().pointer_type();
        let v_values: Vec<_> = captures.into_iter().map(|v| self.as_value(&v)).collect();
        let types = v_values
            .iter()
            .map(|v| self.type_of_value(*v))
            .collect::<Vec<_>>();
        let size = Self::size_of_struct(&types);

        // Create the stack slot for the captures
        let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            size,
            0,
        ));

        // Write our captures to the stack allocation
        let mut offset = 0;
        for (i, v) in v_values.iter().enumerate() {
            self.builder.ins().stack_store(*v, slot, offset);
            offset += Self::offset_of_field(i, &types);
        }

        // Return the pointer
        VirtualValue::Pointer(
            PointeeType::Struct,
            self.builder.ins().stack_addr(size_t, slot, 0),
        )
    }

    pub fn stack_alloc_types(&mut self, types: &[Type]) -> Value {
        let size_t = self.module.isa().pointer_type();

        let size = Self::size_of_struct(types);

        // Create the stack slot for the captures
        let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            size,
            0,
        ));

        // Return the pointer
        self.builder.ins().stack_addr(size_t, slot, 0)
    }

    pub fn type_of_value(&self, v: Value) -> Type {
        self.builder.func.stencil.dfg.value_type(v)
    }
}
