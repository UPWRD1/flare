use cranelift::{
    module::{FuncId, Linkage, Module},
    prelude::{isa::CallConv, *},
};

use crate::{
    passes::backend::target::native::IRConverter,
    resource::rep::backend::native::{Closure, VirtualValue},
};

impl<'bctx, 'module> IRConverter<'bctx, 'module> {
    pub fn signature_from_decl(&self, func: FuncId) -> Signature {
        self.module
            .declarations()
            .get_function_decl(func)
            .signature
            .clone()
    }

    pub fn construct_closure(&mut self, closure_id: FuncId, captures: &[Value]) -> Closure {
        let boxed_captures = self.stack_alloc_values(captures);

        let (forwarding_func_ref, sig) = {
            let capture_types = captures
                .iter()
                .map(|&v| self.type_of_value(v))
                .collect::<Vec<_>>();

            let (func_id, sig) = self.create_forwarding_func(closure_id, &capture_types);

            let fref = self.module.declare_func_in_func(func_id, self.builder.func);
            let size_t = self.module.isa().pointer_type();
            (self.builder.ins().func_addr(size_t, fref), sig) //TODO use function indication for virualvalue
        };

        Closure {
            data: Box::new(VirtualValue::Scalar(boxed_captures)),
            func: Box::new(VirtualValue::Scalar(forwarding_func_ref)),
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

        // Define the signature of the forwarding function to be that of the closure signature but
        // with the opaque captures pointer added as the first parameter.
        let sig = {
            let mut sig = Signature::new(CallConv::Fast);

            // The implicit parameters from the capture will be replaced by an opaque pointer instead.
            let voidptr = AbiParam::new(self.module.isa().pointer_type());
            sig.params.insert(0, voidptr);

            let real_func_sig = self.signature_from_decl(f);
            for &p in real_func_sig.params.iter().skip(captys.len()) {
                sig.params.push(p);
            }
            sig.returns = real_func_sig.returns.clone();

            sig
        };

        // Declare the closure forwarding function
        let func_id = self
            .module
            .declare_function(&symbol, Linkage::Local, &sig)
            .unwrap();

        // Define the contents of the closure forwarding function
        {
            let mut ctx = codegen::Context::new();
            let mut fctx = FunctionBuilderContext::new();

            let mut closure = FunctionBuilder::new(&mut ctx.func, &mut fctx);
            closure.func.signature = sig.clone();

            let block = closure.create_block();
            closure.append_block_params_for_function_params(block);
            closure.switch_to_block(block);

            let mut real_call_params =
                Vec::with_capacity(captys.len() + closure.func.signature.params.len() - 1);

            // Dereference the captures and add them as implicit parameters
            let mut offset = 0;
            for &ty in captys {
                let ptr = closure.block_params(block)[0];
                let v = closure.ins().load(ty, MemFlags::new(), ptr, offset);
                real_call_params.push(v);
                offset += ty.bytes() as i32;
            }

            // Add all other parameters from the forwarding function
            for &v in &closure.block_params(block)[1..] {
                real_call_params.push(v);
            }

            let f_ref = self.module.declare_func_in_func(f, closure.func);
            let call = closure.ins().call(f_ref, &real_call_params);
            let returned = closure.inst_results(call).to_vec();
            closure.ins().return_(&returned);

            self.module
                .define_function(func_id, &mut ctx)
                .expect("Could not define forwarding function");
        };

        (func_id, sig)
    }

    pub fn declare_real_function_for_closure(&mut self) -> FuncId {
        todo!()
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

    pub fn stack_alloc_values(&mut self, captures: &[Value]) -> Value {
        let size_t = self.module.isa().pointer_type();

        let types = captures
            .iter()
            .map(|&v| self.type_of_value(v))
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
        for (i, &v) in captures.iter().enumerate() {
            self.builder.ins().stack_store(v, slot, offset);
            offset += Self::offset_of_field(i, &types);
        }

        // Return the pointer
        self.builder.ins().stack_addr(size_t, slot, 0)
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
