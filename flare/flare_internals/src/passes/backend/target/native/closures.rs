// A lot of this code was ripped from the cranelift examples page.
use cranelift::{
    module::{FuncId, Linkage, Module},
    prelude::*,
};

use crate::{
    passes::backend::target::native::IRConverter,
    resource::rep::backend::{
        native::{Closure, StructPassingMode, VirtualValue},
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
        // dbg!(&closure.sig);
        // dbg!(&closure.captures, params);
        let mut real_params = vec![*closure.captures];
        real_params.extend_from_slice(params);
        // let real_params = params;
        // dbg!(&real_params);
        let sigref = self.builder.import_signature(closure.sig.clone());
        let func = self.as_values(&closure.func)[0];
        let new_params = real_params
            .iter()
            .flat_map(|param| self.as_values(param))
            .collect::<Vec<_>>();

        let call = self.builder.ins().call_indirect(sigref, func, &new_params);
        let ty = closure.ty.destructure_closure().1;
        let mut register_returns = self.builder.inst_results(call).to_vec().into_iter();
        self.type_to_virtual_value(&mut |_, _| register_returns.next().unwrap(), false, ty)
    }

    pub fn construct_closure(&mut self, closure_id: FuncId, captures: &[VirtualValue]) -> Closure {
        let closure_env = if let Some(closure_env @ LIRType::ClosureEnv(_, _)) = self
            .types
            .function_types
            .get(self.types.function_names.get_by_left(&closure_id).unwrap())
            .unwrap()
            .0
            .first()
        {
            closure_env.closure_to_struct_rep()
        } else {
            panic!("Bad Closure")
        };

        let (boxed_captures, is_captures_by_pointer) = {
            if self.types.struct_passing_mode(closure_env) == StructPassingMode::ByPointer {
                (
                    self.stack_alloc_captures(closure_env, captures.to_vec()),
                    true,
                )
            } else {
                let capture_struct = VirtualValue::UnstableStruct {
                    ty: closure_env,
                    fields: captures.to_vec(),
                };
                (capture_struct, false)
            }
        };
        // dbg!(&boxed_captures);
        let (forwarding_func_id, sig) =
            self.create_forwarding_func(closure_id, captures, is_captures_by_pointer);
        let ty = {
            let name = self.types.function_names.get_by_left(&closure_id).unwrap();
            let (args, ret) = self.types.function_types.get(name).unwrap();
            args.iter().fold(*ret, |prev, c| LIRType::closure(*c, prev))
        };

        Closure {
            ty,
            captures: Box::new(boxed_captures),

            func: Box::new(VirtualValue::Func(forwarding_func_id)),
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
    fn create_forwarding_func(
        &mut self,
        f: FuncId,
        captures: &[VirtualValue],
        is_captures_by_pointer: bool,
    ) -> (FuncId, Signature) {
        let captys: Vec<_> = {
            let vvs = captures
                .iter()
                .flat_map(|vv| self.as_values(vv))
                .collect::<Vec<_>>();
            vvs.into_iter().map(|v| self.type_of_value(v)).collect()
        };
        // dbg!(&captys);
        // In a real compiler, this symbol needs to be generated in a way that's guaranteed to be
        // unique. You could for example use source code spans, capture type information, or a global counter.
        let symbol = format!("closure_forward_{f}");

        let mut sig = self.signature_from_decl(f);

        // Declare the closure forwarding function
        let func_id = self
            .module
            .declare_function(&symbol, Linkage::Local, &sig)
            .unwrap();

        // Define the contents of the closure forwarding function
        {
            let mut ctx = codegen::Context::new();
            let mut fctx = FunctionBuilderContext::new();

            let mut converter = IRConverter::new(&mut ctx.func, &mut fctx, self.module, self.types);
            converter.builder.func.signature = sig.clone();

            let real_call_params =
                Self::build_closure_args(is_captures_by_pointer, captys, &mut converter.builder);
            let val = converter.call_func(VirtualValue::Func(f), real_call_params);
            converter.return_(val);

            self.module
                .define_function(func_id, &mut ctx)
                .expect("Could not define forwarding function");
        };

        (func_id, sig)
    }

    fn build_closure_args(
        is_captures_by_pointer: bool,
        captys: Vec<Type>,
        closure_builder: &mut FunctionBuilder<'_>,
    ) -> Vec<VirtualValue> {
        let block = closure_builder.create_block();
        closure_builder.append_block_params_for_function_params(block);
        closure_builder.switch_to_block(block);
        if is_captures_by_pointer {
            let mut real_call_params =
                Vec::with_capacity(captys.len() + closure_builder.func.signature.params.len() - 1); // Dereference the captures and add them as implicit parameters
            let mut offset = 0;
            for (idx, &ty) in captys.iter().enumerate() {
                let ptr = closure_builder.block_params(block)[0];
                let v = VirtualValue::Scalar(closure_builder.ins().load(
                    ty,
                    MemFlags::new(),
                    ptr,
                    offset,
                ));
                real_call_params.push(v);
                offset = Self::offset_of_field(idx, &captys);
            }
            // Add all other parameters from the forwarding function
            for &v in closure_builder
                .block_params(block)
                .iter()
                .skip(captys.len())
            {
                real_call_params.push(VirtualValue::Scalar(v));
            }
            real_call_params
        } else {
            let mut real_call_params =
                Vec::with_capacity(closure_builder.func.signature.params.len());
            // Add all  parameters from the forwarding function AT ONCE
            for &v in closure_builder.block_params(block) {
                real_call_params.push(VirtualValue::Scalar(v));
            }
            real_call_params
        }
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
    pub fn stack_alloc_captures(
        &mut self,
        ty: LIRType,
        captures: Vec<VirtualValue>,
    ) -> VirtualValue {
        let pointer = self.stack_alloc_vvalues(captures);
        VirtualValue::StackStruct { ty, ptr: pointer }
    }

    pub fn stack_alloc_vvalues(&mut self, values: Vec<VirtualValue>) -> Value {
        let size_t = self.module.isa().pointer_type();
        let v_values: Vec<_> = values
            .into_iter()
            .flat_map(|v| self.as_values(&v))
            .collect();
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
        self.builder.ins().stack_addr(size_t, slot, 0)
    }

    pub fn stack_alloc_values(&mut self, values: Vec<Value>) -> Value {
        let size_t = self.module.isa().pointer_type();

        let types = values
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
        for (i, v) in values.iter().enumerate() {
            self.builder.ins().stack_store(*v, slot, offset);
            offset += Self::offset_of_field(i, &types);
        }
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

    pub fn type_of_virtual_value(&mut self, v: &VirtualValue) -> Vec<Type> {
        self.as_values(v)
            .into_iter()
            .map(|v| self.type_of_value(v))
            .collect()
    }
}
