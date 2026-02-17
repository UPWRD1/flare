// A lot of this code was ripped from the cranelift examples page.
use cranelift::{
    module::{FuncId, Module},
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

    pub fn call_closure(&mut self, closure: Closure, params: &[Value]) -> VirtualValue {
        // dbg!(&closure.sig);
        // dbg!(&closure.captures, params);
        let mut real_params: Vec<_> = [*closure.captures]
            .iter()
            .flat_map(|v| self.as_values(v))
            .collect();
        real_params.extend_from_slice(params);
        // let real_params = params;
        // dbg!(&real_params);
        let sigref = self.builder.import_signature(closure.sig.clone());
        let func = self.as_values(&closure.func)[0];

        let call = self.builder.ins().call_indirect(sigref, func, &real_params);
        let ty = closure.func.ty().destructure_closure().1;
        let mut register_returns = self.builder.inst_results(call).to_vec().into_iter();
        self.type_to_virtual_value(
            &mut |_, _| {
                register_returns
                    .next()
                    .expect("Out of register retunrs, but expected more")
            },
            false,
            ty,
        )
    }

    pub fn construct_closure(&mut self, closure_id: FuncId, captures: &[VirtualValue]) -> Closure {
        let closure_env = if let Some(closure_env @ LIRType::ClosureEnv(_, _)) = self
            .types
            .function_types
            .get(&closure_id)
            .expect("Could not get closure env")
            .0
            .first()
        {
            closure_env.closure_to_struct_rep()
        } else {
            panic!("Bad Closure")
        };

        let (boxed_captures, is_captures_by_pointer) = {
            if self.types.struct_passing_mode(&closure_env) == StructPassingMode::ByPointer {
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

        let ty = {
            let (args, ret) = self.types.function_types.get(&closure_id).unwrap();
            LIRType::closure(args, *ret)
        };
        // dbg!(&boxed_captures);
        let (forwarding_func_id, sig) =
            self.create_forwarding_func(closure_id, ty, captures, is_captures_by_pointer);

        Closure {
            // ty,
            captures: Box::new(boxed_captures),

            func: Box::new(VirtualValue::Func(forwarding_func_id, ty)),
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
        ty: LIRType,
        captures: &[VirtualValue],
        is_captures_by_pointer: bool,
    ) -> (FuncId, Signature) {
        // let captys: Vec<_> = {
        //     let vvs = captures
        //         .iter()
        //         .flat_map(|vv| self.as_values(vv))
        //         .collect::<Vec<_>>();
        //     vvs.into_iter().map(|v| self.type_of_value(v)).collect()
        // };
        let capt_struct = VirtualValue::UnstableStruct {
            ty: LIRType::Struct(
                captures
                    .iter()
                    .map(|c| c.ty())
                    .collect::<Vec<_>>()
                    .as_slice()
                    .into(),
            ),
            fields: captures.to_vec(),
        };
        // dbg!(&captys);
        // In a real compiler, this symbol needs to be generated in a way that's guaranteed to be
        // unique. You could for example use source code spans, capture type information, or a global counter.
        let symbol = format!("closure_forward_{f}");

        let sig = self.signature_from_decl(f);

        // Declare the closure forwarding function
        // let func_id = self
        //     .module
        //     .declare_function(&symbol, Linkage::Local, &sig)
        //     .unwrap();

        let func_id = self.module.declare_anonymous_function(&sig).unwrap();

        // Define the contents of the closure forwarding function
        {
            let mut ctx = codegen::Context::new();
            let mut fctx = FunctionBuilderContext::new();

            let mut converter = IRConverter::new(&mut ctx.func, &mut fctx, self.module, self.types);
            converter.builder.func.signature = sig.clone();

            let real_call_params =
                converter.build_closure_args(is_captures_by_pointer, ty, &capt_struct);
            let val = converter.call_func(VirtualValue::Func(f, ty), real_call_params);
            converter.return_(val);

            self.module
                .define_function(func_id, &mut ctx)
                .expect("Could not define forwarding function");
        };

        (func_id, sig)
    }

    fn build_closure_args(
        &mut self,
        is_captures_by_pointer: bool,
        ty: LIRType,
        // captys: &[LIRType]
        capt_struct: &VirtualValue,
    ) -> Vec<VirtualValue> {
        // dbg!(capt_struct);
        let block = self.builder.create_block();
        self.builder.append_block_params_for_function_params(block);
        self.builder.switch_to_block(block);
        let captys = capt_struct.ty().into_struct_fields();
        let (args, ret) = ty.destructure_closure();
        if is_captures_by_pointer {
            let mut real_call_params =
                Vec::with_capacity(captys.len() + self.builder.func.signature.params.len() - 1); // Dereference the captures and add them as implicit parameters
            // let mut offset = 0;
            // for (idx, &ty) in captys.iter().enumerate() {
            //     let ptr = closure_builder.block_params(block)[0];
            //     let v = VirtualValue::Scalar(closure_builder.ins().load(
            //         ty,
            //         MemFlags::new(),
            //         ptr,
            //         offset,
            //     ));
            //     real_call_params.push(v);
            //     offset = Self::offset_of_field(idx, captys);
            // }

            for (idx, &ty) in captys.iter().enumerate() {
                // let ptr = self.builder.block_params(block)[0];
                // let offset = Self::offset_of_field(idx, captys);
                // let offset = self.offset_of_lir_field(&ty, idx);
                // let v = VirtualValue::Scalar(closure_builder.ins().load(
                //     ty,
                //     MemFlags::new(),
                //     ptr,
                //     offset,
                // ));
                let v = self.destruct_field(capt_struct, idx);
                real_call_params.push(v);
            } // Add all other parameters from the forwarding function
            for (i, &v) in self
                .builder
                .block_params(block)
                .iter()
                .skip(captys.len())
                .enumerate()
            {
                real_call_params.push(VirtualValue::Scalar(v, args[i]));
            }
            real_call_params
        } else {
            let mut real_call_params = Vec::with_capacity(self.builder.func.signature.params.len());
            // Add all  parameters from the forwarding function AT ONCE
            for (i, &v) in self.builder.block_params(block).iter().enumerate() {
                real_call_params.push(VirtualValue::Scalar(v, args[i]));
            }
            real_call_params
        }
    }

    fn alignment_of_scalar_type(of: &Type) -> u32 {
        of.bytes()
    }

    // pub fn alignment_of_struct(fields: &[Type]) -> u32 {
    //     todo!()
    // }

    pub fn alignment_of_lir_type(&self, ty: &LIRType) -> u32 {
        let ptr_bytes = self.module.isa().pointer_bytes() as u32;
        match ty {
            LIRType::Int => 4,
            LIRType::Float => 4,
            LIRType::Unit => 1,
            LIRType::String | LIRType::Closure(..) => ptr_bytes,
            LIRType::Struct(fields) => fields
                .iter()
                .map(|f| self.alignment_of_lir_type(f))
                .max()
                .unwrap_or(1), // empty struct → 1-byte aligned (no divide-by-zero)
            LIRType::Union(variants) => {
                // tag is i32 (4 bytes); payload alignment drives the overall alignment.
                let payload_align = variants
                    .iter()
                    .map(|v| self.alignment_of_lir_type(v))
                    .max()
                    .unwrap_or(1);
                4_u32.max(payload_align)
            }
            LIRType::ClosureEnv(..) => {
                let s = ty.closure_to_struct_rep();
                self.alignment_of_lir_type(&s)
            }
        }
    }

    pub fn size_of_lir_type(&self, ty: &LIRType) -> u32 {
        let ptr_bytes = self.module.isa().pointer_bytes() as u32;
        match ty {
            LIRType::Int => 4,
            LIRType::Float => 4,
            LIRType::Unit => 1,
            LIRType::String | LIRType::Closure(..) => ptr_bytes,
            LIRType::Struct(fields) => {
                let mut size: u32 = 0;
                for f in fields.iter() {
                    let align = self.alignment_of_lir_type(f);
                    // Pad so this field starts at its required alignment.
                    size = align_up(size, align);
                    size += self.size_of_lir_type(f);
                }
                // Trailing padding so the struct size is a multiple of its alignment.
                let struct_align = self.alignment_of_lir_type(ty);
                align_up(size, struct_align)
            }
            LIRType::Union(variants) => {
                // Layout: [i32 tag][padding][payload]
                // The whole thing is sized to hold any variant.
                let tag_size: u32 = 4;
                let payload_align = variants
                    .iter()
                    .map(|v| self.alignment_of_lir_type(v))
                    .max()
                    .unwrap_or(1);
                let payload_size = variants
                    .iter()
                    .map(|v| self.size_of_lir_type(v))
                    .max()
                    .unwrap_or(0);
                // Pad after tag so payload starts at its alignment.
                let payload_offset = align_up(tag_size, payload_align);
                let total = payload_offset + payload_size;
                // Trailing padding.
                let union_align = self.alignment_of_lir_type(ty);
                align_up(total, union_align)
            }
            LIRType::ClosureEnv(..) => {
                let s = ty.closure_to_struct_rep();
                self.size_of_lir_type(&s)
            }
        }
    }

    // Byte offset of field `field_idx` within a struct `LIRType`.
    // Panics if `ty` is not a `Struct` or `ClosureEnv`.
    pub fn offset_of_lir_field(&self, ty: &LIRType, field_idx: usize) -> u32 {
        let fields: Vec<LIRType> = match ty {
            LIRType::Struct(f) => f.to_vec(),
            LIRType::ClosureEnv(..) => {
                let s = ty.closure_to_struct_rep();
                return self.offset_of_lir_field(&s, field_idx);
            }
            _ => panic!("offset_of_lir_field called on non-struct type: {ty:?}"),
        };

        let mut offset: u32 = 0;
        for (i, f) in fields.iter().enumerate() {
            let align = self.alignment_of_lir_type(f);
            offset = align_up(offset, align);
            if i == field_idx {
                return offset;
            }
            offset += self.size_of_lir_type(f);
        }
        panic!("field_idx {field_idx} out of bounds for {ty:?}");
    }

    pub fn stack_alloc_captures(
        &mut self,
        ty: LIRType,
        captures: Vec<VirtualValue>,
    ) -> VirtualValue {
        let pointer = self.stack_alloc_vvalues(captures, ty);
        VirtualValue::StackStruct { ty, ptr: pointer }
    }

    pub fn stack_alloc_vvalues(&mut self, vvalues: Vec<VirtualValue>, ty: LIRType) -> Value {
        let size_t = self.module.isa().pointer_type();
        let values: Vec<_> = vvalues
            .into_iter()
            .flat_map(|v| self.as_values(&v))
            .collect();
        self.stack_alloc_values(&values, ty)
    }

    pub fn stack_alloc_values(&mut self, values: &[Value], ty: LIRType) -> Value {
        let size_t = self.module.isa().pointer_type();
        let size = self.size_of_lir_type(&ty);

        // Create the stack slot for the captures
        let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            size,
            0,
        ));

        // Write our captures to the stack allocation
        // let mut offset = 0;
        // for (i, v) in values.iter().enumerate() {
        //     self.builder.ins().stack_store(*v, slot, offset);
        //     offset += Self::offset_of_field(i, &types);
        // }

        for (i, v) in values.iter().enumerate() {
            let offset = self.offset_of_lir_field(&ty, i);
            self.builder.ins().stack_store(*v, slot, offset as i32);
        }
        self.builder.ins().stack_addr(size_t, slot, 0)
    }

    pub fn stack_alloc_lir_type(&mut self, ty: &LIRType) -> Value {
        let size_t = self.module.isa().pointer_type();

        let size = self.size_of_lir_type(ty);

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

#[inline]
pub fn align_up(offset: u32, align: u32) -> u32 {
    debug_assert!(align.is_power_of_two(), "alignment must be a power of two");
    (offset + align - 1) & !(align - 1)
}
