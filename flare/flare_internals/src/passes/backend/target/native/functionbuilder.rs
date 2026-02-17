use std::cmp::Ordering;

use cranelift::{
    codegen::ir::{ArgumentPurpose, BlockCall},
    frontend::FuncInstBuilder,
    module::{FuncId, Module},
    prelude::*,
};

use crate::{
    passes::backend::target::native::IRConverter,
    resource::rep::backend::{
        lir::Var,
        native::{PayloadKind, PointeeType, StructPassingMode, VirtualValue},
        types::LIRType,
    },
};

#[derive(Debug)]
pub enum Param {
    Novar(LIRType),
    Var(Var),
}
impl<'bctx, 'module> IRConverter<'bctx, 'module> {
    pub fn ins(&mut self) -> FuncInstBuilder<'_, 'bctx> {
        self.builder.ins()
    }

    pub fn as_values(&mut self, value: &VirtualValue) -> Vec<Value> {
        match value {
            VirtualValue::Scalar(value) => vec![*value],
            VirtualValue::Pointer(_, ptr) => vec![*ptr],
            VirtualValue::StackStruct { ty, ptr } => {
                vec![*ptr]
            }
            VirtualValue::UnstableStruct { ty, fields } => {
                fields.iter().flat_map(|vv| self.as_values(vv)).collect()
            }
            VirtualValue::Func(func_id) => {
                let ptr = self.types.ptr_type();
                let fref = self
                    .module
                    .declare_func_in_func(*func_id, self.builder.func);
                vec![self.ins().func_addr(ptr, fref)]
            }
            VirtualValue::Closure(c) => {
                let mut vec = self.as_values(&c.captures);
                vec.extend(self.as_values(&c.func));
                vec
            }
            VirtualValue::TaggedUnion {
                // variants,
                body,
                tag,
            } => {
                vec![*tag, *body]
            }
        }
    }

    pub fn int(&mut self, n: impl Into<i64>) -> VirtualValue {
        let v = self.ins().iconst(types::I32, n.into());
        VirtualValue::Scalar(v)
    }

    pub fn float(&mut self, n: impl Into<f32>) -> VirtualValue {
        let v = self.ins().f32const(n.into());
        VirtualValue::Scalar(v)
    }

    pub fn unit(&mut self) -> VirtualValue {
        let v = self.ins().iconst(types::I8, 0i64);
        VirtualValue::Scalar(v)
    }

    // Turns a parameter from our source language into Cranelift block parameters.
    //
    // Since Cranelift parameters can only be primitive types, a single struct will either
    // become a single Cranelift pointer block parameter or multiple block parameters.
    pub fn type_to_block_params(
        &mut self,
        block: Block,
        is_root: bool,
        p: LIRType,
    ) -> VirtualValue {
        self.type_to_virtual_value(
            &mut |this, clty| this.builder.append_block_param(block, clty),
            is_root,
            p,
        )
    }

    // Maps our abstract Type to our abstract VirtualValue
    pub fn type_to_virtual_value<F>(&mut self, f: &mut F, is_root: bool, p: LIRType) -> VirtualValue
    where
        F: FnMut(&mut Self, Type) -> Value,
    {
        match p {
            LIRType::Int => {
                let v = f(self, types::I32);
                VirtualValue::Scalar(v)
            }
            LIRType::Float => {
                let v = f(self, types::F32);
                VirtualValue::Scalar(v)
            }
            LIRType::String => {
                let size_t = self.module.isa().pointer_type();
                let ptr = f(self, size_t);
                VirtualValue::Pointer(PointeeType::String, ptr)
            }
            LIRType::Closure(..) => {
                // dbg!(r);
                // let vv= self.type_to_virtual_value(f, is_root, *r);
                // let res= self.as_value(vv);
                let size_t = self.module.isa().pointer_type();
                let ptr = f(self, size_t);
                let (args, ret) = p.destructure_closure();
                VirtualValue::Pointer(PointeeType::Func(args, ret), ptr)
            }
            LIRType::Struct(ty) => {
                if is_root && self.types.struct_passing_mode(&p) == StructPassingMode::ByPointer {
                    let size_t = self.module.isa().pointer_type();
                    let ptr = f(self, size_t);
                    VirtualValue::StackStruct { ty: p, ptr }
                } else {
                    let fields = ty
                        .iter()
                        .map(|ty| self.type_to_virtual_value(f, false, *ty))
                        .collect();

                    VirtualValue::UnstableStruct { ty: p, fields }
                }
            }
            LIRType::ClosureEnv(..) => {
                let closureenv = p.closure_to_struct_rep();
                self.type_to_virtual_value(f, is_root, closureenv)
            }
            LIRType::Unit => {
                let v = f(self, types::I8);
                VirtualValue::Scalar(v)
            }
            LIRType::Union(variants) => {
                let size_t = self.module.isa().pointer_type();
                let tag = f(self, types::I32);
                // let t = self.builder.func.dfg.value_def(tag).unwrap_inst();
                // dbg!(t);
                // let body_vv = self.type_to_virtual_value(f, false, variants[idx]);
                // let body = self.as_values(&body_vv);
                let body = f(self, size_t);
                VirtualValue::TaggedUnion { body, tag }
            }
        }
    }

    // Turns our virtual values into Cranelift parameters for the call instruction.
    //
    // Since Cranelift parameters can only be primitive types, a single struct will either
    // become a single Cranelift pointer value or multiple Cranelift values.
    fn virtual_value_to_func_params(&mut self, buf: &mut Vec<Value>, v: VirtualValue) {
        match v {
            VirtualValue::Scalar(value) => buf.push(value),
            VirtualValue::StackStruct { ty, ptr: src } => {
                match self.types.struct_passing_mode(&ty) {
                    StructPassingMode::ByScalars => {
                        self.deref_fields(buf, ty, src, 0);
                    }
                    StructPassingMode::ByPointer => buf.push(src),
                }
            }
            VirtualValue::UnstableStruct { ty, fields } => {
                match self.types.struct_passing_mode(&ty) {
                    StructPassingMode::ByScalars => self.virtual_values_to_func_params(buf, fields),
                    StructPassingMode::ByPointer => {
                        todo!();
                        // let field_cl_types = fields.iter().map(|f| todo!()).collect();

                        // let ptr = self.stack_alloc_struct(field_cl_types);
                        // for (field, v) in fields.into_iter().enumerate() {
                        //     self.write_struct_field(ty, field, ptr, v);
                        // }
                        // buf.push(ptr);
                    }
                }
            }
            VirtualValue::Closure(..) => {
                // dbg!(captures_value);
                let vals = self.as_values(&v);
                buf.extend(vals);

                // todo!()
            }
            VirtualValue::Func(f) => {
                let val = self.as_values(&v);
                buf.extend(val);
            }
            VirtualValue::TaggedUnion { body, tag } => {
                buf.push(tag);
                buf.push(body);
            }
            VirtualValue::Pointer(_, v) => buf.push(v),
            _ => todo!("{v:?}"),
        }
    }

    pub fn construct_struct(&mut self, ty: LIRType, fields: &[VirtualValue]) -> VirtualValue {
        VirtualValue::UnstableStruct {
            ty,
            fields: fields.to_vec(),
        }
    }

    pub fn destruct_field(&mut self, of: &VirtualValue, field: usize) -> VirtualValue {
        // dbg!(&of, field);
        match of {
            VirtualValue::Scalar(s) => panic!("cannot destruct field from non-struct: {s:?}"),

            VirtualValue::StackStruct { ty, ptr } => {
                let fields = self.types.struct_fields.get(ty).unwrap_or_else(|| {
                    panic!(
                        "Could not get {ty:?}, table: \n{:?}",
                        self.types.struct_fields
                    )
                });
                let offset = Self::offset_of_field(field, fields);
                let field_ty = ty.into_struct_fields()[field];
                // dbg!(field_ty);
                match field_ty {
                    // Instead of actually dereferencing the inner struct here,
                    // we create another implicit stack pointer that's offset to where the inner struct starts.
                    //
                    // This makes dereferencing lazy.
                    LIRType::Struct(type_) => {
                        let new_ptr = self.ins().iadd_imm(*ptr, offset as i64);
                        VirtualValue::StackStruct {
                            ty: *ty,
                            ptr: new_ptr,
                        }
                    }
                    LIRType::Int => {
                        let v = self.ins().load(types::I32, MemFlags::new(), *ptr, offset);
                        VirtualValue::Scalar(v)
                    }
                    LIRType::Float => {
                        let v = self.ins().load(types::F32, MemFlags::new(), *ptr, offset);
                        VirtualValue::Scalar(v)
                    }
                    LIRType::Unit => {
                        let v = self.ins().load(types::I8, MemFlags::new(), *ptr, offset);
                        VirtualValue::Scalar(v)
                    }
                    LIRType::Closure(l, r) => {
                        // let ret: Type = self
                        //     .types
                        //     .make_sig(isa::CallConv::Fast, vec![*l], *r)
                        //     .returns[0]
                        //     .value_type;
                        let pointer = self.types.ptr_type();
                        // let v = self.ins().load(ret, MemFlags::new(), *ptr, offset);
                        let v = self.ins().load(pointer, MemFlags::new(), *ptr, offset);
                        VirtualValue::Pointer(PointeeType::Func(vec![*l], *r), v)
                    }
                    _ => todo!("{field_ty:?}"),
                }
            }

            VirtualValue::UnstableStruct { fields, ty } => {
                // if let LIRType::ClosureEnv(..) = ty {
                //     self.destruct_field(&fields[1], field - 1)
                //     // fields[field - 1].clone()
                // } else {
                fields[field].clone()
                // }
            }
            _ => todo!(),
        }
    }

    /// Return a value, either by writing to the return struct out pointer or by returning values directly.
    pub fn return_(&mut self, vv: VirtualValue) {
        match vv {
            VirtualValue::Scalar(value) => {
                self.builder.ins().return_(&[value]);
            }
            VirtualValue::StackStruct { ty, ptr: src } => {
                match self.types.struct_passing_mode(&ty) {
                    // We have a stack pointer but want to return in return registers
                    StructPassingMode::ByScalars => {
                        let mut buf = vec![];
                        self.deref_fields(&mut buf, ty, src, 0);
                        self.ins().return_(&buf);
                    }
                    // We have a stack pointer and we want to return by writing to the out pointer
                    StructPassingMode::ByPointer => {
                        let dst = self.struct_return_pointer();
                        self.copy_struct_fields(ty, src, dst);
                        self.ins().return_(&[]);
                    }
                }
            }
            VirtualValue::UnstableStruct { ty, fields } => {
                match self.types.struct_passing_mode(&ty) {
                    StructPassingMode::ByScalars => {
                        let fields = fields
                            .iter()
                            .flat_map(|v| self.as_values(v))
                            .collect::<Vec<_>>();

                        self.builder.ins().return_(&fields);
                    }
                    // We have an abstract struct and we want to write the fields to an out pointer
                    StructPassingMode::ByPointer => {
                        let dst = self.struct_return_pointer();

                        for (field, v) in fields.into_iter().enumerate() {
                            self.write_struct_field(ty, field, dst, v);
                        }

                        self.ins().return_(&[]);
                    }
                }
            }
            // VirtualValue::Union { .. } => {
            //     let vals = self.as_values(&vv);
            //     self.ins().return_(&vals);
            // }
            VirtualValue::Func(f) => {
                // let vals = self.as_values(&vv);
                // self.ins().return_(&vals);
                let call = self.call_func(vv, vec![]);
                self.return_(call);
            }
            VirtualValue::TaggedUnion { .. } => {
                let vals = self.as_values(&vv);
                self.ins().return_(&vals);
            }
            VirtualValue::Pointer(_, v) => {
                self.ins().return_(&[v]);
            }
            VirtualValue::Closure(_) => {
                let vals = self.as_values(&vv);
                self.ins().return_(&vals);
            }
        }
    }

    fn deref_fields(&mut self, buf: &mut Vec<Value>, types: LIRType, src: Value, src_offset: i32) {
        let the_types = types.into_struct_fields();
        let fields = self
            .types
            .struct_fields
            .get(&types)
            .expect("Could not get struct field");
        for (field, ty) in fields.iter().enumerate() {
            let offset = Self::offset_of_field(field, fields) + src_offset;
            let fty = the_types[field];
            match fty {
                LIRType::Int => {
                    let v = self.ins().load(types::I32, MemFlags::new(), src, offset);
                    buf.push(v);
                }
                LIRType::Struct(type_) => {
                    self.deref_fields(buf, fty, src, offset);
                }
                _ => todo!(),
            }
        }
    }

    fn copy_struct_fields(&mut self, type_: LIRType, src: Value, dst: Value) {
        let the_types = type_.into_struct_fields();
        let fields = self.types.struct_fields.get(&type_).unwrap();
        for (field, fty) in fields.iter().enumerate() {
            let offset = Self::offset_of_field(field, fields);
            let fty = the_types[field];
            match fty {
                LIRType::Int => {
                    let n = self.ins().load(types::I32, MemFlags::new(), src, offset);

                    self.ins().store(MemFlags::new(), n, dst, offset);
                }
                LIRType::Struct(type_) => {
                    let src = self.ins().iadd_imm(src, offset as i64);
                    let dst = self.ins().iadd_imm(dst, offset as i64);

                    self.copy_struct_fields(fty, src, dst);
                }
                _ => todo!(),
            }
        }
    }

    fn write_struct_field(&mut self, name: LIRType, field: usize, ptr: Value, v: VirtualValue) {
        let fields = self.types.struct_fields.get(&name).unwrap();
        let offset = Self::offset_of_field(field, fields);

        match v {
            VirtualValue::Scalar(value) => {
                self.ins().store(MemFlags::new(), value, ptr, offset);
            }
            VirtualValue::UnstableStruct { ty, fields } => {
                let field_types = self.types.struct_fields.get(&ty).unwrap();
                for (field, v) in fields.into_iter().enumerate() {
                    // let offset = offset + self.types.offset_of_field(type_, field);
                    let offset = Self::offset_of_field(field, field_types);
                    let nptr = self.ins().iadd_imm(ptr, offset as i64);
                    self.write_struct_field(ty, field, nptr, v);
                }
            }

            VirtualValue::StackStruct {
                ty: src_type,
                ptr: src_ptr,
            } => {
                let nptr = self.ins().iadd_imm(ptr, offset as i64);
                self.copy_struct_fields(src_type, src_ptr, nptr);
            }
            _ => todo!(),
        }
    }
    // Get the pointer parameter declared by the `LookupTable::create_signature` method
    //
    // This will for most targets be the first parameter.
    fn struct_return_pointer(&self) -> Value {
        self.builder
            .func
            .special_param(ArgumentPurpose::StructReturn)
            .expect("current function does not return large struct")
    }

    fn virtual_values_to_func_params(&mut self, buf: &mut Vec<Value>, vs: Vec<VirtualValue>) {
        vs.into_iter()
            .for_each(|v| self.virtual_value_to_func_params(buf, v));
    }

    pub fn call_func(&mut self, func: VirtualValue, params: Vec<VirtualValue>) -> VirtualValue {
        let mut call_params = vec![];
        self.virtual_values_to_func_params(&mut call_params, params);
        self.call_func_with_values(func, call_params)
    }

    pub fn call_func_with_values(
        &mut self,
        func: VirtualValue,
        params: Vec<Value>,
    ) -> VirtualValue {
        match func {
            VirtualValue::Func(func_id) => self.call_direct_func(params, func_id),
            VirtualValue::Pointer(t, ptr) => match t {
                PointeeType::Func(from, to) => {
                    let sig = self
                        .types
                        .make_sig(self.module.isa().default_call_conv(), &from, to);

                    self.call_indirect_func(params, to, sig, ptr)
                }
                _ => unreachable!(),
            },
            VirtualValue::Closure(c) => self.call_closure(c, &params),
            _ => panic!("Invalid function node {func:?}"),
        }
    }

    fn call_direct_func(&mut self, mut call_params: Vec<Value>, func: FuncId) -> VirtualValue {
        let ret = self.types.return_type_of(func);
        // dbg!(ret);

        // If the return type is too large to fit in return registers, we allocate space for it in
        // the current stack frame and pass a pointer as the first parameter for the child function to
        // write its return values to.
        let mut out_ptr_return = None;
        if let LIRType::Struct(_) = ret
            && self.types.struct_passing_mode(&ret) == StructPassingMode::ByPointer
        {
            let fields = self.types.struct_fields.get(&ret).unwrap();
            let ptr = self.stack_alloc_types(fields);
            call_params.insert(0, ptr);
            out_ptr_return = Some(VirtualValue::StackStruct { ty: ret, ptr });
        }

        let mut register_returns = {
            // In order to call a function, we need to first map a global FuncId into a local FuncRef
            // inside the current.
            let fref = self.module.declare_func_in_func(func, self.builder.func);

            let call = self.ins().call(fref, &call_params);

            self.builder.inst_results(call).to_vec().into_iter()
        };
        // dbg!(&register_returns);

        // If the return values were handled through an out pointer, return that pointer
        // Otherwise; collect the returned scalar values into a VirtualValue to turn it back into our typed abstraction.
        out_ptr_return.unwrap_or_else(|| {
            self.type_to_virtual_value(&mut |_, _| register_returns.next().unwrap(), false, ret)
        })
    }

    fn call_indirect_func(
        &mut self,
        mut call_params: Vec<Value>,
        ret: LIRType,
        sig: Signature,
        ptr: Value,
    ) -> VirtualValue {
        // let ret = sig.returns;

        // If the return type is too large to fit in return registers, we allocate space for it in
        // the current stack frame and pass a pointer as the first parameter for the child function to
        // write its return values to.
        let mut out_ptr_return = None;
        if let LIRType::Struct(name) = ret
            && self.types.struct_passing_mode(&ret) == StructPassingMode::ByPointer
        {
            let fields = self.types.struct_fields.get(&ret).unwrap();
            let ptr = self.stack_alloc_types(fields);
            call_params.insert(0, ptr);
            out_ptr_return = Some(VirtualValue::StackStruct { ty: ret, ptr });
        }

        let mut register_returns = {
            // In order to call a function, we need to first map a global FuncId into a local FuncRef
            // inside the current.
            let sigref = self.builder.import_signature(sig);

            let call = self.ins().call_indirect(sigref, ptr, &call_params);

            self.builder.inst_results(call).to_vec().into_iter()
        };

        // If the return values were handled through an out pointer, return that pointer
        // Otherwise; collect the returned scalar values into a VirtualValue to turn it back into our typed abstraction.
        out_ptr_return.unwrap_or_else(|| {
            self.type_to_virtual_value(&mut |_, _| register_returns.next().unwrap(), false, ret)
        })
    }

    pub fn switch_to_branch_block(&mut self, call: BlockCall) {
        let block = call.block(&self.builder.func.dfg.value_lists);
        self.builder.seal_block(block);
        self.builder.switch_to_block(block);
    }

    pub fn get_func(&self, vv: &VirtualValue) -> FuncId {
        match vv {
            VirtualValue::Closure(closure) => self.get_func(&closure.func),
            VirtualValue::Func(func_id) => *func_id,
            VirtualValue::Pointer(PointeeType::Func(..), value) => {
                panic!("Should have been an indirect call")
            }
            _ => panic!("Not a function {vv:?}"),
        }
    }

    pub fn make_payload(&mut self, vv: &VirtualValue, tag: Value) -> Value {
        let size_t = self.module.isa().pointer_type();
        let payload_type = self.type_of_virtual_value(vv);
        let body = self.as_values(vv);
        match self.payload_kind(&payload_type) {
            PayloadKind::InlineCasted(t) => {
                if t.is_float() {
                    let float64 = self.builder.ins().fpromote(types::F64, body[0]);
                    self.builder.ins().bitcast(size_t, MemFlags::new(), float64)
                } else {
                    self.builder.ins().sextend(size_t, body[0])
                }
            }
            PayloadKind::Inline => body[0],
            PayloadKind::Zero => self.builder.ins().iconst(size_t, 0),
            PayloadKind::StackPointer => self.stack_alloc_values(&body),
        }
    }

    pub fn read_payload(&mut self, body: Value, types: &[Type]) -> Vec<Value> {
        let size_t = self.types.ptr_type();
        match self.payload_kind(types) {
            // Reduce the size of the payload to the inlined data size
            PayloadKind::InlineCasted(target) => {
                if target.is_float() {
                    types
                        .iter()
                        .map(|_| {
                            let float64 =
                                self.builder
                                    .ins()
                                    .bitcast(types::F64, MemFlags::new(), body);
                            self.builder.ins().fdemote(types::F32, float64)
                        })
                        .collect()
                } else {
                    types
                        .iter()
                        .map(|_| self.builder.ins().ireduce(target, body))
                        .collect()
                }
            }

            // Use the payload as-is
            PayloadKind::Inline => types.iter().map(|_| body).collect(), //.iter().map(|v| self.ins().),

            // Use zero as the payload so that this payload-less variant still has the same size
            PayloadKind::Zero => vec![self.builder.ins().iconst(size_t, 0)],

            // Dereference the fields from the payload stack pointer
            PayloadKind::StackPointer => {
                // let v = self
                //     .builder
                //     .ins()
                //     .load(, cl::MemFlags::new(), payload[0], offset);
                types
                    .iter()
                    .map(|ty| self.builder.ins().load(*ty, MemFlags::new(), body, 0))
                    .collect()
            }
        }
    }

    pub fn payload_kind(&mut self, params: &[Type]) -> PayloadKind {
        // dbg!(union_vv);
        // todo!();
        let size_t = self.types.ptr_type();
        // dbg!(&params);
        match params[..] {
            // We want to inline the payload if it fits in the bytes of size_t
            [param] => {
                match param.bytes().cmp(&size_t.bytes()) {
                    // Should be cast to size_t
                    Ordering::Less => PayloadKind::InlineCasted(param),
                    // The scalar will already have the same memory layout as a payload
                    Ordering::Equal => PayloadKind::Inline,
                    // It doesn't fit in the bytes of size_t, so the payload will be stack allocated
                    Ordering::Greater => PayloadKind::StackPointer,
                }
            }

            // It still needs to be the same size of other enums of the same type, so we generate a
            // zeroed payload.
            [] => PayloadKind::Zero,

            // Stack allocate larger payloads to store them behind a pointer.
            //
            // One possible optimization is to still inline the payload if it's multiple scalars that
            // fit within size_t by using `iconcat` and `isplit`.
            _ => PayloadKind::StackPointer,
        }
    }
}
