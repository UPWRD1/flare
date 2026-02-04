use std::sync::Arc;

use cranelift::{
    codegen::ir::ArgumentPurpose,
    frontend::FuncInstBuilder,
    module::{FuncId, Linkage, Module},
    object::ObjectModule,
    prelude::*,
};
use internment::Intern;
use rustc_hash::FxHashMap;

use crate::{
    passes::backend::target::native::{FunctionPurpose, IRConverter},
    resource::rep::{
        backend::{lir::Item, native::VirtualValue, types::LIRType},
        midend::ir::ItemId,
    },
};

type Name = &'static str;

#[derive(Debug)]
pub struct LookupTable {
    struct_fields: FxHashMap<Intern<[LIRType]>, Vec<Type>>,
    pub function_types: FxHashMap<ItemId, (Vec<LIRType>, LIRType)>,
    pub function_sigs: FxHashMap<ItemId, Signature>,
    pub function_names: FxHashMap<FuncId, ItemId>,
    ptr_size: u32,
}

// Whether a struct will be passed as a pointer or as a set of independent values directly
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum StructPassingMode {
    ByScalars,
    ByPointer,
}
impl LookupTable {
    pub fn new(module: &mut ObjectModule, items: &[(Item, FunctionPurpose)]) -> Self {
        let mut struct_fields = FxHashMap::default();
        let mut function_types = FxHashMap::default();
        let mut function_sigs = FxHashMap::default();
        let mut function_names = FxHashMap::default();
        let ptr_size = module.isa().pointer_type().bits();

        for (item, purpose) in items {
            let function_name = format!("flare_f_{}", item.id.0);
        }
        Self {
            struct_fields,
            function_types,
            function_sigs,
            function_names,
            ptr_size,
        }
    }

    pub fn declare_func(
        &mut self,
        module: &mut ObjectModule,
        fname: &ItemId,
    ) -> (FuncId, Signature) {
        let call_conv = module.isa().default_call_conv();
        let sig = self.create_signature(call_conv, fname);

        (
            module
                .declare_function(
                    format!("flare_f_{}", fname.0).as_str(),
                    Linkage::Export,
                    &sig,
                )
                .unwrap(),
            sig,
        )
    }

    /// Function signatures in Cranelift can look pretty different from the user-provided signature.
    ///
    /// Since Cranelift types/values can only represent primitives, a Struct will need to be passed
    /// either as multiple types/values or as a pointer implicitly.
    pub fn create_signature(&mut self, call_conv: isa::CallConv, fname: &ItemId) -> Signature {
        if let Some(sig) = self.function_sigs.get(fname) {
            sig.clone()
        } else {
            // Get the type signatures from our source language
            let (fparams, fret) = self.function_types.get(fname).expect("function not found");

            // Buffers for the Cranelift type signature.
            let mut params = vec![];
            let mut returns = vec![];

            // If the return value is a large struct that's passed as pointer, instead of returning its
            // values directly, we use an out pointer as the first parameter. The callee will write
            // the result to that pointer, instead of returning directly through the return registers.
            match fret {
                LIRType::Int => returns.push(AbiParam::new(types::I32)),
                LIRType::Struct(name) => match self.struct_passing_mode(*name) {
                    StructPassingMode::ByScalars => {
                        self.for_scalars_of_struct(&mut |ty| returns.push(AbiParam::new(ty)), *name)
                    }
                    StructPassingMode::ByPointer => {
                        // The `ArgumentPurpose` is needed in-case our target architecture expects the
                        // out pointer to use a specific register.
                        let size_t = Type::int_with_byte_size(self.ptr_size as u16).unwrap();
                        let param = AbiParam::special(size_t, ArgumentPurpose::StructReturn);
                        params.push(param);
                    }
                },

                _ => todo!(),
            };

            for p in fparams {
                match p {
                    LIRType::Int => params.push(AbiParam::new(types::I32)),
                    LIRType::Struct(fields) => match self.struct_passing_mode(*fields) {
                        StructPassingMode::ByScalars => {
                            self.for_scalars_of_struct(
                                &mut |clty| params.push(AbiParam::new(clty)),
                                *fields,
                            );
                        }
                        StructPassingMode::ByPointer => {
                            let size_t = Type::int_with_byte_size(self.ptr_size as u16).unwrap();
                            params.push(AbiParam::new(size_t));
                        }
                    },
                    _ => todo!(),
                }
            }

            let sig = Signature {
                params,
                returns,
                call_conv,
            };
            self.function_sigs.insert(*fname, sig.clone());
            sig
        }
    }

    fn for_scalars<F>(&self, f: &mut F, ty: LIRType)
    where
        F: FnMut(Type),
    {
        match ty {
            LIRType::Int => f(types::I32),
            LIRType::Struct(name) => self.for_scalars_of_struct(f, name),
            _ => todo!(),
        }
    }

    pub fn for_scalars_of_struct<F>(&self, f: &mut F, name: Intern<[LIRType]>)
    where
        F: FnMut(Type),
    {
        name.iter().for_each(|&ty| self.for_scalars(f, ty))
    }

    pub fn return_type_of(&self, id: FuncId) -> LIRType {
        let fname = self.function_names[&id];
        self.function_types[&fname].1
    }

    // If a struct fits in two registers, then avoid stack allocating it.
    pub fn struct_passing_mode(&self, fields: Intern<[LIRType]>) -> StructPassingMode {
        let mut scalars = 0;
        self.for_scalars_of_struct(&mut |_| scalars += 1, fields);
        if scalars < 3 {
            StructPassingMode::ByScalars
        } else {
            StructPassingMode::ByPointer
        }
    }
}

impl<'bctx, 'module> IRConverter<'bctx, 'module> {
    pub fn ins(&mut self) -> FuncInstBuilder<'_, 'bctx> {
        self.builder.ins()
    }

    pub fn int(&mut self, n: impl Into<i64>) -> VirtualValue {
        let v = self.ins().iconst(types::I32, n.into());
        VirtualValue::Scalar(v)
    }

    pub fn float(&mut self, n: impl Into<f32>) -> VirtualValue {
        let v = self.ins().f32const(n.into());
        VirtualValue::Scalar(v)
    }

    // Turns a parameter from our source language into Cranelift block parameters.
    //
    // Since Cranelift parameters can only be primitive types, a single struct will either
    // become a single Cranelift pointer block parameter or multiple block parameters.
    fn type_to_block_params(&mut self, block: Block, is_root: bool, p: LIRType) -> VirtualValue {
        self.type_to_virtual_value(
            &mut |this, clty| this.builder.append_block_param(block, clty),
            is_root,
            p,
        )
    }

    // Maps our abstract Type to our abstract VirtualValue
    fn type_to_virtual_value<F>(&mut self, f: &mut F, is_root: bool, p: LIRType) -> VirtualValue
    where
        F: FnMut(&mut Self, Type) -> Value,
    {
        match p {
            LIRType::Int => {
                let v = f(self, types::I32);
                VirtualValue::Scalar(v)
            }
            LIRType::Struct(ty) => {
                if is_root && self.types.struct_passing_mode(ty) == StructPassingMode::ByPointer {
                    let size_t = self.module.isa().pointer_type();
                    let ptr = f(self, size_t);
                    VirtualValue::StackStruct { ty, ptr }
                } else {
                    let fields = ty
                        .iter()
                        .map(|ty| self.type_to_virtual_value(f, false, *ty))
                        .collect();

                    VirtualValue::UnstableStruct { ty, fields }
                }
            }
            _ => todo!(),
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
                match self.types.struct_passing_mode(ty) {
                    StructPassingMode::ByScalars => {
                        self.deref_fields(buf, ty, src, 0);
                    }
                    StructPassingMode::ByPointer => buf.push(src),
                }
            }
            VirtualValue::UnstableStruct { ty, fields } => {
                match self.types.struct_passing_mode(ty) {
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

            _ => todo!(),
        }
    }

    pub fn construct_struct(
        &mut self,
        ty: Intern<[LIRType]>,
        fields: &[VirtualValue],
    ) -> VirtualValue {
        VirtualValue::UnstableStruct {
            ty,
            fields: fields.to_vec(),
        }
    }

    pub fn destruct_field(&mut self, of: &VirtualValue, field: usize) -> VirtualValue {
        match of {
            VirtualValue::Scalar(_) => panic!("cannot destruct field from non-struct"),

            VirtualValue::StackStruct { ty, ptr } => {
                let fields = self.types.struct_fields.get(ty).unwrap();
                let offset = Self::offset_of_field(field, fields);

                match ty[field] {
                    // Instead of actually dereferencing the inner struct here,
                    // we create another implicit stack pointer that's offset to where the inner struct starts.
                    //
                    // This makes dereferencing lazy.
                    LIRType::Struct(type_) => {
                        let nptr = self.ins().iadd_imm(*ptr, offset as i64);
                        VirtualValue::StackStruct { ty: *ty, ptr: nptr }
                    }
                    LIRType::Int => {
                        let v = self.ins().load(types::I32, MemFlags::new(), *ptr, offset);
                        VirtualValue::Scalar(v)
                    }
                    _ => todo!(),
                }
            }

            VirtualValue::UnstableStruct { fields, .. } => fields[field].clone(),
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
                match self.types.struct_passing_mode(ty) {
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
                match self.types.struct_passing_mode(ty) {
                    StructPassingMode::ByScalars => {
                        let fields = fields
                            .iter()
                            .map(VirtualValue::as_scalar)
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
            _ => todo!(),
        }
    }

    fn deref_fields(
        &mut self,
        buf: &mut Vec<Value>,
        types: Intern<[LIRType]>,
        src: Value,
        src_offset: i32,
    ) {
        let fields = self.types.struct_fields.get(&types).unwrap();
        for (field, ty) in fields.iter().enumerate() {
            let offset = Self::offset_of_field(field, fields) + src_offset;
            let fty = types[field];
            match fty {
                LIRType::Int => {
                    let v = self.ins().load(types::I32, MemFlags::new(), src, offset);

                    buf.push(v);
                }
                LIRType::Struct(type_) => {
                    self.deref_fields(buf, type_, src, offset);
                }
                _ => todo!(),
            }
        }
    }

    fn copy_struct_fields(&mut self, type_: Intern<[LIRType]>, src: Value, dst: Value) {
        let fields = self.types.struct_fields.get(&type_).unwrap();
        for (field, fty) in fields.iter().enumerate() {
            let offset = Self::offset_of_field(field, fields);
            let fty = type_[field];
            match fty {
                LIRType::Int => {
                    let n = self.ins().load(types::I32, MemFlags::new(), src, offset);

                    self.ins().store(MemFlags::new(), n, dst, offset);
                }
                LIRType::Struct(type_) => {
                    let src = self.ins().iadd_imm(src, offset as i64);
                    let dst = self.ins().iadd_imm(dst, offset as i64);

                    self.copy_struct_fields(type_, src, dst);
                }
                _ => todo!(),
            }
        }
    }

    fn write_struct_field(
        &mut self,
        name: Intern<[LIRType]>,
        field: usize,
        ptr: Value,
        v: VirtualValue,
    ) {
        let fields = self.types.struct_fields.get(&name).unwrap();
        let offset = Self::offset_of_field(field, fields);

        match v {
            VirtualValue::Scalar(value) => {
                self.ins().store(MemFlags::new(), value, ptr, offset);
            }

            VirtualValue::UnstableStruct { ty, fields } => {
                for (field, v) in fields.into_iter().enumerate() {
                    // let offset = offset + self.types.offset_of_field(type_, field);
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
    fn struct_return_pointer(&mut self) -> Value {
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
        match func {
            VirtualValue::Func(func) => {
                let mut call_params = vec![];

                let ret = self.types.return_type_of(func);

                // If the return type is too large to fit in return registers, we allocate space for it in
                // the current stack frame and pass a pointer as the first parameter for the child function to
                // write its return values to.
                let mut out_ptr_return = None;
                if let LIRType::Struct(name) = ret {
                    if self.types.struct_passing_mode(name) == StructPassingMode::ByPointer {
                        let fields = self.types.struct_fields.get(&name).unwrap();
                        let ptr = self.stack_alloc_types(fields);
                        call_params.push(ptr);
                        out_ptr_return = Some(VirtualValue::StackStruct { ty: name, ptr });
                    }
                }

                self.virtual_values_to_func_params(&mut call_params, params);

                let mut register_returns = {
                    // In order to call a function, we need to first map a global FuncId into a local FuncRef
                    // inside the current.
                    let fref = self
                        .module
                        .declare_func_in_func(func, &mut self.builder.func);

                    let call = self.ins().call(fref, &call_params);

                    self.builder.inst_results(call).to_vec().into_iter()
                };

                // If the return values were handled through an out pointer, return that pointer
                // Otherwise; collect the returned scalar values into a VirtualValue to turn it back into our typed abstraction.
                out_ptr_return.unwrap_or_else(|| {
                    self.type_to_virtual_value(
                        &mut |_, _| register_returns.next().unwrap(),
                        false,
                        ret,
                    )
                })
            }
            VirtualValue::Closure(c) => c.call(&mut self.builder, &params),
            _ => panic!("Invalid function node {func:?}"),
        }
    }
}
