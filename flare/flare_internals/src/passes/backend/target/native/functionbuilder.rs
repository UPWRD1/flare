use bimap::BiMap;
use cranelift::{
    codegen::ir::ArgumentPurpose,
    frontend::FuncInstBuilder,
    module::{FuncId, Linkage, Module},
    object::ObjectModule,
    prelude::*,
};
use rustc_hash::FxHashMap;

use crate::{
    passes::backend::target::native::{
        ENTRYPOINT_FUNCTION_SYMBOL, FunctionPurpose, IRConverter, translate_ty,
    },
    resource::rep::{
        backend::{
            lir::{Item, Var},
            native::{PointeeType, VirtualValue},
            types::LIRType,
        },
        midend::ir::ItemId,
    },
};

#[derive(Debug)]
pub enum Param {
    Novar(LIRType),
    Var(Var),
}

#[derive(Debug)]
pub struct LookupTable {
    struct_fields: FxHashMap<LIRType, Vec<Type>>,
    pub function_types: FxHashMap<ItemId, (Vec<LIRType>, LIRType)>,
    pub function_sigs: FxHashMap<ItemId, Signature>,
    pub function_names: BiMap<FuncId, ItemId>,

    ptr_size: u32,
}

// Whether a struct will be passed as a pointer or as a set of independent values directly
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum StructPassingMode {
    ByScalars,
    ByPointer,
}
impl LookupTable {
    fn ptr_type(&self) -> Type {
        Type::int_with_byte_size(self.ptr_size as u16).expect("Could not create pointer type")
    }

    fn generate_struct_table(&mut self, module: &mut ObjectModule, ty: LIRType) -> Type {
        match ty {
            LIRType::Int | LIRType::Float | LIRType::String | LIRType::Unit => {
                translate_ty(module, ty)
            }
            LIRType::Struct(tys) => {
                let new_tys = tys
                    .iter()
                    .map(|ty| self.generate_struct_table(module, *ty))
                    .collect();
                self.struct_fields.insert(ty, new_tys);
                module.isa().pointer_type()
            }
            LIRType::Union(intern) => todo!(),
            LIRType::Closure(intern, intern1) => module.isa().pointer_type(),
            LIRType::ClosureEnv(..) => {
                let closure_struct = ty.closure_to_struct_rep();
                self.generate_struct_table(module, closure_struct)
            }
        }
    }

    pub fn new(module: &mut ObjectModule, items: &[(Item, FunctionPurpose)]) -> Self {
        let struct_fields = FxHashMap::default();
        let function_types = FxHashMap::default();
        let function_sigs = FxHashMap::default();
        let function_names = BiMap::default();
        let ptr_size = module.isa().pointer_bytes() as u32;

        let mut me = Self {
            struct_fields,
            function_types,
            function_sigs,
            function_names,
            ptr_size,
        };
        for (item, _) in items {
            // dbg!(item);

            let params: Vec<LIRType> = item.params.iter().map(|v| v.ty).collect();
            for ty in params.iter() {
                me.generate_struct_table(module, *ty);
            }
            me.generate_struct_table(module, item.ret_ty);

            me.function_types.insert(item.id, (params, item.ret_ty));

            let (f_id, sig) = me.declare_func(module, &item.id);
            me.function_names.insert(f_id, item.id);
            // me.function_sigs.insert(item.id, sig);
        }
        me
    }

    pub fn declare_func(
        &mut self,
        module: &mut ObjectModule,
        item_id: &ItemId,
    ) -> (FuncId, Signature) {
        let call_conv = module.isa().default_call_conv();
        let sig = self.create_signature(call_conv, item_id);

        (
            module
                .declare_function(
                    format!("flare_f_{}", item_id.0).as_str(),
                    Linkage::Export,
                    &sig,
                )
                .expect("Could not declare function"),
            sig,
        )
    }
    pub fn declare_main(&mut self, module: &mut ObjectModule) -> (FuncId, Signature) {
        let call_conv = module.isa().default_call_conv();

        let sig = Signature {
            call_conv,
            params: vec![],
            // Since we're linking to libc, we can return the exit code from main.
            returns: vec![AbiParam::new(types::I32)],
        };

        (
            module
                .declare_function(ENTRYPOINT_FUNCTION_SYMBOL, Linkage::Export, &sig)
                .expect("Could not declare function"),
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
            let (fparams, fret) = self
                .function_types
                .get(fname)
                .unwrap_or_else(|| panic!("function not found: {:?}", fname));
            let sig = self.make_sig(call_conv, fparams.clone(), *fret);
            self.function_sigs.insert(*fname, sig.clone());
            sig
        }
    }

    pub fn make_sig(
        &self,
        call_conv: isa::CallConv,
        fparams: Vec<LIRType>,
        fret: LIRType,
    ) -> Signature {
        // Get the type signatures from our source language
        // Buffers for the Cranelift type signature.
        let mut params = vec![];
        let mut returns = vec![];
        let size_t = self.ptr_type();
        // If the return value is a large struct that's passed as pointer, instead of returning its
        // values directly, we use an out pointer as the first parameter. The callee will write
        // the result to that pointer, instead of returning directly through the return registers.
        match fret {
            LIRType::Int => returns.push(AbiParam::new(types::I32)),
            LIRType::Float => returns.push(AbiParam::new(types::F32)),
            LIRType::String | LIRType::Closure(..) => params.push(AbiParam::new(size_t)),
            LIRType::Struct(name) => match self.struct_passing_mode(fret) {
                StructPassingMode::ByScalars => {
                    self.for_scalars_of_struct(&mut |ty| returns.push(AbiParam::new(ty)), fret)
                }
                StructPassingMode::ByPointer => {
                    // The `ArgumentPurpose` is needed in-case our target architecture expects the
                    // out pointer to use a specific register.
                    let param = AbiParam::special(size_t, ArgumentPurpose::StructReturn);
                    params.push(param);
                }
            },

            LIRType::ClosureEnv(f, env) => {
                let env_struct = fret.closure_to_struct_rep();
                match self.struct_passing_mode(env_struct) {
                    StructPassingMode::ByScalars => {
                        self.for_scalars_of_struct(
                            &mut |clty| returns.push(AbiParam::new(clty)),
                            // fret,
                            env_struct,
                        );
                    }
                    StructPassingMode::ByPointer => {
                        // The `ArgumentPurpose` is needed in-case our target architecture expects the
                        // out pointer to use a specific register.
                        let param = AbiParam::special(size_t, ArgumentPurpose::StructReturn);
                        params.push(param);
                    }
                }
            }
            _ => todo!("{fret:?}"),
        };
        for p in fparams {
            match p {
                LIRType::Int => params.push(AbiParam::new(types::I32)),
                LIRType::Unit => params.push(AbiParam::new(types::I8)),
                LIRType::Float => params.push(AbiParam::new(types::F32)),

                LIRType::String | LIRType::Closure(..) => params.push(AbiParam::new(size_t)),

                LIRType::Struct(fields) => match self.struct_passing_mode(p) {
                    StructPassingMode::ByScalars => {
                        self.for_scalars_of_struct(&mut |clty| params.push(AbiParam::new(clty)), p);
                    }
                    StructPassingMode::ByPointer => {
                        params.push(AbiParam::new(size_t));
                    }
                },

                LIRType::ClosureEnv(f, env) => {
                    let env_struct = p.closure_to_struct_rep();
                    match self.struct_passing_mode(env_struct) {
                        StructPassingMode::ByScalars => {
                            self.for_scalars_of_struct(
                                &mut |clty| params.push(AbiParam::new(clty)),
                                env_struct,
                                // p,
                            );
                        }
                        StructPassingMode::ByPointer => {
                            params.push(AbiParam::new(size_t));
                        }
                    }
                }
                _ => todo!("{p:?}"),
            }
        }
        Signature {
            params,
            returns,
            call_conv,
        }
    }

    fn for_scalars<F>(&self, f: &mut F, ty: LIRType)
    where
        F: FnMut(Type),
    {
        match ty {
            LIRType::Int => f(types::I32),
            LIRType::Unit => f(types::I8),
            LIRType::Float => f(types::F32),
            LIRType::String => f(self.ptr_type()),
            LIRType::Closure(l, r) => {
                // self.for_scalars(f, *l);
                // self.for_scalars(f, *r);
                f(self.ptr_type());
            }
            LIRType::Struct(name) => self.for_scalars_of_struct(f, ty),
            _ => todo!("{ty:?}"),
        }
    }

    pub fn for_scalars_of_struct<F>(&self, f: &mut F, name: LIRType)
    where
        F: FnMut(Type),
    {
        let name = name.into_struct_fields();
        name.iter().for_each(|&ty| self.for_scalars(f, ty))
    }

    pub fn return_type_of(&self, id: FuncId) -> LIRType {
        // dbg!(&self.function_names);
        let fname = self.function_names.get_by_left(&id).unwrap();
        self.function_types[fname].1
    }

    // If a struct fits in two registers, then avoid stack allocating it.
    pub fn struct_passing_mode(&self, fields: LIRType) -> StructPassingMode {
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

    pub fn as_value(&mut self, value: &VirtualValue) -> Vec<Value> {
        match value {
            VirtualValue::Scalar(value) => vec![*value],
            VirtualValue::Pointer(_, ptr) => vec![*ptr],
            VirtualValue::StackStruct { ty, ptr } => {
                vec![*ptr]
            }
            VirtualValue::UnstableStruct { ty, fields } => {
                fields.iter().flat_map(|vv| self.as_value(vv)).collect()
            }
            VirtualValue::Func(func_id) => {
                let ptr = self.types.ptr_type();
                let fref = self
                    .module
                    .declare_func_in_func(*func_id, self.builder.func);
                vec![self.ins().func_addr(ptr, fref)]
            }
            VirtualValue::Closure(c) => {
                let mut vec = self.as_value(&c.captures);
                vec.extend(self.as_value(&c.func));
                vec
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
                if is_root && self.types.struct_passing_mode(p) == StructPassingMode::ByPointer {
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

            // LIRType::ClosureEnv(fun, env) => {
            //     // let new_type = p.closure_to_struct_rep();
            //     // self.type_to_virtual_value(f, is_root, new_type)
            //     // let env_struct = LIRType::Struct(env);
            //     if is_root && self.types.struct_passing_mode(p) == StructPassingMode::ByPointer {
            //         let size_t = self.module.isa().pointer_type();
            //         let ptr = f(self, size_t);
            //         VirtualValue::StackStruct { ty: p, ptr }
            //     } else {
            //         let fields = p
            //             .closure_to_struct_rep()
            //             .into_struct_fields()
            //             .iter()
            //             .map(|ty| self.type_to_virtual_value(f, false, *ty))
            //             .collect::<Vec<_>>();
            //         VirtualValue::UnstableStruct { ty: p, fields }
            //     }
            // }
            LIRType::Unit => {
                let v = f(self, types::I8);
                VirtualValue::Scalar(v)
            }
            _ => todo!("{p:?}"),
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
            VirtualValue::Closure(..) => {
                // dbg!(captures_value);
                let vals = self.as_value(&v);
                buf.extend(vals);

                // todo!()
            }
            VirtualValue::Func(f) => {
                let val = self.as_value(&v);
                buf.extend(val);
            }
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
                        let ret: Type = self
                            .types
                            .make_sig(isa::CallConv::Fast, vec![*l], *r)
                            .returns[0]
                            .value_type;
                        // let ret: Vec<Type> = self
                        //     .types
                        //     .make_sig(isa::CallConv::Fast, vec![*l], *r)
                        //     .returns
                        //     .iter()
                        //     .map(|p| p.value_type)
                        //     .collect();
                        // if ret.len() == 1 {
                        //     ret[0]
                        // } else {
                        // }
                        // let pointer = self.types.ptr_type();
                        let v = self.ins().load(ret, MemFlags::new(), *ptr, offset);
                        dbg!(v);
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
                            .flat_map(|v| self.as_value(v))
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

    fn deref_fields(&mut self, buf: &mut Vec<Value>, types: LIRType, src: Value, src_offset: i32) {
        let the_types = types.into_struct_fields();
        let fields = self.types.struct_fields.get(&types).unwrap();
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
            VirtualValue::Func(func_id) => self.call_direct_func(params, func_id),
            VirtualValue::Pointer(t, ptr) => match t {
                PointeeType::Func(from, to) => {
                    let sig = self
                        .types
                        .make_sig(self.module.isa().default_call_conv(), from, to);

                    self.call_indirect_func(params, to, sig, ptr)
                }
                _ => unreachable!(),
            },
            VirtualValue::Closure(c) => self.call_closure(c, &params),
            _ => panic!("Invalid function node {func:?}"),
        }
    }

    fn call_direct_func(&mut self, params: Vec<VirtualValue>, func: FuncId) -> VirtualValue {
        let mut call_params = vec![];

        let ret = self.types.return_type_of(func);

        // If the return type is too large to fit in return registers, we allocate space for it in
        // the current stack frame and pass a pointer as the first parameter for the child function to
        // write its return values to.
        let mut out_ptr_return = None;
        if let LIRType::Struct(name) = ret
            && self.types.struct_passing_mode(ret) == StructPassingMode::ByPointer
        {
            let fields = self.types.struct_fields.get(&ret).unwrap();
            let ptr = self.stack_alloc_types(fields);
            call_params.push(ptr);
            out_ptr_return = Some(VirtualValue::StackStruct { ty: ret, ptr });
        }

        self.virtual_values_to_func_params(&mut call_params, params);

        let mut register_returns = {
            // In order to call a function, we need to first map a global FuncId into a local FuncRef
            // inside the current.
            let fref = self.module.declare_func_in_func(func, self.builder.func);

            let call = self.ins().call(fref, &call_params);

            self.builder.inst_results(call).to_vec().into_iter()
        };

        // If the return values were handled through an out pointer, return that pointer
        // Otherwise; collect the returned scalar values into a VirtualValue to turn it back into our typed abstraction.
        out_ptr_return.unwrap_or_else(|| {
            self.type_to_virtual_value(&mut |_, _| register_returns.next().unwrap(), false, ret)
        })
    }

    fn call_indirect_func(
        &mut self,
        params: Vec<VirtualValue>,
        ret: LIRType,
        sig: Signature,
        ptr: Value,
    ) -> VirtualValue {
        let mut call_params = vec![];

        // let ret = sig.returns;

        // If the return type is too large to fit in return registers, we allocate space for it in
        // the current stack frame and pass a pointer as the first parameter for the child function to
        // write its return values to.
        let mut out_ptr_return = None;
        if let LIRType::Struct(name) = ret
            && self.types.struct_passing_mode(ret) == StructPassingMode::ByPointer
        {
            let fields = self.types.struct_fields.get(&ret).unwrap();
            let ptr = self.stack_alloc_types(fields);
            call_params.push(ptr);
            out_ptr_return = Some(VirtualValue::StackStruct { ty: ret, ptr });
        }

        self.virtual_values_to_func_params(&mut call_params, params);

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
}
