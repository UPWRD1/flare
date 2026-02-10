use std::cmp::Ordering;

use bimap::BiMap;
use cranelift::{codegen::ir::ArgumentPurpose, module::*, object::*, prelude::*};
use rustc_hash::FxHashMap;

use crate::{
    passes::backend::target::native::{ENTRYPOINT_FUNCTION_SYMBOL, FunctionPurpose},
    resource::rep::{
        backend::{
            lir::Item,
            native::{PayloadKind, StructPassingMode},
            types::LIRType,
        },
        midend::ir::ItemId,
    },
};

#[derive(Debug)]
pub struct LookupTable {
    pub struct_fields: FxHashMap<LIRType, Vec<Type>>,
    pub union_variants: FxHashMap<LIRType, PayloadKind>,
    pub function_types: FxHashMap<ItemId, (Vec<LIRType>, LIRType)>,
    pub function_sigs: FxHashMap<ItemId, Signature>,
    pub function_names: BiMap<FuncId, ItemId>,

    ptr_size: u32,
}

impl LookupTable {
    pub fn ptr_type(&self) -> Type {
        Type::int_with_byte_size(self.ptr_size as u16).expect("Could not create pointer type")
    }

    pub fn payload_kind(&self, params: &[Type]) -> PayloadKind {
        let size_t = self.ptr_type();

        match params {
            // We want to inline the payload if it fits in the bytes of size_t
            [param] => {
                match param.bytes().cmp(&size_t.bytes()) {
                    // Should be cast to size_t
                    Ordering::Less => PayloadKind::InlineCasted(*param),
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

    fn translate_ty(&self, ty: LIRType) -> types::Type {
        match ty {
            LIRType::Int => types::F32,
            LIRType::Float => types::F32,
            LIRType::String => self.ptr_type(),
            LIRType::Unit => types::I8,

            LIRType::Union(_)
            | LIRType::Closure(..)
            | LIRType::Struct(_)
            | LIRType::ClosureEnv(..) => {
                panic!("{ty:?}")
            }
        }
    }

    fn generate_struct_table(&mut self, ty: LIRType) -> Type {
        match ty {
            LIRType::Int | LIRType::Float | LIRType::String | LIRType::Unit => {
                self.translate_ty(ty)
            }
            LIRType::Struct(tys) => {
                let new_tys = tys
                    .iter()
                    .map(|ty| self.generate_struct_table(*ty))
                    .collect();
                self.struct_fields.insert(ty, new_tys);
                self.ptr_type()
            }
            LIRType::Union(_) => {
                // let types: Vec<Type> = variants
                //     .iter()
                //     .map(|v| self.generate_struct_table(module, *v))
                //     .collect();
                // let payload_kind = self.payload_kind(types);
                // self.union_variants.insert(ty, payload_kind);
                // match payload_kind {
                //     PayloadKind::InlineCasted(t) => t,
                //     PayloadKind::Inline => todo!(),
                //     PayloadKind::Zero => todo!(),
                //     PayloadKind::StackPointer => todo!(),
                // }
                // todo!()
                self.ptr_type()
            }
            LIRType::Closure(..) => self.ptr_type(),
            LIRType::ClosureEnv(..) => {
                let closure_struct = ty.closure_to_struct_rep();
                self.generate_struct_table(closure_struct)
            }
        }
    }

    pub fn new(module: &mut ObjectModule, items: &[(Item, FunctionPurpose)]) -> Self {
        let struct_fields = FxHashMap::default();
        let union_variants = FxHashMap::default();
        let function_types = FxHashMap::default();
        let function_sigs = FxHashMap::default();
        let function_names = BiMap::default();
        let ptr_size = module.isa().pointer_bytes() as u32;

        let mut me = Self {
            struct_fields,
            union_variants,
            function_types,
            function_sigs,
            function_names,
            ptr_size,
        };
        for (item, _) in items {
            // dbg!(item);

            let params: Vec<LIRType> = item.params.iter().map(|v| v.ty).collect();
            for ty in params.iter() {
                me.generate_struct_table(*ty);
            }
            me.generate_struct_table(item.ret_ty);

            me.function_types.insert(item.id, (params, item.ret_ty));

            let (f_id, sig) = me.declare_func(module, &item.id);
            me.function_names.insert(f_id, item.id);
            me.function_sigs.insert(item.id, sig);
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
        let size_t = self.ptr_type();
        // If the return value is a large struct that's passed as pointer, instead of returning its
        // values directly, we use an out pointer as the first parameter. The callee will write
        // the result to that pointer, instead of returning directly through the return registers.
        let mut params = vec![];
        let returns = {
            let mut returns = vec![];
            match fret {
                LIRType::Int => returns.push(AbiParam::new(types::I32)),
                LIRType::Float => returns.push(AbiParam::new(types::F32)),
                LIRType::String | LIRType::Closure(..) => returns.push(AbiParam::new(size_t)),
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
                LIRType::Union(variants) => {
                    let variant_size = self.union_tag_type(variants.len());
                    returns.push(AbiParam::new(variant_size));
                    returns.push(AbiParam::new(size_t))
                }

                _ => todo!("{fret:?}"),
            };
            returns
        };
        let params = {
            for p in fparams {
                match p {
                    LIRType::Int => params.push(AbiParam::new(types::I32)),
                    LIRType::Unit => params.push(AbiParam::new(types::I8)),
                    LIRType::Float => params.push(AbiParam::new(types::F32)),

                    LIRType::String | LIRType::Closure(..) => params.push(AbiParam::new(size_t)),

                    LIRType::Struct(fields) => match self.struct_passing_mode(p) {
                        StructPassingMode::ByScalars => {
                            self.for_scalars_of_struct(
                                &mut |clty| params.push(AbiParam::new(clty)),
                                p,
                            );
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
                    LIRType::Union(variants) => {
                        let variant_size = self.union_tag_type(variants.len());
                        params.push(AbiParam::new(variant_size));
                        params.push(AbiParam::new(size_t))
                    }
                    _ => todo!("{p:?}"),
                }
            }
            params
        };
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

    pub fn union_tag_type(&self, variants: usize) -> Type {
        match variants {
            0..127 => types::I8,
            _ => self.ptr_type(),
        }
    }
}
