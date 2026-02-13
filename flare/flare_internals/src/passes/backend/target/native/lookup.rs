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
    pub function_types: FxHashMap<FuncId, (Vec<LIRType>, LIRType)>,
    pub function_sigs: FxHashMap<FuncId, Signature>,
    pub function_names: FxHashMap<ItemId, FuncId>,

    ptr_size: u32,
}

impl LookupTable {
    pub fn ptr_type(&self) -> Type {
        Type::int_with_byte_size(self.ptr_size as u16).expect("Could not create pointer type")
    }

    pub fn translate_ty(&self, ty: LIRType) -> Vec<types::Type> {
        match ty {
            LIRType::Int => vec![types::F32],
            LIRType::Float => vec![types::F32],
            LIRType::String => vec![self.ptr_type()],
            LIRType::Unit => vec![types::I8],

            LIRType::Union(_)
            | LIRType::Closure(..)
            | LIRType::Struct(_)
            | LIRType::ClosureEnv(..) => {
                panic!("{ty:?}")
            }
        }
    }

    fn generate_struct_table(&mut self, ty: LIRType) -> Vec<Type> {
        match ty {
            LIRType::Int | LIRType::Float | LIRType::String | LIRType::Unit => {
                self.translate_ty(ty)
            }
            LIRType::Struct(tys) => {
                let new_tys: Vec<Type> = tys
                    .iter()
                    .flat_map(|ty| self.generate_struct_table(*ty))
                    .collect();
                self.struct_fields.insert(ty, new_tys.clone());
                new_tys
            }
            LIRType::Union(_) => {
                vec![types::I32, self.ptr_type()]
            }
            LIRType::Closure(..) => vec![self.ptr_type()],
            LIRType::ClosureEnv(..) => {
                let closure_struct = ty.closure_to_struct_rep();
                self.generate_struct_table(closure_struct)
            }
        }
    }

    pub fn convert_ty(&self, ty: LIRType) -> Vec<Type> {
        match ty {
            LIRType::Int | LIRType::Float | LIRType::String | LIRType::Unit => {
                self.translate_ty(ty)
            }
            LIRType::Struct(tys) => tys.iter().flat_map(|ty| self.convert_ty(*ty)).collect(),
            LIRType::Union(_) => {
                vec![types::I32, self.ptr_type()]
            }
            LIRType::Closure(..) => vec![self.ptr_type()],
            LIRType::ClosureEnv(..) => {
                let closure_struct = ty.closure_to_struct_rep();
                self.convert_ty(closure_struct)
            }
        }
    }

    pub fn new(module: &mut ObjectModule, items: &[(Item, FunctionPurpose)]) -> Self {
        let struct_fields = FxHashMap::default();
        let union_variants = FxHashMap::default();
        let function_types = FxHashMap::default();
        let function_sigs = FxHashMap::default();
        let function_names = FxHashMap::default();
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
            let (f_id, sig) = me.declare_func(
                module,
                Linkage::Export,
                &format!("flare_f_{}", item.id.0),
                &params,
                item.ret_ty,
            );
            me.function_types.insert(f_id, (params, item.ret_ty));

            me.function_names.insert(item.id, f_id);
            me.function_sigs.insert(f_id, sig);
        }
        me
    }

    pub fn declare_func(
        &mut self,
        module: &mut ObjectModule,
        linkage: Linkage,
        name: &str,
        fparams: &[LIRType],
        fret: LIRType,
    ) -> (FuncId, Signature) {
        let call_conv = module.isa().default_call_conv();
        let sig = self.make_sig(call_conv, fparams, fret);
        let id = module
            .declare_function(name, linkage, &sig)
            .expect("Could not declare function");
        (id, sig)
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
    pub fn create_signature(&mut self, call_conv: isa::CallConv, f_id: &FuncId) -> Signature {
        let (fparams, fret) = self
            .function_types
            .get(f_id)
            .unwrap_or_else(|| panic!("function not found: {:?}", f_id));
        let sig = self.make_sig(call_conv, fparams, *fret);
        self.function_sigs.insert(*f_id, sig.clone());
        sig
    }

    pub fn make_sig(
        &self,
        call_conv: isa::CallConv,
        fparams: &[LIRType],
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
                LIRType::Struct(name) => match self.struct_passing_mode(&fret) {
                    StructPassingMode::ByScalars => {
                        self.for_scalars_of_struct(&mut |ty| returns.push(AbiParam::new(ty)), &fret)
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
                    match self.struct_passing_mode(&env_struct) {
                        StructPassingMode::ByScalars => {
                            self.for_scalars_of_struct(
                                &mut |clty| returns.push(AbiParam::new(clty)),
                                // fret,
                                &env_struct,
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
                    let variant_size = types::I32;
                    self.union_tag_type(variants.len());
                    returns.push(AbiParam::new(variant_size));
                    returns.push(AbiParam::new(size_t));
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
                        match self.struct_passing_mode(&env_struct) {
                            StructPassingMode::ByScalars => {
                                self.for_scalars_of_struct(
                                    &mut |clty| params.push(AbiParam::new(clty)),
                                    &env_struct,
                                    // p,
                                );
                            }
                            StructPassingMode::ByPointer => {
                                params.push(AbiParam::new(size_t));
                            }
                        }
                    }
                    LIRType::Union(variants) => {
                        let variant_size = types::I32;
                        // self.union_tag_type(variants.len());
                        params.push(AbiParam::new(variant_size));
                        params.push(AbiParam::new(size_t))
                    }
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

    fn for_scalars<F>(&self, f: &mut F, ty: &LIRType)
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

    pub fn for_scalars_of_struct<F>(&self, f: &mut F, name: &LIRType)
    where
        F: FnMut(Type),
    {
        let name = name.into_struct_fields();
        name.iter().for_each(|&ty| self.for_scalars(f, &ty))
    }

    pub fn return_type_of(&self, id: FuncId) -> LIRType {
        // dbg!(id);         dbg!(&self.function_types);
        self.function_types[&id].1
    }

    // If a struct fits in two registers, then avoid stack allocating it.
    pub fn struct_passing_mode(&self, fields: &LIRType) -> StructPassingMode {
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
