use std::{any::Any, cell::RefCell};

use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{
        AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue,
        PointerValue,
    },
};
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{
    passes::backend::{
        lir::ClosureConvertOut,
        target::{Target as FlareTarget, native::FunctionPurpose},
    },
    resource::{
        errors::CompilerErr,
        rep::{
            backend::{
                lir::{Item, LIR, Var},
                types::LIRType,
            },
            frontend::ast::BinOp,
            midend::ir::ItemId,
        },
    },
};

#[derive(Debug)]
pub struct LLVMContext<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    target: Target,
    machine: TargetMachine,
    sym_table: RefCell<FxHashMap<Var, BasicValueEnum<'ctx>>>,
}

impl<'ctx> LLVMContext<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("flare_module");

        // If the user supplied a specific target, attempt to create
        // it from a string, otherwise, get_default_triple, which is
        // just our native platform, no cross-compilation in this case
        // let triple = match cli_args.target.as_ref() {
        //     None => TargetMachine::get_default_triple(),
        //     Some(target_str) => TargetTriple::create(target_str.as_str()),
        // };
        //
        let mut config = InitializationConfig::default();
        Target::initialize_all(&config);
        let triple = TargetMachine::get_default_triple();

        let target = Target::from_triple(&triple)
            .expect("Unknown target: please specify a valid LLVM target");

        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                inkwell::OptimizationLevel::None,
                // cli_args.opt_level.into(),
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();
        Self {
            context,
            builder,
            module,
            machine,
            target,
            sym_table: RefCell::new(FxHashMap::default()),
        }
    }
}

pub trait LLVMTypeConvert {
    fn convert<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> BasicTypeEnum<'ir>;
}

impl LLVMTypeConvert for LIRType {
    fn convert<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> BasicTypeEnum<'ir> {
        let ty: &dyn AnyType = match self {
            Self::Int => &context.context.i32_type(),
            Self::Float => &context.context.f32_type(),
            Self::String => todo!(),
            Self::Unit => &context.context.i8_type(),
            Self::Struct(fields) => &context.context.ptr_type(AddressSpace::default()),
            Self::Union(intern) => todo!(),
            Self::Closure(intern, intern1) => &context.context.ptr_type(AddressSpace::default()),
            Self::ClosureEnv(intern, intern1) => &context.context.ptr_type(AddressSpace::default()),
        };
        BasicTypeEnum::try_from(ty.as_any_type_enum()).unwrap()
    }
}

impl<'ctx: 'ir, 'ir> LLVMContext<'ctx> {
    fn convert_struct_ty(&self, ty: LIRType) -> BasicTypeEnum<'ir> {
        match ty {
            LIRType::Struct(fields) => self
                .context
                .struct_type(
                    fields
                        .iter()
                        .map(|f| self.convert_struct_ty(*f))
                        .collect_vec()
                        .as_slice(),
                    false,
                )
                .into(),
            LIRType::ClosureEnv(..) => self.convert_struct_ty(ty.closure_to_struct_rep()),
            _ => ty.convert(self),
        }
    }
    fn make_func_type(&self, ret: LIRType, args: &[LIRType]) -> FunctionType<'ir> {
        let ret_ty = ret.convert(self);
        ret_ty.fn_type(
            &args
                .iter()
                .map(|arg| {
                    let bty = arg.convert(self);
                    BasicMetadataTypeEnum::from(bty)
                })
                .collect_vec(),
            false,
        )
    }

    fn codegen_item(&self, item: Item) -> AnyValueEnum<'ir> {
        let name = Self::make_func_name(&item.id);
        let ty = self.make_func_type(item.ret_ty, &item.params.iter().map(|p| p.ty).collect_vec());
        let fn_val = self.module.add_function(&name, ty, Some(Linkage::External));

        let bb_entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(bb_entry);

        // Update the symbol table with the args names and references
        // to their LLVM values, need to resolve them in current function
        // scope
        self.sym_table.borrow_mut().clear();
        for (arg, var) in fn_val.get_params().into_iter().zip(item.params) {
            self.sym_table.borrow_mut().insert(var, arg);
        }

        let ir_body = self.codegen_ir(item.body);
        dbg!(&ir_body);

        self.builder.build_return(Some(&ir_body));

        if !fn_val.verify(true) {
            panic!("Failed to verify func")
        }

        fn_val.as_any_value_enum()
    }
    fn make_func_name(id: &ItemId) -> String {
        format!("flare_f_{}", id.0)
    }
    // fn to_basicvalue<'v>(v: &'v AnyValueEnum<'ir>) -> &'v dyn BasicValue<'ir> {
    //     match v {
    //         AnyValueEnum::IntValue(v) => v as &dyn BasicValue,
    //         AnyValueEnum::FloatValue(v) => v as &dyn BasicValue,
    //         AnyValueEnum::PhiValue(phi_value) => todo!(),
    //         AnyValueEnum::FunctionValue(function_value) => todo!(),
    //         AnyValueEnum::PointerValue(v) => v as &dyn BasicValue,
    //         AnyValueEnum::StructValue(struct_value) => todo!(),
    //         AnyValueEnum::VectorValue(vector_value) => todo!(),
    //         AnyValueEnum::ScalableVectorValue(scalable_vector_value) => todo!(),
    //         AnyValueEnum::InstructionValue(instruction_value) => todo!(),
    //         _ => todo!(),
    //     }
    // }

    fn make_args<'a>(&self, args: &'a [BasicValueEnum<'ir>]) -> Vec<BasicMetadataValueEnum<'ir>> {
        args.iter()
            .map(|arg| match *arg {
                BasicValueEnum::ArrayValue(v) => BasicMetadataValueEnum::ArrayValue(v),
                BasicValueEnum::IntValue(v) => BasicMetadataValueEnum::IntValue(v),

                BasicValueEnum::FloatValue(v) => BasicMetadataValueEnum::FloatValue(v),
                BasicValueEnum::PointerValue(v) => BasicMetadataValueEnum::PointerValue(v),
                BasicValueEnum::StructValue(v) => BasicMetadataValueEnum::StructValue(v),
                BasicValueEnum::VectorValue(v) => BasicMetadataValueEnum::VectorValue(v),
                BasicValueEnum::ScalableVectorValue(v) => {
                    BasicMetadataValueEnum::ScalableVectorValue(v)
                } // _ => panic!("Invalid conversion"),
            })
            .collect()
    }

    fn codegen_ir(&self, ir: LIR) -> BasicValueEnum<'ir> {
        match ir {
            LIR::Var(var) => {
                if let Some(llvm_val) = self.sym_table.borrow().get(&var) {
                    *llvm_val
                } else {
                    panic!("Undefined var {var:?}")
                }
            }
            LIR::Int(i) => self.context.i32_type().const_int(i as u64, false).into(),
            LIR::Str(intern) => todo!(),
            LIR::Unit => self.context.i8_type().const_int(0u64, false).into(),
            LIR::Float(f) => {
                let float_type = self.context.f32_type();
                float_type.const_float(f.0 as f64).into()
            }
            LIR::ClosureBuild(fun_ty, id, ref vars) => {
                let name = Self::make_func_name(&id);
                let closure_ty = ir.type_of();
                let closure_struct_ty = self.convert_struct_ty(closure_ty.closure_to_struct_rep());
                // dbg!(closure_struct_ty);
                // self.builder.insert
                let closure_struct = self
                    .builder
                    .build_alloca(closure_struct_ty, "closure-struct")
                    .unwrap();
                let item_fn = self
                    .module
                    .get_function(&name)
                    .unwrap()
                    .as_global_value()
                    .as_pointer_value()
                    .as_basic_value_enum();
                let function_field = self
                    .builder
                    .build_struct_gep(closure_struct_ty, closure_struct, 0, "closure_func_gep")
                    .unwrap();
                self.builder
                    .build_store(function_field, item_fn)
                    .expect("Could not store closure func");

                let env_struct_ty = self
                    .convert_struct_ty(closure_ty.closure_to_struct_rep().into_struct_fields()[1]);
                for (idx, var) in vars.iter().enumerate() {
                    let val = self
                        .sym_table
                        .borrow()
                        .get(var)
                        .copied()
                        .expect("Undefined variable");
                    let env_struct = self
                        .builder
                        .build_struct_gep(closure_struct_ty, closure_struct, 1, "closure_env_gep")
                        .unwrap();
                    let capt_field = self
                        .builder
                        .build_struct_gep(
                            env_struct_ty,
                            env_struct,
                            idx as u32,
                            "closure_field_gep",
                        )
                        .unwrap();
                    self.builder
                        .build_store(capt_field, val)
                        .expect("Could not store closure env field");
                }

                closure_struct.into()
            }
            LIR::Apply(fun, arg) => self.handle_app(*fun, vec![*arg]),
            LIR::BulkApply(fun, arg_lirs) => self.handle_app(*fun, arg_lirs),
            LIR::FuncRef(app_type) => todo!(),
            LIR::Local(var, def, body) => {
                let v = self.codegen_ir(*def);
                self.sym_table.borrow_mut().insert(var, v);
                self.codegen_ir(*body)
            }
            LIR::Access(obj, idx) => {
                let obj_ptr = self.codegen_ir(*obj.clone()).into_pointer_value();
                if idx == 0 {
                    let fn_gep = self
                        .builder
                        .build_struct_gep(
                            self.context.ptr_type(AddressSpace::default()),
                            obj_ptr,
                            0,
                            "gep-closure-func",
                        )
                        .unwrap();
                    self.builder
                        .build_load(
                            self.context.ptr_type(AddressSpace::default()),
                            fn_gep,
                            "load-closure-func",
                        )
                        .unwrap()
                } else {
                    let idx = idx - 1;
                    let env_ty = obj
                        .type_of()
                        .closure_to_struct_rep()
                        .into_struct_fields()
                        .into_iter()
                        .nth(1)
                        .unwrap();

                    let field_ty = env_ty.into_struct_fields().into_iter().nth(idx).unwrap();
                    let env_ty = self.convert_struct_ty(env_ty);
                    // dbg!(field_ty);
                    let env_gep = self
                        .builder
                        .build_struct_gep(
                            self.convert_struct_ty(obj.type_of()),
                            obj_ptr,
                            1u32,
                            "gep-closure-env",
                        )
                        .unwrap();
                    let capt_gep = self
                        .builder
                        .build_struct_gep(env_ty, env_gep, idx as u32, "gep-closure-field")
                        .unwrap();

                    self.builder
                        .build_load(
                            self.convert_struct_ty(field_ty),
                            capt_gep,
                            "load-closure-capt",
                        )
                        .unwrap()
                }
            }
            LIR::Struct(ref fields) => {
                let struct_ty = self.convert_struct_ty(ir.type_of());

                let the_struct = self
                    .builder
                    .build_alloca(struct_ty, "struct-alloca")
                    .expect("Could not alloca struct");
                for (i, field) in fields.iter().enumerate() {
                    let field_ptr = self
                        .builder
                        .build_struct_gep(struct_ty, the_struct, i as u32, "field-gep")
                        .expect("Could not gep struct field");
                    let field_val = self.codegen_ir(field.clone());
                    self.builder
                        .build_store(field_ptr, field_val)
                        .expect("Could not store struct field");
                }
                the_struct.into()
            }
            LIR::Field(obj, idx) => {
                // dbg!(&obj);
                let struct_ty = self.convert_struct_ty(obj.type_of()).into_struct_type();
                let obj = self.codegen_ir(*obj);
                if obj.is_pointer_value() {
                    let obj = obj.into_pointer_value();

                    let pointer_idx = self
                        .builder
                        .build_struct_gep(struct_ty, obj, idx as u32, "struct-gep")
                        .unwrap();
                    self.builder
                        .build_load(
                            struct_ty.get_field_type_at_index(idx as u32).unwrap(),
                            pointer_idx,
                            "load-field",
                        )
                        .unwrap()
                } else if obj.is_struct_value() {
                    let obj = obj.into_struct_value();
                    // dbg!(obj);

                    let v = obj;
                    dbg!(v);
                    todo!()
                } else {
                    panic!("Not a struct")
                }
            }
            LIR::Case(lirtype, lir, lirs) => todo!(),
            LIR::Tag(lirtype, _, lir) => todo!(),
            LIR::Item(id, lirtype) => {
                let name = Self::make_func_name(&id);
                let item_fn = self.module.get_function(&name).unwrap();
                // let call = self
                //     .builder
                //     .build_call(item_fn, &[], "calltmp")
                //     .expect("LLVM failed to build call expression");
                // call.try_as_basic_value().unwrap_basic()
                item_fn
                    .as_global_value()
                    .as_pointer_value()
                    .as_basic_value_enum()
            }

            LIR::Extern(intern, lirtype) => todo!(),
            LIR::BinOp(left, bin_op, right) => {
                let lhs = self.codegen_ir(*left);
                let rhs = self.codegen_ir(*right);
                BasicValueEnum::from(
                    match bin_op {
                        BinOp::Eq => todo!(),
                        BinOp::Neq => todo!(),
                        BinOp::Gt => todo!(),
                        BinOp::Lt => todo!(),
                        BinOp::Gte => todo!(),
                        BinOp::Lte => todo!(),
                        BinOp::Add => self.builder.build_float_add(
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "add",
                        ),
                        BinOp::Sub => self.builder.build_float_sub(
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "sub",
                        ),
                        BinOp::Mul => self.builder.build_float_mul(
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "mul",
                        ),
                        BinOp::Div => self.builder.build_float_div(
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "div",
                        ),
                        BinOp::And => todo!(),
                        BinOp::Or => todo!(),
                    }
                    .expect("Failed to complete op"),
                )
            }
        }
    }

    fn handle_app(&self, fun: LIR, arg_lirs: Vec<LIR>) -> BasicValueEnum<'ir> {
        let fun_ty = fun.type_of();

        if let LIRType::ClosureEnv(fpointer_ty, capt_tys) = fun_ty {
            let (mut arg_tys, ret_ty) = fpointer_ty.destructure_closure();
            arg_tys.insert(0, LIRType::Struct(capt_tys));
            let the_fun_ty = self.make_func_type(ret_ty, &arg_tys);
            dbg!(the_fun_ty);
            let closure_and_env_pointer = self.codegen_ir(fun).into_pointer_value();
            let closure_struct_ty = self.convert_struct_ty(fun_ty.closure_to_struct_rep());

            let fun_gep = self
                .builder
                .build_struct_gep(closure_struct_ty, closure_and_env_pointer, 0, "fun-gep")
                .expect("Could not gep closure struct");
            let fun_pointer = self
                .builder
                .build_load(
                    self.context.ptr_type(AddressSpace::default()),
                    fun_gep,
                    "fun-pointer",
                )
                .expect("Could not get closure func pointer")
                .into_pointer_value();

            let mut args = arg_lirs
                .into_iter()
                .map(|arg| self.codegen_ir(arg))
                .collect_vec();
            args.insert(0, closure_and_env_pointer.into());
            let args = self.make_args(&args);

            self.builder
                .build_indirect_call(the_fun_ty, fun_pointer, &args, "closure-indirect")
                .expect("Could not build closure call")
                .try_as_basic_value()
                .unwrap_basic()
        } else {
            let (args, ret) = fun_ty.destructure_closure();
            let fun_ty = self.make_func_type(ret, &args);
            // dbg!(fun_ty);
            let fun = self.codegen_ir(fun);
            let args = arg_lirs
                .into_iter()
                .map(|arg| self.codegen_ir(arg))
                .collect_vec();
            let args = self.make_args(&args);
            match fun {
                BasicValueEnum::PointerValue(pointer) => self
                    .builder
                    .build_indirect_call(fun_ty, pointer, &args, "call-indirect-res")
                    .unwrap()
                    .try_as_basic_value()
                    .unwrap_basic(),
                _ => panic!("Invalid function: {fun:?}"),
            }
        }
    }

    fn generate_main_func(&self, flare_entry_id: &ItemId) -> AnyValueEnum<'ir> {
        let name = "main";
        let ty = self.make_func_type(LIRType::Int, &[]);
        let fn_val = self.module.add_function(name, ty, Some(Linkage::External));
        let flare_entry_fn = self
            .module
            .get_function(&Self::make_func_name(flare_entry_id))
            .unwrap();

        let bb_entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(bb_entry);
        let call = self
            .builder
            .build_call(flare_entry_fn, &[], "calltmp")
            .expect("LLVM failed to build call expression");
        let call_val = call.try_as_basic_value().basic();
        let cast = self.builder.build_float_to_signed_int(
            call_val.map(|v| v.into_float_value()).unwrap(),
            LIRType::Int.convert(self).into_int_type(),
            "conversion",
        );

        let ret = self
            .builder
            .build_return(cast.as_ref().map(|v| v as &dyn BasicValue).ok())
            .unwrap();

        if !fn_val.verify(true) {
            panic!("Failed to verify func")
        }
        fn_val.as_any_value_enum()
    }
}

#[derive(Clone, Copy)]
pub struct LLVM;

impl FlareTarget for LLVM {
    type Input = ClosureConvertOut;
    type Output = Vec<u8>;
    #[allow(clippy::unwrap_used)]
    fn generate(&mut self, lir: Vec<ClosureConvertOut>) -> Self::Output {
        let len = lir.len();
        let funcs: Vec<(Item, FunctionPurpose)> = lir
            .into_iter()
            .enumerate()
            .flat_map(|(idx, cco)| {
                let mut new_items: Vec<(Item, FunctionPurpose)> =
                    Vec::with_capacity(cco.closure_items.len() + 1);
                new_items.extend(
                    cco.closure_items
                        .into_values()
                        .map(|closure| (closure, FunctionPurpose::Closure)),
                );
                if idx == len - 1 {
                    new_items.push((cco.item, FunctionPurpose::Main));
                } else {
                    new_items.push((cco.item, FunctionPurpose::Normal));
                }
                new_items
            })
            .collect();

        let ctx = Context::create();
        let llvm_ctx = LLVMContext::new(&ctx);

        for (item, purpose) in funcs.into_iter() {
            match purpose {
                FunctionPurpose::Normal | FunctionPurpose::Closure => {
                    log::info!("{}", item);
                    llvm_ctx.codegen_item(item);
                    log::info!("---------------------------------")
                }

                FunctionPurpose::Main => {
                    let id = item.id;
                    log::info!("{}", item);
                    let main_id = llvm_ctx.codegen_item(item);
                    log::info!("------------main---------");
                    llvm_ctx.generate_main_func(&id);

                    log::info!("---------------------------------")
                }
            }
        }
        let bit = llvm_ctx.module.write_bitcode_to_memory();
        bit.as_slice().to_vec()
        // todo!()
        // llvm_ctx.compile(&cli.output.as_path(), FileType::Object);
        // Generate the object file.
        // product.emit().expect("Could not emit bytes")
    }

    fn ext(&self) -> &str {
        "ll"
    }
}
