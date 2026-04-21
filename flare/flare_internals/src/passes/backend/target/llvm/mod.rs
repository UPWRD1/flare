use std::cell::RefCell;

use inkwell::{
    AddressSpace,
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    passes::PassBuilderOptions,
    targets::{
        CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetMachineOptions,
    },
    types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{
        AggregateValueEnum, AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue,
        BasicValueEnum, FunctionValue, PointerValue,
    },
};
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{
    passes::backend::{lir::ClosureConvertOut, target::Target as FlareTarget},
    resource::{
        errors::CompResult,
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
    current_func: RefCell<Option<FunctionValue<'ctx>>>,
    machine: TargetMachine,
    sym_table: RefCell<FxHashMap<Var, BasicValueEnum<'ctx>>>,
}

impl<'ctx> LLVMContext<'ctx> {
    pub fn new(context: &'ctx Context, machine: TargetMachine) -> Self {
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
        Self {
            current_func: RefCell::new(None),
            context,
            builder,
            module,
            machine,
            sym_table: RefCell::new(FxHashMap::default()),
        }
    }
}

fn make_target_machine() -> TargetMachine {
    let config = InitializationConfig::default();
    Target::initialize_all(&config);
    let triple = TargetMachine::get_default_triple();

    let target =
        Target::from_triple(&triple).expect("Unknown target: please specify a valid LLVM target");
    target
        .create_target_machine_from_options(
            &triple,
            TargetMachineOptions::new()
                .set_cpu("generic")
                .set_features("")
                .set_reloc_mode(RelocMode::Default)
                .set_code_model(CodeModel::Default)
                .set_level(inkwell::OptimizationLevel::None),
        )
        .unwrap()
}

pub trait LLVMTypeConvert {
    fn convert<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> BasicTypeEnum<'ir>;
}

impl LLVMTypeConvert for LIRType {
    fn convert<'ctx: 'ir, 'ir>(&self, context: &LLVMContext<'ctx>) -> BasicTypeEnum<'ir> {
        let ty: &dyn AnyType = match self {
            Self::Int => &context.context.i32_type(),
            Self::Bool => &context.context.bool_type(),
            Self::Float => &context.context.f32_type(),
            Self::String => todo!(),
            Self::Unit => &context.context.i8_type(),
            Self::Struct(_) | Self::Union(_) => panic!(), //&context.context.ptr_type(AddressSpace::default()),
            Self::Closure(intern, intern1) => &context.context.ptr_type(AddressSpace::default()),

            Self::ClosureEnv(intern, intern1) => panic!(), // &context.context.ptr_type(AddressSpace::default()),
        };
        BasicTypeEnum::try_from(ty.as_any_type_enum()).unwrap()
    }
}

enum PassMode<'ir> {
    Normal,
    Sret(BasicTypeEnum<'ir>),
}

impl<'ctx: 'ir, 'ir> LLVMContext<'ctx> {
    fn get_ty_size(&self, t: BasicTypeEnum) -> u64 {
        self.machine.get_target_data().get_bit_size(&t)
    }
    fn create_union_variants(&self, ty: LIRType) -> Vec<BasicTypeEnum<'ir>> {
        match ty {
            LIRType::Union(variants) => variants
                .iter()
                .map(|v| self.convert_aggregate_ty(*v))
                .collect(),
            _ => panic!("Not a union {ty:?}"),
        }
    }

    fn create_variant_instance(&self, ty: LIRType, idx: usize) -> BasicTypeEnum<'ctx> {
        if let LIRType::Union(variants) = ty {
            let the_variant = variants[idx];
            let variant_ty = self.convert_aggregate_ty(the_variant);

            self.context
                .struct_type(&[self.context.i8_type().into(), variant_ty], true)
                .into()
        } else {
            panic!("Not a union: {ty:?}")
        }
    }

    fn convert_aggregate_ty(&self, ty: LIRType) -> BasicTypeEnum<'ir> {
        match ty {
            LIRType::Struct(fields) => self
                .context
                .struct_type(
                    fields
                        .iter()
                        .map(|f| self.convert_aggregate_ty(*f))
                        .collect_vec()
                        .as_slice(),
                    false,
                )
                .into(),
            LIRType::ClosureEnv(..) => self.convert_aggregate_ty(ty.closure_to_struct_rep()),
            LIRType::Union(variants) => {
                let max_variant_size_bytes = self
                    .create_union_variants(ty)
                    .iter()
                    .map(|t| self.get_ty_size(*t) / 8)
                    .max()
                    .unwrap();

                self.context
                    .struct_type(
                        &[
                            self.context.i8_type().as_basic_type_enum(),
                            self.context
                                .i8_type()
                                .array_type(max_variant_size_bytes.try_into().unwrap())
                                .as_basic_type_enum(),
                        ],
                        true,
                    )
                    .into()
            }
            // LIRType::Closure(_, ret) => self.convert_struct_ty(*ret),
            _ => ty.convert(self),
        }
    }
    fn make_func_type(&self, ret: LIRType, args: &[LIRType]) -> (FunctionType<'ir>, PassMode<'ir>) {
        if ret.is_alloca() {
            let sret_pointer = ret.convert(self);
            let sret_ty = self.convert_aggregate_ty(ret);
            let void_ty = self.context.void_type();
            let mut args = args
                .iter()
                .map(|arg| {
                    let bty = self.convert_aggregate_ty(*arg);
                    BasicMetadataTypeEnum::from(bty)
                })
                .collect_vec();
            args.insert(
                0,
                BasicMetadataTypeEnum::PointerType(sret_pointer.into_pointer_type()),
            );
            // dbg!(ret);
            (void_ty.fn_type(&args, false), PassMode::Sret(sret_ty))
        } else {
            let ret_ty = self.convert_aggregate_ty(ret);

            // dbg!(ret, ret_ty);
            (
                ret_ty.fn_type(
                    &args
                        .iter()
                        .map(|arg| {
                            let bty = self.convert_aggregate_ty(*arg);
                            BasicMetadataTypeEnum::from(bty)
                        })
                        .collect_vec(),
                    false,
                ),
                PassMode::Normal,
            )
        }
    }

    fn codegen_item(&self, item: Item) -> inkwell::values::AnyValueEnum<'ir> {
        let name = Self::make_func_name(item.id);
        let (ty, pass_mode) =
            self.make_func_type(item.ret_ty, &item.params.iter().map(|p| p.ty).collect_vec());
        let fn_val = self.module.add_function(&name, ty, Some(Linkage::External));
        self.current_func.replace(Some(fn_val));
        let bb_entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(bb_entry);
        match pass_mode {
            PassMode::Normal => {
                // Update the symbol table with the args names and references
                // to their LLVM values, need to resolve them in current function
                // scope
                self.sym_table.borrow_mut().clear();
                for (arg, var) in fn_val.get_params().into_iter().zip(item.params) {
                    self.sym_table.borrow_mut().insert(var, arg);
                }

                let ir_body = self.codegen_ir(item.body, None);

                self.builder
                    .build_return(Some(&ir_body))
                    .expect("Failed to build return");

                assert!(fn_val.verify(true), "Failed to verify func");

                fn_val.as_any_value_enum()
            }
            PassMode::Sret(sret_ty) => {
                // Same as above, but now skip the sret pointer at the start.
                self.sym_table.borrow_mut().clear();
                for (arg, var) in fn_val.get_params().into_iter().skip(1).zip(item.params) {
                    self.sym_table.borrow_mut().insert(var, arg);
                }

                let sret_attr = self.context.create_type_attribute(
                    Attribute::get_named_enum_kind_id("sret"),
                    sret_ty.as_any_type_enum(), // the type being returned
                );
                let out_slot = fn_val.get_nth_param(0).unwrap().into_pointer_value();
                fn_val.add_attribute(AttributeLoc::Param(0), sret_attr); // Inkwell uses 0-based for params

                let ir_body = self.codegen_ir(item.body, Some(out_slot));
                self.builder.build_return(None);

                assert!(fn_val.verify(true), "Failed to verify func");

                fn_val.as_any_value_enum()
            }
        }
    }
    fn make_func_name(id: ItemId) -> String {
        format!("flare_f_{}", id.0)
    }

    fn make_args<'a>(args: &'a [BasicValueEnum<'ir>]) -> Vec<BasicMetadataValueEnum<'ir>> {
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
                }
            })
            .collect()
    }

    fn build_alloca(
        &self,
        ty: impl BasicType<'ctx>,
        out_slot: Option<PointerValue<'ctx>>,
    ) -> PointerValue<'ctx> {
        match out_slot {
            Some(s) => s,
            None => self
                .builder
                .build_alloca(ty, "alloca")
                .expect("Could not build alloca"),
        }
    }
    #[allow(clippy::cast_sign_loss)]
    fn codegen_ir(
        &self,
        ir: LIR,
        out_slot: Option<PointerValue<'ctx>>, // pre-allocated slot from caller, if needed
    ) -> BasicValueEnum<'ir> {
        match ir {
            LIR::Var(var) => {
                if let Some(llvm_val) = self.sym_table.borrow().get(&var) {
                    *llvm_val
                } else {
                    panic!("Undefined var {var:?}")
                }
            }
            LIR::Int(i) => self.context.i32_type().const_int(i as u64, false).into(),
            LIR::Str(s) => self.context.const_string(s.as_bytes(), false).into(),
            LIR::Unit => self.context.i8_type().const_int(0u64, false).into(),
            LIR::Float(f) => {
                let float_type = self.context.f32_type();
                float_type.const_float(f64::from(f.0)).into()
            }
            LIR::ClosureBuild(fun_ty, id, ref vars) => {
                self.codegen_closure(&ir, out_slot, id, vars)
            }
            LIR::Apply(fun, arg) => {
                self.handle_app(*fun, vec![*arg], |arg| self.codegen_ir(arg, None))
            }
            LIR::BulkApply(fun, arg_lirs) => {
                self.handle_app(*fun, arg_lirs, |arg| self.codegen_ir(arg, None))
            }

            LIR::Local(var, def, body) => {
                let v = if var.ty.is_alloca() {
                    let local_slot = self
                        .builder
                        .build_alloca(self.convert_aggregate_ty(var.ty), "local_alloca")
                        .expect("Could not alloca for local");
                    self.codegen_ir(*def, Some(local_slot))
                } else {
                    self.codegen_ir(*def, None)
                };
                self.sym_table.borrow_mut().insert(var, v);
                self.codegen_ir(*body, out_slot)
            }
            LIR::Access(obj, idx) => self.codegen_access(*obj, idx),
            LIR::Struct(_) => self.codegen_struct(ir, out_slot),
            LIR::Field(obj, idx) => self.codegen_field(&obj, idx.try_into().unwrap()),
            LIR::Case(..) => self.codegen_case(ir, out_slot),
            LIR::Tag(ty, idx, body) => self.codegen_tag(out_slot, ty, idx, &body),
            LIR::Item(id, lirtype) => {
                let name = Self::make_func_name(id);
                let item_fn = self.module.get_function(&name).unwrap();
                item_fn
                    .as_global_value()
                    .as_pointer_value()
                    .as_basic_value_enum()
            }

            LIR::Extern(intern, lirtype) => todo!(),
            LIR::BinOp(left, bin_op, right) => self.codegen_binop(*left, bin_op, *right),
            LIR::If(c, t, o) => {
                let cond_value = self.codegen_ir(*c, None).into_int_value();
                let the_func = self.current_func.borrow().unwrap();
                let cmp = self
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::NE,
                        cond_value,
                        self.context.bool_type().const_zero(),
                        "ifcmp",
                    )
                    .unwrap();
                let then_block = self.context.append_basic_block(the_func, "thenbb");
                let else_block = self.context.append_basic_block(the_func, "elsebb");
                let merge_block = self.context.append_basic_block(the_func, "mergebb");
                self.builder
                    .build_conditional_branch(cmp, then_block, else_block);
                self.builder.position_at_end(then_block);
                let then_res = self.codegen_ir(*t, None);
                self.builder.build_unconditional_branch(merge_block);

                self.builder.position_at_end(else_block);
                let else_res = self.codegen_ir(*o, None);
                self.builder.build_unconditional_branch(merge_block);

                self.builder.position_at_end(merge_block);
                let phi = self
                    .builder
                    .build_phi(then_res.get_type(), "if-phi")
                    .unwrap();
                phi.add_incoming(&[(&then_res, then_block), (&else_res, else_block)]);

                phi.as_basic_value()
            }
        }
    }

    fn codegen_closure(
        &self,
        ir: &LIR,
        out_slot: Option<PointerValue<'ctx>>,
        id: ItemId,
        vars: &[Var],
    ) -> BasicValueEnum<'ir> {
        let name = Self::make_func_name(id);
        let closure_ty = ir.get_fn_ty();
        let closure_struct_ty = self.convert_aggregate_ty(closure_ty);
        if closure_ty.is_alloca() {
            let closure_struct = self.build_alloca(closure_struct_ty, out_slot);
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
                .convert_aggregate_ty(closure_ty.closure_to_struct_rep().into_struct_fields()[1]);

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
                        idx.try_into().unwrap(),
                        "closure_field_gep",
                    )
                    .unwrap();
                self.builder
                    .build_store(capt_field, val)
                    .expect("Could not store closure env field");
            }

            closure_struct.into()
        } else {
            let item_fn = self
                .module
                .get_function(&name)
                .unwrap()
                .as_global_value()
                .as_pointer_value()
                .as_basic_value_enum();
            let env_struct_ty = self
                .convert_aggregate_ty(closure_ty.closure_to_struct_rep().into_struct_fields()[1]);
            let env_vals = vars
                .iter()
                .enumerate()
                .map(|(idx, var)| {
                    self.sym_table
                        .borrow()
                        .get(var)
                        .copied()
                        .expect("Undefined variable")
                })
                .collect_vec();
            let env_struct = {
                let undef = env_struct_ty.into_struct_type().get_undef();
                let mut s: AggregateValueEnum = undef.into();
                for (idx, val) in env_vals.into_iter().enumerate() {
                    s = self
                        .builder
                        .build_insert_value(s, val, idx as u32, "env_insert")
                        .unwrap();
                }
                s.into_struct_value().into()
            };

            let closure_struct = {
                let undef = closure_struct_ty.into_struct_type().get_undef();
                let mut s: AggregateValueEnum = undef.into();
                for (idx, val) in [item_fn, env_struct].into_iter().enumerate() {
                    s = self
                        .builder
                        .build_insert_value(s, val, idx as u32, "env_insert")
                        .unwrap();
                }
                s.into_struct_value()
            };
            closure_struct.into()
        }
    }

    fn codegen_tag(
        &self,
        out_slot: Option<PointerValue<'ctx>>,
        ty: LIRType,
        idx: usize,
        body: &LIR,
    ) -> BasicValueEnum<'ir> {
        let union_ty = self.convert_aggregate_ty(ty);
        if ty.is_alloca() {
            let union_ptr = self.build_alloca(union_ty, out_slot);
            let variant_instance_ty = self.create_variant_instance(ty, idx);
            let variant_ptr = self.build_alloca(variant_instance_ty, None);
            let tag_ptr = self
                .builder
                .build_struct_gep(variant_instance_ty, variant_ptr, 0, "tag_gep")
                .expect("Could not gep tag");
            self.builder
                .build_store(tag_ptr, self.context.i8_type().const_int(idx as u64, false));
            let body = self.codegen_ir(body.clone(), None);
            let body_ptr = self
                .builder
                .build_struct_gep(variant_instance_ty, variant_ptr, 1, "body_gep")
                .expect("Could not gep body");
            self.builder.build_store(body_ptr, body);
            self.builder
                .build_bit_cast(variant_ptr, union_ptr.get_type(), "cast_variant")
                .unwrap()
        } else {
            let union_ty = union_ty.into_struct_type();
            let agg = union_ty.get_undef();

            let agg = self
                .builder
                .build_insert_value(
                    agg,
                    self.context.i8_type().const_int(idx as u64, false),
                    0,
                    "tag_store",
                )
                .unwrap();
            let body = self.codegen_ir(body.clone(), None);

            let slot = self
                .builder
                .build_alloca(body.get_type(), "cast_slot")
                .unwrap();
            self.builder.build_store(slot, body).unwrap();
            let bytes = self
                .builder
                .build_load(union_ty.get_field_type_at_index(1).unwrap(), slot, "bytes")
                .unwrap();

            let agg = self
                .builder
                .build_insert_value(agg, bytes, 1, "body_store")
                .unwrap();

            agg.into_struct_value().into()
        }
    }

    fn codegen_case(&self, ir: LIR, out_slot: Option<PointerValue<'ctx>>) -> BasicValueEnum<'ir> {
        if let LIR::Case(ty, scrutinee, branches) = ir {
            let the_func = self.current_func.borrow().unwrap();
            let scrutinee_ty = scrutinee.type_of();
            let union_ty = self.convert_aggregate_ty(scrutinee_ty);
            let scrutinee_bv = self.codegen_ir(*scrutinee.clone(), None);
            let ret_ty = self.convert_aggregate_ty(ty);

            let merge_block = self.context.append_basic_block(the_func, "case_merge");
            if scrutinee.type_of().is_alloca() {
                let tag_ptr = self
                    .builder
                    .build_struct_gep(union_ty, scrutinee_bv.into_pointer_value(), 0, "tag_gep")
                    .expect("Could not gep tag");
                let tag_value = self
                    .builder
                    .build_load(self.context.i8_type(), tag_ptr, "load-tag")
                    .unwrap()
                    .into_int_value();
                // Create one basic block per branch.
                let branch_blocks = (0..branches.len())
                    .map(|i| {
                        (
                            self.context.i8_type().const_int(i as u64, false),
                            self.context
                                .append_basic_block(the_func, &format!("case_arm_{i}")),
                        )
                    })
                    .collect_vec();
                let trap_block = self.context.append_basic_block(the_func, "trap_block");
                let switch = self
                    .builder
                    .build_switch(tag_value, trap_block, &branch_blocks)
                    .unwrap();
                let mut phi_incoming: Vec<(BasicValueEnum<'ctx>, BasicBlock)> = Vec::new();
                for (branch_idx, branch_lir) in branches.iter().enumerate() {
                    self.builder.position_at_end(branch_blocks[branch_idx].1);

                    let variant = self
                        .create_variant_instance(scrutinee_ty, branch_idx)
                        .into_struct_type();
                    let variant_body_ty = variant.get_field_type_at_index(1).unwrap();
                    let scrutinee_as_tag = self
                        .builder
                        .build_struct_gep(
                            variant,
                            scrutinee_bv.into_pointer_value(),
                            1,
                            "scrutinee_as_variant_gep",
                        )
                        .expect("Could not cast scrutinee into variant");
                    let v = self.handle_app(branch_lir.clone(), vec![scrutinee_as_tag], |arg| {
                        self.builder
                            .build_load(variant_body_ty, arg, "variant_cast")
                            .unwrap()
                    });
                    self.builder
                        .build_unconditional_branch(merge_block)
                        .unwrap();
                    phi_incoming.push((v, branch_blocks[branch_idx].1));
                }
                // Trap block.
                self.builder.position_at_end(trap_block);
                self.builder.build_unreachable().unwrap();
                {
                    self.builder.position_at_end(merge_block);
                    let phi = self.builder.build_phi(ret_ty, "case_phi").unwrap();
                    for (val, from_block) in &phi_incoming {
                        phi.add_incoming(&[(val, *from_block)]);
                    }
                    phi.as_basic_value()
                }
            } else {
                let scrutinee_bv = scrutinee_bv.into_struct_value();
                let tag_value = self
                    .builder
                    .build_extract_value(scrutinee_bv, 0, "extract_tag")
                    .unwrap()
                    .into_int_value(); // Create one basic block per branch.
                let branch_blocks = (0..branches.len())
                    .map(|i| {
                        (
                            self.context.i8_type().const_int(i as u64, false),
                            self.context
                                .append_basic_block(the_func, &format!("case_arm_{i}")),
                        )
                    })
                    .collect_vec();
                let trap_block = self.context.append_basic_block(the_func, "trap_block");
                let switch = self
                    .builder
                    .build_switch(tag_value, trap_block, &branch_blocks)
                    .unwrap();
                let mut phi_incoming: Vec<(BasicValueEnum<'ctx>, BasicBlock)> = Vec::new();
                for (branch_idx, branch_lir) in branches.iter().enumerate() {
                    self.builder.position_at_end(branch_blocks[branch_idx].1);

                    let variant = self
                        .create_variant_instance(scrutinee.type_of(), branch_idx)
                        .into_struct_type();
                    let variant_body_ty = variant.get_field_type_at_index(1).unwrap();
                    let slot = self
                        .builder
                        .build_alloca(scrutinee_bv.get_type(), "cast_slot")
                        .unwrap();

                    self.builder.build_store(slot, scrutinee_bv).unwrap();

                    let payload_ptr = self
                        .builder
                        .build_struct_gep(scrutinee_bv.get_type(), slot, 1, "payload_gep")
                        .unwrap();

                    let scrutinee_as_tag = self
                        .builder
                        .build_load(variant_body_ty, payload_ptr, "payload_load")
                        .unwrap();
                    let v = self.handle_app(branch_lir.clone(), vec![scrutinee_as_tag], |arg| arg);

                    self.builder
                        .build_unconditional_branch(merge_block)
                        .unwrap();

                    phi_incoming.push((v, branch_blocks[branch_idx].1));
                }
                // Trap block.
                self.builder.position_at_end(trap_block);
                self.builder.build_unreachable().unwrap();
                {
                    self.builder.position_at_end(merge_block);
                    let phi = self.builder.build_phi(ret_ty, "case_phi").unwrap();
                    for (val, from_block) in &phi_incoming {
                        phi.add_incoming(&[(val, *from_block)]);
                    }
                    phi.as_basic_value()
                }
            }
        } else {
            panic!("Not a case")
        }
    }

    fn codegen_access(&self, obj: LIR, idx: usize) -> BasicValueEnum<'ir> {
        let obj_ty = obj.type_of();
        if obj_ty.is_alloca() {
            let obj_ptr = self.codegen_ir(obj, None).into_pointer_value();
            if idx == 0 {
                let fn_gep = self
                    .builder
                    .build_struct_gep(
                        self.convert_aggregate_ty(obj_ty), // actual struct type
                        obj_ptr,
                        0,
                        "gep_closure_func",
                    )
                    .unwrap();
                self.builder
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        fn_gep,
                        "load_closure_func",
                    )
                    .unwrap()
            } else {
                let idx = idx - 1;
                let env_ty = obj_ty.into_struct_fields().into_iter().nth(1).unwrap();

                let field_ty = env_ty.into_struct_fields().into_iter().nth(idx).unwrap();
                let env_ty = self.convert_aggregate_ty(env_ty);
                let env_gep = self
                    .builder
                    .build_struct_gep(
                        self.convert_aggregate_ty(obj_ty),
                        obj_ptr,
                        1u32,
                        "gep_closure_env",
                    )
                    .unwrap();
                let capt_gep = self
                    .builder
                    .build_struct_gep(env_ty, env_gep, idx as u32, "gep_closure_field")
                    .unwrap();

                self.builder
                    .build_load(
                        self.convert_aggregate_ty(field_ty),
                        capt_gep,
                        "load_closure_capt",
                    )
                    .unwrap()
            }
        } else {
            let obj_struct = self.codegen_ir(obj, None).into_struct_value();
            if idx == 0 {
                self.builder
                    .build_extract_value(obj_struct, 0, "closure_fn")
                    .unwrap()
            } else {
                let idx: u32 = idx as u32 - 1;
                let env = self
                    .builder
                    .build_extract_value(obj_struct, 1, "closure_env")
                    .unwrap()
                    .into_struct_value();
                self.builder
                    .build_extract_value(env, idx, "closure_field")
                    .unwrap()
            }
        }
    }

    fn codegen_struct(&self, ir: LIR, out_slot: Option<PointerValue<'ctx>>) -> BasicValueEnum<'ir> {
        let struct_lirty = ir.type_of();
        let struct_ty = self.convert_aggregate_ty(struct_lirty);
        if let LIR::Struct(fields) = ir {
            if struct_lirty.is_alloca() {
                // TODO: Maybe incorrect. Review later.
                // Use the provided slot, or alloca a local one if none provided
                let the_struct = self.build_alloca(struct_ty, out_slot);
                for (i, field) in fields.iter().enumerate() {
                    let field_ptr = self
                        .builder
                        .build_struct_gep(struct_ty, the_struct, i as u32, "field_gep")
                        .expect("Could not gep struct field");
                    let field_val = self.codegen_ir(field.clone(), None);
                    self.builder
                        .build_store(field_ptr, field_val)
                        .expect("Could not store struct field");
                }
                the_struct.into()
            } else {
                let undef = struct_ty.into_struct_type().get_undef();
                let mut s: AggregateValueEnum = undef.into();
                for (idx, field) in fields.into_iter().enumerate() {
                    let val = self.codegen_ir(field, None);
                    s = self
                        .builder
                        .build_insert_value(s, val, idx as u32, "field_store")
                        .unwrap();
                }
                s.into_struct_value().into()
            }
        } else {
            panic!("Not a struct")
        }
    }

    fn codegen_field(&self, obj: &LIR, idx: u32) -> BasicValueEnum<'ir> {
        // dbg!(&obj);
        let struct_ty = self.convert_aggregate_ty(obj.type_of()).into_struct_type();
        let obj = self.codegen_ir(obj.clone(), None);
        if obj.is_pointer_value() {
            let obj = obj.into_pointer_value();

            let pointer_idx = self
                .builder
                .build_struct_gep(struct_ty, obj, idx, "struct_gep")
                .unwrap();
            self.builder
                .build_load(
                    struct_ty.get_field_type_at_index(idx).unwrap(),
                    pointer_idx,
                    "load_field",
                )
                .unwrap()
        } else if obj.is_struct_value() {
            let obj = obj.into_struct_value();

            self.builder
                .build_extract_value(obj, idx, "get_field")
                .unwrap()
        } else {
            panic!("Not a struct")
        }
    }

    fn codegen_binop(&self, left: LIR, bin_op: BinOp, right: LIR) -> BasicValueEnum<'ir> {
        let lhs = self.codegen_ir(left, None);
        let rhs = self.codegen_ir(right, None);

        match bin_op {
            BinOp::Eq => self
                .builder
                .build_float_compare(
                    inkwell::FloatPredicate::OEQ,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "feq",
                )
                .map(|v| v.as_basic_value_enum()),
            BinOp::Neq => todo!(),
            BinOp::Gt => todo!(),
            BinOp::Lt => todo!(),
            BinOp::Gte => todo!(),
            BinOp::Lte => todo!(),
            BinOp::Add => self
                .builder
                .build_float_add(lhs.into_float_value(), rhs.into_float_value(), "add")
                .map(|v| v.as_basic_value_enum()),
            BinOp::Sub => self
                .builder
                .build_float_sub(lhs.into_float_value(), rhs.into_float_value(), "sub")
                .map(|v| v.as_basic_value_enum()),
            BinOp::Mul => self
                .builder
                .build_float_mul(lhs.into_float_value(), rhs.into_float_value(), "mul")
                .map(|v| v.as_basic_value_enum()),
            BinOp::Div => self
                .builder
                .build_float_div(lhs.into_float_value(), rhs.into_float_value(), "div")
                .map(|v| v.as_basic_value_enum()),
            BinOp::And => todo!(),
            BinOp::Or => todo!(),
        }
        .expect("Failed to complete op")
    }

    fn handle_app<T>(
        &self,
        fun: LIR,
        arg_lirs: Vec<T>,
        arg_f: impl Fn(T) -> BasicValueEnum<'ir>,
    ) -> BasicValueEnum<'ir> {
        let fun_ty = fun.get_fn_ty();
        let (fun_ptr, the_fun_ty, args, passmode) =
            if let LIRType::ClosureEnv(fpointer_ty, _) = fun_ty {
                self.handle_closure_app(fun, arg_lirs, arg_f, fun_ty, fpointer_ty)
            } else {
                self.handle_normal_app(fun, arg_lirs, arg_f, fun_ty)
            };

        self.make_call(fun_ptr, the_fun_ty, args, &passmode)
    }

    fn handle_closure_app<T>(
        &self,
        closure: LIR,
        args: Vec<T>,
        arg_f: impl Fn(T) -> BasicValueEnum<'ir>,
        closure_ty: LIRType,
        fpointer_ty: internment::Intern<LIRType>,
    ) -> (
        PointerValue<'ir>,
        FunctionType<'ir>,
        Vec<BasicValueEnum<'ir>>,
        PassMode<'ir>,
    ) {
        let (mut arg_tys, ret_ty) = fpointer_ty.destructure_closure();

        let closure_ty = closure_ty.closure_to_struct_rep();

        arg_tys.insert(0, closure_ty);
        let (the_fun_ty, passmode) = self.make_func_type(ret_ty, &arg_tys);

        if closure_ty.is_alloca() {
            let closure_and_env_pointer = self.codegen_ir(closure, None).into_pointer_value();

            let closure_struct_ty = self.convert_aggregate_ty(closure_ty);

            let fun_gep = self
                .builder
                .build_struct_gep(closure_struct_ty, closure_and_env_pointer, 0, "fun-gep")
                .expect("Could not gep closure struct");
            let fun_pointer = self
                .builder
                .build_load(
                    self.context.ptr_type(AddressSpace::default()),
                    fun_gep,
                    "fun_pointer",
                )
                .expect("Could not get closure func pointer")
                .into_pointer_value();

            let mut args = args.into_iter().map(arg_f).collect_vec();

            args.insert(0, closure_and_env_pointer.into());
            (fun_pointer, the_fun_ty, args, passmode)
        } else {
            let closure_and_env_struct = self.codegen_ir(closure, None).into_struct_value();
            let func = self
                .builder
                .build_extract_value(closure_and_env_struct, 0, "closure_fn")
                .unwrap()
                .into_pointer_value();

            let mut args = args.into_iter().map(arg_f).collect_vec();

            args.insert(0, closure_and_env_struct.into());
            (func, the_fun_ty, args, passmode)
        }
    }

    fn handle_normal_app<T>(
        &self,
        fun: LIR,
        args: Vec<T>,
        arg_f: impl Fn(T) -> BasicValueEnum<'ir>,
        fun_ty: LIRType,
    ) -> (
        PointerValue<'ir>,
        FunctionType<'ir>,
        Vec<BasicValueEnum<'ir>>,
        PassMode<'ir>,
    ) {
        let (arg_tys, ret) = fun_ty.destructure_closure();
        let (fun_ty, passmode) = self.make_func_type(ret, &arg_tys);
        let fun = self.codegen_ir(fun, None);
        let args = args.into_iter().map(arg_f).collect_vec();

        match fun {
            BasicValueEnum::PointerValue(pointer) => (pointer, fun_ty, args, passmode),
            _ => panic!("Invalid function: {fun:?}"),
        }
    }

    fn make_call(
        &self,
        fun_ptr: PointerValue<'ir>,
        the_fun_ty: FunctionType<'ir>,
        mut args: Vec<BasicValueEnum<'ir>>,
        passmode: &PassMode<'ir>,
    ) -> BasicValueEnum<'ir> {
        match passmode {
            PassMode::Normal => {
                let args = Self::make_args(&args);

                self.builder
                    .build_indirect_call(the_fun_ty, fun_ptr, &args, "closure_indirect")
                    .expect("Could not build closure call")
                    .try_as_basic_value()
                    .unwrap_basic()
            }
            PassMode::Sret(sret_ty) => {
                // Alloca in the caller
                let result_slot = self.builder.build_alloca(*sret_ty, "sret_slot").unwrap();
                args.insert(0, result_slot.as_basic_value_enum());
                let args = Self::make_args(&args);

                let sret_attr = self.context.create_type_attribute(
                    Attribute::get_named_enum_kind_id("sret"),
                    sret_ty.as_any_type_enum(), // the type being returned
                );
                let call = self
                    .builder
                    .build_indirect_call(the_fun_ty, fun_ptr, &args, "call_indirect")
                    .expect("Could not build closure call");
                call.add_attribute(AttributeLoc::Param(0), sret_attr);
                match call.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(bv) => bv,
                    inkwell::values::ValueKind::Instruction(instruction_value) => {
                        result_slot.as_basic_value_enum()
                    }
                }
            }
        }
    }

    fn generate_main_func(&self, flare_entry_id: ItemId) -> AnyValueEnum<'ir> {
        let name = "main";
        let (ty, passmode) = self.make_func_type(LIRType::Int, &[]);
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

        assert!(fn_val.verify(true), "Failed to verify func");
        fn_val.as_any_value_enum()
    }
}

enum FunctionPurpose {
    Closure,
    Main,
    Normal,
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
        let machine = make_target_machine();
        let llvm_ctx = LLVMContext::new(&ctx, machine);

        for (item, purpose) in funcs {
            match purpose {
                FunctionPurpose::Normal | FunctionPurpose::Closure => {
                    // log::info!("{}", item);
                    llvm_ctx.codegen_item(item);
                    // log::info!("---------------------------------")
                }

                FunctionPurpose::Main => {
                    let id = item.id;
                    // log::info!("{}", item);
                    let main_id = llvm_ctx.codegen_item(item);
                    // log::info!("------------main---------");
                    llvm_ctx.generate_main_func(id);

                    // log::info!("---------------------------------")
                }
            }
        }

        // Then run a pipeline using Clang-style pass pipeline strings
        let options = PassBuilderOptions::create();

        llvm_ctx
            .module
            .run_passes("default<O2>", &llvm_ctx.machine, options)
            .unwrap();

        let bit = llvm_ctx.module.write_bitcode_to_memory();
        bit.as_slice().to_vec()
        // llvm_ctx.compile(&cli.output.as_path(), FileType::Object);
        // Generate the object file.
        // product.emit().expect("Could not emit bytes")
    }

    fn ext(&self) -> &'static str {
        "bc"
    }
}
