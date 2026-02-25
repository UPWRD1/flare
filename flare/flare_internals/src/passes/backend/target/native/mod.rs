use std::sync::Arc;

use cranelift::codegen::ir::{BlockArg, BlockCall, Function};
use cranelift::module::{FuncId, Linkage, Module};
use cranelift::object::{ObjectBuilder, ObjectModule};
use cranelift::prelude::*;
use rustc_hash::FxHashMap;

use crate::passes::backend::target::native::lookup::LookupTable;
use crate::passes::backend::{lir::ClosureConvertOut, target::Target};
use crate::resource::rep::backend::lir::{AppType, Item, LIR, Var};
use crate::resource::rep::backend::native::{PointeeType, VirtualValue};
use crate::resource::rep::backend::types::LIRType;
use crate::resource::rep::frontend::ast::BinOp;

pub mod closures;
pub mod functionbuilder;
pub mod lookup;

const ENTRYPOINT_FUNCTION_SYMBOL: &str = "main";

#[derive(Clone, Copy, Default)]
pub struct Native;

pub struct IRConverter<'builder_ctx, 'module> {
    builder: FunctionBuilder<'builder_ctx>,
    scope: FxHashMap<LIR, VirtualValue>,
    module: &'module mut ObjectModule,
    types: &'module LookupTable,
}

impl<'builder_ctx, 'module> IRConverter<'builder_ctx, 'module> {
    fn new(
        func: &'builder_ctx mut Function,
        func_ctx: &'builder_ctx mut FunctionBuilderContext,
        module: &'module mut ObjectModule,
        types: &'module LookupTable,
    ) -> Self {
        Self {
            builder: FunctionBuilder::new(func, func_ctx),
            scope: FxHashMap::default(),
            module,
            types,
        }
    }

    fn convert_bin_op(&mut self, left: LIR, op: BinOp, right: LIR) -> VirtualValue {
        fn convert_cmp(
            builder: &mut FunctionBuilder,
            left: Value,
            op: BinOp,
            right: Value,
            ty: LIRType,
        ) -> VirtualValue {
            VirtualValue::Scalar(
                match op {
                    BinOp::Eq => builder.ins().fcmp(FloatCC::Equal, left, right),
                    BinOp::Neq => todo!(),
                    BinOp::Gt => todo!(),
                    BinOp::Lt => todo!(),
                    BinOp::Gte => todo!(),
                    BinOp::Lte => todo!(),
                    _ => unreachable!("Bad op"),
                },
                ty,
            )
        }

        fn convert_arith(
            builder: &mut FunctionBuilder,
            left: Value,
            op: BinOp,
            right: Value,
            ty: LIRType,
        ) -> VirtualValue {
            VirtualValue::Scalar(
                match op {
                    BinOp::Add => builder.ins().fadd(left, right),
                    BinOp::Sub => builder.ins().fsub(left, right),
                    BinOp::Mul => builder.ins().fmul(left, right),
                    BinOp::Div => builder.ins().fdiv(left, right),
                    _ => unreachable!("Bad op"),
                },
                ty,
            )
        }
        let ty = left.type_of();
        let left = self.convert_lir(left).as_scalar();
        let right = self.convert_lir(right).as_scalar();
        match op {
            BinOp::Eq | BinOp::Neq | BinOp::Gt | BinOp::Lt | BinOp::Gte | BinOp::Lte => {
                convert_cmp(&mut self.builder, left, op, right, ty)
            }
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                convert_arith(&mut self.builder, left, op, right, ty)
            }
            BinOp::And => todo!(),
            BinOp::Or => todo!(),
        }
    }

    // fn get_var(&mut self, var: Var) -> VirtualValue {
    //     // dbg!(&self.scope);
    //     // TODO: use stack based representation for variables??
    //     if let Some(v) = self.scope.get(&LIR::Var(var)) {
    //         v.clone()
    //     } else {
    //         let block_params = self
    //             .builder
    //             .block_params(self.builder.current_block().expect("Not in block"));
    //         if let Some(v) = block_params.get(var.id.0) {
    //             VirtualValue::Scalar(*v)
    //         } else {
    //             // dbg!(block_params);
    //             panic!("Undefined variable {var:?}")
    //         }
    //     }

    //     // let index= var.id.0;
    // }

    fn scope_get(&self, var: &LIR) -> VirtualValue {
        if let Some(v) = self.scope.get(var) {
            v.clone()
        } else {
            panic!("Undefined variable {var:?}")
        }
    }

    // This returns a vec, which usually only contains one item.
    // This is an obvious performance issue, and should probably be fixed in the future.
    fn convert_lir(&mut self, lir: LIR) -> VirtualValue {
        match lir {
            LIR::Var(_) => self.scope_get(&lir),
            LIR::Int(n) => self.float(n as f32), //self.int(n),
            LIR::Str(intern) => todo!(),
            LIR::Unit => self.unit(),
            LIR::Float(f) => self.float(f),
            LIR::ClosureBuild(_, closure_id, capt_vars) => {
                let func_id = self.types.function_names.get(&closure_id).unwrap();
                let captures: Vec<_> = capt_vars
                    .iter()
                    .map(|v| self.scope_get(&LIR::Var(*v)))
                    .collect();
                VirtualValue::Closure(self.construct_closure(*func_id, &captures))
            }
            LIR::Apply(func, arg) => {
                let func = self.convert_lir(*func);
                // dbg!(&func);
                let arg = self.convert_lir(*arg);
                self.call_func(func, vec![arg])
            }
            LIR::BulkApply(func, args) => {
                let func = self.convert_lir(*func);
                let args = args.into_iter().map(|arg| self.convert_lir(arg)).collect();
                self.call_func(func, args)
            }
            LIR::Local(var, defn, body) => {
                let defn = self.convert_lir(*defn);
                self.scope.insert(LIR::Var(var), defn);
                self.convert_lir(*body)
            }
            LIR::Access(ref closure, idx) => {
                // dbg!(closure);
                if idx == 0 {
                    self.convert_lir(*closure.clone())
                } else {
                    let closure = LIR::Field(closure.clone(), idx - 1);
                    self.field(&closure, &lir, idx - 1)
                }
            }
            LIR::Struct(fields) => {
                let (types, fields): (Vec<_>, Vec<_>) = fields
                    .into_iter()
                    .map(|f| (f.type_of(), self.convert_lir(f)))
                    .unzip();
                let struct_ty = LIRType::Struct(types.as_slice().into());

                self.construct_struct(struct_ty, &fields)
            }
            LIR::Field(ref obj, idx) => self.field(&lir, obj, idx),
            LIR::Case(t, scrutinee, branches) => self.case(t, &scrutinee, branches),
            LIR::Tag(t, usize, b) => self.tag(&t.variants(), usize, *b),
            LIR::FuncRef(app_type) => self.convert_app_type(app_type),
            LIR::BinOp(left, op, right) => self.convert_bin_op(*left, op, *right),

            LIR::Item(item_id, t) => {
                // dbg!(item_id);
                let func_id = *self.types.function_names.get(&item_id).unwrap();
                // let fref = self.module.declare_func_in_func(func_id, self.builder.func);
                VirtualValue::Func(func_id, t)
            }
            LIR::Extern(name, t) => {
                let (fparams, fret) = t.destructure_closure();
                let sig =
                    self.types
                        .make_sig(self.module.isa().default_call_conv(), &fparams, fret);
                let the_fun = self
                    .module
                    .declare_function(&name, Linkage::Import, &sig)
                    .expect("could not declare extern");
                // let fref = self.module.declare_func_in_func(the_fun, self.builder.func);
                VirtualValue::Func(the_fun, t)
            }
        }
    }

    fn convert_app_type(&mut self, app_type: AppType) -> VirtualValue {
        match app_type {
            AppType::LIR(lir) => self.convert_lir(*lir),
            AppType::Item(item_id, t) => {
                let func_id = *self.types.function_names.get(&item_id).unwrap();
                let args = self.types.function_types.get(&func_id).unwrap().0.clone();
                VirtualValue::Func(func_id, t)
            }
            AppType::Extern(name, t) => {
                let (fparams, fret) = t.destructure_closure();
                let sig =
                    self.types
                        .make_sig(self.module.isa().default_call_conv(), &fparams, fret);
                let the_fun = self
                    .module
                    .declare_function(&name, Linkage::Import, &sig)
                    .expect("could not declare extern");
                // let fref = self.module.declare_func_in_func(the_fun, self.builder.func);
                VirtualValue::Func(the_fun, t)
            }
        }
    }

    fn field(&mut self, ref_expr: &LIR, obj: &LIR, idx: usize) -> VirtualValue {
        if let Some(obj_vv) = self.scope.get(ref_expr) {
            obj_vv.clone()
        } else {
            // dbg!(&obj);
            let obj_vv = self.convert_lir(obj.clone());
            dbg!(&obj_vv);
            if let VirtualValue::Scalar(..) = obj_vv {
                // struct was destructured in arguments
                obj_vv
            } else {
                self.destruct_field(&obj_vv, idx)
            }
        }
    }

    fn tag(&mut self, variants: &[LIRType], idx: usize, body: LIR) -> VirtualValue {
        let body_vv = self.convert_lir(body);
        // dbg!(&body_vv);
        let tag = self.ins().iconst(types::I32, idx as i64);
        VirtualValue::TaggedUnion {
            tag,
            body: self.make_payload(&body_vv, tag),
        }
    }

    fn case(&mut self, t: LIRType, scrutinee: &LIR, branches: Vec<LIR>) -> VirtualValue {
        let symbol = format!("case_forward_{}_{}", scrutinee, branches.len());
        let ret_ty = {
            let LIRType::ClosureEnv(c, _) = branches[0].type_of() else {
                panic!("Not a closure")
            };
            c.destructure_closure().1
        };
        // let mut fparams = vec![scrutinee.type_of()];
        // fparams.extend(branches.iter().map(|b| b.type_of().closure_to_struct_rep()));

        let mut fparams: Vec<_> = branches
            .iter()
            .map(|b| b.type_of().closure_to_struct_rep())
            .collect();
        fparams.push(scrutinee.type_of());

        // let (func_id, sig) =
        //     self.types
        //         .declare_func(self.module, Linkage::Local, &symbol, &fparams, ret_ty);
        let sig = self.types.make_sig(isa::CallConv::Fast, &fparams, ret_ty);
        // let scrutinee_v = self.as_values(&scrutinee_vv)[0];
        dbg!(&sig);
        let func_id = self.module.declare_anonymous_function(&sig).unwrap();
        // let func_id = self
        //     .module
        //     .declare_function(&symbol, Linkage::Local, &sig)
        //     .unwrap();

        let mut capture_params: Vec<Var> = vec![];
        {
            let mut ctx = codegen::Context::new();
            let mut fctx = FunctionBuilderContext::new();

            let mut converter = IRConverter::new(&mut ctx.func, &mut fctx, self.module, self.types);
            converter.builder.func.signature = sig.clone();
            converter.scope = self.scope.clone();

            // let scrutinee_vv = converter.convert_lir(scrutinee.clone());
            // let VirtualValue::TaggedUnion {
            //     // ref variants,
            //     body: _,
            //     tag,
            // } = scrutinee_vv
            // else {
            //     panic!("Can't case on non-union")
            // };

            let case_block = converter.builder.create_block();
            converter
                .builder
                .append_block_params_for_function_params(case_block);
            converter.builder.switch_to_block(case_block);

            // let tag_ty = converter.types.union_tag_type(variants.len());
            // let tag = converter.ins().iconst(tag_ty, idx as i64);
            let case_block_params = converter.builder.block_params(case_block).to_vec();
            // Skip the tag
            // let mut case_block_params_iter = case_block_params;
            let mut offset = 0;
            let branch_calls: Vec<_> = branches
                .iter()
                .map(|l| {
                    let LIR::ClosureBuild(t, _, caps) = l else {
                        panic!("Not a closure")
                    };
                    capture_params.extend(caps);
                    let caps: Vec<_> = caps
                        .iter()
                        .map(|v| {
                            converter.type_to_virtual_value(
                                &mut |_, _| Value::from_u32(v.id.0 as u32),
                                false,
                                v.ty,
                            )
                        })
                        .collect();
                    // dbg!(t);
                    let block = converter.builder.create_block();
                    // Add scrutinee pointer

                    // Add captures
                    let mut args: Vec<_> = case_block_params
                        .iter()
                        .skip(offset)
                        .take(caps.len())
                        .map(|v| {
                            let v_ty = converter.type_of_value(*v);
                            converter.builder.append_block_param(block, v_ty);
                            BlockArg::Value(*v)
                        })
                        .collect();
                    let scrutinee_pointer = *case_block_params.last().unwrap();
                    let scrutinee_ty = converter.type_of_value(scrutinee_pointer);
                    converter.builder.append_block_param(block, scrutinee_ty);
                    args.push(BlockArg::Value(scrutinee_pointer));
                    offset += caps.len();
                    BlockCall::new(block, args, &mut converter.builder.func.dfg.value_lists)
                    // BlockCall::new(block, vec![], &mut converter.builder.func.dfg.value_lists)
                })
                .collect();

            // Declare the block for the default branch
            let trap = {
                let block = converter.builder.create_block();
                BlockCall::new(block, vec![], &mut converter.builder.func.dfg.value_lists)
            };

            // Create the table
            let table = {
                let table_data = JumpTableData::new(trap, &branch_calls);
                converter.builder.func.create_jump_table(table_data)
            };
            let tag = case_block_params[case_block_params.len() - 2]; // The tag is the penultimate element
            converter.builder.ins().br_table(tag, table);
            for (branch_lir, branch_block) in branches.into_iter().zip(branch_calls.iter()) {
                let LIR::ClosureBuild(t, id, ref caps) = branch_lir else {
                    panic!("Not a closure")
                };

                converter.switch_to_branch_block(*branch_block);

                let block_args = converter
                    .builder
                    .block_params(branch_block.block(&converter.builder.func.dfg.value_lists))
                    .to_vec();

                let branch_arg_tys = t.destructure_closure().0;
                // dbg!(&target_ty);
                let types: Vec<_> = branch_arg_tys
                    .iter()
                    .flat_map(|t| converter.types.convert_ty(*t))
                    .collect();

                let limit = block_args.len() - 1;
                let last = block_args[limit];
                let captures: Vec<_> = block_args
                    .into_iter()
                    .zip(branch_arg_tys)
                    .take(limit)
                    .map(|(v, t)| VirtualValue::Scalar(v, t))
                    .collect();
                let payload = converter.read_payload(last, &types);

                let closure_id = converter.types.function_names[&id];
                let new_closure = converter.construct_closure(closure_id, &captures);
                let closure = VirtualValue::Closure(new_closure);

                let call = converter.call_func_with_values(closure, payload);
                converter.return_(call);
            }

            // Trap the default block
            //
            // _ => unreachable!(),
            {
                converter.switch_to_branch_block(trap);

                const TRAP_UNREACHABLE: u8 = 100;

                converter
                    .builder
                    .ins()
                    .trap(TrapCode::user(TRAP_UNREACHABLE).unwrap());
            }

            self.module
                .define_function(func_id, &mut ctx)
                .expect("Could not define case function");
        }

        // This little dance is needed to call the function indirectly...
        let scrutinee_vv = self.convert_lir(scrutinee.clone());
        let size_t = self.types.ptr_type();
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let func_pointer = self.ins().func_addr(size_t, func_ref);
        let func = VirtualValue::Pointer(
            PointeeType::Func(fparams.as_slice().into(), ret_ty),
            func_pointer,
        );
        let mut params: Vec<_> = capture_params
            .into_iter()
            .map(|var| self.scope.get(&LIR::Var(var)).cloned().unwrap())
            .collect();
        params.push(scrutinee_vv);
        self.call_func(func, params)
    }

    pub fn create_entry_block(&mut self, params: &[LIRType]) -> (Block, Vec<VirtualValue>) {
        let block = self.builder.create_block();
        self.builder.seal_block(block);

        // See `LookupTable::create_signature` for more information
        if self.builder.func.signature.uses_struct_return_param() {
            let size_t = self.module.isa().pointer_type();
            self.builder.append_block_param(block, size_t);
        }

        let vparams = params
            .iter()
            .map(
                |&p| self.type_to_block_params(block, true, p), // self.scope.insert(var, vv.clone());
            )
            .collect();

        (block, vparams)
    }
    fn convert(mut self, sig: Signature, item: Item, func_id: &FuncId) {
        self.builder.func.signature = sig;
        // println!("{}", item);
        let params = self.types.function_types.get(func_id).unwrap().0.clone();

        let (entry_block, vparams) = self.create_entry_block(&params);
        self.builder.switch_to_block(entry_block);
        for (var, vparam) in item.params.into_iter().zip(vparams) {
            match vparam {
                VirtualValue::Scalar(..)
                | VirtualValue::StackStruct { .. }
                | VirtualValue::TaggedUnion { .. } => {
                    // dbg!(var);
                    self.scope.insert(LIR::Var(var), vparam);
                }
                VirtualValue::UnstableStruct { ty, ref fields } => {
                    let the_types = ty.into_struct_fields();
                    self.scope.insert(LIR::Var(var), vparam.clone());
                    for (i, (fp, ty)) in fields.iter().zip(the_types.iter()).enumerate() {
                        let new_var = LIR::index(LIR::Var(var), i);
                        self.scope.insert(new_var, fp.clone());
                    }
                }
                VirtualValue::Closure(closure) => todo!(),
                VirtualValue::Func(..) => todo!(),
                VirtualValue::Pointer(..) => {
                    self.scope.insert(LIR::Var(var), vparam);
                }
            }
            // dbg!(&vparam);
        }

        let ret_val = self.convert_lir(item.body);
        self.return_(ret_val);

        // self.builder.seal_block(entry_block);
        self.builder.seal_all_blocks();
        self.builder.finalize();
    }
}

pub struct NativeGen {
    isa: Arc<dyn isa::TargetIsa>,
    module: ObjectModule,
    ctx: codegen::Context,
    fctx: FunctionBuilderContext,
    lookuptable: LookupTable,
}

impl NativeGen {
    fn new(isa: Arc<dyn isa::TargetIsa>, items: &[(Item, FunctionPurpose)]) -> Self {
        let mut module = {
            let translation_unit_name = b"flarec";
            let libcall_names = cranelift::module::default_libcall_names();
            let builder = ObjectBuilder::new(isa.clone(), translation_unit_name, libcall_names)
                .expect("Could not create ObjectBuilder");
            ObjectModule::new(builder)
        };

        let ctx = codegen::Context::new();
        let fctx = FunctionBuilderContext::new();
        let lookuptable = LookupTable::new(&mut module, items);
        Self {
            isa,
            module,
            ctx,
            fctx,
            lookuptable,
        }
    }

    fn generate_function(&mut self, item: Item) -> FuncId {
        // let (function_id, sig) = self.lookuptable.declare_func(&mut self.module, &item.id);
        let function_id = *self.lookuptable.function_names.get(&item.id).unwrap();
        let sig = self
            .lookuptable
            .function_sigs
            .get(&function_id)
            .unwrap()
            .clone();
        let converter = IRConverter::new(
            &mut self.ctx.func,
            &mut self.fctx,
            &mut self.module,
            &self.lookuptable,
        );
        converter.convert(sig, item, &function_id);

        //println!("fn {function_name}:\n{}", &self.ctx.func);

        self.module
            .define_function(function_id, &mut self.ctx)
            .expect("Could not define function");
        self.ctx.clear();
        function_id
    }

    fn generate_main_func(&mut self, flare_main: FuncId) {
        let (main_func_id, sig) = self.lookuptable.declare_main(&mut self.module);
        // These contain the context needed for generating code for a function.
        //
        // It's a lot more efficient to construct them once, and then re-use them for all functions.
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.fctx);
        builder.func.signature = sig;

        // Create the functions entry block.
        let block0 = builder.create_block();
        builder.switch_to_block(block0);

        // When we know that there are no more blocks to be written which may jump to this block, we want to seal
        // it. This improves the quality of code generation.
        builder.seal_block(block0);

        let flare_entry_func = self.module.declare_func_in_func(flare_main, builder.func);

        let call_flare = builder.ins().call(flare_entry_func, &Vec::new());
        let result_value = {
            let temp_result = builder.inst_results(call_flare)[0];
            // if flare returns a float, cast it to an int for the exit code
            // if builder.func.signature.returns[0].value_type == types::F32 {
            builder.ins().fcvt_to_uint_sat(types::I32, temp_result)
            // } else {
            // temp_result
            //    }
        };

        builder.ins().return_(&[result_value]);
        if let Err(err) = codegen::verify_function(builder.func, self.isa.as_ref()) {
            panic!("verifier error: {err}");
        }

        builder.finalize();

        // println!("fn {ENTRYPOINT_FUNCTION_SYMBOL}:\n{}", &self.ctx.func);

        self.module
            .define_function(main_func_id, &mut self.ctx)
            .expect("Could not define main");

        self.ctx.clear();
    }
}

pub enum FunctionPurpose {
    Normal,
    Closure,
    Main,
}

impl Target for Native {
    type Input = ClosureConvertOut;
    type Output = Vec<u8>;
    #[allow(clippy::unwrap_used)]
    fn generate(&mut self, lir: Vec<ClosureConvertOut>) -> Self::Output {
        let isa = {
            let mut builder = settings::builder();

            // disable optimizations so disassembly will more directly correlated to our Cranelift usage
            builder.set("opt_level", "none").unwrap();

            builder.enable("is_pic").unwrap();

            let flags = settings::Flags::new(builder);

            let target_triple = "arm64-apple-darwin";

            isa::lookup_by_name(target_triple)
                .unwrap()
                .finish(flags)
                .unwrap()
        };

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

        let mut native_gen = NativeGen::new(isa.clone(), &funcs);

        for (item, purpose) in funcs.into_iter() {
            match purpose {
                FunctionPurpose::Normal | FunctionPurpose::Closure => {
                    log::info!("{}", item);
                    native_gen.generate_function(item);
                    log::info!("---------------------------------")
                }

                FunctionPurpose::Main => {
                    let main_id = native_gen.generate_function(item);
                    native_gen.generate_main_func(main_id);
                }
            }
        }

        let product = native_gen.module.finish();
        // Generate the object file.
        product.emit().expect("Could not emit bytes")
    }

    fn ext(&self) -> &str {
        "o"
    }
}
