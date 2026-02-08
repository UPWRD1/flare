use std::sync::Arc;

use cranelift::codegen::ir::Function;
use cranelift::module::{FuncId, Linkage, Module};
use cranelift::object::{ObjectBuilder, ObjectModule};
use cranelift::prelude::*;
use rustc_hash::FxHashMap;

use crate::passes::backend::target::native::functionbuilder::LookupTable;
use crate::passes::backend::{lir::ClosureConvertOut, target::Target};
use crate::resource::rep::backend::lir::{Item, LIR, Var};
use crate::resource::rep::backend::native::VirtualValue;
use crate::resource::rep::backend::types::LIRType;
use crate::resource::rep::frontend::ast::BinOp;

pub mod closures;
pub mod functionbuilder;

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
        ) -> VirtualValue {
            VirtualValue::Scalar(match op {
                BinOp::Eq => builder.ins().fcmp(FloatCC::Equal, left, right),
                BinOp::Neq => todo!(),
                BinOp::Gt => todo!(),
                BinOp::Lt => todo!(),
                BinOp::Gte => todo!(),
                BinOp::Lte => todo!(),
                _ => unreachable!("Bad op"),
            })
        }

        fn convert_arith(
            builder: &mut FunctionBuilder,
            left: Value,
            op: BinOp,
            right: Value,
        ) -> VirtualValue {
            VirtualValue::Scalar(match op {
                BinOp::Add => builder.ins().fadd(left, right),
                BinOp::Sub => builder.ins().fsub(left, right),
                BinOp::Mul => builder.ins().fmul(left, right),
                BinOp::Div => builder.ins().fdiv(left, right),
                _ => unreachable!("Bad op"),
            })
        }
        let left = self.convert_lir(left).as_scalar();
        let right = self.convert_lir(right).as_scalar();
        match op {
            BinOp::Eq | BinOp::Neq | BinOp::Gt | BinOp::Lt | BinOp::Gte | BinOp::Lte => {
                convert_cmp(&mut self.builder, left, op, right)
            }
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                convert_arith(&mut self.builder, left, op, right)
            }
            BinOp::And => todo!(),
            BinOp::Or => todo!(),
        }
    }

    fn get_var(&mut self, var: Var) -> VirtualValue {
        // dbg!(&self.scope);
        // TODO: use stack based representation for variables??
        if let Some(v) = self.scope.get(&LIR::Var(var)) {
            v.clone()
        } else {
            let block_params = self
                .builder
                .block_params(self.builder.current_block().expect("Not in block"));
            if let Some(v) = block_params.get(var.id.0) {
                VirtualValue::Scalar(*v)
            } else {
                dbg!(block_params);
                panic!("Undefined variable {var:?}")
            }
        }

        // let index= var.id.0;
    }

    fn scope_get(&mut self, var: LIR) -> VirtualValue {
        if let Some(v) = self.scope.get(&var) {
            v.clone()
        } else {
            dbg!(&self.scope);
            panic!("Undefined variable {var:?}")
        }
    }

    // This returns a vec, which usually only contains one item.
    // This is an obvious performance issue, and should probably be fixed in the future.
    fn convert_lir(&mut self, lir: LIR) -> VirtualValue {
        match lir {
            LIR::Var(_) => self.scope_get(lir),
            LIR::Int(n) => self.float(n as f32), //self.int(n),
            LIR::Str(intern) => todo!(),
            LIR::Unit => self.unit(),
            LIR::Float(f) => self.float(f),
            LIR::ClosureBuild(_, closure_id, capt_vars) => {
                // dbg!(t);
                let func_id = self.types.function_names.get_by_right(&closure_id).unwrap();

                let captures: Vec<_> = capt_vars
                    .iter()
                    .map(|v| self.scope_get(LIR::Var(*v)))
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
                // let var_ty = translate_ty(self.module, defn.type_of());
                let defn = self.convert_lir(*defn);
                // self.builder.def_var(Variable::from_u32(var.id.0 as u32), defn.clone().as_value());
                // dbg!(var);
                self.scope.insert(LIR::Var(var), defn);
                self.convert_lir(*body)
            }
            LIR::Access(ref closure, idx) => {
                let closure = LIR::Field(closure.clone(), idx);
                self.field(&closure, &lir, idx)
                // let vv = self.convert_lir(closure);
                // self.destruct_field(&vv, idx)
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
            LIR::Case(lir, lirs) => todo!(),
            LIR::Item(item_id, _) => {
                // dbg!(item_id);
                let func_id = *self.types.function_names.get_by_right(&item_id).unwrap();
                // let fref = self.module.declare_func_in_func(func_id, self.builder.func);
                VirtualValue::Func(func_id)
            }
            LIR::Extern(name, t) => {
                let (fparams, fret) = t.destructure_closure();
                let sig = self
                    .types
                    .make_sig(self.module.isa().default_call_conv(), fparams, fret);
                let the_fun = self
                    .module
                    .declare_function(&name, Linkage::Import, &sig)
                    .expect("could not declare extern");
                // let fref = self.module.declare_func_in_func(the_fun, self.builder.func);
                VirtualValue::Func(the_fun)
            }
            LIR::BinOp(left, op, right) => self.convert_bin_op(*left, op, *right),
        }
    }

    fn field(&mut self, ref_expr: &LIR, obj: &LIR, idx: usize) -> VirtualValue {
        if let Some(obj_vv) = self.scope.get(ref_expr) {
            obj_vv.clone()
        } else {
            // dbg!(&obj);
            let obj_vv = self.convert_lir(obj.clone());
            // dbg!(obj_vv);
            if let VirtualValue::Scalar(_) = obj_vv {
                // struct was destructured in arguments
                obj_vv
            } else {
                self.destruct_field(&obj_vv, idx)
            }
        }
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
    fn convert(mut self, sig: Signature, item: Item) {
        self.builder.func.signature = sig;
        let params = &self.types.function_types.get(&item.id).unwrap().0;

        let (entry_block, vparams) = self.create_entry_block(params);
        self.builder.switch_to_block(entry_block);
        for (var, vparam) in item.params.into_iter().zip(vparams) {
            match vparam {
                VirtualValue::Scalar(_) | VirtualValue::StackStruct { .. } => {
                    self.scope.insert(LIR::Var(var), vparam);
                }
                VirtualValue::UnstableStruct { ty, fields } => {
                    let the_types = ty.into_struct_fields();
                    for (i, (fp, ty)) in fields.into_iter().zip(the_types.iter()).enumerate() {
                        let new_var = LIR::index(LIR::Var(var), i);
                        self.scope.insert(new_var, fp);
                    }
                }
                VirtualValue::Closure(closure) => todo!(),
                VirtualValue::Func(func_id) => todo!(),
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

fn translate_ty(module: &ObjectModule, ty: LIRType) -> types::Type {
    match ty {
        LIRType::Int => types::F32,
        LIRType::Float => types::F32,
        LIRType::String => module.isa().pointer_type(),
        LIRType::Unit => types::I8,

        LIRType::Union(lirtypes) => todo!(),
        LIRType::Closure(..)|// => module.isa().pointer_type(),
        LIRType::Struct(_) | LIRType::ClosureEnv(..) => {
            panic!("{ty:?}")
            // module.isa().pointer_type()
        }
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
        let (function_id, sig) = self.lookuptable.declare_func(&mut self.module, &item.id);
        let converter = IRConverter::new(
            &mut self.ctx.func,
            &mut self.fctx,
            &mut self.module,
            &self.lookuptable,
        );
        converter.convert(sig, item);

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
                if idx == len {
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
                    println!("{}", item);
                    native_gen.generate_function(item);
                    println!("---------------------------------")
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
