use std::sync::Arc;

use cranelift::codegen::ir::{BlockArg, Function};
use cranelift::module::{FuncId, Linkage, Module};
use cranelift::object::{ObjectBuilder, ObjectModule};
use cranelift::prelude::*;
use rustc_hash::FxHashMap;

use crate::passes::backend::{lir::ClosureConvertOut, target::Target};
use crate::resource::rep::backend::lir::{Item, LIR, Var};
use crate::resource::rep::backend::types::LIRType;
use crate::resource::rep::frontend::ast::BinOp;
use crate::resource::rep::midend::ir::ItemId;

const ENTRYPOINT_FUNCTION_SYMBOL: &str = "main";

#[derive(Clone, Copy, Default)]
pub struct Native;

// enum LocalOrParam {
//     Local(Value), // Value of the local variable
//     Param(usize), // Index of the parameter in the function signature
// }

pub struct IRConverter<'builder_ctx, 'module> {
    builder: FunctionBuilder<'builder_ctx>,
    scope: FxHashMap<Var, usize>,
       module: &'module mut ObjectModule
       }

impl<'builder_ctx, 'module> IRConverter<'builder_ctx,'module> {
    fn new(func:&'builder_ctx mut Function, func_ctx: &'builder_ctx mut FunctionBuilderContext,
         module: &'module mut ObjectModule) -> Self {
        Self {builder:  FunctionBuilder::new(func,func_ctx), scope: FxHashMap::default(), module,}
        
    }

    fn convert_bin_op(&mut self, left: LIR, op: BinOp, right: LIR) -> Value {
        fn convert_cmp(builder: &mut FunctionBuilder, left:Value, op: BinOp, right: Value) -> Value{
            match op {
                BinOp::Eq => builder.ins().fcmp(FloatCC::Equal, left, right),
                BinOp::Neq => todo!(),
                BinOp::Gt => todo!(),
                BinOp::Lt => todo!(),
                BinOp::Gte => todo!(),
                BinOp::Lte => todo!(),
                _ => unreachable!("Bad op")
            }
        }

        fn convert_arith(builder: &mut FunctionBuilder, left:Value, op: BinOp, right: Value) -> Value {
            match op {
                BinOp::Add => builder.ins().fadd(left, right),
                BinOp::Sub => builder.ins().fsub(left,right),
                BinOp::Mul => builder.ins().fmul(left, right),
                BinOp::Div => builder.ins().fdiv(left,right),
                _ => unreachable!("Bad op")
            }
        }
        let left = self.convert_lir(left)[0];
        let right = self.convert_lir(right)[0];
        match op {
            BinOp::Eq         |    BinOp::Neq |            
            BinOp::Gt |            BinOp::Lt |
            BinOp::Gte|            BinOp::Lte => convert_cmp(&mut self.builder, left, op, right),            
            BinOp::Add |
            BinOp::Sub |
            BinOp::Mul |
            BinOp::Div => convert_arith(&mut self.builder, left, op, right),
            BinOp::And => todo!(),
            BinOp::Or => todo!(),
        }
    }

    // This returns a vec, which usually only contains one item.
    // This is an obvious performance issue, and should probably be fixed in the future.
    fn convert_lir(&mut self, lir: LIR) -> Vec<Value> {
        let res = match lir {
            LIR::Var(var) => {
                // TODO: use stack based representation for variables
                let index =  self.scope.get(&var).expect("undefined variable");
                self.builder.block_params(self.builder.current_block().expect("Not in block"))[*index]
                
                
            },
            LIR::Int(n) => self.builder.ins().iconst(types::I32, n as i64), // Don't know why this conversion is needed, check docs later
            LIR::Str(intern) => todo!(),
            LIR::Unit => todo!(),
            LIR::Float(f) => self.builder.ins().f32const(f.0),
            LIR::Closure(lirtype, item_id, vars) => todo!(),
            LIR::Apply(lir, lir1) => todo!(),
            LIR::BulkApply(func, args) => {
                
let args: Vec<_> = args.into_iter().flat_map(|arg| self.convert_lir(arg)).collect();
                let the_fun = if let LIR::Item(id) = *func {
                                        FuncId::from_u32(id.0)
                    
                 } else {
                    todo!()
                };
                let the_fun = self.module.declare_func_in_func(the_fun, self.builder.func);
let the_call = self.builder.ins().call(the_fun, &args);
                
self.builder.inst_results(the_call)[0]
            },
            LIR::Local(var, defn, body) => {
                // let var_ty = translate_ty(var.ty);
                self.scope.insert(var, self.builder.block_params(self.builder.current_block().expect("not_in_block")).len() );
                let defn = self.convert_lir(*defn)[0];

let var_ty = self.builder.func.stencil.dfg.value_type(defn);
                let var = self.builder.declare_var(var_ty);
                self.builder.def_var(var, defn );
                let body_block= self.builder.create_block();
                self.builder.append_block_param(body_block, var_ty);
                // self.builder.seal_block(body_block);
                self.builder.ins().jump(body_block, vec![&BlockArg::Value(defn)]);
                self.builder.switch_to_block(body_block);
                self.convert_lir(*body)[0]
                
            },
            LIR::Access(lir, _) => todo!(),
            LIR::Struct(lirs) => todo!(),
            LIR::Field(lir, _) => todo!(),
            LIR::Case(lir, lirs) => todo!(),
            LIR::Item(item_id) =>  {
                // solo item indicates no args
let the_fun = FuncId::from_u32(item_id.0);
let the_fun = self.module.declare_func_in_func(the_fun, self.builder.func);
let the_call = self.builder.ins().call(the_fun, &Vec::new());
                
self.builder.inst_results(the_call)[0]
            },
            LIR::Extern(intern) => todo!(),
            LIR::BinOp(left, op, right) => self.convert_bin_op(*left, op, *right),
        };
        vec![res]
    }

    fn convert(mut self, sig: Signature, item: Item) {
        self.builder.func.signature = sig;
        for (idx, param) in item.params.into_iter().enumerate() {
            self.scope.insert(param, idx);
        }
                
        let entry_block = self.builder.create_block();
        self.builder.append_block_params_for_function_params(entry_block);
        self.builder.switch_to_block(entry_block);

        let ret_val = self.convert_lir(item.body);
        self.builder.ins().return_(&ret_val);
       
        self.builder.seal_block(entry_block);
        self.builder.seal_all_blocks();
        self.builder.finalize();
    }
}


fn translate_ty( ty: LIRType) -> types::Type {
        match ty {
            LIRType::Int => types::I32,
            LIRType::Float => types::F32,
            LIRType::String => types::I8X8,
            LIRType::Unit => todo!(),
            LIRType::Array(lirtypes) => todo!(),
            LIRType::Union(lirtypes) => todo!(),
            LIRType::Closure(lirtype, lirtype1) => todo!(),
            LIRType::ClosureEnv(lirtype, lirtypes) => todo!(),
        }
    }

pub struct NativeGen {
    isa: Arc<dyn isa::TargetIsa>,
    module: ObjectModule,
    ctx: codegen::Context,
    fctx: FunctionBuilderContext,
   }

impl NativeGen {
    fn new(isa: Arc<dyn isa::TargetIsa>) -> Self {
        let module = {
            let translation_unit_name = b"flarec";
            let libcall_names = cranelift::module::default_libcall_names();
            let builder =
                ObjectBuilder::new(isa.clone(), translation_unit_name, libcall_names).expect("Could not create ObjectBuilder");
            ObjectModule::new(builder)
        };

        let ctx = codegen::Context::new();
        let fctx = FunctionBuilderContext::new();
        Self {
            isa,
            module,
            ctx,
            fctx,
                    }
    }

    fn main_signature(&self) -> Signature {
        // The `CallConv` defines how primitives in parameters and return values are handled.
        // Mainly which registers are used and when stack spills are used.
        //
        // In general, it's best to use `CallConv::Fast`.
        //
        // However, since the function we define is invoked from our targeted OS, we need to use
        // the calling convention the OS expects.
        let call_conv = self.isa.default_call_conv();

        Signature {
            call_conv,
            params: vec![],
            // Since we're linking to libc, we can return the exit code from main.
            returns: vec![AbiParam::new(types::I32)],
        }
    }

    
    fn convert_signature(&self, item: &Item) -> Signature {
        let returns = vec![AbiParam::new(translate_ty(item.ret_ty.clone()))];
        let params = item.params.iter().map(|var| AbiParam::new(translate_ty(var.ty.clone()))).collect();
         Signature { params, returns, call_conv: self.isa.default_call_conv() }
    }

    fn preload_function(&mut self, item: &Item) -> (Signature, FuncId) {
        
 let function_name = format!("flare_f_{}", item.id.0);
        let sig = self.convert_signature(item);
        let function_id = self
            .module
            .declare_function(&function_name, Linkage::Export, &sig)
            .expect("Could not declare function");
        // self.itemid_to_funcid.insert(item.id, function_id);
        (sig, function_id)
    }

    fn generate_function(&mut self, sig: Signature, item: Item, function_id: FuncId, ) {
               // These contain the context needed for generating code for a function.
        //
        // It's a lot more efficient to construct them once, and then re-use them for all functions.

        let converter = IRConverter::new(&mut self.ctx.func, &mut self.fctx, &mut self.module);
        converter.convert(sig, item);
                      
        // println!("fn {function_name}:\n{}", &self.ctx.func);

        self.module
            .define_function(function_id, &mut self.ctx)
            .expect("Could not define function");
        self.ctx.clear();
            }

    fn generate_main_func(&mut self, flare_main: FuncId) {
        let sig = self.main_signature();

        let main_declaration_func_id = 
            // Add this function to our Module.
            self.module
                .declare_function(ENTRYPOINT_FUNCTION_SYMBOL, Linkage::Export, &sig)
                .expect("Could not declare main")
        ;
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
        let result_value = builder.inst_results(call_flare)[0];
        let cast = builder.ins().fcvt_to_uint_sat(types::I32, result_value);
        // Use the result of the addition as an exit code
        builder.ins().return_(&[cast]);

        if let Err(err) = codegen::verify_function(builder.func, self.isa.as_ref()) {
            panic!("verifier error: {err}");
        }

        builder.finalize();

        println!("fn {ENTRYPOINT_FUNCTION_SYMBOL}:\n{}", &self.ctx.func);

        self.module
            .define_function(main_declaration_func_id, &mut self.ctx)
            .expect("Could not define main");

        self.ctx.clear();
    }
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

        let mut native_gen = NativeGen::new(isa.clone());

    
                let funcs: Vec<Item> = lir.into_iter().flat_map(|mut cco|{cco.closure_items.insert(ItemId((cco.closure_items.len() + 1) as u32), cco.item); cco.closure_items.into_values() }).collect();

        
        let ids:Vec<(Signature, FuncId)> = funcs.iter().map(|func| native_gen.preload_function(func)).collect();
        
let main_id = ids.last().unwrap().1;
for (item, (sig, function_id)) in funcs.into_iter().zip(ids) {
            native_gen.generate_function(sig, item, function_id);
        }
        
        native_gen.generate_main_func(main_id);

        
        let product = native_gen.module.finish();

        // Generate the object file.

        product.emit().expect("Could not emit bytes")

               // todo!()
    }

    fn ext(&self) -> &str {
        "o"
    }
}
