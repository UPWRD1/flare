use std::sync::Arc;

use cranelift::codegen::ir::{BlockArg, Function};
use cranelift::module::{FuncId, Linkage, Module};
use cranelift::object::{ObjectBuilder, ObjectModule};
use cranelift::prelude::*;
use rustc_hash::{ FxHashMap, };

use crate::passes::backend::{lir::ClosureConvertOut, target::Target};
use crate::resource::rep::backend::lir::{Item, LIR, Var};
use crate::resource::rep::backend::types::LIRType;
use crate::resource::rep::frontend::ast::BinOp;

pub mod closures;
pub mod functionbuilder;

const ENTRYPOINT_FUNCTION_SYMBOL: &str = "main";

#[derive(Clone, Copy, Default)]
pub struct Native;

pub struct IRConverter<'builder_ctx, 'module> {
    builder: FunctionBuilder<'builder_ctx>,
    scope: FxHashMap<Var, usize>,
    module: &'module mut ObjectModule,
          
}

impl<'builder_ctx, 'module> IRConverter<'builder_ctx,'module> {
    fn new(func:&'builder_ctx mut Function, func_ctx: &'builder_ctx mut FunctionBuilderContext,
         module: &'module mut ObjectModule) -> Self {
        Self {builder:  FunctionBuilder::new(func,func_ctx), scope: FxHashMap::default(), module, }
        
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
    
         match lir {
            LIR::Var(var) => {
                // TODO: use stack based representation for variables
                let index =  self.scope.get(&var).expect("undefined variable");
               vec![self.builder.block_params(self.builder.current_block().expect("Not in block"))[*index]]
                
                
            },
            LIR::Int(n) => vec![self.builder.ins().iconst(types::I32, n as i64)], // Don't know why this conversion is needed, check docs later
             // LIR::Int(n) => vec![self.builder.ins().f32const(n as f32)],
            LIR::Str(intern) => todo!(),
            LIR::Unit => todo!(),
            LIR::Float(f) => vec![self.builder.ins().f32const(f.0)],
            LIR::ClosureBuild(lirtype, item_id, vars) => todo!(),
            LIR::Apply(func, arg) => {
                let arg = self.convert_lir(*arg);
                let the_fun = if let LIR::Item(id, ..) = *func {
                                        FuncId::from_u32(id.0)    
                 } else {
                    let f = self.convert_lir(*func);
                    todo!("{f:?}")
                };
                let the_fun = self.module.declare_func_in_func(the_fun, self.builder.func);
let the_call = self.builder.ins().call(the_fun, &arg);
                
self.builder.inst_results(the_call).to_vec()
            },
            LIR::BulkApply(func, args) => {
                
let args: Vec<_> = args.into_iter().flat_map(|arg| self.convert_lir(arg)).collect();
                let the_fun = if let LIR::Item(id, ..) = *func {
                                        FuncId::from_u32(id.0)
                    
                 }                  
              else {
                    todo!()
                };
                let the_fun = self.module.declare_func_in_func(the_fun, self.builder.func);
let the_call = self.builder.ins().call(the_fun, &args);
                
self.builder.inst_results(the_call).to_vec()
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
                self.convert_lir(*body)
                
            },
            LIR::Access(lir, _) => todo!(),
            LIR::Struct(fields) => {
                let field_values:Vec<_> = fields.into_iter().flat_map(|f| self.convert_lir(f)            ).collect();
                 vec![self.stack_alloc_struct(&field_values)]
                
              
                          },
            LIR::Field(lir, idx) => {
                if let LIR::Struct(fields) = *lir {
                    self.convert_lir(fields[idx].clone())
                } else {
                  let converted = self.convert_lir(*lir)[0];
                  let res = self.type_of_value(converted);
                  dbg!(res);
                  todo!()            }
            },
            LIR::Case(lir, lirs) => todo!(),
            LIR::Item(item_id, _) =>  {
                // solo item indicates no args
let the_fun = FuncId::from_u32(item_id.0);
let the_fun = self.module.declare_func_in_func(the_fun, self.builder.func);
let the_call = self.builder.ins().call(the_fun, &Vec::new());
                
self.builder.inst_results(the_call).to_vec()
            },
            LIR::Extern(name, t) => {
                let (param_tys, ret_ty) = t.destructure_closure();
                let params = param_tys.into_iter().map(|ty| {
                    let t = translate_ty(self.module, ty);
                    AbiParam::new(t)
                }).collect();
                let returns = vec![AbiParam::new(translate_ty(self.module, ret_ty))];
                let sig = Signature {
                    params,
                    returns,
                    call_conv: self.module.isa().default_call_conv()
                };
                let the_fun = self.module.declare_function(&name, Linkage::Import, &sig).expect("could not declare extern");

let the_fun = self.module.declare_func_in_func(the_fun, self.builder.func);
let the_call = self.builder.ins().call(the_fun, &Vec::new());

self.builder.inst_results(the_call).to_vec()                          },
            LIR::BinOp(left, op, right) => vec![self.convert_bin_op(*left, op, *right)],
        }
        
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


fn translate_ty(module: &ObjectModule, ty: LIRType) -> types::Type {
    match ty {
            LIRType::Int => types::I32,
            LIRType::Float => types::F32,
            LIRType::String => module.isa().pointer_type(),
            LIRType::Unit => todo!(),
            LIRType::Struct(lirtypes) => todo!(),
            LIRType::Union(lirtypes) => todo!(),
            LIRType::Closure(lirtype, lirtype1) => module.isa().pointer_type(),            
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

    

    pub fn convert_signature(&self, ref_params: &mut [Var], ret_ty: LIRType) -> Signature {
        
        let mut params = vec![];
        let mut returns = vec![];
        match ret_ty {
            LIRType::Struct(intern) => params.push(AbiParam::new(self.isa.pointer_type())),            LIRType::Union(intern) => todo!(),
            LIRType::Closure(l, r) => {
                params.push(AbiParam::new(self.isa.pointer_type()))
                // dbg!(l, r);
                // todo!()
            }
            LIRType::ClosureEnv(intern, intern1) => todo!(),
            _ => returns.push(AbiParam::new(translate_ty(&self.module, ret_ty))),
        }
        for param in ref_params {
            match param.ty {
                LIRType::Struct(intern) => params.push(AbiParam::new(self.isa.pointer_type())),
                LIRType::Union(intern) => todo!(),
                LIRType::Closure(l, r) => {
                    params.push(AbiParam::new(self.isa.pointer_type()))
                // dbg!(t, env);
                // todo!()
                }
                LIRType::ClosureEnv(t, env) => {
                    params.push(AbiParam::new(translate_ty(&self.module, *t)));
                    params.push(AbiParam::new(self.isa.pointer_type()))
                                    },
            _ => params.push(AbiParam::new(translate_ty(&self.module, param.ty))),
        }
        }

        Signature {
            params,
            returns,
            call_conv: self.module.isa().default_call_conv(),
        }
    }

    fn preload_function(&mut self, item: &mut Item) -> (Signature, FuncId) {
        // dbg!(&item);
        let ret_ty = item.body.type_of();
        let function_name = format!("flare_f_{}", item.id.0);
        let sig = self.convert_signature(&mut item.params, ret_ty);
        
        let function_id = self
            .module
            .declare_function(&function_name, Linkage::Export, &sig)
            .expect("Could not declare function");
        // self.itemid_to_funcid.insert(item.id, function_id);
        (sig, function_id)
    }

    fn generate_function(&mut self, sig: Signature, item: Item, function_id: FuncId) {
        let converter = IRConverter::new(&mut self.ctx.func, &mut self.fctx, &mut self.module);
        converter.convert(sig, item);
                      
        // println!("fn {function_name}:\n{}", &self.ctx.func);

        self.module
            .define_function(function_id, &mut self.ctx)
            .expect("Could not define function");
        self.ctx.clear();
    }

    fn generate_closure(&mut self, sig: Signature, item: Item, function_id: FuncId) {
       let converter = IRConverter::new(&mut self.ctx.func, &mut self.fctx, &mut self.module);
       dbg!(sig);
       todo!();
        let f = converter.declare_real_function_for_closure();
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
        enum IsClosure {
            Yes,
            No,
        }
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

    
        let mut funcs: Vec<(Item, IsClosure)> = lir
            .into_iter()
            .flat_map(|cco|{
                let mut new_items:Vec<(Item, IsClosure)> = Vec::with_capacity(cco.closure_items.len() + 1);
                new_items.extend(cco.closure_items.into_values().map(|closure| (closure, IsClosure::Yes)));
                new_items.push((cco.item, IsClosure::No));
                new_items
            })
            .collect();

        let ids:Vec<(Signature, FuncId)> = funcs
            .iter_mut()
            .map(|(func, _)| native_gen.preload_function(func))
            .collect();
        
        let main_id = ids.last().unwrap().1;
        
        for ((item, is_closure), (sig, function_id)) in funcs.into_iter().zip(ids) {
            if let IsClosure::Yes = is_closure {
                println!("closure");
                native_gen.generate_closure(sig, item, function_id);
            } else {
                native_gen.generate_function(sig, item, function_id);
            }
        }
        
        native_gen.generate_main_func(main_id);

        
        let product = native_gen.module.finish();
        // Generate the object file.
         product.emit().expect("Could not emit bytes")
        }

    fn ext(&self) -> &str {
        "o"
    }
}
