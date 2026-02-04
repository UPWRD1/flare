use std::sync::Arc;

use cranelift::codegen::ir::{Function};
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
    scope: FxHashMap<Var, VirtualValue>,
    module: &'module mut ObjectModule,
    types: &'module LookupTable,
          
}

impl<'builder_ctx, 'module> IRConverter<'builder_ctx,'module> {
    fn new(func:&'builder_ctx mut Function, func_ctx: &'builder_ctx mut FunctionBuilderContext,
         module: &'module mut ObjectModule,types: &'module LookupTable) -> Self {
        Self {builder:  FunctionBuilder::new(func,func_ctx), scope:FxHashMap::default(), module, types}
        
    }
   
    fn convert_bin_op(&mut self, left: LIR, op: BinOp, right: LIR) -> VirtualValue {
        fn convert_cmp(builder: &mut FunctionBuilder, left:Value, op: BinOp, right: Value) -> VirtualValue{
            VirtualValue::Scalar(match op {
                BinOp::Eq => builder.ins().fcmp(FloatCC::Equal, left, right),
                BinOp::Neq => todo!(),
                BinOp::Gt => todo!(),
                BinOp::Lt => todo!(),
                BinOp::Gte => todo!(),
                BinOp::Lte => todo!(),
                _ => unreachable!("Bad op")
            })
        }

        fn convert_arith(builder: &mut FunctionBuilder, left:Value, op: BinOp, right: Value) -> VirtualValue {
            VirtualValue::Scalar(match op {
                BinOp::Add => builder.ins().fadd(left, right),
                BinOp::Sub => builder.ins().fsub(left,right),
                BinOp::Mul => builder.ins().fmul(left, right),
                BinOp::Div => builder.ins().fdiv(left,right),
                _ => unreachable!("Bad op")
            })
        }
        let left = self.convert_lir(left).as_scalar();
        let right = self.convert_lir(right).as_scalar();
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

    fn get_var(&mut self, var: Var) -> VirtualValue {
        let block_params = self.builder.block_params(self.builder.current_block().expect("Not in block"));
        // TODO: use stack based representation for variables??
        if let Some(v) = self.scope.get(&var) {
            v.clone()
        } else {
            panic!("Undefined variable")
        }
       
                // let index= var.id.0;
    }

    // This returns a vec, which usually only contains one item.
    // This is an obvious performance issue, and should probably be fixed in the future.
    fn convert_lir(&mut self, lir: LIR) -> VirtualValue {
    
         match lir {
            LIR::Var(var) => {
               self.get_var(var)                            
                
            },
            LIR::Int(n) => self.int(n),              
            LIR::Str(intern) => todo!(),
            LIR::Unit => todo!(),
            LIR::Float(f) => self.float(f),
            LIR::ClosureBuild(_, closure_id,capt_vars ) => todo!(),
            LIR::Apply(func, arg) => {
                let func = self.convert_lir(*func);
                let arg = self.convert_lir(*arg);
                self.call_func(func, vec![arg])
            },
            LIR::BulkApply(func, args) => {
                let func = self.convert_lir(*func);
                let args = args.into_iter().map(|arg| self.convert_lir(arg)).collect();
                self.call_func(func, args)
            },
            LIR::Local(var, defn, body) => {
                let var_ty = translate_ty(self.module, defn.type_of());
                let defn = self.convert_lir(*defn);
                       
                self.scope.insert(var, defn);
                // // let body_block= self.builder.create_block();
                // // self.builder.append_block_params_for_function_params(body_block);
                // // self.builder.append_block_param(body_block, var_ty);
                
                
                // // let current_block = self.builder.current_block().expect("not_in_block");
                // // let mut block_args = self.builder.block_params(current_block).to_vec();
                // // block_args.push(defn);
                
                // // let new_block_args:Vec<_> = block_args.iter().copied().map(BlockArg::Value).collect();                
                
                // self.builder.ins().jump(body_block, new_block_args.as_slice());
                // self.builder.switch_to_block(body_block);
                self.convert_lir(*body)
                
            },
            LIR::Access(closure, idx) => {
                let closure = self.convert_lir(*closure);
                self.destruct_field(&closure, idx)
            },
            LIR::Struct(fields) => {
                let (types, fields): (Vec<_>, Vec<_>) = fields.into_iter().map(|f| (f.type_of(), self.convert_lir(f))).unzip();
                
                self.construct_struct(types.as_slice().into(), &fields)
                            },
            LIR::Field(obj, idx) => {
                let obj = &self.convert_lir(*obj);
                self.destruct_field(obj, idx)
            },
            LIR::Case(lir, lirs) => todo!(),
            LIR::Item(item_id, _) =>  {
                // solo item indicates no args
let the_fun = FuncId::from_u32(item_id.0);
let the_fun = self.module.declare_func_in_func(the_fun, self.builder.func);
let the_call = self.builder.ins().call(the_fun, &Vec::new());
                
VirtualValue::Scalar(self.builder.inst_results(the_call)[0])
            },
            LIR::Extern(name, t) => {
                let (param_tys, ret_ty) = t.destructure_closure();
                let params = param_tys.into_iter().map(|ty| {
                     translate_ty(self.module, ty)
                   
                }).map(AbiParam::new).collect();
                
                let returns = vec![translate_ty(self.module, ret_ty)].into_iter().map(AbiParam::new).collect();
                let sig = Signature {
                    params,
                    returns,
                    call_conv: self.module.isa().default_call_conv()
                };
                let the_fun = self.module.declare_function(&name, Linkage::Import, &sig).expect("could not declare extern");

let the_fun = self.module.declare_func_in_func(the_fun, self.builder.func);
let the_call = self.builder.ins().call(the_fun, &Vec::new());

            VirtualValue::Scalar(self.builder.inst_results(the_call)[0])                          },
            LIR::BinOp(left, op, right) =>self.convert_bin_op(*left, op, *right),
        }
        
    }

    fn convert(mut self, sig: Signature, item: Item) {
        self.builder.func.signature = sig;
        
                
        let entry_block = self.builder.create_block();
        self.builder.append_block_params_for_function_params(entry_block);
        self.builder.switch_to_block(entry_block);

        let ret_val = self.convert_lir(item.body);
        self.return_(ret_val);
       
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
            
            LIRType::Union(lirtypes) => todo!(),
            LIRType::Struct(_)    |            LIRType::Closure(..) |            LIRType::ClosureEnv(..)  => module.isa().pointer_type(),          
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
    fn new(isa: Arc<dyn isa::TargetIsa>, items:&[(Item, FunctionPurpose)]) -> Self {
        let mut module = {
            let translation_unit_name = b"flarec";
            let libcall_names = cranelift::module::default_libcall_names();
            let builder =
                ObjectBuilder::new(isa.clone(), translation_unit_name, libcall_names).expect("Could not create ObjectBuilder");
            ObjectModule::new(builder)
        };

        let ctx = codegen::Context::new();
        let fctx = FunctionBuilderContext::new();
        let lookuptable = LookupTable::new(&mut module, items);
        Self {
            isa,
            module,
            ctx,
            fctx,lookuptable
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
  
    fn generate_function(&mut self,  item: Item) -> FuncId {
         let (function_id, sig) = self.lookuptable.declare_func(&mut self.module, &item.id);
let converter = IRConverter::new(&mut self.ctx.func, &mut self.fctx, &mut self.module, &self.lookuptable);
        converter.convert(sig, item);
                      
        //println!("fn {function_name}:\n{}", &self.ctx.func);

        self.module
            .define_function(function_id, &mut self.ctx)
            .expect("Could not define function");
        self.ctx.clear();
        function_id
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

        let len =  lir.len();
        let funcs: Vec<(Item, FunctionPurpose)> = lir
            .into_iter().enumerate()
            .flat_map(|(idx, cco)|{
                let mut new_items:Vec<(Item, FunctionPurpose)> = Vec::with_capacity(cco.closure_items.len() + 1);
                new_items.extend(cco.closure_items.into_values().map(|closure| (closure, FunctionPurpose::Closure)));
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
                FunctionPurpose::Normal | FunctionPurpose::Closure => {native_gen.generate_function(item);},
                
                FunctionPurpose::Main => {
                    let main_id = native_gen.generate_function(item);
native_gen.generate_main_func(main_id);
                },
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
