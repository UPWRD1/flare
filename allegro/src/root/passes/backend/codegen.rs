use std::collections::HashMap;

//use inkwell::basic_block::BasicBlock;
use inkwell::builder::{Builder, BuilderError};
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{
     AnyValue, AnyValueEnum, AsValueRef, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, InstructionValue, PointerValue };
use inkwell::FloatPredicate;

use crate::root::resource::ast::{self, BinOp, Expr, Function, Stmt};

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub function: &'a Function,

    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    /// Gets a defined function given its name.
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    /// Returns the `FunctionValue` representing the function being compiled.
    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    /// Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.context.i32_type(), name).unwrap()
    }

    pub fn build_load(&self, ptr: PointerValue<'ctx>, name: &str) -> BasicValueEnum<'ctx> {
        self.builder.build_load(ptr, name).unwrap()
    }

    /// Compiles the specified `Expr` into an LLVM `FloatValue`.
    fn compile_expr(&mut self, expr: Expr) -> Result<AnyValueEnum<'ctx>, &'static str> {
        match expr {
            Expr::Scalar(i) => match i {
                crate::root::resource::itypes::Itype::Mute => Err("Used mute value"),
                crate::root::resource::itypes::Itype::Int(nb) => Ok(AnyValueEnum::IntValue(
                    self.context
                        .i32_type()
                        .const_int(nb.unwrap().try_into().unwrap(), true),
                )),
                crate::root::resource::itypes::Itype::Flt(nb) => Ok(AnyValueEnum::FloatValue(
                    self.context.f64_type().const_float(nb.unwrap()),
                )),
                crate::root::resource::itypes::Itype::Str(s) => Ok(AnyValueEnum::ArrayValue(
                    self.context.const_string(
                        s.unwrap()
                            .chars()
                            .collect::<Vec<char>>()
                            .iter()
                            .map(|f| *f as u8)
                            .collect::<Vec<u8>>()
                            .as_slice(),
                        false,
                    ),
                )),
                crate::root::resource::itypes::Itype::Bool(_) => todo!(),
            },

            Expr::Variable(ref name) => match self.variables.get(name.as_str()) {
                Some(var) => Ok(self.build_load(*var, name.as_str()).into()),
                None => Err("Could not find a matching variable."),
            },

            // Expr::VarIn {
            //     ref variables,
            //     ref body,
            // } => {
            //     let mut old_bindings = Vec::new();

            //     for (var_name, initializer) in variables {
            //         let var_name = var_name.as_str();

            //         let initial_val = match *initializer {
            //             Some(ref init) => self.compile_expr(init)?,
            //             None => self.context.f64_type().const_float(0.),
            //         };

            //         let alloca = self.create_entry_block_alloca(var_name);

            //         self.builder.build_store(alloca, initial_val).unwrap();

            //         if let Some(old_binding) = self.variables.remove(var_name) {
            //             old_bindings.push(old_binding);
            //         }

            //         self.variables.insert(var_name.to_string(), alloca);
            //     }

            //     let body = self.compile_expr(body)?;

            //     for binding in old_bindings {
            //         self.variables
            //             .insert(binding.get_name().to_str().unwrap().to_string(), binding);
            //     }

            //     Ok(body)
            // },
            Expr::BinaryOp(op, e) => {
                let (left, right) = *e;
                if op == BinOp::Assign {
                    // handle assignment
                    let var_name = match left {
                        Expr::Variable(ref var_name) => var_name,
                        _ => {
                            return Err("Expected variable as left-hand operator of assignment.");
                        }
                    };

                    let var_val = self.compile_expr(right)?;
                    let var = match self
                        .variables
                        .get(var_name.as_str()) {
                            Some(v) => *v,
                            None => {
                                let t = var_val.clone().as_value_ref();
                                let y = unsafe {  PointerValue::new(t) };
                                y
                            },
                        };

                    self.builder
                        .build_store::<BasicValueEnum>(var, var_val.try_into().unwrap())
                        .unwrap();

                    Ok(var_val)
                } else {
                    let lhs = self.compile_expr(left)?.into_float_value();
                    let rhs = self.compile_expr(right)?.into_float_value();

                    match op {
                        BinOp::Plus => Ok(AnyValueEnum::FloatValue(
                            self.builder.build_float_add(lhs, rhs, "tmpadd").unwrap(),
                        )),
                        BinOp::Minus => Ok(AnyValueEnum::FloatValue(
                            self.builder.build_float_sub(lhs, rhs, "tmpsub").unwrap(),
                        )),
                        BinOp::Mult => Ok(AnyValueEnum::FloatValue(
                            self.builder.build_float_mul(lhs, rhs, "tmpmul").unwrap(),
                        )),
                        BinOp::Div => Ok(AnyValueEnum::FloatValue(
                            self.builder.build_float_div(lhs, rhs, "tmpdiv").unwrap(),
                        )),
                        BinOp::Less => Ok({
                            let cmp = self
                                .builder
                                .build_float_compare(FloatPredicate::ULT, lhs, rhs, "tmpcmp")
                                .unwrap();

                            AnyValueEnum::FloatValue(
                                self.builder
                                    .build_unsigned_int_to_float(
                                        cmp,
                                        self.context.f64_type(),
                                        "tmpbool",
                                    )
                                    .unwrap(),
                            )
                        }),
                        BinOp::Greater => Ok({
                            let cmp = self
                                .builder
                                .build_float_compare(FloatPredicate::ULT, rhs, lhs, "tmpcmp")
                                .unwrap();

                            AnyValueEnum::FloatValue(
                                self.builder
                                    .build_unsigned_int_to_float(
                                        cmp,
                                        self.context.f64_type(),
                                        "tmpbool",
                                    )
                                    .unwrap(),
                            )
                        }),

                        custom => {
                            let mut name = String::from("binary");

                            name.push(custom.to_char());

                            match self.get_function(name.as_str()) {
                                Some(fun) => {
                                    match self
                                        .builder
                                        .build_call(fun, &[lhs.into(), rhs.into()], "tmpbin")
                                        .unwrap()
                                        .try_as_basic_value()
                                        .left()
                                    {
                                        Some(value) => {
                                            Ok(AnyValueEnum::FloatValue(value.into_float_value()))
                                        }
                                        None => Err("Invalid call produced."),
                                    }
                                }

                                None => Err("Undefined binary operator."),
                            }
                        }
                    }
                }
            }

            Expr::Call { args, name, on } => match self.get_function(&name.as_str()) {
                Some(fun) => {
                    let mut compiled_args = Vec::with_capacity(args.len());

                    for arg in args {
                        compiled_args.push(self.compile_expr(arg)?);
                    }

                    let argsv: Vec<BasicMetadataValueEnum> = compiled_args
                        .iter()
                        .by_ref()
                        .map(|&val| val.try_into().unwrap())
                        .collect();

                    match self
                        .builder
                        .build_call(fun, argsv.as_slice(), "tmp")
                        .unwrap()
                        .try_as_basic_value()
                        .left()
                    {
                        Some(value) => Ok(AnyValueEnum::FloatValue(value.into_float_value())),
                        None => Err("Invalid call produced."),
                    }
                }
                None => Err("Unknown function."),
            },
            Expr::Array(_) => todo!(),
            Expr::UnaryOp(_, _) => todo!(),
            //Expr::Assign(a) => todo!(),
            Expr::FnExpr(_, _, _) => todo!(),
            // Expr:: {
            //     ref cond,
            //     ref consequence,
            //     ref alternative,
            // } => {
            //     let parent = self.fn_value();
            //     let zero_const = self.context.f64_type().const_float(0.0);

            //     // create condition by comparing without 0.0 and returning an int
            //     let cond = self.compile_expr(cond)?;
            //     let cond = self
            //         .builder
            //         .build_float_compare(FloatPredicate::ONE, cond, zero_const, "ifcond")
            //         .unwrap();

            //     // build branch
            //     let then_bb = self.context.append_basic_block(parent, "then");
            //     let else_bb = self.context.append_basic_block(parent, "else");
            //     let cont_bb = self.context.append_basic_block(parent, "ifcont");

            //     self.builder.build_conditional_branch(cond, then_bb, else_bb).unwrap();

            //     // build then block
            //     self.builder.position_at_end(then_bb);
            //     let then_val = self.compile_expr(consequence)?;
            //     self.builder.build_unconditional_branch(cont_bb).unwrap();

            //     let then_bb = self.builder.get_insert_block().unwrap();

            //     // build else block
            //     self.builder.position_at_end(else_bb);
            //     let else_val = self.compile_expr(alternative)?;
            //     self.builder.build_unconditional_branch(cont_bb).unwrap();

            //     let else_bb = self.builder.get_insert_block().unwrap();

            //     // emit merge block
            //     self.builder.position_at_end(cont_bb);

            //     let phi = self.builder.build_phi(self.context.f64_type(), "iftmp").unwrap();

            //     phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

            //     Ok(phi.as_basic_value().into_float_value())
            // },

            //         Expr::For {
            //             ref var_name,
            //             ref start,
            //             ref end,
            //             ref step,
            //             ref body,
            //         } => {
            //             let parent = self.fn_value();

            //             let start_alloca = self.create_entry_block_alloca(var_name);
            //             let start = self.compile_expr(start)?;

            //             self.builder.build_store(start_alloca, start).unwrap();

            //             // go from current block to loop block
            //             let loop_bb = self.context.append_basic_block(parent, "loop");

            //             self.builder.build_unconditional_branch(loop_bb).unwrap();
            //             self.builder.position_at_end(loop_bb);

            //             let old_val = self.variables.remove(var_name.as_str());

            //             self.variables.insert(var_name.to_owned(), start_alloca);

            //             // emit body
            //             self.compile_expr(body)?;

            //             // emit step
            //             let step = match *step {
            //                 Some(ref step) => self.compile_expr(step)?,
            //                 None => self.context.f64_type().const_float(1.0),
            //             };

            //             // compile end condition
            //             let end_cond = self.compile_expr(end)?;

            //             let curr_var = self.build_load(start_alloca, var_name);
            //             let next_var = self
            //                 .builder
            //                 .build_float_add(curr_var.into_float_value(), step, "nextvar")
            //                 .unwrap();

            //             self.builder.build_store(start_alloca, next_var).unwrap();

            //             let end_cond = self
            //                 .builder
            //                 .build_float_compare(
            //                     FloatPredicate::ONE,
            //                     end_cond,
            //                     self.context.f64_type().const_float(0.0),
            //                     "loopcond",
            //                 )
            //                 .unwrap();
            //             let after_bb = self.context.append_basic_block(parent, "afterloop");

            //             self.builder
            //                 .build_conditional_branch(end_cond, loop_bb, after_bb)
            //                 .unwrap();
            //             self.builder.position_at_end(after_bb);

            //             self.variables.remove(var_name);

            //             if let Some(val) = old_val {
            //                 self.variables.insert(var_name.to_owned(), val);
            //             }

            //             Ok(self.context.f64_type().const_float(0.0))
            //         },
            //     }
        }
    }

    fn compile_statement(&mut self, st: Stmt) -> Result<InstructionValue<'ctx>, BuilderError> {
        match st {
            Stmt::Expr(e) =>  todo!(), //Ok(self)self.compile_expr(e),
            Stmt::Block(b) => {
                for s in b {
                     return self.compile_statement(s)
                }
                todo!()
                //self.builder.v;
                //return self.builder.build_int
            }
            Stmt::If(_, _) => todo!(),
            Stmt::While(_, _) => todo!(),
            Stmt::ForEach(_, _, _) => todo!(),
            Stmt::ForRange(_, _, _, _) => todo!(),

            Stmt::Return(r) => self.builder.build_return(Some(self.compile_expr(r).unwrap().into())),
            Stmt::Break => todo!(),
            Stmt::Continue => todo!(),
        }
    }

    /// Compiles the specified `Prototype` into an extern LLVM `FunctionValue`.
    fn compile_prototype(&self, proto: Function) -> Result<FunctionValue<'ctx>, &'static str> {
        let ret_type = self.context.i32_type();
        let args_types = std::iter::repeat(ret_type)
            .take(proto.args.len())
            .map(|f| f.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();
        let args_types = args_types.as_slice();

        let fn_type = self.context.i32_type().fn_type(args_types, false);
        let fn_val = self
            .module
            .add_function(&proto.name.name.as_str(), fn_type, None);

        // set arguments names
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.into_int_value().set_name(proto.args[i].name.as_str());
        }

        // finally return built prototype
        Ok(fn_val)
    }

    /// Compiles the specified `Function` into an LLVM `FunctionValue`.
    fn compile_fn(&mut self) -> Result<FunctionValue<'ctx>, &'static str> {
        let function = self.compile_prototype(self.function.clone())?;

        // got external function, returning only compiled prototype
        // if self.function.code. {
        //     return Ok(function);
        // }

        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);

        // update fn field
        self.fn_value_opt = Some(function);

        // build variables map
        self.variables.reserve(self.function.args.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = self.function.args[i].name.as_str();
            let alloca = self.create_entry_block_alloca(arg_name);

            self.builder.build_store(alloca, arg).unwrap();

            self.variables
                .insert(self.function.args[i].name.clone(), alloca);
        }

        // compile body
        let body = self.compile_statement(self.function.code.clone()).unwrap();


        // return the whole thing after verification and optimization
        if function.verify(true) {
            Ok(function)
        } else {
            unsafe {
                function.delete();
            }
            Err("Invalid generated function.")
        }
    }

    /// Compiles the specified `Function` in the given `Context` and using the specified `Builder` and `Module`.
    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
        function: &Function,
    ) -> Result<FunctionValue<'ctx>, &'static str> {
        let mut compiler = Compiler {
            context,
            builder,
            module,
            function,
            fn_value_opt: None,
            variables: HashMap::new(),
        };

        compiler.compile_fn()
    }
}
pub fn generate_code(prg: ast::Program) -> String {
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("tmp");

    let mut accum = "".to_string();

    for f in prg.funcs {
        accum = format!(
            "{}{}",
            accum,
            Compiler::compile(&context, &builder, &module, &f).expect("failed").to_string()
        );
    }
    accum
}
