use crate::root::Quantifier;
use crate::{
    quantifier,
    root::resource::{
        cst::{Expr, SymbolType},
        errors::TypecheckingError,
    },
};
use anyhow::{ensure, Ok, Result};
use std::collections::HashMap;

use super::environment::{Environment, FunctionTableEntry, GenericValue, VariableTableEntry};

pub struct Typechecker {
    env: Environment,
    //current_func: Option<&'a mut FunctionTableEntry>,
    //current_method_parent: Option<String>,
}

impl Typechecker {
    pub fn new(env: Environment) -> Self {
        Typechecker {
            env: env.clone(),
            //current_func: None,
            //current_method_parent: None,
        }
    }

    fn compare_types(&mut self, l: &SymbolType, r: &SymbolType, is_method: bool) -> bool {
        //dbg!(l);
        //dbg!(r);
        if l.is_generic() && !r.is_generic() {
            // self.env.current_generics.entries.insert(
            //     l.get_generic_name(),
            //     GenericTableEntry {
            //         value: GenericValue::Perfect(l.get_generic_name(), Box::new(r.clone())),
            //     },
            // );

            true
        } else if !l.is_generic() && r.is_generic() {
            // self.env.current_generics.entries.insert(
            //     r.get_generic_name(),
            //     GenericTableEntry {
            //         value: GenericValue::Perfect(r.get_generic_name(), Box::new(l.clone())),
            //     },
            // );

            true
        } else if l.is_generic() && r.is_generic() {
            if l != r {
                // self.env.current_generics.set(
                //     l.get_generic_name(),
                //     GenericTableEntry {
                //         value: GenericValue::Ref(r.get_generic_name()),
                //     },
                // );
            }
            true
        } else if l.is_custom() && r.is_custom()
        /*&& r.get_generic_count() == l.get_generic_count()*/
        {
            true
        } else {
            if l.is_pointer() && r.is_pointer() {
                return self.compare_types(&l.extract(), &r.extract(), is_method);
            } else {
                if l.is_custom() && r.is_self() && is_method {
                    true
                } else {
                    l == r
                }
            }
        }
    }

    fn check_expr(&mut self, e: &Expr, func: &mut FunctionTableEntry) -> Result<SymbolType> {
        //dbg!(e);
        let res = match e {
            Expr::BinAdd { l, r }
            | Expr::BinSub { l, r }
            | Expr::BinMul { l, r }
            | Expr::BinDiv { l, r } => {
                let lhs_type = self.check_expr(l, func)?;
                let rhs_type = self.check_expr(r, func)?;
                ensure!(lhs_type == rhs_type);
                lhs_type
            }
            Expr::Logical { l, op, r } => {
                let lhs_type = self.check_expr(l, func)?;
                let rhs_type = self.check_expr(r, func)?;
                //dbg!(lhs_type.clone());
                //dbg!(rhs_type.clone());

                match op {
                    crate::root::resource::cst::LogicOp::Is => todo!(),
                    _ => assert!(lhs_type == rhs_type),
                }
                SymbolType::Bool
            }
            Expr::Assignment { name, value, and_in } => self.check_expr_assignment(name, value, func)?,
            Expr::Return { value } => self.check_expr_return(value, func)?,
            Expr::If {
                condition,
                then,
                otherwise,
            } => {
                let condition_ty = self.check_expr(condition, func)?;
                let then_type = self.check_expr(then, func)?;
                let otherwise_type = self.check_expr(otherwise, func)?;
                ensure!(then_type == otherwise_type);
                ensure!(condition_ty.is_bool());
                otherwise_type
            }
            Expr::Int(_) => SymbolType::Int,
            Expr::Naught => SymbolType::Unit,

            Expr::Uint(_) => SymbolType::Usize,
            Expr::Byte(_) => SymbolType::Byte,
            Expr::Flt(_) => SymbolType::Flt,
            Expr::Str(_) => SymbolType::Str,
            Expr::Char(_) => SymbolType::Char,
            Expr::Bool(_) => SymbolType::Bool,
            Expr::Symbol(name) => self.check_expr_symbol(name, func)?,
            Expr::StructInstance { name, fields } => {
                self.check_expr_structinstance(name, fields, func)?
            }
            Expr::FieldAccess(expr, v) => self.check_expr_field_access(expr, v, func)?,
            Expr::Call { name, args } => self.check_expr_call(name, args, func)?,
            Expr::MethodCall { obj, name, args } => {
                //dbg!(e);
                //dbg!(func.clone());
                self.check_expr_method_call(obj, name, args, func)?
            }
            Expr::VariantInstance { name: _, fields: _ } => {
                todo!();
                //self.check_expr_variantinstance(name, fields)?
            }
            Expr::Path(l, r) => self.check_path(l, r, func)?,
            Expr::AddressOf(t) => SymbolType::Pointer(Box::new(self.check_expr(t, func)?)),
            _ => todo!("{e:?}"),
        };
        Ok(res)
    }

    fn check_expr_structinstance(
        &mut self,
        name: &Expr,
        fields: &Vec<(String, Expr)>,
        func: &mut FunctionTableEntry,
    ) -> Result<SymbolType, anyhow::Error> {
        let name_string = name.get_symbol_name().unwrap();
        let the_struct = self
            .env
            .items
            .get(&quantifier!(Root, Type(name_string.clone()), End))
            .ok_or(TypecheckingError::UndefinedType {
                name: name_string.clone(),
            })?
            .clone();
        let defined_fields: Vec<(String, SymbolType)> = the_struct.to_ty().kind.get_fields()?;
        let mut real_fields: Vec<(String, SymbolType)> = vec![];
        for f in fields {
            real_fields.push((f.0.clone(), self.check_expr(&f.1, func)?));
        }
        assert_eq!(defined_fields.len(), fields.len());
        let mut generic_vec: Vec<SymbolType> = vec![];
        for ((def_fld_name, def_fld_ty), (fnd_fld_name, fnd_fld_ty)) in
            defined_fields.into_iter().zip(real_fields)
        {
            if !(def_fld_name == fnd_fld_name && self.compare_types(&def_fld_ty, &fnd_fld_ty, false)) {
                return Err(TypecheckingError::InvalidStructInstanceField {
                    obj: name_string,
                    field: def_fld_name,
                    expected: def_fld_ty,
                    found: fnd_fld_ty,
                }
                .into());
            }
            if def_fld_ty.is_generic() {
                if !fnd_fld_ty.is_generic() {
                    let v = SymbolType::Generic(GenericValue::Perfect(
                        def_fld_ty.get_generic_name(),
                        Box::new(fnd_fld_ty),
                    ));
                    generic_vec.push(v.clone());
                } else {
                    generic_vec.push(SymbolType::Generic(GenericValue::Ref(
                        def_fld_ty.get_generic_name(),
                    )));
                }
            }
        }
        // the_struct.generic_monomorphs.insert(
        //     generic_vec
        //         .clone()
        //         .iter()
        //         .map(|x| x.get_generic_value())
        //         .collect(),
        // );

        // self.env
        //     .usertype_table
        //     .get_mut(&name_string)
        //     .unwrap()
        //     .generic_monomorphs = the_struct.generic_monomorphs;
        Ok(SymbolType::Custom(name_string, generic_vec))
    }

    //     fn check_expr_variantinstance(
    //         &mut self,
    //         name: &Expr,
    //         fields: &Vec<Expr>,
    //     ) -> Result<SymbolType, anyhow::Error> {
    //         dbg!(name, fields);
    //         let name_string = name.get_symbol_name();
    //         let the_enum = self.env.usertype_table.get_id(&name_string).ok_or(
    //             TypecheckingError::UndefinedType {
    //                 name: name_string.clone(),
    //             },
    //         )?;
    //         let real_fields: Vec<SymbolType> = vec![];

    //         todo!();

    // //        Ok(SymbolType::Custom(name_string, generic_vec))
    //     }

    fn check_expr_return(
        &mut self,
        value: &Expr,
        func: &mut FunctionTableEntry,
    ) -> Result<SymbolType, anyhow::Error> {
        let value_type = self.check_expr(value, func)?;
        //dbg!(self.current_func.clone());

        // dbg!(value_type.clone());
        // dbg!(self.current_func.clone().unwrap().return_type);
        assert!(self.compare_types(&value_type, &func.return_type.clone(), func.method_parent.is_some()));
        Ok(value_type)
    }

    fn check_expr_assignment(
        &mut self,
        name: &Expr,
        value: &Expr,
        func: &mut FunctionTableEntry,
    ) -> Result<SymbolType, anyhow::Error> {
        let name = name.get_symbol_name().unwrap();
        Ok(match func.variables.get(&name) {
            Some(_) => return Err(TypecheckingError::NonMutableReassignment { name }.into()),
            None => {
                let rhs_type = self.check_expr(value, func)?;
                //println!(
                //    "Adding variable {} with type {:?} and value {:?}",
                //    name, rhs_type, value
                //);
                //dbg!(self.env.current_variables.clone());
                func.variables.insert(
                    name.clone(),
                    VariableTableEntry {
                        mytype: rhs_type.clone(),
                        myvalue: value.clone(),
                    },
                );
                //dbg!(func.clone());

                rhs_type
            }
        })
    }

    fn check_expr_symbol(
        &mut self,
        name: &String,
        func: &mut FunctionTableEntry,
    ) -> Result<SymbolType, anyhow::Error> {
        //dbg!(name);
        //dbg!(&func);
        // if name == "self" {
        //     let the_name =
        //         func.method_parent
        //             .clone()
        //             .ok_or(TypecheckingError::SelfOutsideMethod {
        //                 the_func: func.name.clone(),
        //             })?;
        //     let the_type = self
        //         .env
        //         .usertype_table
        //         .get(&the_name)
        //         .ok_or(TypecheckingError::UndefinedType { name: the_name })?
        //         .raw
        //         .clone();
        //     Ok(the_type)
        // } else {
        Ok(func
            .variables
            .get(name)
            .map_or_else( || {
                func.args.iter().find(|e| e.0 == *name).ok_or(
                    TypecheckingError::UndefinedVariable {
                        name: name.to_string(),
                    },
                ).unwrap().1.clone()
            }, |x| x.mytype.clone(),)
            .clone())
        // }
    }

    fn check_expr_field_access(
        &mut self,
        obj: &Expr,
        field: &Expr,
        func: &mut FunctionTableEntry,
    ) -> Result<SymbolType, anyhow::Error> {
        let fieldname = field.get_symbol_name().unwrap();
        let the_obj = if let Expr::Selff = obj {
            func.method_parent.clone().unwrap()
        } else {
            // if let Some(vtable_entry) = func.variables.get(&obj.get_symbol_name().unwrap()) {
            //     vtable_entry.mytype.get_custom_name()
            // } else {
            panic!()
            // }
        };
        let the_entry = self
            .env
            .items
            .get(&the_obj)
            .ok_or(TypecheckingError::UndefinedType {
                name: the_obj.to_string(),
            })?
            .to_ty();
        Ok(the_entry
            .kind
            .get_fields()?
            .iter()
            .filter(|e| e.0 == *fieldname)
            .nth(0)
            .ok_or(TypecheckingError::UndefinedField {
                obj: the_obj.to_string(),
                field: fieldname.to_string(),
            })?
            .1
            .clone())
    }

    fn check_path(
        &mut self,
        l: &Expr,
        r: &Expr,
        func: &mut FunctionTableEntry,
    ) -> Result<SymbolType, anyhow::Error> {
        //let lty = self.check_expr(l, func);
        let rty = match r {
            Expr::Call { name, args } => self.check_expr_path_call(l, name, args, func),
            Expr::MethodCall { name, args, obj } => self.check_expr_path_call(l, name, args, func),

            _ => panic!("{r:?}"),
        };

        // todo: checking?
        return rty;
    }

    fn check_expr_call(
        &mut self,
        name: &Expr,
        args: &Vec<Expr>,
        func: &mut FunctionTableEntry,
    ) -> Result<SymbolType, anyhow::Error> {
        let name: String = name.get_symbol_name().unwrap();
        let mut the_function = self
            .env
            .items
            .get(&quantifier!(Root, Func(name.clone()), End))
            .ok_or(TypecheckingError::UndefinedFunction {
                name: name.to_string(),
            })?
            .to_func();
        if !the_function.is_extern {
            let mut generated_call_arg_types: Vec<SymbolType> = vec![];
            let mut func_generics: HashMap<String, GenericValue> = HashMap::new();

            for expression in args {
                generated_call_arg_types.push(self.check_expr(expression, func)?);
            }

            for (i, arg) in generated_call_arg_types
                .iter()
                .zip(the_function.args.clone())
                .enumerate()
            {
                //if !(*arg.0 == arg.1 .1 || arg.1 .1.is_generic()) {
                ensure!(
                    self.compare_types(arg.0, &arg.1 .1, false),
                    TypecheckingError::InvalidFunctionArgumentType {
                        name,
                        arg: arg.1 .0,
                        expected: arg.1 .1,
                        found: arg.0.clone(),
                    }
                );

                the_function.args[i] = (arg.1 .0, arg.0.clone());
                if arg.1 .1.is_generic() {
                    func_generics.insert(
                        arg.1 .1.get_generic_name(),
                        GenericValue::Perfect(arg.1 .1.get_generic_name(), Box::new(arg.0.clone())),
                    );
                }
            }

            if !the_function.is_checked {
                self.env
                    .items
                    .get_mut(&quantifier!(Root, Func(name), End))
                    .unwrap()
                    .to_mut_func()
                    .is_checked = true;
                self.check_function_body(&mut the_function, args)?;
            }
            Ok(the_function.return_type)
        } else {
            assert!(args.len() >= the_function.args.len());
            the_function.is_checked = true;
            Ok(the_function.return_type) // VERY UNSAFE
        }
    }

    fn check_expr_method_call(
        &mut self,
        obj: &Expr,
        name: &Expr,
        args: &Vec<Expr>,
        func: &mut FunctionTableEntry,
    ) -> Result<SymbolType, anyhow::Error> {
        let obj_name: String = obj.get_parent_name();
        let obj_ty = self.check_expr(obj, func)?;
        let func_name = name.get_symbol_name().unwrap();
        let quant_name = quantifier!(
            Root,
            Type(obj_ty.get_custom_name()),
            Func(func_name.clone()),
            End
        );
        let mut the_function = self
            .env
            .items
            .get(&quant_name)
            .ok_or(TypecheckingError::UndefinedMethod {
                obj: obj_name.clone(),
                name: func_name.clone(),
            })?
            .to_func();

        let mut generated_call_arg_types: Vec<SymbolType> = vec![];
        generated_call_arg_types.push(obj_ty);
        for expression in args {
            generated_call_arg_types.push(self.check_expr(expression, func)?);
        }

        //dbg!(&args);

        //dbg!(&generated_call_arg_types);

        //dbg!(the_function.clone().args);

        let mut func_generics: HashMap<String, GenericValue> = HashMap::new();

        for (i, (found, (name, expected))) in generated_call_arg_types
            .iter()
            .zip(the_function.args.clone())
            .enumerate()
        {
            ensure!(
                self.compare_types(found, &expected, true),
                TypecheckingError::InvalidFunctionArgumentType {
                    name: func_name,
                    arg: name,
                    expected: expected,
                    found: found.clone(),
                }
            );
            the_function.args[i] = (name, found.clone());
            if expected.is_generic() {
                func_generics.insert(
                    expected.get_generic_name(),
                    GenericValue::Perfect(expected.get_generic_name(), Box::new(found.clone())),
                );
            }
        }

        if !the_function.is_checked {
            self.env
                .items
                .get_mut(&quant_name)
                .unwrap()
                .to_mut_func()
                .is_checked = true;

            self.check_function_body(&mut the_function, args)?;
        }
        Ok(the_function.return_type)
    }

    fn check_expr_path_call(
        &mut self,
        obj: &Expr,
        name: &Expr,
        args: &Vec<Expr>,
        func: &mut FunctionTableEntry,
    ) -> Result<SymbolType, anyhow::Error> {
        let obj_name: String = obj.get_parent_name();
        let func_name = name.get_symbol_name().unwrap();
        let quant_name = quantifier!(Root, Type(obj_name), Func(func_name.clone()), End);
        let mut the_function = self
            .env
            .items
            .get(&quant_name)
            .ok_or(TypecheckingError::UndefinedAssoc {
                obj: obj_name.clone(),
                name: func_name.clone(),
            })?
            .clone()
            .to_func();
        let mut generated_call_arg_types: Vec<SymbolType> = vec![];
        for expression in args {
            generated_call_arg_types.push(self.check_expr(expression, func)?);
        }
        ensure!(the_function.args.len() == args.len());
        //dbg!(generated_call_arg_types.clone());
        let mut func_generics: HashMap<String, GenericValue> = HashMap::new();

        for (i, arg) in generated_call_arg_types
            .iter()
            .zip(the_function.args.clone())
            .enumerate()
        {
            if !(self.compare_types(arg.0, &arg.1 .1, false)) {
                return Err(TypecheckingError::InvalidFunctionArgumentType {
                    name: func_name,
                    arg: arg.1 .0,
                    expected: arg.1 .1,
                    found: arg.0.clone(),
                }
                .into());
            }
            the_function.args[i] = (arg.1 .0, arg.0.clone());
            if arg.1 .1.is_generic() {
                func_generics.insert(
                    arg.1 .1.get_generic_name(),
                    GenericValue::Perfect(arg.1 .1.get_generic_name(), Box::new(arg.0.clone())),
                );
            }
        }

        if !the_function.is_checked {
            self.env
                .items
                .get_mut(&quant_name)
                .unwrap()
                .to_mut_func()
                .is_checked = true;

            self.check_function_body(&mut the_function, args)?;
        }
        Ok(the_function.return_type)
    }

    pub fn check_function_body(
        &mut self,
        func: &mut FunctionTableEntry,
        args: &Vec<Expr>,
    ) -> anyhow::Result<SymbolType> {
        func.effect = if func.effect.is_some() {Some(self.env.items.get(&quantifier!(Root, Effect(func.effect.clone().unwrap().name), End)).unwrap().to_effect())} else {None};
        //let prev_curr_variables = self.env.current_variables.clone();
        // let mut prev_curr_func = self.current_func.cloned();
        // self.current_func = Some(func);
        // self.current_method_parent.clone_from(&func.method_parent);
        // self.env
        //     .current_variables
        //     .insert(self.current_func.clone().unwrap().name, HashMap::new());
        for (arg, v) in func.args.iter().zip(args) {
            let name = arg.0.clone();
            func.variables.insert(
                name,
                VariableTableEntry {
                    mytype: arg.1.clone(),
                    myvalue: v.clone(),
                },
            );
        }
        let mut last_expr: SymbolType = func.return_type.clone();
        let t = self.check_expr(&func.body.clone(), func)?;
        last_expr = t;
            
        
        ensure!(
            self.compare_types(&last_expr, &func.return_type, func.method_parent.is_some()),
            TypecheckingError::InvalidFunctionReturnType {
                expected: func.return_type.clone(),
                found: last_expr
            }
        );
        // if func.name != "main" {
        //     self.env.current_variables = prev_curr_variables;
        // }
        //self.current_func = prev_curr_func.as_mut();

        func.is_checked = true;

        Ok(last_expr)
    }

    pub fn check(&mut self, main_mod_name: String) -> anyhow::Result<Environment, anyhow::Error> {
        //dbg!(self.env.clone());
        let main_func: FunctionTableEntry = self
            .env
            .items
            .get(&quantifier!(
                Root,
                /*Module(main_mod_name),*/ Func("main".to_string()),
                End
            ))
            .ok_or(TypecheckingError::MissingMainFunction)?
            .to_func();

        //self.current_func = Some(Box::leak(Box::new(main_func.clone())));
        let rt = self
            .check_function_body(&mut main_func.clone(), &vec![])?
            .clone();
        self.env
            .items
            .get_mut(&quantifier!(Root, Func("main"), End))
            .unwrap()
            .to_mut_func()
            .is_checked = true;
        self.env
            .items
            .get_mut(&quantifier!(Root, Func("main"), End))
            .unwrap()
            .to_mut_func()
            .return_type = rt;
        //self.env.function_table.entries.retain(|_, v| v.is_checked);
        //self.env.usertype_table.iter_mut().for_each(|e| e.1.methods.retain(|_, v| v.is_checked));
        // for (n, ty) in &self.env.usertype_table.clone() {
        //     if !ty.generics.is_empty() {
        //         self.env.usertype_table.remove(n);
        //         for morph in ty.collapse_generics() {
        //             self.env
        //                 .usertype_table
        //                 .insert(morph.raw.get_custom_name(), morph);
        //         }
        //     }
        // }
        // for (n, func) in &self.env.function_table.entries.clone() {
        //     if !func.args.iter().any(|a|a.1.is_generic()) {
        //         self.env.function_table.entries.remove(n);
        //         for morph in func.collapse_generics() {
        //             self.env
        //                 .function_table.entries
        //                 .insert(morph.name.clone(), morph);
        //         }
        //     }
        // }
        Ok(self.env.clone())
    }
}
