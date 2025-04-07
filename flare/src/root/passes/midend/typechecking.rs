use crate::root::Quantifier;
use crate::{
    quantifier,
    root::resource::{
        cst::{Expr, SymbolType},
        errors::TypecheckingError,
    },
};
use anyhow::{ensure, Ok, Result};
use ordermap::Equivalent;
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

    fn check_expr(&mut self, e: &Expr, func_id: &Quantifier) -> Result<SymbolType> {
        //dbg!(e);
        let res = match e {
            Expr::BinAdd { l, r }
            | Expr::BinSub { l, r }
            | Expr::BinMul { l, r }
            | Expr::BinDiv { l, r } => {
                let lhs_type = self.check_expr(l, func_id)?;
                let rhs_type = self.check_expr(r, func_id)?;
                ensure!(lhs_type == rhs_type);
                lhs_type
            }
            Expr::Logical { l, op, r } => {
                let lhs_type = self.check_expr(l, func_id)?;
                let rhs_type = self.check_expr(r, func_id)?;
                //dbg!(lhs_type.clone());
                //dbg!(rhs_type.clone());

                match op {
                    crate::root::resource::cst::LogicOp::Is => todo!(),
                    _ => assert!(lhs_type == rhs_type),
                }
                SymbolType::Bool
            }
            Expr::Assignment {
                name,
                value,
                and_in,
            } => self.check_expr_assignment(name, value, &and_in, func_id)?,
            Expr::If {
                condition,
                then,
                otherwise,
            } => {
                let condition_ty = self.check_expr(condition, func_id)?;
                let then_type = self.check_expr(then, func_id)?;
                let otherwise_type = self.check_expr(otherwise, func_id)?;
                ensure!(then_type == otherwise_type);
                ensure!(&condition_ty.is_bool());
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
            Expr::Symbol(name) => self.check_expr_symbol(name, func_id)?,
            Expr::StructInstance { name, fields } => {
                self.check_expr_structinstance(name, fields, func_id)?
            }
            Expr::FieldAccess(expr, v) => self.check_expr_field_access(expr, v, func_id)?,
            Expr::Call { name, args } => self.check_expr_call(name, args, func_id)?,
            Expr::MethodCall { obj, name, args } => {
                //dbg!(e);
                //dbg!(func.clone());
                self.check_expr_method_call(obj, name, args, func_id)?
            }
            Expr::VariantInstance { name, fields } => {
                todo!();
                //self.check_expr_variantinstance(name, fields, func_id)?
            }
            Expr::Path(l, r) => self.check_path(l, r, func_id)?,
            Expr::AddressOf(t) => SymbolType::Pointer(Box::new(self.check_expr(t, func_id)?)),
            _ => todo!("{e:?}"),
        };
        Ok(res)
    }

    fn check_expr_structinstance(
        &mut self,
        name: &Expr,
        fields: &Vec<(String, Expr)>,
        func_id: &Quantifier,
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
            let checked = self.check_expr(&f.1, func_id)?;
            real_fields.push((f.0.clone(), checked));
        }
        assert_eq!(defined_fields.len(), fields.len());
        let mut generic_vec: Vec<SymbolType> = vec![];
        for ((def_fld_name, def_fld_ty), (fnd_fld_name, fnd_fld_ty)) in
            defined_fields.into_iter().zip(real_fields)
        {
            if !(def_fld_name == fnd_fld_name
                && self.compare_types(&def_fld_ty, &fnd_fld_ty, false))
            {
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
        Ok(SymbolType::Quant(quantifier!(
            Root,
            Type(name_string.clone()),
            End
        )))
    }

    fn check_expr_variantinstance(
        &mut self,
        parent: &Expr,
        name: &Expr,
        fields: &Vec<Expr>,
        func_id: &Quantifier,
    ) -> Result<SymbolType, anyhow::Error> {
        //dbg!(parent, name, fields);
        // let name_string = name.get_symbol_name();
        // let the_enum = self.env.usertype_table.get_id(&name_string).ok_or(
        //     TypecheckingError::UndefinedType {
        //         name: name_string.clone(),
        //     },
        // )?;
        // let real_fields: Vec<SymbolType> = vec![];

        // todo!();

        let parent_name_string = parent.get_symbol_name().unwrap();
        let variant_name_string = name.get_symbol_name().unwrap();

        let the_enum = self
            .env
            .items
            .get(&quantifier!(Root, Type(parent_name_string.clone()), End))
            .ok_or(TypecheckingError::UndefinedType {
                name: parent_name_string.clone(),
            })?
            .clone();
        let variants: Vec<(String, Vec<SymbolType>)> = the_enum.to_ty().kind.get_variants()?;
        //dbg!(&variants);
        //dbg!(&variant_name_string);
        let the_variant = variants
            .iter()
            .find(|x| x.0 == variant_name_string)
            .unwrap(); // FIXME
        let mut real_fields: Vec<(String, SymbolType)> = vec![];
        for (i, f) in fields.iter().enumerate() {
            real_fields.push((i.to_string().clone(), self.check_expr(&f, func_id)?));
        }
        //dbg!(&real_fields);
        // assert_eq!(defined_fields.len(), fields.len());
        let mut generic_vec: Vec<SymbolType> = vec![];
        // for ((def_fld_name, def_fld_ty), (fnd_fld_name, fnd_fld_ty)) in
        //     defined_fields.into_iter().zip(real_fields)
        // {
        //     if !(def_fld_name == fnd_fld_name
        //         && self.compare_types(&def_fld_ty, &fnd_fld_ty, false))
        //     {
        //         return Err(TypecheckingError::InvalidStructInstanceField {
        //             obj: name_string,
        //             field: def_fld_name,
        //             expected: def_fld_ty,
        //             found: fnd_fld_ty,
        //         }
        //         .into());
        //     }
        //     if def_fld_ty.is_generic() {
        //         if !fnd_fld_ty.is_generic() {
        //             let v = SymbolType::Generic(GenericValue::Perfect(
        //                 def_fld_ty.get_generic_name(),
        //                 Box::new(fnd_fld_ty),
        //             ));
        //             generic_vec.push(v.clone());
        //         } else {
        //             generic_vec.push(SymbolType::Generic(GenericValue::Ref(
        //                 def_fld_ty.get_generic_name(),
        //             )));
        //         }
        //     }
        // }
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
        Ok(SymbolType::Quant(quantifier!(
            Root,
            Type(parent_name_string.clone()),
            Type(the_variant.0),
            End
        )))

        //        Ok(SymbolType::Custom(name_string, generic_vec))
    }

    fn check_expr_assignment(
        &mut self,
        name: &Expr,
        value: &Expr,
        and_in: &Expr,
        func_id: &Quantifier,
    ) -> Result<SymbolType, anyhow::Error> {
        let name = name.get_symbol_name().unwrap();
        let the_ident = func_id.append(Quantifier::Variable(name.clone()));
        Ok(match self.env.get_q(&the_ident) {
            Some(_) => return Err(TypecheckingError::NonMutableReassignment { name }.into()),
            None => {
                let rhs_type = self.check_expr(value, func_id)?;
                //println!(
                //    "Adding variable {} with type {:?} and value {:?}",
                //    name, rhs_type, value
                //);
                //dbg!(self.env.current_variables.clone());
                self.env.add(
                    the_ident,
                    VariableTableEntry {
                        mytype: rhs_type.clone(),
                        myvalue: value.clone(),
                    },
                );
                //dbg!(func.clone());
                self.check_expr(and_in, func_id)?
                //rhs_type
            }
        })
    }

    fn check_expr_symbol(
        &mut self,
        name: &String,
        func_id: &Quantifier,
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
        let the_name = func_id.append(Quantifier::Variable(name.clone()));
        if let Some(entry) = self.env.items.iter().find(|x| x.0.equivalent(&the_name)) {
            Ok(entry.1.to_variable().mytype)
        } else {
            anyhow::bail!(TypecheckingError::UndefinedVariable {
                name: name.to_string()
            })
        }

        // }
    }

    fn check_expr_field_access(
        &mut self,
        obj: &Expr,
        field: &Expr,
        func_id: &Quantifier,
    ) -> Result<SymbolType, anyhow::Error> {
        let fieldname = field.get_symbol_name().unwrap();
        let the_func = self.env.get_q(func_id).unwrap().to_func();
        let the_obj = if let Expr::Selff = obj {
            the_func.method_parent.clone().unwrap()
        } else {
            func_id.append(Quantifier::Variable(obj.get_symbol_name().unwrap()))
            // if let Some(vtable_entry) = func.variables.get(&obj.get_symbol_name().unwrap()) {
            //     vtable_entry.mytype.get_custom_name()
            // } else {
            // panic!()
            // }
        };
        let the_entry = match self
            .env
            .get_q(&the_obj)
            .ok_or(TypecheckingError::UndefinedType {
                name: the_obj.to_string(),
            })? {
            super::environment::Entry::Type(t) => t,
            super::environment::Entry::Variable(v) => {
                self.env.get_q(&v.mytype.get_quant()).unwrap().to_ty()
            }
            _ => panic!(),
        };
        //dbg!(&the_entry);
        //dbg!(&the_obj);
        match the_entry.kind {
            super::environment::UserTypeKind::Struct { fields } => Ok(fields
                .iter()
                .filter(|e| e.0 == *fieldname)
                .nth(0)
                .ok_or(TypecheckingError::UndefinedStructField {
                    obj: the_obj.to_string(),
                    field: fieldname.to_string(),
                })?
                .1
                .clone()),
            super::environment::UserTypeKind::Enum { variants } => {
                todo!()
            }
            super::environment::UserTypeKind::VariantInstance { parent, ident } => {
                let parent_entry = self.env.get_q(&parent).unwrap().to_ty();
                let the_variant = parent_entry
                    .kind
                    .get_variants()
                    .unwrap()
                    .iter()
                    .find(|x| x.0 == ident)
                    .unwrap().clone();
                let idx = match field {
                    Expr::Int(i) => i,
                    _ => panic!(),
                };
                let the_field = the_variant.1.get(*idx as usize).ok_or(TypecheckingError::UndefinedVariantField { v: the_variant.0, t: parent_entry.name, field: idx.to_string() })?.clone();
                Ok(the_field)
            }
        }
    }

    fn check_path(
        &mut self,
        l: &Expr,
        r: &Expr,
        func_id: &Quantifier,
    ) -> Result<SymbolType, anyhow::Error> {
        //let lty = self.check_expr(l, func);
        //dbg!(&r);
        let rty = match r {
            Expr::Call { name, args } => self.check_expr_path_call(l, name, args, func_id),
            Expr::MethodCall { name, args, obj } => {
                self.check_expr_path_call(l, name, args, func_id)
            }
            Expr::VariantInstance { name, fields } => {
                self.check_expr_variantinstance(l, name, fields, func_id)
            }
            Expr::Int(_) => self.check_expr_field_access(l, r, func_id),
            Expr::Symbol(_) => self.check_expr_field_access(l, r, func_id),

            _ => panic!("{r:?}"),
        };

        // todo: checking?
        return rty;
    }

    fn check_expr_call(
        &mut self,
        name: &Expr,
        args: &Vec<Expr>,
        func_id: &Quantifier,
    ) -> Result<SymbolType, anyhow::Error> {
        let name: String = name.get_symbol_name().unwrap();
        let quant_name = quantifier!(Root, Func(name.clone()), End);
        let mut the_function = self
            .env
            .items
            .get(&quant_name)
            .ok_or(TypecheckingError::UndefinedFunction {
                name: name.to_string(),
            })?
            .to_func();
        if !the_function.is_extern {
            let mut generated_call_arg_types: Vec<SymbolType> = vec![];
            let mut func_generics: HashMap<String, GenericValue> = HashMap::new();

            for expression in args {
                generated_call_arg_types.push(self.check_expr(expression, func_id)?);
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
                self.check_function_body(&quant_name, args)?;
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
        func_id: &Quantifier,
    ) -> Result<SymbolType, anyhow::Error> {
        let obj_name: String = obj.get_parent_name();
        let obj_ty = self.check_expr(obj, func_id)?;
        let func_name = name.get_symbol_name().unwrap();
        let quant_name = if let SymbolType::Quant(q) = obj_ty.clone() {q.append(Quantifier::Func(func_name.clone(), Box::new(Quantifier::End)))} else {
            quantifier!(
                    Root,
                    Type(obj_ty.get_custom_name()),
                    Func(func_name.clone()),
                    End
                )
        };
        // let quant_name = 
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
            generated_call_arg_types.push(self.check_expr(expression, func_id)?);
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

            self.check_function_body(&quant_name, args)?;
        }
        Ok(the_function.return_type)
    }

    fn check_expr_path_call(
        &mut self,
        obj: &Expr,
        name: &Expr,
        args: &Vec<Expr>,
        func_id: &Quantifier,
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
            generated_call_arg_types.push(self.check_expr(expression, func_id)?);
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

            self.check_function_body(&quant_name, args)?;
        }
        Ok(the_function.return_type)
    }

    pub fn check_function_body(
        &mut self,
        func_id: &Quantifier,
        args: &Vec<Expr>,
    ) -> anyhow::Result<SymbolType> {
        let mut func = self.env.get_q(func_id).unwrap().to_func();
        func.effect = if let Some(effect) = func.effect {
            Some(
                self.env
                    .get_q(&quantifier!(Root, Effect(effect.name), End))
                    .unwrap()
                    .to_effect(),
            )
        } else {
            None
        };
        //let prev_curr_variables = self.env.current_variables.clone();
        // let mut prev_curr_func = self.current_func.cloned();
        // self.current_func = Some(func);
        // self.current_method_parent.clone_from(&func.method_parent);
        // self.env
        //     .current_variables
        //     .insert(self.current_func.clone().unwrap().name, HashMap::new());
        for (arg, v) in func.args.iter().zip(args) {
            let name = arg.0.clone();
            self.env.add(
                func_id.append(Quantifier::Variable(name)),
                VariableTableEntry {
                    mytype: arg.1.clone(),
                    myvalue: v.clone(),
                },
            );
        }
        let mut last_expr: SymbolType = func.return_type.clone();
        let t = self.check_expr(&func.body.clone(), func_id)?;
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

    pub fn check(&mut self) -> anyhow::Result<Environment, anyhow::Error> {
        //dbg!(self.env.clone());
        let main_func = quantifier!(
            Root,
            /*Module(main_mod_name),*/ Func("main".to_string()),
            End
        );

        //self.current_func = Some(Box::leak(Box::new(main_func.clone())));
        let rt = self.check_function_body(&main_func, &vec![])?.clone();
        self.env
            .items
            .get_mut(&main_func)
            .unwrap()
            .to_mut_func()
            .is_checked = true;
        self.env
            .items
            .get_mut(&main_func)
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
