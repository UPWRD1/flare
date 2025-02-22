use std::collections::HashMap;

use anyhow::{Ok, Result};

use crate::root::resource::{
    ast::{Expr, SymbolType},
    errors::TypecheckingError,
};

use super::environment::{
    Environment, FunctionTableEntry, GenericValue, VariableTableEntry,
};


pub struct Typechecker {
    env: Environment,
    current_func: Option<FunctionTableEntry>,
    current_method_parent: Option<String>,
}

impl Typechecker {
    pub fn new(env: Environment) -> Self {
        Typechecker {
            env,
            current_func: None,
            current_method_parent: None,
        }
    }

    fn compare_types(&mut self, l: &SymbolType, r: &SymbolType) -> bool {
        // dbg!(l);
        // dbg!(r);
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
        } else if l.is_custom() && r.is_custom() && r.get_generic_count() == l.get_generic_count() {
        true            
}else {
                return l == r;

            
        }
    }

    fn check_expr(&mut self, e: &Expr) -> Result<SymbolType> {
        //dbg!(e);
        let res = match e {
            Expr::BinAdd { l, r }
            | Expr::BinSub { l, r }
            | Expr::BinMul { l, r }
            | Expr::BinDiv { l, r } => {
                let lhs_type = self.check_expr(l)?;
                let rhs_type = self.check_expr(r)?;
                assert!(lhs_type == rhs_type);
                lhs_type
            }
            Expr::Logical { l, op, r } => {
                let lhs_type = self.check_expr(l)?;
                let rhs_type = self.check_expr(r)?;
                //dbg!(lhs_type.clone());
                //dbg!(rhs_type.clone());

                match op {
                    crate::root::resource::ast::LogicOp::Is => todo!(),
                    _ => assert!(lhs_type == rhs_type),
                }
                SymbolType::Bool
            }
            Expr::Assignment { name, value } => self.check_expr_assignment(name, value)?,
            Expr::Return { value } => self.check_expr_return(value)?,
            Expr::If {
                condition,
                then,
                otherwise,
            } => {
                let condition_ty = self.check_expr(condition)?;
                let then_type = self.check_expr(then)?;
                let otherwise_type = self.check_expr(otherwise)?;
                assert!(then_type == otherwise_type);
                assert!(condition_ty.is_bool());
                otherwise_type
            }
            Expr::Int(_) => SymbolType::Int,
            Expr::Naught => SymbolType::Naught,

            Expr::Uint(_) => SymbolType::Uint,
            Expr::Byte(_) => SymbolType::Byte,
            Expr::Flt(_) => SymbolType::Flt,
            Expr::Str(_) => SymbolType::Str,
            Expr::Char(_) => SymbolType::Char,
            Expr::Bool(_) => SymbolType::Bool,
            Expr::Symbol(name) => self.check_expr_symbol(name)?,
            Expr::StructInstance { name, fields } => {
                self.check_expr_structinstance(name, fields)?
            }
            Expr::FieldAccess(expr, v) => self.check_expr_field_access(expr, v)?,
            Expr::Call { name, args } => self.check_expr_call(name, args)?,
            Expr::MethodCall { obj, name, args } => self.check_expr_method_call(obj, name, args)?,
            Expr::VariantInstance { name: _, fields: _ } => {
                todo!();
                //self.check_expr_variantinstance(name, fields)?
            }
            _ => todo!(),
        };
        Ok(res)
    }

    fn check_expr_structinstance(
        &mut self,
        name: &Expr,
        fields: &Vec<(String, Expr)>,
    ) -> Result<SymbolType, anyhow::Error> {
        let name_string = name.get_symbol_name();
        let the_struct = self.env.usertype_table.get_id(&name_string).ok_or(
            TypecheckingError::UndefinedType {
                name: name_string.clone(),
            },
        )?;
        let defined_fields: Vec<(String, SymbolType)> = the_struct.kind.get_fields()?;
        let mut real_fields: Vec<(String, SymbolType)> = vec![];
        for f in fields {
            real_fields.push((f.0.clone(), self.check_expr(&f.1)?));
        }
        assert_eq!(defined_fields.len(), fields.len());
        let mut generic_vec: Vec<SymbolType> = vec![];
        for f in defined_fields.into_iter().zip(real_fields) {
            if !(f.0 .0 == f.1 .0 && self.compare_types(&f.0 .1, &f.1 .1)) {
                return Err(TypecheckingError::InvalidStructInstanceField {
                    obj: name_string,
                    field: f.0 .0,
                    expected: f.0 .1,
                    found: f.1 .1,
                }
                .into());
            }
            if f.0 .1.is_generic() {
                if !f.1 .1.is_generic() {
                    generic_vec.push(SymbolType::Generic(GenericValue::Perfect(
                        f.0 .1.get_generic_name(),
                        Box::new(f.1 .1),
                    )));
                } else {
                    generic_vec.push(SymbolType::Generic(GenericValue::Ref(
                        f.0 .1.get_generic_name(),
                    )));
                }
            }
        }

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

    fn check_expr_return(&mut self, value: &Expr) -> Result<SymbolType, anyhow::Error> {
        let value_type = self.check_expr(value)?;
         //dbg!(self.current_func.clone());

        // dbg!(value_type.clone());
        // dbg!(self.current_func.clone().unwrap().return_type);
        assert!(self.compare_types(&value_type, &self.current_func.clone().unwrap().return_type));
        Ok(value_type)
    }

    fn check_expr_assignment(
        &mut self,
        name: &Expr,
        value: &Expr,
    ) -> Result<SymbolType, anyhow::Error> {
        let name = name.get_symbol_name();
        Ok(match self.env.current_variables.get_mut(&self.current_func.clone().unwrap().name).unwrap().get(&name) {
            Some(_) => return Err(TypecheckingError::NonMutableReassignment { name }.into()),
            None => {
                let rhs_type = self.check_expr(value)?;
                //println!(
                //    "Adding variable {} with type {:?} and value {:?}",
                //    name, rhs_type, value
                //);
                //dbg!(self.env.current_variables.clone());
                //dbg!(self.current_func.clone());
                self.env.current_variables.get_mut(&self.current_func.clone().unwrap().name).unwrap().insert(
                    name,
                    VariableTableEntry {
                        mytype: rhs_type.clone(),
                    },
                );

                rhs_type
            }
        })
    }

    fn check_expr_symbol(&mut self, name: &String) -> Result<SymbolType, anyhow::Error> {
        if name == "self" {
            let the_name =
                self.current_method_parent
                    .clone()
                    .ok_or(TypecheckingError::SelfOutsideMethod {
                        the_func: self.current_func.clone().expect("UNREACHABLE").name,
                    })?;
            let the_type = self
                .env
                .usertype_table
                .get_id(&the_name)
                .ok_or(TypecheckingError::UndefinedType { name: the_name })?
                .raw;
            Ok(the_type)
        } else {
            Ok(self
                .env
                .current_variables.get_mut(&self.current_func.clone().unwrap().name).unwrap()
                .get(name)
                .ok_or(TypecheckingError::UndefinedVariable {
                    name: name.to_string(),
                })?
                .mytype.clone())
        }
    }

    fn check_expr_field_access(
        &mut self,
        obj: &Expr,
        field: &String,
    ) -> Result<SymbolType, anyhow::Error> {
        let the_obj = self
            .check_expr_symbol(&obj.get_symbol_name())?
            .get_custom_name();
        let the_entry =
            self.env
                .usertype_table
                .get_id(&the_obj)
                .ok_or(TypecheckingError::UndefinedType {
                    name: the_obj.to_string(),
                })?;
        Ok(the_entry
            .kind
            .get_fields()?
            .iter()
            .filter(|e| e.0 == *field)
            .nth(0)
            .ok_or(TypecheckingError::UndefinedField {
                obj: the_obj.to_string(),
                field: field.to_string(),
            })?
            .1
            .clone())
    }

    fn check_expr_call(
        &mut self,
        name: &Expr,
        args: &Vec<Expr>,
    ) -> Result<SymbolType, anyhow::Error> {
        let name: String = name.get_symbol_name();
        let mut the_function =
            self.env
                .function_table
                .get_id(&name)
                .ok_or(TypecheckingError::UndefinedFunction {
                    name: name.to_string(),
                })?;
        let mut generated_call_arg_types: Vec<SymbolType> = vec![];
        let mut func_generics: HashMap<String, GenericValue> = HashMap::new();

        for expression in args {
            generated_call_arg_types.push(self.check_expr(expression)?);
        }

        
        for (i,arg) in generated_call_arg_types.iter().zip(the_function.args.clone()).enumerate() {
            //if !(*arg.0 == arg.1 .1 || arg.1 .1.is_generic()) {
            if !(self.compare_types(arg.0, &arg.1 .1)) {
                return Err(TypecheckingError::InvalidFunctionArgumentType {
                    name,
                    arg: arg.1 .0,
                    expected: arg.1 .1,
                    found: arg.0.clone(),
                }
                .into());
            }
            the_function.args[i] = (arg.1.0, arg.0.clone());
            if arg.1.1.is_generic() {
                func_generics.insert(arg.1.1.get_generic_name(), GenericValue::Perfect(arg.1.1.get_generic_name(), Box::new(arg.0.clone())));
            }

        }
        let sanitized_rt = self.sanitize_type(&the_function.return_type, func_generics);

        if !the_function.is_checked {
            self.env.function_table[name.clone()].is_checked = true;
            let mut sanitized_func = the_function.clone();

            sanitized_func.return_type = sanitized_rt.clone();
            self.check_function_body(&sanitized_func)?;
        }
        Ok(sanitized_rt)
    }

    fn check_expr_method_call(
        &mut self,
        obj: &Expr,
        name: &Expr,
        args: &Vec<Expr>,
    ) -> Result<SymbolType, anyhow::Error> {
        let obj_name: String = obj.get_symbol_name();
        let func_name = name.get_symbol_name();
        let the_object: String = match self.env.usertype_table.get_id(&obj_name) {
            Some(_) => obj_name,
            None => {
                if let Some(o) = self.env.current_variables.get_mut(&self.current_func.clone().unwrap().name).unwrap().get(&obj_name) {
                    o.mytype.get_custom_name()
                } else {
                    return Err(TypecheckingError::UndefinedType {
                        name: obj_name.clone(),
                    }
                    .into());
                }
            }
        };
        let binding =
            self.env
                .method_table
                .get_id(&the_object)
                .ok_or(TypecheckingError::NoMethods {
                    name: the_object.clone(),
                })?;
        let mut the_function = binding
            .the_functions
            .get_id(&func_name)
            .ok_or(TypecheckingError::UndefinedMethod {
                obj: the_object.clone(),
                name: func_name.clone(),
            })?
            .clone();
        let mut generated_call_arg_types: Vec<SymbolType> = vec![];
        for expression in args {
            generated_call_arg_types.push(self.check_expr(expression)?);
        }
        //dbg!(generated_call_arg_types.clone());
        let mut func_generics: HashMap<String, GenericValue> = HashMap::new();

        for (i, arg) in generated_call_arg_types
            .iter()
            .zip(the_function.args.clone()).enumerate()
        {
            if !(self.compare_types(arg.0, &arg.1.1)) {
                return Err(TypecheckingError::InvalidFunctionArgumentType {
                    name: func_name,
                    arg: arg.1 .0,
                    expected: arg.1.1,
                    found: arg.0.clone(),
                }
                .into());
            }
            the_function.args[i] = (arg.1.0, arg.0.clone());
            if arg.1.1.is_generic() {
                func_generics.insert(arg.1.1.get_generic_name(), GenericValue::Perfect(arg.1.1.get_generic_name(), Box::new(arg.0.clone())));
            }
        }
        let sanitized_rt = self.sanitize_type(&the_function.return_type, func_generics);

        if !the_function.is_checked {
            self.env.method_table[the_object.clone()].the_functions[func_name.clone()].is_checked =
                true;
                let mut sanitized_func = the_function.clone();

                sanitized_func.return_type = sanitized_rt.clone();
                self.check_function_body(&sanitized_func)?;
        }
        Ok(
            sanitized_rt,
        )
    }

    fn sanitize_type(&mut self, ty: &SymbolType, generics: HashMap<String, GenericValue>) -> SymbolType {
    if ty.is_custom() {
        let type_generics: Vec<SymbolType> = ty.get_custom_generics();
        let mut new_generics: Vec<SymbolType> = vec![];
        for g in type_generics {
         if generics.contains_key(&g.get_generic_name()) {
             new_generics.push(SymbolType::Generic(generics.get(&g.get_generic_name()).unwrap().clone()));
         }
        }
        SymbolType::Custom(ty.get_custom_name(), new_generics)
 
    } else if ty.is_generic() && generics.contains_key(&ty.get_generic_name()) {
        return generics.get(&ty.get_generic_name()).unwrap().clone().get_ty();
    } else {
        ty.clone()

    }
    }

    pub fn check_function_body(&mut self, func: &FunctionTableEntry) -> anyhow::Result<SymbolType> {
        //let prev_curr_variables = self.env.current_variables.clone();
        let prev_curr_func = self.current_func.clone();
        self.current_func = Some(func.clone());
        self.current_method_parent.clone_from(&func.method_parent);
        self.env.current_variables.insert(self.current_func.clone().unwrap().name, HashMap::new());
        for arg in func.args.clone() {
            let name = arg.0.clone();
            self.env
                .current_variables.get_mut(&self.current_func.clone().unwrap().name).unwrap()
                .insert(name, VariableTableEntry { mytype: arg.1 });
        }
        let mut last_expr: SymbolType = func.return_type.clone();
        for (i, e) in func.body.iter().enumerate() {
            // Check each expression in body
            let t = self.check_expr(e)?;
            if i == func.body.len() {
                last_expr = t;
            }
        }
        assert!(self.compare_types(&last_expr, &func.return_type));
        // if func.name != "main" {
        //     //self.env.current_variables = prev_curr_variables;
        // }
        self.current_func = prev_curr_func;
        Ok(last_expr)
    }

    pub fn check(&mut self) -> anyhow::Result<Environment, anyhow::Error> {
        //dbg!(self.env.clone());
        let main_func: FunctionTableEntry = self
        .env
        .function_table
        .get_id(&"main".to_string())
            .ok_or(TypecheckingError::MissingMainFunction)?;
        self.current_func = Some(main_func.clone());
        let rt = self.check_function_body(&main_func.clone())?.clone();
        self.env.function_table["main".to_string()].is_checked = true;
        self.env.function_table["main".to_string()].return_type = rt;
        self.env.function_table.entries.retain(|_, v| v.is_checked);
        Ok(self.env.clone())
    }
}
