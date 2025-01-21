use rayon::vec;
use thiserror::Error;

use anyhow::{Ok, Result};

use crate::root::{passes::midend::environment::UserTypeKind, resource::{ast::{Expr, SymbolType}, errors::TypecheckingError}};

use super::environment::{Environment, FunctionTableEntry, Table, VariableTableEntry};

#[derive(Clone, Debug, Default, Hash, Eq, PartialEq, Copy, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum PartialType {
    Module,
    Variable(TypeVar),
    Record,
    Field(TypeVar, &'static Self),
    Enum,
    Variant,
    Int,
    Uint,
    Byte,
    Flt,
    Bool,
    Char,
    Str,
    Naught,
    Fn(&'static [Self], &'static Self),
    Mut(&'static Self),
    Pointer(&'static Self),
    Generic(&'static str),
    Custom(usize),
}

pub struct Typechecker {
    env: Environment,
    current_func: Option<FunctionTableEntry>,
}

impl Typechecker {
    pub fn new(env: Environment) -> Self {
        return Typechecker { env, current_func: None};
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
                assert!(lhs_type == rhs_type && rhs_type == SymbolType::Bool);
                SymbolType::Bool
            }
            Expr::Assignment { name, value } => self.check_expr_assignment(name, value)?,
            Expr::Return { value } => self.check_expr_return(value)?,
            Expr::If {
                condition,
                then,
                otherwise,
            } => todo!(),
            Expr::Int(_) => SymbolType::Int,
            Expr::Uint(_) => SymbolType::Uint,
            Expr::Byte(_) => SymbolType::Byte,
            Expr::Flt(ordered_float) => SymbolType::Flt,
            Expr::Str(_) => SymbolType::Str,
            Expr::Char(_) => SymbolType::Char,
            Expr::Bool(_) => SymbolType::Bool,
            Expr::Symbol(name) => self.check_expr_symbol(name)?,
            Expr::StructInstance { name, fields } => self.check_expr_structinstance(name, fields)?,
            Expr::FieldAccess(expr, v) => todo!(),
            Expr::Call { name, args } => self.check_expr_call(name, args)?,
            Expr::MethodCall { obj, name, args } => self.check_expr_method_call(obj, name, args)?,
            _ => todo!(),
        };
        return Ok(res);
    }

    fn check_expr_structinstance(&mut self, name: &Box<Expr>, fields: &Vec<(String, Expr)>) -> Result<SymbolType, anyhow::Error> {
        let name_string = name.get_symbol_name();
        let the_struct = self.env.usertype_table.get_name(&name_string).ok_or(TypecheckingError::UndefinedType { name: name_string.clone() })?;
        let defined_fields: Vec<(String, SymbolType)> = the_struct.kind.get_fields()?;
        let mut real_fields: Vec<(String, SymbolType)> = vec![];
        for f in fields {
            real_fields.push((f.0.clone(), self.check_expr(&f.1)?));
        }
        assert!(defined_fields.len() == fields.len());
        for f in defined_fields.into_iter().zip(real_fields) {
            assert!(f.0 == f.1)
        }
        return Ok(SymbolType::Custom(name_string, vec![]))
    }

    fn check_expr_return(&mut self, 
    value: &Box<Expr>) -> Result<SymbolType, anyhow::Error> {
        let value_type = self.check_expr(&value)?;
        //dbg!(value_type.clone());
        //dbg!(self.current_func.clone().unwrap().return_type);
        assert!(self.current_func.clone().unwrap().return_type == value_type);
        Ok(value_type)
    }

    fn check_expr_assignment(
        &mut self,
        name: &Box<Expr>,
        value: &Box<Expr>,
    ) -> Result<SymbolType, anyhow::Error> {
        let name = name.get_symbol_name();
        Ok(match self.env.current_variables.get_name(&name) {
            Some(_) => return Err(TypecheckingError::NonMutableReassignment { name }.into()),
            None => {
                let rhs_type = self.check_expr(value)?;
                let id = self.env.get_new_id();
                self.env.current_variables.set(
                    &name.clone(),
                    id,
                    VariableTableEntry {
                        name,
                        mytype: rhs_type.clone(),
                    },
                );
                rhs_type
            }
        })
    }

    fn check_expr_symbol(&mut self, name: &String) -> Result<SymbolType, anyhow::Error> {
        Ok(self
            .env
            .current_variables
            .get_name(&name)
            .ok_or(TypecheckingError::UndefinedVariable {
                name: name.to_string(),
            })?
            .mytype)
    }

    fn check_expr_call(
        &mut self,
        name: &Box<Expr>,
        args: &Vec<Expr>,
    ) -> Result<SymbolType, anyhow::Error> {
        let name: String = name.get_symbol_name();
        let mut the_function = self.env.function_table.get_name(&name).ok_or(
            TypecheckingError::UndefinedFunction {
                name: name.to_string(),
            },
        )?;
        if !the_function.is_checked {
            self.check_function_body(&the_function)?;
        }
        let mut generated_call_arg_types: Vec<SymbolType> = vec![];
        for expression in args {
            generated_call_arg_types.push(self.check_expr(expression)?);
        }
        for arg in generated_call_arg_types.iter().zip(the_function.args) {
            assert!(*arg.0 == arg.1 .1)
        }
        return Ok(the_function.return_type);
    }

    fn check_expr_method_call(
        &mut self,
        obj: &Box<Expr>,
        name: &Box<Expr>,
        args: &Vec<Expr>,
    ) -> Result<SymbolType, anyhow::Error> {
        let obj_name: String = obj.get_symbol_name();
        let func_name = name.get_symbol_name();
        let the_object: String = match self
            .env
            .usertype_table
            .get_name(&obj_name) {
                Some(e) => e.name,
                None => {
                    if let Some(o) = self.env.current_variables.get_name(&obj_name) {
                        o.mytype.get_custom_name()
                    } else {
                        return Err(TypecheckingError::UndefinedType { name: obj_name.clone() }.into())
                    }
                }
            };
        let binding = self.env.method_table.get_name(&the_object).ok_or(TypecheckingError::NoMethods {
             name: the_object.clone(),
         },)?;
        let mut the_function = binding.the_functions.iter().filter(|el| el.name == func_name).nth(0).ok_or(TypecheckingError::UndefinedMethod { obj: the_object, name: func_name } )?.clone();
            if !the_function.is_checked {
                self.check_function_body(&mut the_function)?;
                the_function.is_checked = true
            }
            let mut generated_call_arg_types: Vec<SymbolType> = vec![];
            for expression in args {
                generated_call_arg_types.push(self.check_expr(expression)?);
            }
            for arg in generated_call_arg_types.iter().zip(the_function.args) {
                assert!(*arg.0 == arg.1 .1)
            }
            return Ok(the_function.return_type);
    }

    pub fn check_function_body(&mut self, func: &FunctionTableEntry) -> anyhow::Result<()> {
        let prev_curr_variables = self.env.current_variables.clone();
        self.env.current_variables = Table::new();
        self.current_func = Some(func.clone());
        for arg in func.args.clone() {
            let id = self.env.get_new_id();
            let name = arg.0.clone();
            self.env.current_variables.set(&name, id, VariableTableEntry { name: arg.0, mytype: arg.1 });
        }
        for e in &func.body {
            // Check each expression in body
            self.check_expr(e)?;
        }
//        func.is_checked = true;   
        self.env.current_variables = prev_curr_variables;
        Ok(())
    }

    pub fn check(&mut self) -> anyhow::Result<Environment, anyhow::Error> {
        //dbg!(self.env.clone());
        let mut main_func: FunctionTableEntry = self
            .env
            .function_table
            .get_name("main")
            .ok_or(TypecheckingError::MissingMainFunction)?;
        self.check_function_body(&main_func)?;
        main_func.is_checked = true;
        return Ok(self.env.clone());
    }
}
