use std::collections::HashMap;

use crate::root::resource::{ast::{Expr, Function, Program, Stmt}, itypes::Itype};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Flt,
    Str,
    Bool,
    Var(usize),
    Fn(Vec<Type>, Box<Type>),
    Mute,
}

impl Type {
    pub fn from_itype(i: Itype) -> Self {
        match i {
            Itype::Mute => Self::Mute,
            Itype::Int(_) => Self::Int,
            Itype::Flt(_) => Self::Flt,
            Itype::Str(_) => Self::Str,
            Itype::Bool(_) => Self::Bool,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeError(String);

pub struct TypeChecker {
    env: HashMap<String, Type>,
    next_var: usize,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            env: HashMap::new(),
            next_var: 0,
        }
    }

    fn new_var(&mut self) -> Type {
        let var = Type::Var(self.next_var);
        self.next_var += 1;
        var
    }

    fn infer(&mut self, expr: &Expr) -> Result<Type, TypeError> {
        let t = match expr {
            Expr::Variable(name) => self
                .env
                .get(name)
                .cloned()
                .ok_or(TypeError(format!("Unknown variable: {}", name))),
            Expr::Scalar(s) => match s {
                crate::root::resource::itypes::Itype::Mute => todo!(),
                crate::root::resource::itypes::Itype::Int(_) => Ok(Type::Int),
                crate::root::resource::itypes::Itype::Flt(_) => Ok(Type::Flt),
                crate::root::resource::itypes::Itype::Str(_) => Ok(Type::Str),
                crate::root::resource::itypes::Itype::Bool(_) => Ok(Type::Bool),
            },
            Expr::BinaryOp(op, args) => match op {
                crate::root::resource::ast::BinOp::Assign => {
                    let (lhs, rhs) = *args.clone();
                    let rhs_type = self.infer(&rhs)?;
                    if let Some(var_type) = self.env.clone().get(&lhs.to_string()) {
                        self.unify(var_type, &rhs_type)?;
                    } else {
                        self.env.insert(lhs.to_string().clone(), rhs_type.clone());
                    }
                    Ok(rhs_type)
                }
                _ => {
                    let (lhs, rhs) = *args.clone();
                    let lhs_type = self.infer(&lhs)?;
                    let rhs_type = self.infer(&rhs)?;

                    self.unify(&lhs_type, &rhs_type)?;
                    Ok(lhs_type)
                }
            },
            Expr::Call { name, on:_, args } => {
                let func_type = self
                    .env
                    .get(name)
                    .cloned()
                    .ok_or(TypeError(format!("Unknown function: {}", name)))?;
                match func_type {
                    Type::Fn(arg_types, ret_type) => {
                        if arg_types.len() != args.len() {
                            return Err(TypeError(format!(
                                "Function {} called with incorrect number of arguments",
                                name
                            )));
                        }
                        for (arg_type, arg_expr) in arg_types.iter().zip(args) {
                            let inferred_arg_type = self.infer(arg_expr)?;
                            self.unify(arg_type, &inferred_arg_type)?;
                        }
                        Ok(*ret_type)
                    }
                    _ => Err(TypeError(format!("{} is not a function", name))),
                }
            }
            _ => todo!(),
        };
        return t
    }

    fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), TypeError> {
        dbg!(self.env.clone());

        match (t1, t2) {
            (Type::Int, Type::Int) => Ok(()),
            (Type::Flt, Type::Flt) => Ok(()),

            (Type::Str, Type::Str) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),

            (Type::Var(n), t) | (t, Type::Var(n)) => {
                self.env.insert(format!("t{}", n), t.clone());
                Ok(())
            }
            (Type::Fn(arg1, ret1), Type::Fn(arg2, ret2)) => {
                if arg1.len() != arg2.len() {
                    return Err(TypeError(format!(
                        "Cannot unify functions with different arities: {:?} and {:?}",
                        t1, t2
                    )));
                }
                for (a1, a2) in arg1.iter().zip(arg2) {
                    self.unify(a1, a2)?;
                }
                self.unify(ret1, ret2)
            }
            (t1, t2) => Err(TypeError(format!(
                "Cannot unify types: {:?} and {:?}",
                t1, t2
            ))),
        }
    }

    fn infer_function(&mut self, func: &Function) -> Result<(), TypeError> {
        let mut arg_types = Vec::new();
        for arg in &func.args {
            let arg_type = self.new_var();
            self.env.insert(arg.clone().name, arg_type.clone());
            arg_types.push(arg_type);
        }
        let mut ret_type: Type = self.new_var();
        self.env.insert(func.name.clone().name, Type::Fn(arg_types.clone(), Box::new(ret_type.clone())));
        for stmt in &func.code {
            ret_type = match stmt {
                Stmt::Expr(e) => self.infer(e)?,
                _ => todo!(),
            };
        }
        let func_type = Type::Fn(arg_types, Box::new(ret_type));
        self.env.insert(func.name.clone().name, func_type);
        Ok(())
    }

    fn var_to_vname(&mut self, t: usize) -> String {
        format!("t{t}")
    }


    pub fn infer_program(&mut self, program: &Program) -> Result<HashMap<String, Type>, TypeError> {
        for func in &program.funcs {
            self.infer_function(func)?;
        }
        dbg!(self.env.clone());
        Ok(self.env.clone())
    }

}
