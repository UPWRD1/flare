use std::collections::HashMap;

use crate::root::resource::{ast::{Expr, Function, Program}, itypes::Itype};

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

#[derive(Debug, Clone)]
pub struct Environment {
    pub items: HashMap<String, Type>,
    pub name: String,
}

pub struct TypeChecker {
    pub env: Vec<Environment>,
    current_env: usize,
    next_var: usize,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            env: vec![Environment {name: "CORE".to_string(), items: HashMap::new()}],
            next_var: 0,
            current_env: 0,
        }
    }

    fn current(&mut self) -> HashMap<String, Type> {
        self.env[self.current_env].clone().items
    }

    fn new_scope(&mut self, name: String) {
        self.env.push(Environment {name, items: HashMap::new()});
        self.current_env = self.env.len() - 1;
    }

    fn close_scope(&mut self) {
        self.current_env = 0;
    }

    fn new_var(&mut self) -> Type {
        let var = Type::Var(self.next_var);
        self.next_var += 1;
        var
    }

    fn get_var(&mut self, var: Type) -> Type {
        match var {
            Type::Var(i) => {
                let name = self.var_to_vname(i);
                let val = self.current().get(&name).unwrap().clone();
                self.get_var(val)
            },
            _ => return var,
        }
    }

    fn infer(&mut self, expr: &Expr) -> Type {
        dbg!(self.env.clone());

        let t = match expr {
            Expr::Variable(name) => self
                .current().get(name)
                .cloned()
                .expect(&format!("Unknown variable: {}", name)),
            Expr::Scalar(s) => match s {
                crate::root::resource::itypes::Itype::Mute => todo!(),
                crate::root::resource::itypes::Itype::Int(_) => Type::Int,
                crate::root::resource::itypes::Itype::Flt(_) => Type::Flt,
                crate::root::resource::itypes::Itype::Str(_) => Type::Str,
                crate::root::resource::itypes::Itype::Bool(_) => Type::Bool,
            },
            Expr::BinaryOp(op, args) => match op {
                crate::root::resource::ast::BinOp::Assign => {
                    let (lhs, rhs) = *args.clone();
                    let rhs_type = self.infer(&rhs);
                    if let Some(var_type) = self.current().clone().get(&lhs.to_string()) {
                        self.unify(var_type, &rhs_type);
                    } else {
                        self.env[self.current_env].items
                        .insert(lhs.to_string().clone(), rhs_type.clone());
                    }
                    rhs_type
                }
                _ => {
                    let (lhs, rhs) = *args.clone();
                    let lhs_type = self.infer(&lhs);
                    let rhs_type = self.infer(&rhs);
                    // if !((lhs_type == Type::Int && rhs_type == Type::Int) || (lhs_type == Type::Flt && rhs_type == Type::Flt)) {
                    //     panic!("Binary operation cannot be applied between {:?} and {:?}", lhs_type, rhs_type)
                    // }
                    self.unify(&lhs_type, &rhs_type);
                    lhs_type
                }
            },
            Expr::Call { name, on:_, args } => {
                let func_type = 
                self.env[self.current_env].items.get(name)
                    .cloned()
                    .ok_or(TypeError(format!("Unknown function: {}", name)));
                match func_type {
                    Ok(Type::Fn(arg_types, ret_type)) => {
                        if arg_types.len() != args.len() {
                            panic!(
                                "Function {} called with incorrect number of arguments",
                                name
                            )
                        }
                        for (arg_type, arg_expr) in arg_types.iter().zip(args) {
                            let inferred_arg_type = self.infer(arg_expr);
                            self.unify(arg_type, &inferred_arg_type);
                        }
                        *ret_type
                    }
                    _ => panic!("{} is not a function", name)
                }
            }
            _ => todo!(),
        };
        return t
    }

    fn unify(&mut self, t1: &Type, t2: &Type) -> (){

        match (t1, t2) {
            (Type::Int, Type::Int) => (),
            (Type::Flt, Type::Flt) => (),

            (Type::Str, Type::Str) => (),
            (Type::Bool, Type::Bool) => (),

            (Type::Var(n), t) | (t, Type::Var(n)) => {
                self.env[self.current_env].items.insert(format!("t{}", n), t.clone());
            }
            (Type::Fn(arg1, ret1), Type::Fn(arg2, ret2)) => {
                if arg1.len() != arg2.len() {
                    panic!(
                        "Cannot unify functions with different arities: {:?} and {:?}",
                        t1, t2
                    )
                }
                for (a1, a2) in arg1.iter().zip(arg2) {
                    self.unify(a1, a2);
                }
                self.unify(ret1, ret2)
            }
            (t1, t2) => panic!(
                "Cannot unify types: {:?} and {:?}",
                t1, t2
            )
        }
    }

    fn infer_function(&mut self, func: &Function) {
        let mut arg_types = Vec::new();
        self.new_scope(func.name.name.clone());
        for arg in &func.args {
            let arg_type = self.new_var();
            self.env[self.current_env].items.insert(arg.clone().name, arg_type.clone());
            arg_types.push(arg_type);
        }
        let mut ret_type: Type = self.new_var();
        self.env[self.current_env].items.insert(func.name.clone().name, Type::Fn(arg_types.clone(), Box::new(ret_type.clone())));
        for expr in &func.code {
            ret_type = self.infer(expr);
        }
        let func_type = Type::Fn(arg_types, Box::new(ret_type));
        self.env[self.current_env].items.insert(func.name.clone().name, func_type);
        self.close_scope();
    }

    fn var_to_vname(&mut self, t: usize) -> String {
        format!("t{t}")
    }


    pub fn infer_program(&mut self, program: &Program) -> HashMap<String, Type> {
        for func in &program.funcs {
            self.infer_function(func);
        }
        dbg!(self.env.clone());
        self.current().clone()
    }

}
