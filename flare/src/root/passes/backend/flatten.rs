use std::collections::HashMap;

use crate::{
    quantifier,
    root::{
        passes::midend::environment::{Environment, FunctionTableEntry, Quantifier},
        resource::
            cst::{Expr, SymbolType},
        
    },
};

pub struct Flattener {
    env: Environment,
    current_func: FunctionTableEntry,
    current_parent: Option<Expr>,
    scopes: Vec<HashMap<Expr, Expr>>,
}

impl Flattener {
    pub fn new(env: Environment) -> Self {
        Self {
            current_func: FunctionTableEntry {
                name: "".to_string(),
                method_parent: None,
                arity: 0,
                args: vec![],
                limits: vec![],
                effect: None,
                return_type: SymbolType::Unit,
                body: Expr::Naught,
                is_checked: false,
                is_extern: false,
                variadic: false,
                // variables: HashMap::new(),
            },
            env,
            current_parent: None,
            scopes: vec![],
        }
    }

    fn flatten_expr(&mut self, e: &Expr) -> Expr {
        //dbg!(&self.scopes);
        // if let Some(e) = self.scopes.iter().rev().filter_map(|s| s.get(e)).next() {
        //     e.clone()
        // } else {
            //dbg!(&e);
            let res = match e {
                Expr::BinAdd { l, r } => {
                    let k = self.flatten_expr(l);
                    let v = self.flatten_expr(r);
                    if k.is_constant() && v.is_constant() {
                        let res = k + v;
                        self.scopes
                            .last_mut()
                            .unwrap()
                            .insert(e.clone(), res.clone());
                        res
                    } else {
                        let res = Expr::BinAdd {
                            l: Box::new(k),
                            r: Box::new(v),
                        };
                        self.scopes
                            .last_mut()
                            .unwrap()
                            .insert(e.clone(), res.clone());
                        res
                    }
                }
                Expr::BinSub { l, r } => {
                    let k = self.flatten_expr(l);
                    let v = self.flatten_expr(r);
                    if k.is_constant() && v.is_constant() {
                        let res = k - v;
                        self.scopes
                            .last_mut()
                            .unwrap()
                            .insert(e.clone(), res.clone());
                        res
                    } else {
                        let res = Expr::BinSub {
                            l: Box::new(k),
                            r: Box::new(v),
                        };
                        self.scopes
                            .last_mut()
                            .unwrap()
                            .insert(e.clone(), res.clone());
                        res
                    }
                }
                Expr::BinMul { l, r } => {
                    let k = self.flatten_expr(l);
                    let v = self.flatten_expr(r);
                    if k.is_constant() && v.is_constant() {
                        let res = k * v;
                        self.scopes
                            .last_mut()
                            .unwrap()
                            .insert(e.clone(), res.clone());
                        res
                    } else {
                        let res = Expr::BinMul {
                            l: Box::new(k),
                            r: Box::new(v),
                        };
                        self.scopes
                            .last_mut()
                            .unwrap()
                            .insert(e.clone(), res.clone());
                        res
                    }
                }
                Expr::BinDiv { l, r } => {
                    let k = self.flatten_expr(l);
                    let v = self.flatten_expr(r);
                    if k.is_constant() && v.is_constant() {
                        let res = k / v;
                        self.scopes
                            .last_mut()
                            .unwrap()
                            .insert(e.clone(), res.clone());
                        res
                    } else {
                        let res = Expr::BinDiv {
                            l: Box::new(k),
                            r: Box::new(v),
                        };
                        self.scopes
                            .last_mut()
                            .unwrap()
                            .insert(e.clone(), res.clone());
                        res
                    }
                }
                Expr::Logical { l, op, r } => {
                    let left = self.flatten_expr(l);
                    //dbg!(&left);
                    let right = self.flatten_expr(r);
                    //dbg!(&right);
                    if left.get_numeric().is_some() && right.get_numeric().is_some() {
                        let leftval = left.get_numeric().unwrap();
                        let rightval = right.get_numeric().unwrap();

                        let eval = match op {
                            crate::root::resource::cst::LogicOp::CEQ => leftval == rightval,
                            crate::root::resource::cst::LogicOp::CLT => leftval < rightval,
                            crate::root::resource::cst::LogicOp::CLE => leftval <= rightval,
                            crate::root::resource::cst::LogicOp::CGT => leftval > rightval,
                            crate::root::resource::cst::LogicOp::CGE => leftval >= rightval,
                            crate::root::resource::cst::LogicOp::Is => todo!(),
                        };
                        let res = Expr::Bool(eval);
                        self.scopes
                            .last_mut()
                            .unwrap()
                            .insert(e.clone(), res.clone());
                        res
                    } else {
                        let res = Expr::Logical { l: Box::new(left), op: op.clone(), r: Box::new(right) };
                        self.scopes
                            .last_mut()
                            .unwrap()
                            .insert(e.clone(), res.clone());
                        res
                    }

                    
                    
                },
                Expr::Assignment { name, value, and_in } => {
                    let v = self.flatten_expr(value);
                    self.scopes
                    .last_mut()
                    .unwrap()
                    .insert(*name.clone(), v.clone());

                    let res = self.flatten_expr(and_in);

                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(e.clone(), res.clone());
                    res
                    //panic!()
                }
                Expr::Closure { args: _, body: _ } => todo!(),
                Expr::SeqComp { l, r } => todo!(),
                Expr::If {
                    condition,
                    then,
                    otherwise,
                } => {
                    let condition = self.flatten_expr(condition);
                    //dbg!(&condition);
                    if let Expr::Bool(v) = condition {
                        //dbg!(v);
                        if v {
                            let res = self.flatten_expr(then);
    
                            self.scopes
                                .last_mut()
                                .unwrap()
                                .insert(e.clone(), res.clone());
                            //dbg!(&res);
                            res
                        } else {
                            let res = self.flatten_expr(&otherwise);
    
                            self.scopes
                                .last_mut()
                                .unwrap()
                                .insert(e.clone(), res.clone());
                            res
                        }
                        
                    } else {
                        // let t = self.flatten_expr(then);
                        // let o = self.flatten_expr(&otherwise);
                        // let res = Expr::If { condition: Box::new(condition), then: Box::new(t), otherwise: Box::new(o) };
                        // self.scopes
                        //         .last_mut()
                        //         .unwrap()
                        //         .insert(e.clone(), res.clone());
                        //     res
                        panic!()
                    }
                    
                }
                Expr::Naught => e.clone(),
                Expr::Int(_) => e.clone(),
                Expr::Uint(_) => e.clone(),
                Expr::Word(_) => e.clone(),
                Expr::Byte(_) => e.clone(),
                Expr::Flt(_) => e.clone(),
                Expr::Str(_) => e.clone(),
                Expr::Char(_) => e.clone(),
                Expr::Bool(_) => e.clone(),
                Expr::AddressOf(expr) => todo!(),
                Expr::Symbol(_) => {
                    self
                    .scopes
                    .iter()
                    .rev()
                    .filter_map(|s| s.get(e))
                    .next()
                    .expect(&format!("Could not find {e:?}"))
                    .clone()},
                Expr::Selff => {
                    if let Some(e) = &self.current_parent {
                        e.clone()
                    } else {
                        panic!()
                    }
                }
                Expr::Call { name, args } => {
                    let res = self.flatten_func_body(
                        quantifier!(Root, Func(name.get_symbol_name().unwrap()), End),
                        args.to_vec(),
                    );
                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(e.clone(), res.clone());
                    res
                }
                Expr::MethodCall { obj, name, args } => {
                    //dbg!(&e);
                    let obj = self.flatten_expr(obj);
                    //dbg!(&obj);
                    let mut nargs = vec![obj.clone()];
                    nargs.append(&mut args.clone());
                    let res = self.flatten_func_body(
                        quantifier!(
                            Root,
                            Type(obj.get_parent_name()),
                            Func(name.get_symbol_name().unwrap()),
                            End
                        ),
                        nargs,
                    );
                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(e.clone(), res.clone());
                    //dbg!(&res);
                    res
                }
                Expr::Path(l, r) => {
                    //self.flatten_expr(r)
                    if let Expr::Call { name, args } = &**r {
                        let quant = quantifier!(
                            Root,
                            Type(l.get_symbol_name().unwrap()),
                            Func(name.get_symbol_name().unwrap()),
                            End
                        );
                        let res = self.flatten_func_body(quant, args.to_vec());
                        self.scopes
                            .last_mut()
                            .unwrap()
                            .insert(e.clone(), res.clone());
                        res
                    } else {
                        panic!("{e:?}")
                    }
                }
                Expr::StructInstance { name, fields } => {
                    let fields = fields
                        .iter()
                        .map(|f| (f.0.clone(), self.flatten_expr(&f.1)))
                        .collect();
                    let res = Expr::StructInstance {
                        name: name.clone(),
                        fields,
                    };
                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(e.clone(), res.clone());
                    //dbg!(&res);
                    res
                }
                Expr::FieldAccess(l, r) => {
                    //dbg!(&e);
                    let left = self.flatten_expr(l);

                    //dbg!(&left);
                    let res = left
                    .get_fields()
                    .iter()
                    .filter(|x| x.0 == *r.get_symbol_name().unwrap())
                    .next()
                    .unwrap()
                    .1
                    .clone();
                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(e.clone(), res.clone());
                    res
                }
                _ => todo!("{:?}", e),
            };
            return res;
        //}
    }

    fn flatten_func_body(&mut self, id: Quantifier, args: Vec<Expr>) -> Expr {
        let prev_func = self.current_func.clone();
        //dbg!(&id);
        self.current_func = self.env.items.get(&id).unwrap().clone().into();
        // dbg!(&self.current_func);
        self.scopes.push(HashMap::new());
        let flat_args: Vec<Expr> = args.iter().map(|arg| self.flatten_expr(arg)).collect();
        //dbg!(args);
        //dbg!(&flat_args);
        //dbg!(&self.current_func.args);
        if self.current_func.method_parent.is_some() {
            self.current_parent = Some(flat_args.first().unwrap().clone());
        }

        for ((name, _), expr_val) in self.current_func.args.clone().iter().zip(flat_args) {
            //dbg!(&self.scopes);

            self.scopes
                .last_mut()
                .unwrap()
                .insert(Expr::Symbol(name.to_string()), expr_val.clone());
        }

        let mut res = Expr::Naught;
        let expr = self.current_func.body.clone();
            //dbg!(self.scopes.clone());
            // if let Expr::Assignment { name, value , and_in} = expr {
            //     let v = self.flatten_expr(&value);
            //     self.scopes.last_mut().unwrap().insert(*name.clone(), v);
            //     res = self.flatten_expr(&and_in);

            // } else if let Expr::Return { ref value } = expr {
            //     let v = self.flatten_expr(&value);
            //     self.scopes.last_mut().unwrap().insert(expr.clone(), v);
            // } else {
                res = self.flatten_expr(&expr);
            //}
        

        self.current_func = prev_func;
        self.current_parent = None;
        res
    }

    pub fn flatten(&mut self) -> Environment {
        let res = self.flatten_func_body(quantifier!(Root, Func("main"), End), vec![]);
        //dbg!(res.clone());
        self.env
            .items
            .get_mut(&quantifier!(Root, Func("main"), End))
            .unwrap()
            .to_mut_func()
            .body = res;
        return self.env.clone();
    }
}
