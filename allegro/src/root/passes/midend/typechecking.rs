use crate::root::resource::ast::{Expr, GenericTable, SymbolTable, SymbolType};

#[derive(Debug, Clone)]
pub struct Typechecker {
    pub s: SymbolTable,
    pub currentfunc: String,
}

impl Typechecker {
    pub fn new() -> Self {
        Typechecker {
            s: SymbolTable::new(),
            currentfunc: "".to_string(),
        }
    }

    // pub fn check(&mut self, p: TypedProgram) {
    //     for mut m in p.modules {
    //         for mut a in m.body {
    //             match a {
    //                 TypedAst::FnDef {
    //                     name,
    //                     body,
    //                     args,
    //                     rettype,
    //                     ..
    //                 } => {
    //                     /*
    //                     let mut temp = vec![];
    //                     for a in args.clone() {
    //                         temp.push(a.1)
    //                     }
    //                     */
    //                     /*
    //                     self.s.set(
    //                         name,
    //                         SymbolType::Fn(
    //                             args.iter()
    //                                 .map(|f| f.1.clone())
    //                                 .collect::<Vec<SymbolType>>(),
    //                             Box::new(rettype),
    //                         )
    //                     */
    //                     // );
    //                     for mut e in body {
    //                         e.t = Some(self.synth_type(e.e));
    //                     }
    //                 }

    //                 _ => println!(""),
    //             }
    //         }
    //     }
    // }

    // fn check_expr(&mut self, e: crate::root::resource::ast::Expr) -> ast::SymbolType {
    //     dbg!(self.clone());
    //     match e {
    //         crate::root::resource::ast::Expr::BinAdd { l, r } => {
    //             let lt = self.check_expr(*l.clone());
    //             let rt = self.check_expr(*r.clone());
    //             if self.compare_ty(&lt, &rt) {
    //                 lt
    //             } else {
    //                 panic!("Cannot add {lt:?} {l:?} with {rt:?} {r:?}")
    //             }
    //         }
    //         crate::root::resource::ast::Expr::BinSub { l, r } => {
    //             let lt = self.check_expr(*l.clone());
    //             let rt = self.check_expr(*r.clone());
    //             if self.compare_ty(&lt, &rt) {
    //                 lt
    //             } else {
    //                 panic!("Cannot subtract {lt:?} {l:?} with {rt:?} {r:?}")
    //             }
    //         }
    //         crate::root::resource::ast::Expr::BinMul { l, r } => {
    //             let lt = self.check_expr(*l.clone());
    //             let rt = self.check_expr(*r.clone());
    //             if self.compare_ty(&lt, &rt) {
    //                 lt
    //             } else {
    //                 panic!("Cannot multiply {lt:?} {l:?} with {rt:?} {r:?}")
    //             }
    //         }
    //         crate::root::resource::ast::Expr::BinDiv { l, r } => {
    //             let lt = self.check_expr(*l.clone());
    //             let rt = self.check_expr(*r.clone());
    //             if self.compare_ty(&lt, &rt) {
    //                 lt
    //             } else {
    //                 panic!("Cannot divide {lt:?} {l:?} with {rt:?} {r:?}")
    //             }
    //         }
    //         crate::root::resource::ast::Expr::Logical { l, op, r } => {
    //             let lt = self.check_expr(*l.clone());
    //             let rt = self.check_expr(*r.clone());
    //             if self.compare_ty(&lt, &rt) {
    //                 lt
    //             } else {
    //                 panic!("Cannot divide {lt:?} {l:?} with {rt:?} {r:?}")
    //             }
    //         }
    //         crate::root::resource::ast::Expr::Assignment { name, value } => {
    //             todo!()
    //             // let lt = name.get_symbol_name();
    //             // let valt = self.check_expr(*value.clone());
    //             // self.s.set(lt, valt.clone());
    //             // return valt;
    //         }
    //         crate::root::resource::ast::Expr::MutableAssignment { name, value } => {
    //             todo!()
    //             // let lt = name.get_symbol_name();
    //             // let valt = self.check_expr(*value.clone());
    //             // self.s.insert(SymbolTableEntry::new(lt, valt.clone()));
    //             // return valt;
    //         }
    //         crate::root::resource::ast::Expr::Closure { args, body } => todo!(),
    //         crate::root::resource::ast::Expr::Return { value } => self.check_expr(*value),
    //         crate::root::resource::ast::Expr::If {
    //             condition,
    //             then,
    //             otherwise,
    //         } => todo!(),
    //         crate::root::resource::ast::Expr::Int(_) => ast::SymbolType::Int,
    //         crate::root::resource::ast::Expr::Flt(_) => ast::SymbolType::Flt,
    //         crate::root::resource::ast::Expr::Str(_) => ast::SymbolType::Str,
    //         crate::root::resource::ast::Expr::Bool(_) => ast::SymbolType::Bool,
    //         crate::root::resource::ast::Expr::Symbol(s) => {
    //             todo!()
    //             // self.s
    //             //     .get(s.clone())
    //             //     .expect(&format!("Could not find symbol {s}"))
    //             //     .kind
    //         }

    //         ast::Expr::FieldAccess(_) => todo!(),
    //         ast::Expr::Call { name, .. } => {
    //             //todo!()
    //             self.s.get(name.get_symbol_name())
    //         },
    //         ast::Expr::Composition { l, r } => {
    //             let lt = self.synth_type(*l.clone());
    //             let rt = self.synth_type(*r.clone());
    //             if self.compare_ty(&lt, &rt) {
    //                 lt
    //             } else {
    //                 panic!("Cannot compose {lt:?} {l:?} with {rt:?} {r:?}")
    //             }
    //         }
    //     }
    // }

    pub fn compare_ty(&mut self, lt: &SymbolType, rt: &SymbolType) -> bool {
        if lt.is_generic() && !rt.is_generic() {
            self.s.set(lt.get_generic_name(), rt.clone());
            true
        } else if !lt.is_generic() && rt.is_generic() {
            self.s.set(lt.get_generic_name(), rt.clone());
            true
        } else if lt.is_generic() && rt.is_generic() {
            self.s.set(lt.get_generic_name(), rt.clone());
            true
        } else {
            lt.compare(rt.clone())
        }
    }

    fn redefine(&mut self, f: &String, rt: &SymbolType) {
        self.s.redefine(f.to_string(), rt.clone());
    }

    pub fn synth_type(&mut self, e: Expr) -> SymbolType {
        //dbg!(self.clone());
        match e {
            Expr::Int(_) => SymbolType::Int,
            Expr::Flt(_) => SymbolType::Flt,
            Expr::Str(_) => SymbolType::Str,
            Expr::Bool(_) => SymbolType::Bool,
            Expr::Symbol(t) => self.s.get(t.clone()),
            Expr::BinAdd { l, r } => {
                let lt = self.synth_type(*l.clone());
                let rt = self.synth_type(*r.clone());
                if self.compare_ty(&lt, &rt) {
                    lt
                } else {
                    panic!("Cannot add {lt:?} {l:?} with {rt:?} {r:?}")
                }
            }
            Expr::BinSub { l, r } => {
                let lt = self.synth_type(*l.clone());
                let rt = self.synth_type(*r.clone());
                if self.compare_ty(&l, &lt, &r, &rt) {
                    lt
                } else {
                    panic!("Cannot subtract {lt:?} {l:?} with {rt:?} {r:?}")
                }
            }
            Expr::BinMul { l, r } => {
                let lt = self.synth_type(*l.clone());
                let rt = self.synth_type(*r.clone());
                if self.compare_ty(&l, &lt, &r, &rt) {
                    lt
                } else {
                    panic!("Cannot multiply {lt:?} {l:?} with {rt:?} {r:?}")
                }
            }
            Expr::BinDiv { l, r } => {
                let lt = self.synth_type(*l.clone());
                let rt = self.synth_type(*r.clone());
                if self.compare_ty(&l, &lt, &r, &rt) {
                    lt
                } else {
                    panic!("Cannot divide {lt:?} {l:?} with {rt:?} {r:?}")
                }
            }
            Expr::Logical { l, op: _, r } => {
                let lt: SymbolType = self.synth_type(*l.clone());
                let rt = self.synth_type(*r.clone());
                if self.compare_ty(&l, &lt, &r, &rt) {
                    SymbolType::Bool
                } else {
                    panic!("Cannot compare {lt:?} {l:?} with {rt:?} {r:?}")
                }
            }
            Expr::Assignment { name, value } => {
                //let lt = self.synth_type(*name.clone());
                let rt = self.synth_type(*value.clone());
                self.s.set(name.get_symbol_name(), rt.clone());
                rt
            }
            Expr::MutableAssignment { name, value } => {
                //let lt = self.synth_type(*name.clone());
                let rt = SymbolType::Mut(Box::new(self.synth_type(*value.clone())));
                self.s.set(name.get_symbol_name(), rt.clone());
                rt
            }
            Expr::Closure { args, body } => {
                self.s.new_scope();
                for a in args.clone() {
                    self.s.set(a.0.clone(), a.1.clone());
                }
                let mut x: SymbolType = SymbolType::Naught;
                for e in body {
                    x = self.synth_type(e)
                }
                SymbolType::Fn(args.iter().map(|a| a.1.clone()).collect(), Box::new(x))
            }
            Expr::Call {
                name,
                args,
                namespace: _,
            } => {
                let callee = self.synth_type(*name.clone());
                if !callee.is_fn() {
                    panic!("{name:?} is not a function")
                } else {
                    //dbg!(name.clone());
                    let cargs: Vec<SymbolType> = callee.get_args();
                    if cargs.len() != args.len() {
                        panic!(
                            "Invalid arg length on func: {:?}. Expected {}, found {}",
                            name,
                            cargs.len(),
                            args.len()
                        )
                    } else {
                        for (i, e) in cargs.into_iter().enumerate() {
                            let arg = self.synth_type(args[i].clone());
                            self.compare_callargs(&arg, &e);
                        }
                        return callee.get_rt();
                    }
                }
            }

            Expr::Return { value } => {
                let vt = self.synth_type(*value);
                let fnt = self.s.get(self.currentfunc.clone());
                if self.compare_ret(&self.currentfunc.clone(), &fnt, &vt) {
                    vt
                } else {
                    panic!(
                        "Return types don't match! Expected: {:?}; found: {:?}",
                        fnt, vt
                    )
                }
            }

            Expr::If {
                condition,
                then,
                otherwise,
            } => {
                let cond_type = self.synth_type(*condition.clone());
                if cond_type.is_bool() {
                    let tt = self.synth_type(*then);
                    let ot = self.synth_type(*otherwise);
                    if self.compare_callargs(&tt, &ot) {
                        tt
                    } else {
                        panic!("If branches have differing types!")
                    }
                } else {
                    panic!("Expression {:?} is not boolean", condition);
                }
            }

            _ => panic!("unknown expr: {:?}", e),
        }
    }
}
