use crate::root::resource::ast::{self, Expr, Program, SymbolTable, SymbolTableEntry, SymbolType};

#[derive(Debug, Clone)]
pub struct Typechecker {
    s: SymbolTable,
}

impl Typechecker {
    pub fn new() -> Self {
        Typechecker {
            s: SymbolTable::new(),
        }
    }

    pub fn check(&mut self, p: Program) {
        self.s.open_scope();
        for m in p.modules {
            for a in m.body {
                match a {
                    crate::root::resource::ast::Ast::FnDef {
                        name,
                        body,
                        args,
                        rettype,
                        ..
                    } => {
                        let mut temp = vec![];
                        for a in args.clone() {
                            temp.push(a.1)
                        }
                        self.s.insert(SymbolTableEntry {
                            rawname: name,
                            kind: SymbolType::Fn(
                                args.iter()
                                    .map(|f| f.1.clone())
                                    .collect::<Vec<SymbolType>>(),
                                Box::new(rettype),
                            ),
                        });
                        self.s.open_scope();
                        for a in args {
                            self.s.insert(SymbolTableEntry {
                                rawname: a.0,
                                kind: a.1,
                            });
                        }
                        for e in body {
                            self.check_expr(e);
                        }
                        self.s.pop_scope();
                    }

                    _ => println!(""),
                }
            }
        }
    }

    fn check_expr(&mut self, e: crate::root::resource::ast::Expr) -> ast::SymbolType {
        //dbg!(self.clone());
        match e {
            crate::root::resource::ast::Expr::BinAdd { l, r } => {
                let lt = self.check_expr(*l.clone());
                let rt = self.check_expr(*r.clone());
                if self.compare_ty(&lt, &rt) {
                    lt
                } else {
                    panic!("Cannot add {lt:?} {l:?} with {rt:?} {r:?}")
                }
            }
            crate::root::resource::ast::Expr::BinSub { l, r } => {
                let lt = self.check_expr(*l.clone());
                let rt = self.check_expr(*r.clone());
                if self.compare_ty(&lt, &rt) {
                    lt
                } else {
                    panic!("Cannot subtract {lt:?} {l:?} with {rt:?} {r:?}")
                }
            }
            crate::root::resource::ast::Expr::BinMul { l, r } => {
                let lt = self.check_expr(*l.clone());
                let rt = self.check_expr(*r.clone());
                if self.compare_ty(&lt, &rt) {
                    lt
                } else {
                    panic!("Cannot multiply {lt:?} {l:?} with {rt:?} {r:?}")
                }
            }
            crate::root::resource::ast::Expr::BinDiv { l, r } => {
                let lt = self.check_expr(*l.clone());
                let rt = self.check_expr(*r.clone());
                if self.compare_ty(&lt, &rt) {
                    lt
                } else {
                    panic!("Cannot divide {lt:?} {l:?} with {rt:?} {r:?}")
                }
            }
            crate::root::resource::ast::Expr::Logical { l, op, r } => {
                let lt = self.check_expr(*l.clone());
                let rt = self.check_expr(*r.clone());
                if self.compare_ty(&lt, &rt) {
                    lt
                } else {
                    panic!("Cannot divide {lt:?} {l:?} with {rt:?} {r:?}")
                }
            }
            crate::root::resource::ast::Expr::Assignment { name, value } => {
                let lt = name.get_symbol_name();
                let valt = self.check_expr(*value.clone());
                self.s.insert(SymbolTableEntry::new(lt, valt.clone()));
                return valt;
            }
            crate::root::resource::ast::Expr::MutableAssignment { name, value } => {
                let lt = name.get_symbol_name();
                let valt = self.check_expr(*value.clone());
                self.s.insert(SymbolTableEntry::new(lt, valt.clone()));
                return valt;
            }
            crate::root::resource::ast::Expr::Closure { args, body } => todo!(),
            crate::root::resource::ast::Expr::Return { value } => self.check_expr(*value),
            crate::root::resource::ast::Expr::If {
                condition,
                then,
                otherwise,
            } => todo!(),
            crate::root::resource::ast::Expr::Int(_) => ast::SymbolType::Int,
            crate::root::resource::ast::Expr::Flt(_) => ast::SymbolType::Flt,
            crate::root::resource::ast::Expr::Str(_) => ast::SymbolType::Str,
            crate::root::resource::ast::Expr::Bool(_) => ast::SymbolType::Bool,
            crate::root::resource::ast::Expr::Symbol(s) => {
                self.s
                    .get(s.clone())
                    .expect(&format!("Could not find symbol {s}"))
                    .kind
            }

            ast::Expr::FieldAccess(_) => todo!(),
            ast::Expr::Call { name, .. } => self.s.get(name.get_symbol_name()).unwrap().kind,
            ast::Expr::Composition { l, r } => {
                let lt = self.synth_type(*l.clone());
                let rt = self.synth_type(*r.clone());
                if self.compare_ty(&lt, &rt) {
                    lt
                } else {
                    panic!("Cannot compose {lt:?} {l:?} with {rt:?} {r:?}")
                }
            }
        }
    }

    fn compare_ty(&mut self, l: &SymbolType, r: &SymbolType) -> bool {
        // if l.is_generic() && !r.is_generic() {
        //     self.t.update(l.get_generic_name(), r.clone());
        //     return true
        // }
        // if r.is_generic() && !l.is_generic() {
        //     self.t.update(r.get_generic_name(), l.clone());
        //     return true
        // }
        if r == l {
            true
        } else {
            false
        }
    }

    pub fn synth_type(&mut self, e: Expr) -> SymbolType {
        match e {
            Expr::Int(_) => SymbolType::Int,
            Expr::Flt(_) => SymbolType::Flt,
            Expr::Str(_) => SymbolType::Str,
            Expr::Bool(_) => SymbolType::Bool,
            Expr::Symbol(t) => {
                self.s
                    .get(t.clone())
                    .expect(&format!("Unbound symbol {t}"))
                    .kind
            }
            Expr::Call {
                name,
                args,
                namespace,
            } => {
                let callee = self.synth_type(*name.clone());
                if !callee.is_fn() {
                    panic!("{name:?} is not a function")
                } else {
                    let cargs = callee.get_args();
                    if cargs.len() != args.len() {
                        panic!("Invalid arg length")
                    } else {
                        for (i, e) in cargs.into_iter().enumerate() {
                            let arg = self.synth_type(args[i].clone());
                            self.compare_ty(&arg, &e);
                        }
                        return callee.get_rt()
                    }
                }
            }
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeTable {
    entries: Vec<TypeTableEntry>,
}

#[derive(Debug, Clone)]
pub struct TypeTableEntry {
    name: String,
    value: SymbolType,
}

impl TypeTable {
    pub fn new() -> Self {
        Self { entries: vec![] }
    }

    pub fn insert(&mut self, name: String, value: SymbolType) {
        self.entries.push(TypeTableEntry { name, value })
    }

    pub fn update(&mut self, name: String, value: SymbolType) {
        for mut i in &self.entries {
            if i.name == name {
                i = &TypeTableEntry {
                    name: name.clone(),
                    value: value.clone(),
                }
            }
        }
    }
}
