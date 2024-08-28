
use crate::root::resource::ast::{self, Program, SymbolTable, SymbolTableEntry};

#[derive(Debug, Clone)]
pub struct Typechecker {
    t: SymbolTable
}

impl Typechecker {
    pub fn new() -> Self {
        Typechecker {
            t: SymbolTable::new()
        }
    }

    pub fn check(&mut self, p: Program) {
        self.t.open_scope();
        for m in p.modules {
            for a in m.body {
                match a {
                    crate::root::resource::ast::Ast::FnDef { name, body, args, .. } => {
                        self.t.insert(SymbolTableEntry { rawname: name , kind: ast::ASTType::Unknown});
                        for a in args {
                            self.t.insert(SymbolTableEntry { rawname: a, kind: ast::ASTType::Unknown })
                        }
                        for e in body {
                            self.check_expr(e);
                        }
                    },

                    _ => println!(""),
                }
            }
        }
    }

    fn check_expr(&mut self, e: crate::root::resource::ast::Expr) -> ast::ASTType {
        //dbg!(e.clone());
        match e {
            crate::root::resource::ast::Expr::BinAdd { l, r } => {
                let lt = self.check_expr(*l.clone());
                let rt = self.check_expr(*r.clone());
                if lt == rt {
                    lt
                } else {
                    panic!("Cannot add {lt:?} {l:?} with {rt:?} {r:?}")
                }
            },
            crate::root::resource::ast::Expr::BinSub { l, r } => {
                let lt = self.check_expr(*l.clone());
                let rt = self.check_expr(*r.clone());
                if lt == rt {
                    lt
                } else {
                    panic!("Cannot subtract {lt:?} {l:?} with {rt:?} {r:?}")
                }
            },
            crate::root::resource::ast::Expr::BinMul { l, r } => {
                let lt = self.check_expr(*l.clone());
                let rt = self.check_expr(*r.clone());
                if lt == rt {
                    lt
                } else {
                    panic!("Cannot multiply {lt:?} {l:?} with {rt:?} {r:?}")
                }
            },
            crate::root::resource::ast::Expr::BinDiv { l, r } => {
                let lt = self.check_expr(*l.clone());
                let rt = self.check_expr(*r.clone());
                if lt == rt {
                    lt
                } else {
                    panic!("Cannot divide {lt:?} {l:?} with {rt:?} {r:?}")
                }
            },
            crate::root::resource::ast::Expr::Logical { l, op, r } => todo!(),
            crate::root::resource::ast::Expr::Assignment { name, value } => {
                let lt = name.get_symbol_name();
                let valt = self.check_expr(*value.clone());
                self.t.insert(SymbolTableEntry::new(lt, valt.clone()));
                return valt;
            },
            crate::root::resource::ast::Expr::MutableAssignment { name, value } => todo!(),
            crate::root::resource::ast::Expr::Closure { args, body } => todo!(),
            crate::root::resource::ast::Expr::Return { value } => {self.check_expr(*value)},
            crate::root::resource::ast::Expr::If { condition, then, otherwise } => todo!(),
            crate::root::resource::ast::Expr::Int(_) => ast::ASTType::Int,
            crate::root::resource::ast::Expr::Flt(_) => ast::ASTType::Flt,
            crate::root::resource::ast::Expr::Str(_) => ast::ASTType::Str,
            crate::root::resource::ast::Expr::Bool(_) => ast::ASTType::Bool,
            crate::root::resource::ast::Expr::Symbol(s) => {
                self.t.get(s.clone()).expect(&format!("Could not find symbol {s}")).kind
            },
            crate::root::resource::ast::Expr::BinAdd { l, r } => {
                let lt = self.check_expr(*l.clone());
                let rt = self.check_expr(*r.clone());
                if lt == rt {
                    lt
                } else {
                    panic!()
                }

            },
            ast::Expr::BinSub { l, r } => todo!(),
            ast::Expr::BinMul { l, r } => todo!(),
            ast::Expr::BinDiv { l, r } => todo!(),
            ast::Expr::Logical { l, op, r } => todo!(),
            ast::Expr::Assignment { name, value } => todo!(),
            ast::Expr::MutableAssignment { name, value } => todo!(),
            ast::Expr::Closure { args, body } => todo!(),
            ast::Expr::Composition { l, r } => todo!(),
            ast::Expr::Return { value } => todo!(),
            ast::Expr::If { condition, then, otherwise } => todo!(),
            ast::Expr::Int(_) => todo!(),
            ast::Expr::Flt(_) => todo!(),
            ast::Expr::Str(_) => todo!(),
            ast::Expr::Bool(_) => todo!(),
            ast::Expr::Symbol(_) => todo!(),
            ast::Expr::FieldAccess(_) => todo!(),
            ast::Expr::Call { name, .. } => self.t.get(name.get_symbol_name()).expect(&format!("Could not find function {}", name.get_symbol_name())).kind,
        }
    }
}

