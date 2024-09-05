use std::collections::HashMap;

use crate::root::resource::ast::{Ast, Expr, FnArgLimit, Module, Program, SymbolType};

#[derive(Debug, Clone)]
pub struct Typechecker {
    pub s: SymbolTable,
    pub currentfunc: String,
}

impl Default for Typechecker {
    fn default() -> Self {
        Self::new()
    }
}

impl Typechecker {
    pub fn new() -> Self {
        Typechecker {
            s: SymbolTable::new(),
            currentfunc: String::new(),
        }
    }

    pub fn compare_ty(&mut self, lt: &SymbolType, rt: &SymbolType) -> bool {
        //println!("{:?} vs {:?}",lt ,rt);
        //dbg!(self.clone());
        if lt.is_generic() {
            self.s.redefine(lt.get_generic_name(), rt.clone());
            return true;
        }
        if rt.is_generic() {
            self.s.redefine(rt.get_generic_name(), lt.clone());
            return true;
        }
        lt.compare(rt.clone())
        //}
    }

    pub fn synth_type(&mut self, e: Expr) -> SymbolType {
        //dbg!(self.clone());
        match e {
            Expr::Int(_) => SymbolType::Int,
            Expr::Flt(_) => SymbolType::Flt,
            Expr::Str(_) => SymbolType::Str,
            Expr::Bool(_) => SymbolType::Bool,
            Expr::Symbol(t) => self.s.get(t.clone()),
            Expr::BinAdd { l, r }
            | Expr::BinSub { l, r }
            | Expr::BinMul { l, r }
            | Expr::BinDiv { l, r } => {
                let lt = self.synth_type(*l.clone());
                let rt = self.synth_type(*r.clone());
                if self.compare_ty(&lt, &rt) {
                    lt
                } else {
                    panic!("Cannot operate {lt:?} {l:?} with {rt:?} {r:?}")
                }
            }
            Expr::Logical { l, op: _, r } => {
                let lt: SymbolType = self.synth_type(*l.clone());
                let rt: SymbolType = self.synth_type(*r.clone());
                if self.compare_ty(&lt, &rt) {
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
                    x = self.synth_type(e);
                }
                SymbolType::Fn(args.iter().map(|a| a.1.clone()).collect(), Box::new(x))
            }
            Expr::Call {
                name,
                args,
                namespace: _,
            } => {
                let callee = self.synth_type(*name.clone());
                if callee.is_fn() {
                    //dbg!(name.clone());
                    let cargs: Vec<SymbolType> = callee.get_args();
                    if cargs.len() == args.len() {
                        for (i, e) in cargs.into_iter().enumerate() {
                            let arg = self.synth_type(args[i].clone());
                            self.compare_ty(&arg, &e);
                        }
                        self.s.get(name.get_symbol_name()).get_rt()

                    } else {
                        
                        panic!(
                            "Invalid arg length on func: {:?}. Expected {}, found {}",
                            name,
                            cargs.len(),
                            args.len()
                        )
                    }
                    
                } else {
                    panic!("{name:?} is not a function")
                }
            }

            Expr::Return { value } => {
                let vt = self.synth_type(*value);
                let fnt = self.s.get(self.currentfunc.clone());
                if self.compare_ty(&fnt, &vt) {
                    vt
                } else {
                    panic!(
                        "Return types don't match! Expected: {fnt:?}; found: {vt:?}",
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

                    if tt.compare(ot.clone()) {
                        tt
                    } else {
                        panic!("If branches have differing types! {:?} vs {:?}", tt, ot)
                    }
                } else {
                    panic!("Expression {:?} is not boolean", condition);
                }
            }

            _ => panic!("unknown expr: {:?}", e),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub entries: HashMap<String, SymbolType>,
    pub parent: Box<Option<Self>>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            parent: Box::new(None),
        }
    }

    pub fn get(&mut self, name: String) -> SymbolType {
        //dbg!(self.clone());
        let x = self.entries.get(&name);
        if x.is_none() {
            if self.parent.is_some() {
                self.parent.clone().unwrap().get(name)
            } else {
                panic!("Undefined binding: {}", name)
            }
        } else {
            let res = x.unwrap().clone();
            if res.is_generic() {
                if self.entries.contains_key(&res.get_generic_name()) {
                    self.get(res.get_generic_name())
                } else {
                    self.set(
                        res.get_generic_name(),
                        SymbolType::Mut(Box::new(SymbolType::Unknown)),
                    );
                    SymbolType::Unknown
                }
            } else {
                res
            }
        }
    }

    pub fn set(&mut self, name: String, t: SymbolType) {
        dbg!(self.clone());
        println!("set {} to {:?}", name, t);
        // if self.get(name.clone()) == t {
        //     // do nothing
        // }
        if self.entries.contains_key(&name) {
            if self.get(name.clone()).is_mut() {
                self.entries.insert(name, SymbolType::Mut(Box::new(t)));
            } else {
                panic!("Cannot redefine immutable value {name}")
            }
        } else {
            self.entries.insert(name, t);
        }
    }

    pub fn redefine(&mut self, name: String, t: SymbolType) {
        dbg!(self.clone());
        println!("redefine {} to {:?}", name, t);
        let x = self.entries.get(&name);
        if x.is_none() {
            if self.parent.is_some() {
                let mut m = self.parent.clone().unwrap();
                m.redefine(name, t);
                self.parent = Box::new(Some(m));
            } else {
                panic!("Undefined binding: {}", name)
            }
        } else {
            self.entries.insert(name, t);
        }
    }

    pub fn new_scope(&mut self) {
        let temp = self.clone();
        self.parent = Box::new(Some(temp));
        self.entries = HashMap::new();
    }

    pub fn pop_scope(&mut self) {
        let temp = self.parent.clone().unwrap();
        self.entries = temp.entries;
        self.parent = temp.parent;
    }
}

pub trait Typecheck<T> {
    fn convert(value: T, t: &mut Typechecker) -> Self;
}

#[derive(Debug, Clone)]
pub struct TypedProgram {
    pub modules: Vec<TypedModule>,
    pub dependencies: Vec<String>,
}

impl From<Program> for TypedProgram {
    fn from(value: Program) -> Self {
        let mut t = Typechecker::new();
        t.s.new_scope();
        let mut vtm: Vec<TypedModule> = vec![];
        let mut vmr = value.modules.clone();
        vmr.reverse();
        for m in vmr {
            let tm = TypedModule::convert(m, &mut t);
            vtm.push(tm);
        }
        Self {
            modules: vtm,
            dependencies: value.dependencies,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedModule {
    pub body: Vec<TypedAst>,
}

impl Typecheck<Module> for TypedModule {
    fn convert(value: Module, t: &mut Typechecker) -> Self {
        let mut b: Vec<TypedAst> = vec![];
        for a in value.body {
            let ta = TypedAst::convert(a.clone(), t);
            b.push(ta)
        }
        Self { body: b }
    }
}
#[derive(Debug, Clone)]
pub enum TypedAst {
    FnDef {
        name: String,
        rettype: SymbolType,
        args: Vec<(String, SymbolType)>,
        limits: Option<Vec<FnArgLimit>>,
        body: Vec<TypedExpr>,
    },
    Record {
        name: String,
        members: Vec<(String, SymbolType)>,
    },
    TypeDef {
        name: String,
        funcs: Vec<Self>,
    },
    WithClause {
        include: Vec<String>,
    },
}

impl Typecheck<Ast> for TypedAst {
    fn convert(value: Ast, t: &mut Typechecker) -> Self {
        match value {
            Ast::FnDef {
                name,
                rettype,
                args,
                limits,
                body,
            } => {
                t.s.set(
                    name.clone(),
                    SymbolType::Fn(
                        args.clone().iter().map(|a| a.1.clone()).collect(),
                        Box::new(rettype.clone()),
                    ),
                );
                t.s.new_scope();
                for a in &args {
                    t.s.set(a.0.clone(), a.1.clone())
                }
                t.currentfunc.clone_from(&name);
                let b: Vec<TypedExpr> = body
                    .iter()
                    .map(|e| TypedExpr::convert(e.clone(), t))
                    .collect();
                let mut nrt = rettype.clone();
                if rettype.is_generic() {
                    nrt = b.last().unwrap().t.clone().unwrap();
                }
                let mut nargs: Vec<(String, SymbolType)> = vec![];

                for i in args.clone() {
                    if i.1.is_generic() {
                        nargs.push((i.0, t.s.get(i.1.get_generic_name())))
                    } else {
                        nargs.push(i)
                    }
                }
                t.s.redefine(
                    name.clone(),
                    SymbolType::Fn(
                        nargs.clone().iter().map(|a| a.1.clone()).collect(),
                        Box::new(nrt.clone()),
                    ),
                );

                t.s.pop_scope();

                // if !t.compare_ty(&rettype, &b.last().unwrap().t.clone().unwrap()) {
                //     panic!("Return type was not equal to the last expression in function body")
                // }
                Self::FnDef {
                    name,
                    rettype: nrt,
                    args: nargs,
                    limits,
                    body: b,
                }
            }
            Ast::Record {
                name: _,
                members: _,
            } => todo!(),
            Ast::TypeDef { name: _, funcs: _ } => todo!(),
            Ast::WithClause { include } => Self::WithClause {
                include: include.iter().map(|e| e.get_symbol_name()).collect(),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub e: Expr,
    pub t: Option<SymbolType>,
}

impl Typecheck<Expr> for TypedExpr {
    fn convert(value: Expr, t: &mut Typechecker) -> Self {
        Self {
            e: value.clone(),
            t: Some(t.synth_type(value)),
        }
    }
}
