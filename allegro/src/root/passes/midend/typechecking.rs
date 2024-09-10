use std::collections::HashMap;

trace::init_depth_var!();

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
        println!("{:?} vs {:?}", lt, rt);
        //dbg!(self.clone());
        if lt.is_generic() {
            self.s.redefine(&lt.get_generic_name(), rt);
            return true;
        }
        if rt.is_generic() {
            self.s.redefine(&rt.get_generic_name(), &lt);
            return true;
        }
        if lt.is_custom() {
            let nlt = self.s.get(lt.get_custom_name());
            return self.compare_ty(&nlt, rt)
        }
        if rt.is_custom() {
            let nrt = self.s.get(rt.get_custom_name());
            return self.compare_ty(lt, &nrt)
        }
        if lt.is_variant() && !lt.get_variant_members().iter().all(|x| !x.is_generic()) {
            let mut newt: Vec<SymbolType> = vec![];
            for t in lt.get_variant_members() {
                if t.is_generic() {
                    newt.push(self.s.get(t.get_generic_name()))
                } else {
                    newt.push(t)
                }
            }
            let fin = SymbolType::Variant(lt.get_variant_name(), newt);
            return self.compare_ty(&fin, rt);
        }
        if rt.is_variant() && !rt.get_variant_members().iter().all(|x| !x.is_generic()) {
            let mut newt: Vec<SymbolType> = vec![];
            for t in rt.get_variant_members() {
                if t.is_generic() {
                    newt.push(self.s.get(t.get_generic_name()))
                } else {
                    newt.push(t)
                }
            }
            let fin = SymbolType::Variant(rt.get_variant_name(), newt);
            return self.compare_ty(lt, &fin);
        }
        if lt.is_enum() {
            let variants = lt.get_variants();
            for variant in variants {
                if self.compare_ty(&variant, rt) {
                    return true
                } else {
                    continue
                }
            }
            return false
        }
        lt.compare(rt)
    }

    pub fn synth_type(&mut self, e: &Expr) -> SymbolType {
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
                let lt = self.synth_type(&*l);
                let rt = self.synth_type(&*r);
                if self.compare_ty(&lt, &rt) {
                    lt
                } else {
                    panic!("Cannot operate {lt:?} {l:?} with {rt:?} {r:?}")
                }
            }
            Expr::Logical { l, op: _, r } => {
                let lt = self.synth_type(&*l);
                let rt = self.synth_type(&*r);
                if self.compare_ty(&lt, &rt) {
                    SymbolType::Bool
                } else {
                    panic!("Cannot compare {lt:?} {l:?} with {rt:?} {r:?}")
                }
            }
            Expr::Assignment { name, value } => {
                //let lt = self.synth_type(*name.clone());
                let rt = self.synth_type(&*value);
                self.s.set(name.get_symbol_name(), &rt);
                rt
            }
            Expr::MutableAssignment { name, value } => {
                //let lt = self.synth_type(*name.clone());
                let rt = SymbolType::Mut(Box::new(self.synth_type(&*value)));
                self.s.set(name.get_symbol_name(), &rt.clone());
                rt
            }
            Expr::Closure { args, body } => {
                self.s.new_scope();
                for a in args.clone() {
                    self.s.set(a.0.clone(), &a.1.clone());
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
            } => {
                let callee = self.synth_type(&*name);
                if callee.is_fn() {
                    //dbg!(name.clone());
                    let cargs: Vec<SymbolType> = callee.get_args();
                    if cargs.len() == args.len() {
                        for (i, e) in cargs.into_iter().enumerate() {
                            let arg = self.synth_type(&args[i]);
                            if self.compare_ty(&e, &arg) {
                                continue
                            } else {
                                panic!("invalid type on argument, expected {e:?}, found {arg:?}")
                            }
                        }
                        let fin = self.s.get(name.get_symbol_name()).get_rt();
                        if fin.is_variant() {
                            let mut newt: Vec<SymbolType> = vec![];
                            for t in fin.get_variant_members() {
                                if t.is_generic() {
                                    newt.push(self.s.get(t.get_generic_name()))
                                } else {
                                    newt.push(t)
                                }
                            }
                            SymbolType::Variant(fin.get_variant_name(), newt)
                        } else {
                            return fin
                        }
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
                let vt = self.synth_type(&*value);
                let fnt = self.s.get(self.currentfunc.clone());
                if self.compare_ty(&fnt, &vt) {
                    vt
                } else {
                    panic!("Return types don't match! Expected: {fnt:?}; found: {vt:?}",)
                }
            }

            Expr::If {
                condition,
                then,
                otherwise,
            } => {
                let cond_type = self.synth_type(&*condition);
                if cond_type.is_bool() {
                    let tt = self.synth_type(&*then);
                    let ot = self.synth_type(&*otherwise);

                    if tt.compare(&ot) {
                        tt
                    } else {
                        panic!("If branches have differing types! {:?} vs {:?}", tt, ot)
                    }
                } else {
                    panic!("Expression {:?} is not boolean", condition);
                }
            }
            Expr::StructInstance { name, fields } => {
                let f: Vec<(String, SymbolType)> = fields
                    .iter()
                    .map(|e| {
                        let x = e.get_assignment();
                        (x.0, self.synth_type(&x.1).extract())
                    })
                    .collect();
                let dt = self.s.get(name.get_symbol_name());
                if dt.is_obj() {
                    let v = dt.get_members();
                    if v.len() == f.len() {
                        for i in v.iter().enumerate() {
                            let fv = &f[i.0];
                            if !i.1.1.compare(&fv.1) {
                                panic!("{name:?} is not instantiated correctly!")
                            }
                        }
                    } else {
                        panic!("{name:?} is not instantiated correctly!")
                    }

                    return SymbolType::Obj(f);
                } else {
                    panic!("{name:?} is not an object!")
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
                        &SymbolType::Mut(Box::new(SymbolType::Unknown)),
                    );
                    SymbolType::Unknown
                }
            } else {
                res
            }
        }
    }

    pub fn handle_custom(&mut self, t: SymbolType) -> SymbolType {
        if t.is_custom() {
            let a = self.get(t.get_custom_name());
            match a {
                SymbolType::Generic(v) => self.get(v),
                SymbolType::Custom(v, _) => self.get(v),
                _ => a,
            }
        } else if t.is_fn() {
            let mut nargs: Vec<SymbolType> = vec![];
            for a in t.get_args() {
                nargs.push(self.handle_custom(a))
            }
            let nrt = self.handle_custom(t.get_rt());
            SymbolType::Fn(nargs, Box::new(nrt))
        } else if t.is_obj() {
            let members = t.get_members();
            let mut nm: Vec<(String, SymbolType)> = vec![];
            for m in members {
                nm.push((m.0, self.handle_custom(m.1)));
            }
            SymbolType::Obj(nm)
        } else {
            t
        }
    }

    pub fn set(&mut self, name: String, t: &SymbolType) {
        //dbg!(self.clone());
        let newt = self.handle_custom(t.clone());
        //println!("set {} to {:?}", name, newt);
        if self.entries.contains_key(&name) {
            if self.get(name.clone()).is_mut() {
                self.entries.insert(name, SymbolType::Mut(Box::new(newt)));
            } else {
                panic!("Cannot redefine immutable value {name}")
            }
        } else {
            self.entries.insert(name, newt);
        }
    }

    pub fn redefine(&mut self, name: &String, t: &SymbolType) {
        let x = self.entries.get(name);
        if x.is_none() {
            if self.parent.is_some() {
                let mut m = self.parent.clone().unwrap();
                m.redefine(name, t);
                self.parent = Box::new(Some(m));
            } else {
                self.entries.insert(name.clone(), t.clone());
                //panic!("Undefined binding: {}", name)
            }
        } else {
            //dbg!(self.clone());
            //println!("redefine {} to {:?}", name, t);
            self.entries.insert(name.clone(), t.clone());
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
    fn convert(value: &T, t: &mut Typechecker) -> Self;
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
        let mut vmr = value.modules;
        vmr.reverse();
        for m in vmr {
            let tm = TypedModule::convert(&m, &mut t);
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
    fn convert(value: &Module, t: &mut Typechecker) -> Self {
        let mut b: Vec<TypedAst> = vec![];
        for a in &value.body {
            let ta = TypedAst::convert(&a, t);
            dbg!(t.clone());
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
    Struct {
        name: String,
        members: Vec<(String, SymbolType)>,
    },
    Enum {
        name: String,
        members: Vec<SymbolType>,
    },
    TypeDef {
        name: SymbolType,
        funcs: Vec<Self>,
    },
    WithClause {
        include: Vec<String>,
    },    
}

impl Typecheck<Ast> for TypedAst {
    //#[trace]
    fn convert(value: &Ast, t: &mut Typechecker) -> Self {
        //dbg!(t.clone());

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
                    &SymbolType::Fn(
                        args.clone().iter().map(|a| a.1.clone()).collect(),
                        Box::new(rettype.clone()),
                    ),
                );
                t.s.new_scope();
                for a in args {
                    t.s.set(a.0.clone(), &a.1);
                }
                t.currentfunc.clone_from(&name);
                let b: Vec<TypedExpr> = body
                    .iter()
                    .map(|e| TypedExpr::convert(e, t))
                    .collect();
                let mut nrt = rettype.clone();
                if rettype.is_generic() {
                    nrt = b.last().unwrap().t.clone().unwrap();
                }
                let mut nargs: Vec<(String, SymbolType)> = vec![];

                for i in args.clone() {
                    if i.1.is_generic() {
                        nargs.push((i.0, t.s.get(i.1.get_generic_name())));
                    } else {
                        nargs.push(i);
                    }
                }
                t.s.redefine(
                    name,
                    &SymbolType::Fn(
                        nargs.clone().iter().map(|a| a.1.clone()).collect(),
                        Box::new(nrt.clone()),
                    ),
                );

                t.s.pop_scope();

                if !t.compare_ty(rettype, &b.last().unwrap().t.clone().unwrap()) {
                    panic!("Return type was not equal to the last expression in function body; {:?} vs {:?}", rettype, &b.last().unwrap().t.clone().unwrap())
                }
                Self::FnDef {
                    name: name.clone(),
                    rettype: nrt,
                    args: nargs,
                    limits: limits.clone(),
                    body: b,
                }
            }
            Ast::WithClause { include } => Self::WithClause {
                include: include.iter().map(|e| e.get_symbol_name()).collect(),
            },
            Ast::Struct { name, members } => {
                t.s.set(name.clone(), &SymbolType::Obj(members.to_vec()));
                //dbg!(members.clone());
                Self::Struct { name: name.to_string(), members: members.to_vec() }
            }

            Ast::Enum { name, members } => {
                t.s.set(name.clone(), &SymbolType::Enum(members.to_vec()));
                for m in members.clone() {
                    let mm = m.get_variant_members();
                    for mg in mm {
                        if mg.is_generic() {
                            t.s.set(mg.get_generic_name(), &SymbolType::Unknown)
                        }
                    }
                    t.s.set(
                        m.get_variant_name(),
                        &SymbolType::Fn(m.get_variant_members(), Box::new(SymbolType::Variant(m.get_variant_name(), m.get_variant_members()))),
                    );
                    
                }
                Self::Enum { name: name.to_string(), members: members.to_vec() }
            }
            Ast::TypeDef { name, funcs } => {
                // && t.s.get(name.get_custom_name()).compare(name.clone()) 
                if t.s.entries.contains_key(&name.get_custom_name()) {
                    let mut nfuncs: Vec<TypedAst> = vec![];
                    // TODO Why does this stackoverflow?
                    
                        for f in funcs {
                            let res = Self::convert(&f.clone(), &mut t.clone());
                            nfuncs.push(res);
                        }
                    
                    
                    Self::TypeDef { name: name.clone(), funcs: nfuncs }
                } else {
                    panic!("Cannot define implementation for undefined type {name:?}")
                }
                
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub e: Expr,
    pub t: Option<SymbolType>,
}

impl Typecheck<Expr> for TypedExpr {
    fn convert(value: &Expr, t: &mut Typechecker) -> Self {
        Self {
            e: value.clone(),
            t: Some(t.synth_type(value)),
        }
    }
}
