use std::collections::HashMap;

use crate::root::resource::ast::{Ast, Expr, FnArgLimit, Module, Program, Property, SymbolType};
use serde::{Deserialize, Serialize};
use thin_vec::ThinVec;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Typechecker {
    pub symbol_table: SymbolTable,
    pub current_func: String,
    pub current_parent: Option<SymbolType>,
}

impl Default for Typechecker {
    fn default() -> Self {
        Self::new()
    }
}

impl Typechecker {
    pub fn new() -> Self {
        Typechecker {
            symbol_table: SymbolTable::new(),
            current_func: String::new(),
            current_parent: None
        }
    }

    #[inline(always)]
    pub fn compare_ty(&mut self, lt: &SymbolType, rt: &SymbolType) -> bool {
        //println!("tc : {:?} vs {:?}", lt, rt);
        //dbg!(self.clone());
        if lt.is_generic() {
            if lt == rt {
                return true;
            }
            if self.symbol_table.has(&lt.get_generic_name()) {
                let new = self.symbol_table.get(lt.get_generic_name());
                return self.compare_ty(&new, rt);
            } else {
                self.symbol_table.redefine(&lt.get_generic_name(), rt);
            }
            //let new = self.symbol_table.get(lt.get_generic_name());
            //return self.compare_ty(lt, rt)
        }
        if rt.is_generic() {
            if rt == lt {
                return true;
            }
            if self.symbol_table.has(&rt.get_generic_name()) {
                let new = self.symbol_table.get(rt.get_generic_name());
                return self.compare_ty(lt, &new);
            } else {
                self.symbol_table.redefine(&rt.get_generic_name(), lt);
            }
            //let new = self.symbol_table.get(lt.get_generic_name());
            //return self.compare_ty(lt, rt)
        }
        if lt.is_custom() {
            let nlt = self.symbol_table.get(lt.get_custom_name());
            //dbg!(nlt.clone());
            return self.compare_ty(&nlt, rt);
        }
        if rt.is_custom() {
            let nrt = self.symbol_table.get(rt.get_custom_name());
            return self.compare_ty(lt, &nrt);
        }
        if lt.is_variant() && !lt.get_variant_members().iter().all(|x| !x.is_generic()) {
            let mut newt: Vec<SymbolType> = vec![];
            for t in lt.get_variant_members() {
                if t.is_generic() {
                    newt.push(self.symbol_table.get(t.get_generic_name()))
                } else {
                    newt.push(t)
                }
            }
            let fin = SymbolType::Variant(lt.get_variant_name(), newt.into());
            return self.compare_ty(&fin, rt);
        }
        if rt.is_variant() && !rt.get_variant_members().iter().all(|x| !x.is_generic()) {
            let mut newt: Vec<SymbolType> = vec![];
            for t in rt.get_variant_members() {
                if t.is_generic() {
                    newt.push(self.symbol_table.get(t.get_generic_name()))
                } else {
                    newt.push(t)
                }
            }
            let fin = SymbolType::Variant(rt.get_variant_name(), newt.into());
            return self.compare_ty(lt, &fin);
        }
        if lt.is_enum() {
            if rt.is_enum() {
                let variants = lt.get_variants();
                let rvt = rt.get_variants();
                for variant in variants.iter().enumerate() {
                    //println!("vtc: {:?} vs {:?}", variant.1, &rvt[variant.0]);
                    if self.compare_ty(&variant.1, &rvt[variant.0]) {
                        continue;
                    } else {
                        return false;
                    }
                }
                return true;
            }

            return false;
        }
        lt.compare(rt)
    }

    pub fn synth_type(&mut self, e: &Expr) -> SymbolType {
        //dbg!(self.clone());
        let l = match e {
            Expr::Int(_) => SymbolType::Int,
            Expr::Flt(_) => SymbolType::Flt,
            Expr::Str(_) => SymbolType::Str,
            Expr::Bool(_) => SymbolType::Bool,
            Expr::Symbol(t) => if t == "self" {self.current_parent.clone().unwrap()} else {self.symbol_table.get(t.clone())} ,
            Expr::BinAdd { l, r }
            | Expr::BinSub { l, r }
            | Expr::BinMul { l, r }
            | Expr::BinDiv { l, r } => {
                let lt = self.synth_type(&*l);
                let rt = self.synth_type(&*r);
                //dbg!(lt.clone());
                //dbg!(rt.clone());
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
                self.symbol_table.set(&name.get_symbol_name(), &rt);
                rt
            }
            Expr::MutableAssignment { name, value } => {
                //let lt = self.synth_type(*name.clone());
                let rt = SymbolType::Mut(Box::new(self.synth_type(&*value)));
                self.symbol_table.set(&name.get_symbol_name(), &rt.clone());
                rt
            }
            Expr::Closure { args, body } => {
                self.symbol_table.new_scope();
                for a in args.clone() {
                    self.symbol_table.set(&a.0.clone(), &a.1.clone());
                }
                let mut x: SymbolType = SymbolType::Naught;
                for e in body {
                    x = self.synth_type(e);
                }
                SymbolType::Fn(
                    args.iter().map(|a| a.1.clone()).collect(),
                    Box::new(x),
                    false,
                )
            }
            Expr::Call { name, args } => {
                //dbg!(name.clone());
                if name.len() == 1 {
                    let name = name.last().unwrap();
                    let callee = self.synth_type(name);
                    if callee.is_fn() {
                        //dbg!(name.clone());
                        let cargs: Vec<SymbolType> = callee.get_args().to_vec();
                        if cargs.len() == args.len() {
                            for (i, e) in cargs.into_iter().enumerate() {
                                let arg = self.synth_type(&args[i]);
                                if self.compare_ty(&e, &arg) {
                                    continue;
                                } else {
                                    panic!(
                                        "invalid type on argument, expected {e:?}, found {arg:?}"
                                    )
                                }
                            }
                            let fin = self.symbol_table.get(name.get_symbol_name()).get_rt();
                            if fin.is_variant() {
                                let mut newt: Vec<SymbolType> = vec![];
                                for t in fin.get_variant_members() {
                                    if t.is_generic() {
                                        newt.push(self.symbol_table.get(t.get_generic_name()))
                                    } else {
                                        newt.push(t)
                                    }
                                }
                                SymbolType::Variant(fin.get_variant_name(), newt.into())
                            } else {
                                return fin;
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
                } else {
                    assert!(self.symbol_table.has(&name.first().unwrap().get_symbol_name()));
                    let callee = self.synth_type(name.last().unwrap());
                    if callee.is_fn() {
                        //dbg!(name.clone());
                        let cargs: Vec<SymbolType> = callee.get_args().to_vec();
                        if cargs.len() == args.len() {
                            for (i, e) in cargs.into_iter().enumerate() {
                                let arg = self.synth_type(&args[i]);
                                if self.compare_ty(&e, &arg) {
                                    continue;
                                } else {
                                    panic!(
                                        "invalid type on argument, expected {e:?}, found {arg:?}"
                                    )
                                }
                            }
                            let fin = self
                                .symbol_table
                                .get(name.last().unwrap().get_symbol_name())
                                .get_rt();
                            if fin.is_variant() {
                                let mut newt: Vec<SymbolType> = vec![];
                                for t in fin.get_variant_members() {
                                    if t.is_generic() {
                                        newt.push(self.symbol_table.get(t.get_generic_name()))
                                    } else {
                                        newt.push(t)
                                    }
                                }
                                SymbolType::Variant(fin.get_variant_name(), newt.into())
                            } else {
                                return fin;
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
            }

            Expr::Return { value } => {
                let vt = self.synth_type(&*value);
                let fnt = self.symbol_table.get(self.current_func.clone());
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
                    self.symbol_table.new_scope();
                    let tt = self.synth_type(&*then);
                    self.symbol_table.pop_scope();
                    self.symbol_table.new_scope();
                    let ot = self.synth_type(&*otherwise);
                    self.symbol_table.pop_scope();

                    if self.compare_ty(&ot, &tt) {
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
                        (x.0, self.synth_type(&x.1))
                    })
                    .collect();
                let dt = self.symbol_table.get(name.get_symbol_name());
                let mut generic_vec: Vec<SymbolType> = vec![];
                if dt.is_obj() {
                    let v = dt.get_members();
                    if v.len() == f.len() {
                        for i in v.iter().enumerate() {
                            let fv = &f[i.0];
                            if !self.compare_ty(&i.1 .1, &fv.1) {
                                panic!(
                                    "{name:?} is not instantiated correctly! {:?} vs {:?}",
                                    i.1 .1, fv.1
                                )
                            } else if fv.1.is_generic() {
                                generic_vec.push(fv.1.clone())
                            }
                        }
                    } else {
                        panic!("{name:?} is not instantiated correctly!")
                    }

                    return SymbolType::Custom(name.get_symbol_name(), generic_vec.into());
                } else {
                    panic!("{name:?} is not an object!")
                }
            }
            Expr::AddressOf(t) => SymbolType::Pointer(Box::new(self.synth_type(t))),
            Expr::Composition { l, r } => {
                let n =  r.get_callee();
                assert!(self.symbol_table.get(n.clone()).is_fn());

                let mut a = r.get_call_args();
                let mut finargs = vec![*l.clone()];
                finargs.append(&mut a);
                self.synth_type(&Expr::Call { name: vec![Expr::Symbol(n)], args: finargs })
            }
            Expr::FieldAccess(o, f) => {
                //dbg!(self.clone());
                let mut ot = self.synth_type(o);
                if matches!(ot, SymbolType::TypeDefSelf) {
                    ot = self.current_parent.clone().unwrap();
                }
                //dbg!(self.clone());
                assert!(ot.is_obj());
                let om = ot.get_members();
                let n = format!("$_{}", f.get_symbol_name());
                for e in om {
                    if e.0 == n {
                        return e.1;
                    } else {
                        continue;
                    }
                }
                panic!("{:?} is not a field of {:?}", n, o)
            }
            _ => panic!("unknown expr: {:?}", e),
        };
        self.symbol_table.handle_custom(l)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SymbolTableEntry {
    t: SymbolType,
    sub: Option<HashMap<String, SymbolType>>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SymbolTable {
    pub entries: HashMap<String, SymbolTableEntry>,
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

    //#[recursive]
    pub fn get(&mut self, name: String) -> SymbolType {
        //println!("get {:?}", name);
        let x = self.entries.get(&name);
        if x.is_none() {
            if self.parent.is_some() {
                self.parent.clone().unwrap().get(name)
            } else {
                if name.starts_with("?_") {
                    //todo!();
                    SymbolType::Generic(name)
                } else {
                    panic!("Undefined binding: {}", name)
                }
            }
        } else {
            let res = x.unwrap().clone();
            // if res.is_generic() {
            //     dbg!(res.get_generic_name().clone());
            //     dbg!(self.clone());
            //     if self.has(&res.get_generic_name()) {
            //         self.get(res.get_generic_name())
            //     } else {
            //         //res
            //         panic!("invalid generic type {:?}", name)
            //     }
            // } else {
            res.t
            //}
        }
    }

    pub fn has(&mut self, name: &String) -> bool {
        //println!("has {:?}", name);
        let x = self.entries.get(name);
        if x.is_none() {
            if self.parent.is_some() {
                self.parent.clone().unwrap().has(name)
            } else {
                false
            }
        } else {
            let res = x.unwrap().clone();
            if res.t.is_generic() {
                //dbg!(res.get_generic_name().clone());
                if self.entries.contains_key(&res.t.get_generic_name()) {
                    true
                } else {
                    false
                }
            } else {
                true
            }
        }
    }

    /*
    else if t.is_generic() {
            if self.entries.contains_key(&t.get_generic_name()) {
                self.get(t.get_generic_name())
            } else {
                self.set(
                    t.get_generic_name(),
                    &SymbolType::Mut(Box::new(SymbolType::Unknown)),
                );
                SymbolType::Unknown
            }
        }
     */
    //#[recursive]
    pub fn handle_custom(&mut self, t: SymbolType) -> SymbolType {
        if t.is_custom() {
            //println!("handling {:?}", t);
            let a = self.get(t.get_custom_name());
            match a {
                SymbolType::Generic(v) => self.get(v),
                SymbolType::Custom(v, _) => self.get(v),
                _ => a,
            }
        } else if t.is_generic() {
            //println!("handling {:?}", t);

            self.get(t.get_generic_name())
        } else if t.is_fn() {
            //println!("handling {:?}", t);
            let mut nargs: Vec<SymbolType> = vec![];
            for a in t.get_args() {
                nargs.push(self.handle_custom(a))
            }
            let nrt = self.handle_custom(t.get_rt());
            SymbolType::Fn(nargs.into(), Box::new(nrt), t.is_variant_constructor())
        // } else if t.is_obj() {
        //     println!("handling {:?}", t);
        //     let members = t.get_members();
        //     let mut nm: Vec<(String, SymbolType)> = vec![];
        //     for m in members {
        //         nm.push((m.0, self.handle_custom(m.1)));
        //     }
        //     SymbolType::Obj(nm.into())
        } else {
            t
        }
    }

    //#[inline]
    pub fn set(&mut self, name: &String, t: &SymbolType) {
        //dbg!(self.clone());
        let newt = self.handle_custom(t.clone());
        if self.entries.contains_key(name) {
            if self.get(name.clone()).is_mut() {
                //println!("set {} to {:?}", name, newt);
                self.entries
                    .insert(name.to_string(), SymbolTableEntry{t: SymbolType::Mut(Box::new(newt)), sub: None});
            } else {
                panic!("Cannot redefine immutable value {name}")
            }
        } else {
            //println!("set {} to {:?}", name, newt);
            self.entries.insert(name.to_string(), SymbolTableEntry{t: newt, sub: None});
        }
    }

    // #[recursive]
    pub fn redefine(&mut self, name: &String, t: &SymbolType) {
        //println!("redefine {} to {:?}", name, t);

        let x = self.entries.get(name);
        if x.is_none() {
            if self.parent.is_some() {
                let mut m = self.parent.clone().unwrap();
                m.redefine(name, t);
                self.parent = Box::new(Some(m));
            } else {
                if name.starts_with("?_") {
                    self.entries.insert(name.clone(), SymbolTableEntry {t: t.clone(), sub: None});
                } else {
                    panic!("Undefined binding: {}", name)
                }
            }
        } else {
            self.entries.insert(name.clone(), SymbolTableEntry {t: t.clone(), sub: None});
        }
    }

    pub fn new_scope(&mut self) {
        //println!("open scope");
        let temp = self.clone();
        self.parent = Box::new(Some(temp));
        self.entries = HashMap::new();
    }

    pub fn pop_scope(&mut self) {
        //println!("close scope");
        let temp = self.parent.clone().unwrap();
        self.entries = temp.entries;
        self.parent = temp.parent;
    }
}

pub trait Typecheck<T> {
    fn convert(value: &T, t: &mut Typechecker, parent: Option<String>) -> Self;
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedProgram {
    pub modules: Vec<TypedModule>,
    pub dependencies: Vec<String>,
}

impl From<Program> for TypedProgram {
    fn from(value: Program) -> Self {
        let mut t = Typechecker::new();
        t.symbol_table.new_scope();
        let mut vtm: Vec<TypedModule> = vec![];
        let mut vmr = value.modules;
        vmr.reverse();
        for m in vmr {
            let tm = TypedModule::convert(&m, &mut t, None);
            vtm.push(tm);
        }
        //dbg!(t);
        Self {
            modules: vtm,
            dependencies: value.dependencies,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedModule {
    pub body: Vec<TypedAst>,
}

impl Typecheck<Module> for TypedModule {
    fn convert(value: &Module, t: &mut Typechecker, parent: Option<String>) -> Self {
        let mut b: Vec<TypedAst> = vec![];
        for a in &value.body {
            let ta = TypedAst::convert(&a, t, parent.clone());
            //dbg!(t.clone());
            b.push(ta)
        }
        Self { body: b }
    }
}
#[derive(Debug, Clone, Serialize, Deserialize)]
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
    Propdef {
        p: Property,
    },
    TypeAlias {
        name: String,
        is: SymbolType,
    },
}

impl Typecheck<Ast> for TypedAst {
    fn convert(value: &Ast, t: &mut Typechecker, parent: Option<String>) -> Self {
        //dbg!(t.clone());

        match value {
            Ast::FnDef {
                name,
                rettype,
                args,
                limits,
                body,
            } => {
                let n: String = name.clone();
                if parent.is_some() {
                    t.symbol_table.set(
                        &n.clone(),
                        &SymbolType::MethodFn {
                            parent: parent.clone().unwrap(),
                            f: Box::new(SymbolType::Fn(
                                args.clone().iter().map(|a| a.1.clone()).collect(),
                                Box::new(rettype.clone()),
                                false,
                            )),
                        },
                    );
                } else {
                    t.symbol_table.set(
                        &n.clone(),
                        &SymbolType::Fn(
                            args.clone().iter().map(|a| a.1.clone()).collect(),
                            Box::new(rettype.clone()),
                            false,
                        ),
                    );
                }
                //dbg!(t.symbol_table.clone());

                t.symbol_table.new_scope();
                for a in args {
                    t.symbol_table.set(&a.0.clone(), &a.1);
                }
                t.current_func.clone_from(&name);
                let b: Vec<TypedExpr> = body
                    .iter()
                    .map(|e| TypedExpr::convert(e, t, None))
                    .collect();
                let mut nrt = rettype.clone();
                if rettype.is_generic() {
                    nrt = b.last().unwrap().exprtype.clone().unwrap();
                }
                let mut nargs: Vec<(String, SymbolType)> = vec![];

                for i in args.clone() {
                    if i.1.is_generic() {
                        nargs.push((i.0, t.symbol_table.get(i.1.get_generic_name())));
                    } else {
                        nargs.push(i);
                    }
                }
                if parent.is_some() {
                    t.symbol_table.redefine(
                        &n.clone(),
                        &SymbolType::MethodFn {
                            parent: parent.unwrap().clone(),
                            f: Box::new(SymbolType::Fn(
                                args.clone().iter().map(|a| a.1.clone()).collect(),
                                Box::new(rettype.clone()),
                                false,
                            )),
                        },
                    );
                } else {
                    t.symbol_table.redefine(
                        &n,
                        &SymbolType::Fn(
                            nargs.clone().iter().map(|a| a.1.clone()).collect(),
                            Box::new(nrt.clone()),
                            false,
                        ),
                    );
                }
    
                t.symbol_table.pop_scope();

                if !t.compare_ty(rettype, &b.last().unwrap().exprtype.clone().unwrap()) {
                    panic!("Return type was not equal to the last expression in function body; {:?} vs {:?}", rettype, &b.last().unwrap().exprtype.clone().unwrap())
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
                t.symbol_table
                    .set(&name.clone(), &SymbolType::Obj(members.clone()));
                Self::Struct {
                    name: name.to_string(),
                    members: members.to_vec(),
                }
            }

            Ast::Enum { name, members } => {
                let mut gc = 0;
                for m in members.clone() {
                    let mm = m.get_variant_members();
                    for mg in mm {
                        if mg.is_generic() {
                            gc += 1;
                        }
                    }
                }
                t.symbol_table
                    .set(&name.clone(), &SymbolType::Enum(gc, members.clone()));

                for m in members.clone() {
                    let mm = m.get_variant_members();
                    for mg in mm {
                        if mg.is_generic() {
                            t.symbol_table
                                .set(&mg.get_generic_name(), &SymbolType::Unknown)
                        }
                    }
                    t.symbol_table.set(
                        &m.get_variant_name(),
                        &SymbolType::Fn(
                            m.get_variant_members().into(),
                            Box::new(SymbolType::Enum(gc, members.clone())),
                            true,
                        ),
                    );
                }

                Self::Enum {
                    name: name.to_string(),
                    members: members.to_vec(),
                }
            }
            Ast::TypeDef { name, funcs } => {
                let gett = &t.symbol_table.get(name.get_custom_name());
                if t.symbol_table.has(&name.get_custom_name()) && t.compare_ty(gett, name) {
                    let mut nfuncs: ThinVec<TypedAst> = vec![].into();
                    let nn = name.get_custom_name();
                    t.current_parent = Some(name.clone());
                    for f in funcs {
                        //dbg!(f.clone());
                        //t.symbol_table.new_scope();
                        //t.symbol_table.set(&"self".to_string(), name );
                        let res = Self::convert(&f, t, Some(nn.clone()));
                        //t.symbol_table.pop_scope();

                        nfuncs.push(res);
                    }
                    t.current_parent = None;

                    return Self::TypeDef {
                        name: name.clone(),
                        funcs: nfuncs.to_vec(),
                    };
                } else {
                    panic!("Cannot define implementation for undefined type {name:?}")
                }
            }

            Ast::Propdef { p: _ } => {
                todo!()
                // t.symbol_table.set(p.name, &SymbolType::Property);
                // for f in &p.req {
                //     t.symbol_table.set()
                // }
                // return Self::Propdef {p: p.clone()}
            }
            Ast::TypeAlias { name, is } => {
                t.symbol_table.set(name, is);
                Self::TypeAlias {
                    name: name.to_string(),
                    is: is.clone(),
                }
            } // _ => todo!()
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedExpr {
    pub e: Expr,
    pub exprtype: Option<SymbolType>,
}

impl Typecheck<Expr> for TypedExpr {
    fn convert(value: &Expr, t: &mut Typechecker, _parent: Option<String>) -> Self {
        Self {
            e: value.clone(),
            exprtype: Some(t.synth_type(value)),
        }
    }
}
