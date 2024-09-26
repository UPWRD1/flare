use std::collections::HashMap;

use super::symboltable::{SymbolTable, SymbolTableGroup};
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
            current_parent: None,
        }
    }

    pub fn synth_type(&mut self, e: &Expr) -> SymbolType {
        //dbg!(e.clone());
        let l = match e {
            Expr::Int(_) => SymbolType::Int,
            Expr::Flt(_) => SymbolType::Flt,
            Expr::Str(_) => SymbolType::Str,
            Expr::Bool(_) => SymbolType::Bool,
            Expr::Symbol(t) => {
                if t == "self" {
                    self.current_parent.clone().unwrap()
                } else {
                    self.symbol_table.get(t.clone())
                }
            }
            Expr::BinAdd { l, r }
            | Expr::BinSub { l, r }
            | Expr::BinMul { l, r }
            | Expr::BinDiv { l, r } => self.synth_binop(l, r),
            Expr::Logical { l, op: _, r } => self.synth_logical(l, r),
            Expr::Assignment { name, value } => self.synth_assignment(value, name),
            Expr::MutableAssignment { name, value } => self.synth_mut_assignment(value, name),
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
                let callee = self.synth_type(&name);
                //dbg!(name.clone());
                if callee.is_fn() {
                    //dbg!(name.clone());
                    let cargs: Vec<SymbolType> = callee.get_args().to_vec();
                    if cargs.len() == args.len() {
                        self.symbol_table.new_scope();
                        for (i, e) in cargs.into_iter().enumerate() {
                            let arg = self.synth_type(&args[i]);
                            if self.symbol_table.compare_ty(&e, &arg) {
                                continue;
                            } else {
                                panic!("invalid type on argument, expected {e:?}, found {arg:?}")
                            }
                        }

                        let ofin = self.symbol_table.get(name.get_symbol_name()).get_rt();
                        let fin = self.symbol_table.handle_custom(ofin);
                        self.symbol_table.pop_scope();

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
                    //dbg!(self.clone());
                    panic!("{name:?} is not a function, is {:?}", self.synth_type(&name))
                }
            }

            Expr::Return { value } => {
                let vt = self.synth_type(&*value);
                let fnt = self.symbol_table.get(self.current_func.clone()).get_rt();
                if self.symbol_table.compare_ty(&fnt, &vt) {
                    vt
                } else {
                    panic!("Return types don't match! Expected: {fnt:?}; found: {vt:?} with expr {:?} in function {:?}", value, self.current_func)
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

                    if self.symbol_table.compare_ty(&ot, &tt) {
                        tt
                    } else {
                        panic!("If branches have differing types! {:?} vs {:?}", tt, ot)
                    }
                } else {
                    panic!("Expression {:?} is not boolean", condition);
                }
            }
            Expr::StructInstance { name, fields } => {
                let typedfields: Vec<(String, SymbolType)> = fields
                    .iter()
                    .map(|e| {
                        let x = e.get_assignment();
                        (x.0, self.synth_type(&x.1))
                    })
                    .collect();
                let structuretype = self.symbol_table.get(name.get_symbol_name());
                let mut generic_vec: Vec<SymbolType> = vec![];
                if structuretype.is_obj() {
                    let members = structuretype.get_members();
                    if members.len() == typedfields.len() {
                        for member in members.iter().enumerate() {
                            let field = &typedfields[member.0];
                            if !self.symbol_table.compare_ty( &field.1, &member.1.1) {
                                panic!(
                                    "{name:?} is not instantiated correctly! {:?} vs {:?}",
                                    member.1 .1, field.1
                                )
                            } else if field.1.is_generic() {
                                generic_vec.push(field.1.clone())
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
                let n = r.get_callee();
                assert!(self.symbol_table.get(n.clone()).is_fn());

                let mut a = r.get_call_args();
                let mut finargs = vec![*l.clone()];
                finargs.append(&mut a);
                self.synth_type(&Expr::Call {
                    name: Box::new(Expr::Symbol(n)),
                    args: finargs,
                })
            }
            Expr::FieldAccess(o, f) => {
                let mut a: SymbolType = self.synth_type(o);
                if matches!(a, SymbolType::TypeDefSelf) && self.current_parent.is_some() {
                    a = self.current_parent.clone().unwrap();
                }
                let ot = self.symbol_table.get(a.get_custom_name());
                //dbg!(ot.clone());
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
            Expr::MethodCall { obj, name, args } => {
                let msig = self.symbol_table.get_module(obj.get_symbol_name(), name.get_symbol_name());
                let mut nargs = args.clone();
                nargs.insert(0, *obj.clone());
                let callargs: Vec<SymbolType> = nargs.iter().map(|e| self.synth_type(e)).collect();
                
                assert!(callargs.len() == msig.args.len(), "call to {:?} on {:?} expects {} arguments, found {}", name, obj, msig.args.len(), callargs.len());
                for arg in msig.args.iter().enumerate() {
                    let carg = &callargs[arg.0];
                    //dbg!(arg.1.clone());
                    //dbg!(carg.clone());
                    if !self.symbol_table.compare_ty(&arg.1, &carg) {
                        panic!("invalid type on argument, expected {arg:?}, found {carg:?}")
                    }
                }
                return msig.rettype
            }
            Expr::ModuleCall { module, name, args } => {
                let msig = self.symbol_table.get_module(module.get_symbol_name(), name.get_symbol_name());
                let callargs: Vec<SymbolType> = args.iter().map(|e| self.synth_type(e)).collect();
                
                assert!(callargs.len() == msig.args.len(), "call to {:?} on {:?} expects {} arguments, found {}", name, module, msig.args.len(), callargs.len());
                return msig.rettype
            }
            _ => panic!("unknown expr: {:?}", e),
        };
        //self.symbol_table.handle_custom(l)
        l
    }

    fn synth_mut_assignment(&mut self, value: &Box<Expr>, name: &Box<Expr>) -> SymbolType {
        //let lt = self.synth_type(*name.clone());
        let rt = SymbolType::Mut(Box::new(self.synth_type(&*value)));
        self.symbol_table.set(&name.get_symbol_name(), &rt.clone());
        rt
    }

    fn synth_assignment(&mut self, value: &Box<Expr>, name: &Box<Expr>) -> SymbolType {
        //let lt = self.synth_type(*name.clone());
        let rt = self.synth_type(&*value);
        self.symbol_table.set(&name.get_symbol_name(), &rt);
        rt
    }

    fn synth_logical(&mut self, l: &Box<Expr>, r: &Box<Expr>) -> SymbolType {
        let lt = self.synth_type(&*l).extract();
        let rt = self.synth_type(&*r).extract();
        if self.symbol_table.compare_ty(&lt, &rt) {
            SymbolType::Bool
        } else {
            panic!("Cannot compare {lt:?} {l:?} with {rt:?} {r:?}")
        }
    }

    fn synth_binop(&mut self, l: &Box<Expr>, r: &Box<Expr>) -> SymbolType {
        let lt = self.synth_type(&*l);
        let rt = self.synth_type(&*r);
        //dbg!(lt.clone());
        //dbg!(rt.clone());
        if self.symbol_table.compare_ty(&lt, &rt) {
            lt
        } else {
            panic!("Cannot operate {lt:?} {l:?} with {rt:?} {r:?}")
        }
    }
}

pub trait Typecheck<T> {
    fn convert(value: &T, t: &mut Typechecker) -> Self;
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
            let tm = TypedModule::convert(&m, &mut t);
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
    fn convert(value: &Module, t: &mut Typechecker) -> Self {
        let mut b: Vec<TypedAst> = vec![];
        for a in &value.body {
            let ta = TypedAst::convert(&a, t);
            //dbg!(t.clone());
            b.push(ta)
        }
        Self { body: b }
    }
}
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TypedAst {
    FnDef {
        name: String,
        rettype: SymbolType,
        args: Vec<(String, SymbolType)>,
        limits: Option<Vec<FnArgLimit>>,
        body: Vec<TypedExpr>,
    },
    MethodDef {
        parent: String,
        name: String,
        rettype: SymbolType,
        args: ThinVec<(String, SymbolType)>,
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
                let n: String = name.clone();
                t.symbol_table.set(
                    &n.clone(),
                    &SymbolType::Fn(
                        args.clone().iter().map(|a| a.1.clone()).collect(),
                        Box::new(rettype.clone()),
                        false,
                    ),
                );
                t.symbol_table.new_scope();
                for a in args {
                    t.symbol_table.set(&a.0.clone(), &a.1);
                }
                t.current_func.clone_from(&name);
                let b: Vec<TypedExpr> = body
                    .iter()
                    .map(|e| TypedExpr::convert(e, t))
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

                t.symbol_table.redefine(
                    &n,
                    &SymbolType::Fn(
                        nargs.clone().iter().map(|a| a.1.clone()).collect(),
                        Box::new(nrt.clone()),
                        false,
                    ),
                );

                t.symbol_table.pop_scope();

                if !t
                    .symbol_table
                    .compare_ty(rettype, &b.last().unwrap().exprtype.clone().unwrap())
                {
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
                include: include.iter().map(|e| e.get_lit()).collect(),
            },
            Ast::Struct { name, members } => {
                t.symbol_table
                    .set(&name.clone(), &SymbolType::Obj(name.to_string(), members.clone()));
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
                    .set(&name.clone(), &SymbolType::Enum(name.to_string(), gc, members.clone()));

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
                            Box::new(SymbolType::Enum(m.get_variant_name(), gc, members.clone())),
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
                if t.symbol_table.has(&name.get_custom_name())
                    && t.symbol_table.compare_ty(gett, name)
                {
                    let nn = name.get_custom_name();
                    let mut nfuncs: Vec<TypedAst> = vec![];
                    t.current_parent = Some(name.clone());
                    let mut new_group = SymbolTableGroup {
                        name: nn.clone(),
                        children: HashMap::new(),
                    };
                    for f in funcs {
                        //dbg!(f.clone());
                        let res = Self::convert(&f, t);
                        new_group
                            .children
                            .insert(f.get_fnname(), res.clone().into());
                        nfuncs.push(res)
                    }
                    t.current_parent = None;
                    t.symbol_table.groups.push(new_group);
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
            }
            Ast::MethodDef { parent, name, rettype, args, limits, body } => {
                
                t.symbol_table.new_scope();
                let n: String = name.clone();
                t.symbol_table.set(
                    &n.clone(),
                    &SymbolType::MethodFn {parent: parent.to_string(),f: 
                    Box::new(SymbolType::Fn(
                        args.clone().iter().map(|a| a.1.clone()).collect(),
                        Box::new(rettype.clone()),
                        false,
                    ))},
                );
                for a in args {
                    t.symbol_table.set(&a.0.clone(), &a.1);
                }
                t.current_func.clone_from(&name);
                let b: Vec<TypedExpr> = body
                    .iter()
                    .map(|e| TypedExpr::convert(e, t))
                    .collect();
                let mut nrt = rettype.clone();
                if rettype.is_generic() {
                    nrt = b.last().unwrap().exprtype.clone().unwrap();
                }
                //let mut nargs: Vec<(String, SymbolType)> = vec![];

                // for i in args.clone() {
                //     if i.1.is_generic() {
                //         nargs.push((i.0, t.symbol_table.get(i.1.get_generic_name())));
                //     } else {
                //         nargs.push(i);
                //     }
                // }
                t.symbol_table.pop_scope();
                return Self::MethodDef {
                    parent: t.current_parent.clone().unwrap().clone().get_custom_name(),
                    name: name.to_string(),
                    rettype: nrt,
                    args: args.clone(),
                    limits: limits.clone(),
                    body: b,
                }
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TypedExpr {
    pub e: Expr,
    pub exprtype: Option<SymbolType>,
}

impl Typecheck<Expr> for TypedExpr {
    fn convert(value: &Expr, t: &mut Typechecker) -> Self {
        Self {
            e: value.clone(),
            exprtype: Some(t.synth_type(value)),
        }
    }
}
