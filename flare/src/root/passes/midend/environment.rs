use chumsky::span::SimpleSpan;
use core::panic;
use std::cell::RefCell;
use std::rc::Rc;
use itertools::Itertools;
use serde::Deserialize;
use serde::Serialize;
use std::fmt::Display;
use std::hash::Hash;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    path::PathBuf,
};

use crate::root::passes::midend::typechecking::Solver;
//use crate::root::passes::midend::typechecking::Ty;
use crate::root::resource::errors::CompResult;
use crate::root::resource::rep::Definition;
use crate::root::resource::rep::Expr;
use crate::root::resource::rep::OptSpanned;
use crate::root::resource::rep::Program;
use crate::root::resource::rep::StructDef;
use crate::root::resource::rep::Ty;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Quantifier {
    Root(Box<Self>),
    Package(String, Box<Self>),
    Type(String, Box<Self>),
    Effect(String, Box<Self>),
    Func(String, Box<Self>),
    Variable(String),
    End,
}

impl Quantifier {
    pub fn append(&self, a: Self) -> Self {
        let res = match self {
            Self::Root(quantifier) => Self::Root(Box::new(quantifier.append(a))),
            Self::Package(n, quantifier) => {
                Self::Package(n.to_string(), Box::new(quantifier.append(a)))
            }
            Self::Type(n, quantifier) => Self::Type(n.to_string(), Box::new(quantifier.append(a))),
            Self::Effect(n, quantifier) => {
                Self::Type(n.to_string(), Box::new(quantifier.append(a)))
            }
            Self::Func(n, quantifier) => Self::Func(n.to_string(), Box::new(quantifier.append(a))),
            Self::Variable(_) => todo!(),
            Self::End => a,
        };
        return res;
    }

    pub fn get_func_name(&self) -> Option<&String> {
        match self {
            Quantifier::Root(e) => e.get_func_name(),
            Quantifier::Package(_, e) => e.get_func_name(),
            Quantifier::Func(n, _) => Some(n),
            Quantifier::End => None,
            _ => panic!(),
        }
    }
}

impl Display for Quantifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Quantifier::Root(quantifier) => {
                //f.write_str("Root, ")?;
                Display::fmt(quantifier, f)
            }
            Quantifier::Package(n, quantifier) => {
                f.write_str(&format!("Module {n}, "))?;
                Display::fmt(quantifier, f)
            }
            Quantifier::Type(n, quantifier) => {
                f.write_str(&format!("Type {n}, "))?;
                Display::fmt(quantifier, f)
            }
            Quantifier::Effect(n, quantifier) => {
                f.write_str(&format!("Effect {n}, "))?;
                Display::fmt(quantifier, f)
            }

            Quantifier::Func(n, quantifier) => {
                f.write_str(&format!("Function {n}, "))?;
                Display::fmt(quantifier, f)
            }
            Quantifier::Variable(n) => {
                f.write_str(&format!("Variable {n}, "))?;
                Ok(())
            }
            Quantifier::End => {
                //f.write_str(&format!("End"))?;
                Ok(())
            }
        }
    }
}

#[macro_export]
macro_rules! quantifier {
    // Base case: just End
    (End) => {
        Quantifier::End
    };

    // Variable case (no children)
    (Variable($name:expr)) => {
        Quantifier::Variable($name)
    };

    // Root with child
    (Root, $($rest:tt)*) => {
        Quantifier::Root(Box::new(quantifier!($($rest)*)))
    };

    // Module with child
    (Package($name:expr), $($rest:tt)*) => {
        Quantifier::Package($name.to_string(), Box::new(quantifier!($($rest)*)))
    };

    // Type with child
    (Type($name:expr), $($rest:tt)*) => {
        Quantifier::Type($name.to_string(), Box::new(quantifier!($($rest)*)))
    };

    (Effect($name:expr), $($rest:tt)*) => {
        Quantifier::Effect($name.to_string(), Box::new(quantifier!($($rest)*)))
    };

    // Func with child
    (Func($name:expr), $($rest:tt)*) => {
        Quantifier::Func($name.to_string(), Box::new(quantifier!($($rest)*)))
    };
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Entry {
    Package {
        name: (Expr, SimpleSpan),
        file: PathBuf,
        deps: Vec<(Expr, SimpleSpan)>,
        src: String,
    },
    Struct {
        name: (Expr, SimpleSpan),
        fields: Vec<((Expr, SimpleSpan), OptSpanned<Ty>)>,
    },
    Let {
        name: (Expr, SimpleSpan),
        sig: Option<Ty>,
        body: (Expr, SimpleSpan),
    },
}

impl PartialOrd for Entry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Entry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let left_order = match self {
            Entry::Package { .. } => 0,
            Entry::Struct { .. } => 1,
            Entry::Let { .. } => 2,
        };
        let right_order = match other {
            Entry::Package { .. } => 0,
            Entry::Struct { .. } => 1,
            Entry::Let { .. } => 2,
        };
        left_order.cmp(&right_order)
    }
}

impl Entry {
    pub fn get_sig(&self) -> Option<&Ty> {
        match self {
            Entry::Let { sig, .. } => sig.as_ref(),
            _ => None,
        }
    }
}

// impl Ord for Entry {
//     fn cmp(&self, other: &Self) -> std::cmp::Ordering {
//         std::mem::discriminant(self).cmp(&std::mem::discriminant(other))
//     }
// }

#[derive(Debug, Clone)]
pub struct Environment {
    pub items: HashMap<Quantifier, Rc<RefCell<Entry>>>,
    pub current_parent: Quantifier,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        let mut me = Self {
            items: HashMap::new(),
            current_parent: quantifier!(Root, End),
        };
        // me.items.insert(
        //     quantifier!(Root, Effect("$ROOT"), End),
        //     Entry::Effect(EffectEntry {
        //         name: "$ROOT".to_string(),
        //         deps: EffectDeps::Root,
        //     }),
        // );

        // me.items.insert(
        //     quantifier!(Root, Effect("IO"), End),
        //     Entry::Effect(EffectEntry {
        //         name: "IO".to_string(),
        //         deps: EffectDeps::Product(vec!["$ROOT".to_string()]),
        //     }),
        // );
        me
    }

    // pub fn get_q(&mut self, id: &Quantifier) -> Option<Entry> {
    //     self.items.get(id).cloned().map(|e| e.borrow().clone())
    // }

    // pub fn get_mut_q(&mut self, id: &Quantifier) -> Option<&mut Entry> {
    //     self.items.get_mut(id)
    // }

    pub fn add(&mut self, id: Quantifier, value: Entry) {
        self.items.insert(id, RefCell::from(value).into());
    }

    // pub fn update(&mut self, id: &Quantifier, value: Entry) {
    //     self.items.insert(id.clone(), value.into());
    // }

    pub fn build(&mut self, p: Program) -> CompResult<()> {
        for package in p.packages {
            //println!("Building {:?}", module.get_module_name());
            let the_package_name =
                quantifier!(Root, Package(package.0.name.0.get_ident().unwrap()), End);

            self.current_parent = the_package_name.clone();

            let mut deps = vec![];

            for item in package.0.items {
                match item {
                    Definition::Import(import_item) => {
                        for import in import_item.items {
                            match import.0 {
                                Expr::Ident(ref name) => deps.push(import),
                                //Expr::FieldAccess(l, r) => deps.push(),
                                _ => panic!("Import path must be identifiers"),
                            }
                        }
                    }
                    Definition::Struct(StructDef { name, fields }) => self.add(
                        self.current_parent.append(Quantifier::Type(
                            name.0.get_ident().unwrap(),
                            Box::new(Quantifier::End),
                        )),
                        Entry::Struct { name, fields },
                    ),
                    Definition::Let(name, body) => self.add(
                        self.current_parent.append(Quantifier::Func(
                            name.0.get_ident().unwrap(),
                            Box::new(Quantifier::End),
                        )),
                        Entry::Let {
                            name,
                            sig: None,
                            body: body.clone(),
                        },
                    ),
                }
            }

            self.add(
                the_package_name.clone(),
                Entry::Package {
                    name: package.0.name,
                    file: package.1,
                    deps,
                    src: package.2,
                },
            );

            //println!("{:#?}",self);
        }
        Ok(())
    }

    pub fn check(&mut self) -> CompResult<()> {
        
        for (name, entry) in self.items.iter().sorted()/* .sorted_by_key(|(q,_)| q. ) */{
            println!("{:?} => {:?}", name, entry);
            match *entry.borrow_mut() {
                Entry::Let {
                    ref mut sig, ref body, ..
                } => {
                    let mut tc = Solver::new(&self.items);
                    let tv = tc.check_expr(&body)?;
                    //dbg!(&tv);
                    let fn_sig = tc.solve(tv)?;
                    *sig = Some(fn_sig);
                    //sig.replace(fn_sig);
                }
                _ => (),
            }
        }
        dbg!(&self.items);
        todo!()
    }
}
