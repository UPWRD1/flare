use chumsky::span::SimpleSpan;
use serde::Deserialize;
use serde::Serialize;
use core::panic;
use std::fmt::Display;
use std::hash::Hash;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    path::PathBuf,
};

use crate::root::passes::midend::typechecking::Ty;
use crate::root::resource::rep::Definition;
use crate::root::resource::rep::Expr;
use crate::root::resource::rep::Program;
use crate::root::resource::errors::CompResult;

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
    Let {
        name: (Expr, SimpleSpan),
        sig: Option<Ty>,
        body: (Expr, SimpleSpan),
    },
}

#[derive(Debug)]
pub struct Environment {
    pub items: HashMap<Quantifier, Entry>,
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

    pub fn get_q(&mut self, id: &Quantifier) -> Option<Entry> {
        self.items.get(id).cloned()
    }

    pub fn get_mut_q(&mut self, id: &Quantifier) -> Option<&mut Entry> {
        self.items.get_mut(id)
    }

    pub fn add(&mut self, id: Quantifier, value: Entry) {
        self.items.insert(id, value.into());
    }

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
                                Expr::Ident(ref name) => {deps.push(import)},
                                //Expr::FieldAccess(l, r) => deps.push(),
                                _ => panic!("Import path must be identifiers"),
                            }
                        }
                    },
                    Definition::Struct(struct_def) => todo!(),
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
}
