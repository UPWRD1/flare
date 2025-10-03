use chumsky::span::SimpleSpan;
use trie_rs::map::Trie;
use trie_rs::map::TrieBuilder;
//use ptrie::Trie;
use core::panic;
use itertools::Itertools;
use std::rc::Weak;

use serde::Deserialize;
use serde::Serialize;
use std::cell::RefCell;
use std::fmt::Display;
use std::hash::Hash;
use std::rc::Rc;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    path::PathBuf,
};

use crate::passes::midend::typechecking::Solver;
//use crate::passes::midend::typechecking::Ty;
use crate::resource::errors::CompResult;
use crate::resource::rep::Definition;
use crate::resource::rep::Expr;
use crate::resource::rep::OptSpanned;
use crate::resource::rep::Program;
use crate::resource::rep::StructDef;
use crate::resource::rep::Ty;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Quantifier {
    Root(Rc<Self>),
    Package(String, Rc<Self>),
    Type(String, Rc<Self>),
    Effect(String, Rc<Self>),
    Func(String, Rc<Self>),
    Variable(String),
    End,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SimpleQuant {
    Root,
    Package(String),
    Type(String),
    Func(String),
}

impl std::fmt::Display for SimpleQuant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Root => write!(f, "Root"),
            Self::Package(n) => write!(f, "Package {}", n),
            Self::Type(n) => write!(f, "Type {}", n),
            Self::Func(n) => write!(f, "Func {}", n),
        }
    }
}

// impl TrieKey for Quantifier {
//     fn encode_bytes(&self) -> Vec<u8> {
//         let v: Vec<u8> = match self {
//             Quantifier::Root(quantifier) => quantifier.encode_bytes(),
//             Quantifier::Package(n, quantifier) | Quantifier::Func(n, quantifier ) => {
//                 let mut name = n.as_bytes().encode_bytes();
//                 name.extend_from_slice(&quantifier.encode_bytes());
//                 name
//             }
//             Quantifier::Type(_, quantifier) => quantifier.encode_bytes(),
//             Quantifier::End => vec![],
//             _ => todo!()
//         };
//         v
//     }
// }

impl Quantifier {
    pub fn append(&self, a: Self) -> Self {
        let res = match self {
            Self::Root(quantifier) => Self::Root(Rc::new(quantifier.append(a))),
            Self::Package(n, quantifier) => {
                Self::Package(n.to_string(), Rc::new(quantifier.append(a)))
            }
            Self::Type(n, quantifier) => Self::Type(n.to_string(), Rc::new(quantifier.append(a))),
            Self::Effect(n, quantifier) => Self::Type(n.to_string(), Rc::new(quantifier.append(a))),
            Self::Func(n, quantifier) => Self::Func(n.to_string(), Rc::new(quantifier.append(a))),
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

    pub fn into_simple(&self) -> Vec<SimpleQuant> {
        let mut res = vec![];
        fn collapse(top: &Quantifier, result: &mut Vec<SimpleQuant>) {
            match top {
                Quantifier::Root(q) => {
                    result.push(SimpleQuant::Root);
                    collapse(q, result);
                }
                Quantifier::Package(n, q) => {
                    result.push(SimpleQuant::Package(n.to_string()));
                    collapse(q, result);
                }
                Quantifier::Type(n, q) => {
                    result.push(SimpleQuant::Type(n.to_string()));
                    collapse(q, result);
                }
                Quantifier::Func(n, q) => {
                    result.push(SimpleQuant::Func(n.to_string()));
                    collapse(q, result);
                }
                Quantifier::End => {}
                _ => todo!(),
            }
        }
        collapse(self, &mut res);
        res.reverse();
        res

        // let mut h = DefaultHasher::new();
        // self.hash(&mut h);
        // h.finish().to_be_bytes().to_vec()
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
        Quantifier::Root(Rc::new(quantifier!($($rest)*)))
    };

    // Module with child
    (Package($name:expr), $($rest:tt)*) => {
        Quantifier::Package($name.to_string(), Rc::new(quantifier!($($rest)*)))
    };

    // Type with child
    (Type($name:expr), $($rest:tt)*) => {
        Quantifier::Type($name.to_string(), Rc::new(quantifier!($($rest)*)))
    };

    (Effect($name:expr), $($rest:tt)*) => {
        Quantifier::Effect($name.to_string(), Rc::new(quantifier!($($rest)*)))
    };

    // Func with child
    (Func($name:expr), $($rest:tt)*) => {
        Quantifier::Func($name.to_string(), Rc::new(quantifier!($($rest)*)))
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
        parent: Quantifier,
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
    //pub items: HashMap<Quantifier, Rc<RefCell<Entry>>>,
    pub items: Trie<SimpleQuant, Rc<RefCell<Entry>>>,
    pub current_parent: Quantifier,
}

impl Environment {
    // pub fn get_q(&mut self, id: &Quantifier) -> Option<Entry> {
    //     self.items.get(id).cloned().map(|e| e.borrow().clone())
    // }

    // pub fn get_mut_q(&mut self, id: &Quantifier) -> Option<&mut Entry> {
    //     self.items.get_mut(id)
    // }

    // pub fn add(&mut self, id: Quantifier, value: Entry) {
    //     self.items
    //         .insert(id.into_simple().into_iter(), RefCell::from(value).into());
    // }

    // pub fn update(&mut self, id: &Quantifier, value: Entry) {
    //     self.items.insert(id.clone(), value.into());
    // }

    pub fn build(p: Program) -> CompResult<Self> {
        let mut env: TrieBuilder<SimpleQuant, Rc<RefCell<Entry>>> = TrieBuilder::new();
        let mut current_parent = Quantifier::End;
        for package in p.packages {
            //println!("Building {:?}", module.get_module_name());
            let the_package_name =
                quantifier!(Root, Package(package.0.name.0.get_ident().unwrap()), End);

            current_parent = the_package_name.clone();

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
                    Definition::Struct(StructDef { name, fields }) => env.insert(
                        current_parent
                            .append(Quantifier::Type(
                                name.0.get_ident().unwrap(),
                                Rc::new(Quantifier::End),
                            ))
                            .into_simple(),
                        RefCell::from(Entry::Struct { name, fields }).into(),
                    ),
                    Definition::Let(name, body) => env.insert(
                        current_parent
                            .append(Quantifier::Func(
                                name.0.get_ident().unwrap(),
                                Rc::new(Quantifier::End),
                            ))
                            .into_simple(),
                        RefCell::from(Entry::Let {
                            parent: the_package_name.clone(),
                            name,
                            sig: None,
                            body: body.clone(),
                        })
                        .into(),
                    ),
                }
            }

            env.insert(
                the_package_name.clone().into_simple(),
                RefCell::from(Entry::Package {
                    name: package.0.name,
                    file: package.1,
                    deps,
                    src: package.2,
                })
                .into(),
            );

            //println!("{:#?}",self);
        }
        let trie = env.build();
        Ok(Self {
            items: trie,
            current_parent,
        })
    }

    pub fn check(&self) -> CompResult<()> {
        let main = self
            .items
            .exact_match(
                quantifier!(Root, Package("Main"), Func("main"), End)
                    .into_simple()
                    .into_iter(),
            )
            .unwrap(); //*self.items.find_postfixes(Quantifier::Func("main".into(), Quantifier::End.into()).into_bits().into_iter()).first().unwrap();
        self.check_entry(main)?;
        Ok(())
    }

    pub fn check_entry<'e>(
        &self,
        entry: &'e Rc<RefCell<Entry>>,
    ) -> CompResult<&'e Rc<RefCell<Entry>>> {
        match *entry.borrow_mut() {
            Entry::Let {
                ref mut sig,
                ref body,
                ref name,
                ref parent,
            } => {
                let mut tc = Solver::new(&self);
                let tv = tc.check_expr(&body).map_err(|e| {
                    let the_parent = self.items.exact_match(parent.into_simple()).unwrap();
                    match *the_parent.borrow() {
                        Entry::Package {
                            ref file, ref src, ..
                        } => e.get_dyn().src(src).filename(
                            file.file_name()
                                .unwrap_or(std::ffi::OsStr::new(""))
                                .to_str()
                                .unwrap(),
                        ),
                        _ => panic!("Should always be a package!"),
                    }
                })?;
                //dbg!(&tv);
                let fn_sig = tc.solve(tv)?;
                //println!("{:?} : {}", name.0, fn_sig);
                //*sig = Some(fn_sig);
                sig.replace(fn_sig);
                //sig.replace(fn_sig);
            }
            _ => (),
        }
        Ok(entry)
    }
}
