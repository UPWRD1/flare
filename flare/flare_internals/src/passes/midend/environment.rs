use generational_arena::Arena;
use generational_arena::Index;
use trie_rs::map::Trie;
use trie_rs::map::TrieBuilder;
//use ptrie::Trie;
use core::panic;
use log::info;
use std::cell::OnceCell;

use serde::{Deserialize, Serialize};
use std::{
    fmt::{Debug, Display},
    hash::Hash,
    path::PathBuf,
    rc::Rc,
};

use crate::passes::midend::typechecking::Solver;
//use crate::passes::midend::typechecking::Ty;
use crate::resource::{
    errors::CompResult,
    rep::{Definition, Expr, Program, Spanned, StructDef, Ty},
};

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
            Self::Package(n) => write!(f, "Package {n}"),
            Self::Type(n) => write!(f, "Type {n}"),
            Self::Func(n) => write!(f, "Func {n}"),
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
    #[must_use]
    pub fn append(&self, a: Self) -> Self {
        match self {
            Self::Root(quantifier) => Self::Root(Rc::new(quantifier.append(a))),
            Self::Package(n, quantifier) => {
                Self::Package(n.to_string(), Rc::new(quantifier.append(a)))
            }
            Self::Type(n, quantifier) => Self::Type(n.to_string(), Rc::new(quantifier.append(a))),
            Self::Effect(n, quantifier) => Self::Type(n.to_string(), Rc::new(quantifier.append(a))),
            Self::Func(n, quantifier) => Self::Func(n.to_string(), Rc::new(quantifier.append(a))),
            Self::Variable(_) => todo!(),
            Self::End => a,
        }
    }

    #[must_use]
    pub fn get_func_name(&self) -> Option<&String> {
        match self {
            Quantifier::Root(e) |
            Quantifier::Package(_, e) => e.get_func_name(),
            Quantifier::Func(n, _) => Some(n),
            Quantifier::End => None,
            _ => panic!(),
        }
    }

    #[must_use = "Quantifiers should be consumed for queries or generation"]
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

// impl Display for Quantifier {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Quantifier::Root(quantifier) => {
//                 //f.write_str("Root, ")?;
//                 Display::fmt(quantifier, f)
//             }
//             Quantifier::Package(n, quantifier) => {
//                 f.write_str(&format!("Module {n}, "))?;
//                 Display::fmt(quantifier, f)
//             }
//             Quantifier::Type(n, quantifier) => {
//                 f.write_str(&format!("Type {n}, "))?;
//                 Display::fmt(quantifier, f)
//             }
//             Quantifier::Effect(n, quantifier) => {
//                 f.write_str(&format!("Effect {n}, "))?;
//                 Display::fmt(quantifier, f)
//             }

//             Quantifier::Func(n, quantifier) => {
//                 f.write_str(&format!("Function {n}, "))?;
//                 Display::fmt(quantifier, f)
//             }
//             Quantifier::Variable(n) => {
//                 f.write_str(&format!("Variable {n}, "))?;
//                 Ok(())
//             }
//             Quantifier::End => {
//                 //f.write_str(&format!("End"))?;
//                 Ok(())
//             }
//         }
//     }
// }

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

#[derive(Debug, PartialEq, Eq)]
pub enum Entry {
    Root,
    Filename(String),
    Package {
        name: Spanned<Expr>,
        file: PathBuf,
        deps: Vec<Spanned<Expr>>,
        src: String,
    },
    Struct {
        parent: Quantifier,
        name: Spanned<Expr>,
        ty: Ty,
        fields: Vec<(Spanned<Expr>, Spanned<Ty>)>,
    },
    Let {
        parent: Quantifier,
        name: Spanned<Expr>,
        sig: OnceCell<Ty>,
        body: Spanned<Expr>,
    },
    Extern {
        parent: Quantifier,
        name: Spanned<Expr>,
        sig: Ty,
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
            Entry::Extern { .. } => 3,
            Entry::Root | Entry::Filename(_) => panic!("Root should not be compared!"),
        };
        let right_order = match other {
            Entry::Package { .. } => 0,
            Entry::Struct { .. } => 1,
            Entry::Let { .. } => 2,
            Entry::Extern { .. } => 3,
            Entry::Root | Entry::Filename(_) => panic!("Root should not be compared!"),
        };
        left_order.cmp(&right_order)
    }
}

impl Entry {
    #[must_use]
    pub fn get_sig(&self) -> Option<&Ty> {
        match self {
            Entry::Let { sig, .. } => sig.get(),
            Entry::Extern { sig, .. } => Some(sig),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_parent(&self) -> Option<&Quantifier> {
        match self {
            Entry::Let { parent, .. } |
            Entry::Struct { parent, .. } => Some(parent),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_file(&self) -> Option<&PathBuf> {
        match self {
            Entry::Package { file, .. } => Some(file),
            _ => None,
        }
    }
}

impl Display for Entry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Entry::Package { name, .. } => write!(f, "{}", name.0.get_ident().unwrap()),
            Entry::Struct { name, fields, .. } => write!(
                f,
                "{}: {{{}}}",
                name.0.get_ident().unwrap(),
                fields
                    .iter()
                    .map(|(_n, t)| format!("{}", t.0))
                    .collect::<Vec<_>>()
                    .join(" * ")
            ),
            Entry::Let { name, sig, .. } => {
                if let Some(sig) = sig.get() {
                    write!(f, "{}: {}", name.0.get_ident().unwrap(), sig)
                } else {
                    write!(f, "{}: ?", name.0.get_ident().unwrap())
                }
            }
            Entry::Filename(n) => write!(f, "File {n}"),
            Entry::Root => write!(f, "Root"),
            Entry::Extern { name, sig, .. } => {
                write!(f, "extern {}: {}", name.0.get_ident().unwrap(), sig)
            }
        }
    }
}

#[derive(Debug)]
pub struct Environment {
    //pub items: HashMap<Quantifier, Rc<RefCell<Entry>>>,
    pub items: Trie<SimpleQuant, Index>,
    pub arena: Arena<Entry>,
    //pub current_parent: Quantifier,
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
        let mut env: TrieBuilder<SimpleQuant, Index> = TrieBuilder::new();
        let mut arena = Arena::new();
        for package in p.packages {
            let package_name = package.0.name.0.get_ident().unwrap();
            let current_parent = quantifier!(Root, Package(package_name), End);

            let mut deps = Vec::new();

            for item in package.0.items {
                match item {
                    Definition::Import(import_item) => {
                        build_import(&mut deps, import_item);
                    }
                    Definition::Struct(StructDef { name, fields }) => {
                        let ident = name.0.get_ident().unwrap();
                        let q = current_parent
                            .append(Quantifier::Type(ident, Rc::new(Quantifier::End)))
                            .into_simple();

                        let entry = Entry::Struct {
                            name: name.clone(),
                            parent: current_parent.clone(),
                            fields,
                            ty: Ty::User(name, vec![]),
                        };
                        let idx = arena.insert(entry);

                        env.insert(q, idx);
                    }
                    Definition::Let(name, body, ty) => {
                        let ident = name.0.get_ident().unwrap();
                        let q = current_parent
                            .append(Quantifier::Func(ident, Rc::new(Quantifier::End)))
                            .into_simple();

                        let cell = OnceCell::new();
                        if let Some(ty) = ty.map(|t| t.0) {
                            let _ = cell.set(ty);
                        }
                        let entry = Entry::Let {
                            parent: current_parent.clone(),
                            name,
                            sig: cell,
                            body,
                        };
                        let idx = arena.insert(entry);
                        env.insert(q, idx);
                    }
                    Definition::Extern(n, ty) => {
                        let ident = n.0.get_ident().unwrap();
                        let q = current_parent
                            .append(Quantifier::Func(ident, Rc::new(Quantifier::End)))
                            .into_simple();
                        let entry = Entry::Extern {
                            parent: current_parent.clone(),
                            name: n,
                            sig: ty.0,
                        };
                        let idx = arena.insert(entry);
                        env.insert(q, idx);
                    }
                }
            }

            let entry = Entry::Package {
                name: package.0.name,
                file: package.1,
                deps,
                src: package.2,
            };
            let idx = arena.insert(entry);
            env.insert(current_parent.clone().into_simple(), idx);
        }

        Ok(Self {
            items: env.build(),
            arena,
        })
    }

    pub fn check(&self) -> CompResult<()> {
        let main_idx = self
            .items
            .exact_match(
                quantifier!(Root, Package("Main"), Func("main"), End)
                    .into_simple()
                    .into_iter(),
            )
            .unwrap();
        let main = self.arena.get(*main_idx).unwrap();
        //*self.items.find_postfixes(Quantifier::Func("main".into(), Quantifier::End.into()).into_bits().into_iter()).first().unwrap();
        self.check_entry(main)?;
        //dbg!(&main);
        Ok(())
    }

    pub fn check_entry<'e>(&self, entry: &'e Entry) -> CompResult<&'e Entry> {
        //println!("Checking {:?}", entry);
        //match *entry.borrow_mut() {

        if let Entry::Let {
            ref sig,
            ref body,
            ..
        } = if entry.get_sig().is_none() {
            entry
        } else {
            return Ok(entry);
        } {
            let mut tc = Solver::new(self);
            let tv = tc.check_expr(body)?;
            let fn_sig = tc.solve(tv)?;
            let _ = sig.set(fn_sig);
        }
        info!("Checked {}", entry);
        Ok(entry)
    }
}

fn build_import(deps: &mut Vec<Spanned<Expr>>, import_item: crate::resource::rep::ImportItem) {
    for import in import_item.items {
        match import.0 {
            Expr::Ident(ref _name) => deps.push(import),
            //Expr::FieldAccess(l, r) => deps.push(),
            _ => panic!("Import path must be identifiers"),
        }
    }
}
