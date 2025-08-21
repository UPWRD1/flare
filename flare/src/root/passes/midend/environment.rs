use anyhow::anyhow;
use anyhow::bail;
use serde::Deserialize;
use serde::Serialize;
use std::fmt::Display;
use std::hash::Hash;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    path::PathBuf,
};

use crate::root::resource::errors::TypecheckingError;
use crate::root::resource::{
    cst::{Cst, Expr, Program, SymbolType},
};

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
            Self::Package(n, quantifier) => Self::Package(n.to_string(), Box::new(quantifier.append(a))),
            Self::Type(n, quantifier) => Self::Type(n.to_string(), Box::new(quantifier.append(a))),
            Self::Effect(n, quantifier) => Self::Type(n.to_string(), Box::new(quantifier.append(a))),
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
                f.write_str("Root, ")?;
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
                f.write_str(&format!("Func {n}, "))?;
                Display::fmt(quantifier, f)
            }
            Quantifier::Variable(n) => {
                f.write_str(&format!("Variable {n}, "))?;
                Ok(())
            }
            Quantifier::End => {
                f.write_str(&format!("End"))?;
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
    (Module($name:expr), $($rest:tt)*) => {
        Quantifier::Module($name.to_string(), Box::new(quantifier!($($rest)*)))
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

#[derive(Debug, Clone)]
pub struct ModuleTableEntry {
    pub imports_from: HashSet<u64>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionTableEntry {
    pub name: Expr,
    pub method_parent: Option<Quantifier>,
    pub arity: usize,
    pub args: Vec<(Expr, SymbolType)>,
    pub limits: Vec<Expr>,
    pub effect: Option<EffectEntry>,
    pub return_type: SymbolType,
    pub body: Expr,
    pub is_checked: bool,
    pub is_extern: bool,
    pub variadic: bool,
    // pub variables: HashMap<String, VariableTableEntry>,
    //pub generic_monomorphs: HashSet<Vec<GenericValue>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct VariableTableEntry {
    pub mytype: SymbolType,
    pub myvalue: Expr,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub enum GenericValue {
    Ref(String),
    Perfect(String, Box<SymbolType>),
}

impl GenericValue {
    pub fn get_name(&self) -> String {
        match self {
            GenericValue::Ref(n) => n.clone(),
            GenericValue::Perfect(n, _) => n.clone(),
        }
    }

    pub fn get_ty(&self) -> SymbolType {
        match self {
            GenericValue::Ref(_) => panic!(),
            GenericValue::Perfect(_, t) => *t.clone(),
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct GenericTableEntry {
    pub to_gen: Vec<SymbolType>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UserTypeTableEntry {
    pub name: String,
    pub isdefined: bool,
    //pub generics: Vec<String>,
    pub kind: UserTypeKind,
    pub is_checked: bool,
    //pub raw: SymbolType,
    //pub generic_monomorphs: HashSet<Vec<GenericValue>>,
    //pub methods: HashSet<Quantifier>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub enum UserTypeKind {
    Struct { fields: Vec<(String, SymbolType)> },
    Enum { variants: Vec<(String, Vec<SymbolType>)> },
    VariantInstance {parent: Quantifier, ident: String},
}

impl UserTypeKind {
    pub fn get_fields(&self) -> anyhow::Result<Vec<(String, SymbolType)>> {
        match self {
            UserTypeKind::Struct { fields } => Ok(fields.clone()),
            _ => panic!("{self:?}"),
        }
    }

    pub fn get_variants(&self) -> anyhow::Result<Vec<(String, Vec<SymbolType>)>> {
        match self {
            UserTypeKind::Enum { variants } => Ok(variants.clone()),
            _ => panic!(),
        }
    }


    pub fn collapse_generics(&self, generics: &Vec<GenericValue>) -> Self {
        match self {
            UserTypeKind::Struct { fields } => {
                let generic_names: Vec<(String, SymbolType)> = generics
                    .iter()
                    .map(|x| (x.get_name(), x.get_ty()))
                    .collect();
                let map: HashMap<String, SymbolType> = generic_names.iter().cloned().collect();
                let mut new_fields: Vec<(String, SymbolType)> = vec![];
                for field in fields {
                    if field.1.is_generic() {
                        new_fields.push(
                            (
                                field.0.clone(),
                                map.get(&field.1.get_generic_name()).unwrap().clone(),
                            )
                                .clone(),
                        );
                    }
                }
                Self::Struct { fields: new_fields }
            }
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct FileTableEntry {
    name: String,
    path: PathBuf,
    src: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EffectDeps {
    Root,
    Product(Vec<String>),
    Unsolved,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectEntry {
    pub name: String,
    pub deps: EffectDeps,
}

#[derive(Debug, Clone)]
pub enum Entry {
    Module,
    Func(FunctionTableEntry),
    Type(UserTypeTableEntry),
    Variable(VariableTableEntry),
    Effect(EffectEntry),
}

impl From<FunctionTableEntry> for Entry {
    fn from(value: FunctionTableEntry) -> Self {
        Entry::Func(value)
    }
}

impl From<UserTypeTableEntry> for Entry {
    fn from(value: UserTypeTableEntry) -> Self {
        Entry::Type(value)
    }
}

impl From<Entry> for FunctionTableEntry {
    fn from(value: Entry) -> Self {
        match value {
            Entry::Func(f) => f,
            _ => panic!(),
        }
    }
}

impl From<VariableTableEntry> for Entry {
    fn from(value: VariableTableEntry) -> Self {
        Entry::Variable(value)
    }
}

impl From<Entry> for VariableTableEntry {
    fn from(value: Entry) -> Self {
        match value {
            Entry::Variable(v) => v,
            _ => panic!(),
        }
    }
}

impl Entry {
    pub fn to_func(&self) -> FunctionTableEntry {
        if let Self::Func(f) = self {
            f.clone()
        } else {
            panic!("{self:?}")
        }
    }

    pub fn to_mut_func(&mut self) -> &mut FunctionTableEntry {
        match self {
            Self::Func(ref mut f) => f,
            _ => panic!("{self:?}")
            ,
        }
    }

    pub fn to_ty(&self) -> UserTypeTableEntry {
        if let Self::Type(t) = self {
            t.clone()
        } else {
            panic!("{:?}", self)
        }
    }

    pub fn to_mut_ty(&mut self) -> &mut UserTypeTableEntry {
        match self {
            Self::Type(ref mut t) => t,
            _ => panic!(),
        }
    }

    pub fn to_effect(&self) -> EffectEntry {
        if let Self::Effect(e) = self {
            e.clone()
        } else {
            panic!()
        }
    }

    pub fn to_mut_effect(&mut self) -> &mut EffectEntry {
        match self {
            Self::Effect(ref mut e) => e,
            _ => panic!(),
        }
    }

    pub fn to_variable(&self) -> VariableTableEntry {
        if let Self::Variable(v) = self {
            v.clone()
        } else {
            panic!()
        }
    }

    pub fn to_mut_variable(&mut self) -> &mut VariableTableEntry {
        match self {
            Self::Variable(ref mut v) => v,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub items: HashMap<Quantifier, Entry>,
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
        };
        me.items.insert(
            quantifier!(Root, Effect("$ROOT"), End),
            Entry::Effect(EffectEntry {
                name: "$ROOT".to_string(),
                deps: EffectDeps::Root,
            }),
        );

        me.items.insert(
            quantifier!(Root, Effect("IO"), End),
            Entry::Effect(EffectEntry {
                name: "IO".to_string(),
                deps: EffectDeps::Product(vec!["$ROOT".to_string()]),
            }),
        );
        me
    }

    pub fn add_func(
        &mut self,
        f: FunctionTableEntry,
        current_module: &Quantifier,
    ) -> anyhow::Result<Quantifier> {
        let n = Box::leak(Box::new(f.name.clone()));
        let q = current_module.append(quantifier!(Func(n), End));
        if let Some(e) = self.items.insert(q.clone(), Entry::Func(f)) {
            Err(anyhow!("Cannot overwrite {e:?}"))
        } else {
            Ok(q)
        }
    }

    pub fn get_q(&mut self, id: &Quantifier) -> Option<Entry> {
        self.items.get(id).cloned()
    }

    pub fn get_mut_q(&mut self, id: &Quantifier) -> Option<&mut Entry> {
        self.items.get_mut(id)
    }

    pub fn add(&mut self, id: Quantifier, value: impl Into<Entry>) {
        self.items.insert(id, value.into());
    }

    pub fn build(&mut self, p: Program) -> anyhow::Result<()> {
        let rev_modules: Vec<&Cst> = p.modules.iter().rev().collect();
        for module in rev_modules {
            //println!("Building {:?}", module.get_module_name());
            self.build_ast(
                &module,
                &quantifier!(Root, /*Module(module.get_module_name()),*/ End),
                false,
            )?;
        }
        Ok(())
        //println!("{:#?}",self);
    }

    fn build_ast(
        &mut self,
        astnode: &Cst,
        current_module: &Quantifier,
        is_in_defblock: bool,
    ) -> anyhow::Result<()> {
        //dbg!(&current_module);
        match astnode.clone() {
            Cst::Module { name, body } => {
                for cst in body {
                    self.build_ast(&cst, current_module /*&current_module.append(quantifier!(Module(name.clone()), End))*/, false)?
                }
                Ok(())
            }
            Cst::FnDef {
                name,
                rettype,
                args,
                limits,
                effect,
                body,
            } => self.build_funcdef(
                name,
                rettype,
                args,
                limits.unwrap_or(vec![]),
                effect,
                body,
                current_module,
                is_in_defblock,
            ),

            Cst::Struct { name, members } => self.build_structdef(name, members, current_module),
            Cst::Enum { name, members } => self.build_enumdef(name, members, current_module),
            Cst::DefBlock { name, funcs } => self.build_defblock(name, funcs, current_module),
            Cst::WithClause { include: _ } => Ok(()),
            Cst::ExternClause {
                name,
                args,
                variadic,
                ret,
                effect,
            } => self.build_externdef(name, ret, args, variadic, effect),
            _ => todo!("{astnode:?}"),
        }
    }

    fn build_funcdef(
        &mut self,
        name: Expr,
        rettype: SymbolType,
        args: Vec<(Expr, SymbolType)>,
        limits: Vec<Expr>,
        effect: Option<Expr>,
        body: Expr,
        current_module: &Quantifier,
        is_in_defblock: bool,
    ) -> anyhow::Result<()> {
        let rettype: SymbolType = match rettype {
            SymbolType::Selff => todo!(),
            SymbolType::Unknown => panic!(),
            SymbolType::Generic(generic_value) => todo!(),
            SymbolType::Custom(name, symbol_types) => {
                let query= self.get_q(&quantifier!(Root, Type(name), End));
                if query.is_some() {
                    SymbolType::Quant(quantifier!(Root, Type(name), End))
                } else {
                    bail!("Undefined Type {}", name)
                }
            },
            SymbolType::Property => panic!(),
            SymbolType::TypeDefSelf => todo!(),
            _ => rettype
        };

        let entry = FunctionTableEntry {
            name,
            method_parent: if is_in_defblock {
                Some(current_module.clone())
            } else {
                None
            },
            arity: args.len(),
            args,
            limits,
            effect: effect.clone().and_then(|x| {
                Some(EffectEntry {
                    name: x.get_symbol_name().expect("Expected a symbol"),
                    deps: EffectDeps::Unsolved,
                })
            }),
            return_type: rettype,
            body,
            is_checked: false,
            is_extern: false,
            variadic: false,
            // variables: HashMap::new(),
            //generic_monomorphs: HashSet::new(),
        };
        dbg!(entry.clone());
        //println!("Adding function '{}()' (id# {})", name, id);
        self.add_func(entry, current_module)?;
        Ok(())
    }

    fn build_externdef(
        &mut self,
        name: Expr,
        rettype: SymbolType,
        args: Vec<SymbolType>,
        variadic: bool,
        effect: Option<Expr>,
    ) -> anyhow::Result<()> {
        let mut mangled_args = vec![];
        for arg in args.iter().enumerate() {
            mangled_args.push((Expr::Symbol(format!("{}_{}", name, arg.0)), arg.1.clone()))
        }
        let entry = FunctionTableEntry {
            name: name.clone(),
            method_parent: None,
            arity: args.len(),
            args: mangled_args,
            limits: vec![],
            effect: effect.clone().and_then(|x| {
                Some(EffectEntry {
                    name: x.get_symbol_name().expect("Expected a symbol"),
                    deps: EffectDeps::Unsolved,
                })
            }),
            return_type: rettype,
            body: Expr::Naught,
            is_checked: true, // unsafe?
            is_extern: true,
            variadic,
            // variables: HashMap::new(),

            //generic_monomorphs: HashSet::new(),
        };

        //println!("Adding function '{}()' (id# {})", name, id);

        self.items.insert(
            quantifier!(Root, Func(name.clone()), End),
            Entry::Func(entry),
        );
        Ok(())
    }

    // fn build_methoddef(
    //     &mut self,
    //     parent: Quantifier,
    //     name: String,
    //     rettype: SymbolType,
    //     args: Vec<(String, SymbolType)>,
    //     limits: Vec<Expr>,
    //     body: Vec<Expr>,
    // ) -> anyhow::Result<()> {
    //     let mut nargs: Vec<(String, SymbolType)> = vec![];
    //     nargs.push(("self".to_string(), self.items.get(&parent).unwrap().to_ty().raw.clone()));
    //     nargs.append(&mut args.clone());
    //     //println!("Adding method '{}()' to {}", name, parent);

    //             //generic_monomorphs: HashSet::new(),

    //         // the type has no methods defined yet
    //         let the_entry = FunctionTableEntry {
    //             name: name.clone(),
    //             method_parent: Some(parent.clone()),
    //             arity: args.len(),
    //             args: nargs,
    //             limits,
    //             return_type: rettype,
    //             body,
    //             is_checked: false,
    //             is_extern: false,
    //             variadic: false,
    //             variables: HashMap::new(),

    //             //generic_monomorphs: HashSet::new(),
    //         };
    //         self.items
    //             .insert(
    //                 // using raw access to ensure the method is applied
    //                 parent,
    //                 the_entry.into()
    //             );

    //     Ok(())
    // }

    fn build_defblock(
        &mut self,
        parent: SymbolType,
        funcs: Vec<Cst>,
        current_module: &Quantifier,
    ) -> anyhow::Result<()> {
        let new_current = current_module.append(quantifier!(Type(parent.get_custom_name()), End));
        for node in funcs {
            self.build_ast(&node, &new_current, true)?;
        }
        Ok(())
    }

    fn build_structdef(
        &mut self,
        name: String,
        members: Vec<(String, SymbolType)>,
        current_module: &Quantifier,
    ) -> anyhow::Result<()> {
        let quant_name = current_module.append(quantifier!(Type(name), End));

        let entry = UserTypeTableEntry {
            name: name.clone(),
            isdefined: true,
            //generics: members
            //    .iter()
            //    .filter(|e| e.1.is_generic())
            //    .map(|e| e.0.clone())
            //    .collect(),
            kind: UserTypeKind::Struct {
                fields: members.clone(),
            },
            is_checked: false,
            //raw: SymbolType::Quant(quant_name.clone()),
            //generic_monomorphs: HashSet::new(),
            //methods: HashSet::new(),
        };
        if self.items.get(&quant_name).is_none() {
            //println!("Adding struct '{}' (id# {})", name, id);
            self.items.insert(quant_name, entry.into());
        } else {
            return Err(TypecheckingError::RedefinedType { name: name.clone() })?;
        }
        Ok(())
    }

    fn build_enumdef(
        &mut self,
        name: String,
        members: Vec<(String, Vec<SymbolType>)>,
        current_module: &Quantifier,
    ) -> anyhow::Result<(), anyhow::Error> {
        //dbg!(members.clone());
        let quant_name = current_module.append(quantifier!(Type(name), End));

        let entry = UserTypeTableEntry {
            name: name.clone(),
            isdefined: true,
            //generics: members
            //    .iter()
            //    .filter(|e| e.1.iter().contains(|x| matches!(x, SymbolType::Generic(..))))
            //    .map(|e| e.get_generic_name().clone())
            //    .collect(),
            kind: UserTypeKind::Enum {
                variants: members.clone(),
            },
            is_checked: false,
            //raw: SymbolType::Quant(quant_name.clone()),
            //generic_monomorphs: HashSet::new(),
            //methods: HashSet::new(),
        };
        if self.items.get(&quant_name).is_none() {
            //println!("Adding enum '{}' (id# {})", name, id);
            self.items.insert(quant_name.clone(), Entry::Type(entry.clone()));
            for v in members {
                let variant_entry = UserTypeTableEntry { name: v.0.clone(), isdefined: true, kind: UserTypeKind::VariantInstance { parent: quant_name.clone(), ident: v.0.clone() }, is_checked: false};
                self.items.insert(quant_name.append(Quantifier::Type(v.0, Box::new(Quantifier::End))), Entry::Type(variant_entry));

            }
        } else {
            return Err(TypecheckingError::RedefinedType { name })?;
        }
        Ok(())
    }
}
