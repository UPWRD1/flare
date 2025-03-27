use anyhow::anyhow;
use serde::Deserialize;
use serde::Serialize;
use std::fmt::Display;
use std::hash::Hash;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    path::PathBuf,
};

use crate::root::resource::{
    cst::{Cst, Expr, Program, SymbolType},
    errors::TypecheckingError,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Quantifier {
    Root(Box<Self>),
    Module(&'static str, Box<Self>),
    Type(&'static str, Box<Self>),
    Effect(&'static str, Box<Self>),
    Func(&'static str, Box<Self>),
    Variable(&'static str),
    End,
}

impl Quantifier {
    pub fn append(&self, a: Self) -> Self {
        let res = match self {
            Self::Root(quantifier) => Self::Root(Box::new(quantifier.append(a))),
            Self::Module(n, quantifier) => Self::Module(n, Box::new(quantifier.append(a))),
            Self::Type(n, quantifier) => Self::Type(n, Box::new(quantifier.append(a))),
            Self::Effect(n, quantifier) => Self::Type(n, Box::new(quantifier.append(a))),
            Self::Func(n, quantifier) => Self::Func(n, Box::new(quantifier.append(a))),
            Self::Variable(_) => todo!(),
            Self::End => a,
        };
        return res
    }
}

impl Display for Quantifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Quantifier::Root(quantifier) => {f.write_str("Root, ")?; Display::fmt(quantifier, f)},
            Quantifier::Module(n, quantifier) => {f.write_str(&format!("Module {n}, "))?; Display::fmt(quantifier, f)},
            Quantifier::Type(n, quantifier) => {f.write_str(&format!("Type {n}, "))?; Display::fmt(quantifier, f)},
            Quantifier::Effect(n, quantifier) => {f.write_str(&format!("Effect {n}, "))?; Display::fmt(quantifier, f)},

            Quantifier::Func(n, quantifier) => {f.write_str(&format!("Func {n}, "))?; Display::fmt(quantifier, f)},
            Quantifier::Variable(n) => {f.write_str(&format!("Variable {n}, "))?; Ok(())},
            Quantifier::End => {f.write_str(&format!("End"))?; Ok(())},
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
        Quantifier::Module(Box::leak(Box::new($name.clone())), Box::new(quantifier!($($rest)*)))
    };
    
    // Type with child
    (Type($name:expr), $($rest:tt)*) => {
        Quantifier::Type(Box::leak(Box::new($name.clone())), Box::new(quantifier!($($rest)*)))
    };

    (Effect($name:expr), $($rest:tt)*) => {
        Quantifier::Effect(Box::leak(Box::new($name.clone())), Box::new(quantifier!($($rest)*)))
    };
    
    // Func with child
    (Func($name:expr), $($rest:tt)*) => {
        Quantifier::Func(Box::leak(Box::new($name)), Box::new(quantifier!($($rest)*)))
    };
}

#[derive(Debug, Clone)]
pub struct ModuleTableEntry {
    pub imports_from: HashSet<u64>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionTableEntry {
    pub name: String,
    pub method_parent: Option<Quantifier>,
    pub arity: usize,
    pub args: Vec<(String, SymbolType)>,
    pub limits: Vec<Expr>,
    pub effect: Option<EffectEntry>,
    pub return_type: SymbolType,
    pub body: Expr,
    pub is_checked: bool,
    pub is_extern: bool,
    pub variadic: bool,
    pub variables: HashMap<String, VariableTableEntry>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UserTypeTableEntry {
    pub isdefined: bool,
    pub generics: Vec<String>,
    pub kind: UserTypeKind,
    pub is_checked: bool,
    pub raw: SymbolType,
    //pub generic_monomorphs: HashSet<Vec<GenericValue>>,
    pub methods: HashSet<Quantifier>,
}


#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub enum UserTypeKind {
    Struct { fields: Vec<(String, SymbolType)> },
    Enum { variants: Vec<SymbolType> },
}

impl UserTypeKind {
    pub fn get_fields(&self) -> anyhow::Result<Vec<(String, SymbolType)>> {
        match self {
            UserTypeKind::Struct { fields } => Ok(fields.clone()),
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
            UserTypeKind::Enum { variants } => todo!(),
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
            _ => panic!()
        }
    }
}


impl Entry {
    pub fn to_func(&self) -> FunctionTableEntry {
        if let Self::Func(f) = self {
            f.clone()
        } else {
            panic!()
        }
    }

    pub fn to_mut_func(&mut self) -> &mut FunctionTableEntry {
        match self {
            Self::Func(ref mut f) => f,
            _ => panic!()
        }
    }

    pub fn to_ty(&self) -> UserTypeTableEntry {
        if let Self::Type(t) = self {
            t.clone()
        } else {
            panic!()
        }
    }

    pub fn to_mut_ty(&mut self) -> &mut UserTypeTableEntry {
        match self {
            Self::Type(ref mut t) => t,
            _ => panic!()
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
            _ => panic!()
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
        me.items.insert(quantifier!(Root, Effect("$ROOT"), End), Entry::Effect(EffectEntry { name: "$ROOT".to_string(), deps: EffectDeps::Root }));

        me.items.insert(quantifier!(Root, Effect("IO"), End), Entry::Effect(EffectEntry { name: "IO".to_string(), deps: EffectDeps::Product(vec!["$ROOT".to_string()]) }));
        me
    }

    pub fn add_func(&mut self, f: FunctionTableEntry, current_module: &Quantifier) -> anyhow::Result<Quantifier> {
        let n = Box::leak(Box::new(f.name.clone()));
        let q = current_module.append(quantifier!(Func(n), End));
        if let Some(e) = self.items.insert(q.clone(), Entry::Func(f)) {
            Err(anyhow!("Cannot overwrite {e:?}"))
        } else {
            Ok(q)
        }
    }

    pub fn build(&mut self, p: Program) -> anyhow::Result<()> {
        let rev_modules: Vec<&Cst> = p.modules.iter().rev().collect();
        for module in rev_modules {
            //println!("Building {:?}", module.get_module_name());
            self.build_ast(&module, &quantifier!(Root, /*Module(module.get_module_name()),*/ End), false)?;
        }
        Ok(())
        //println!("{:#?}",self);
    }

    fn build_ast(&mut self, astnode: &Cst, current_module: &Quantifier, is_in_defblock: bool) -> anyhow::Result<()> {
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
            } => self.build_funcdef(name, rettype, args, limits.unwrap_or(vec![]), effect, body, current_module, is_in_defblock),

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
        name: String,
        rettype: SymbolType,
        args: Vec<(String, SymbolType)>,
        limits: Vec<Expr>,
        effect: Option<Expr>,
        body: Expr,
        current_module: &Quantifier,
        is_in_defblock: bool
    ) -> anyhow::Result<()> {
        let entry = FunctionTableEntry {
            name: name.clone(),
            method_parent: if is_in_defblock {Some(current_module.clone())} else {None},
            arity: args.len(),
            args,
            limits,
            effect: effect.clone().and_then(|x| Some(EffectEntry{name: x.get_symbol_name().expect("Expected a symbol"), deps: EffectDeps::Unsolved})),
            return_type: rettype,
            body,
            is_checked: false,
            is_extern: false,
            variadic: false,
            variables: HashMap::new(),
            //generic_monomorphs: HashSet::new(),
        };

        //println!("Adding function '{}()' (id# {})", name, id);
        self.add_func(entry, current_module)?;
        Ok(())
    }

    fn build_externdef(
        &mut self,
        name: String,
        rettype: SymbolType,
        args: Vec<SymbolType>,
        variadic: bool,
        effect: Option<Expr>,
    ) -> anyhow::Result<()> {
        let mut mangled_args = vec![];
        for arg in args.iter().enumerate() {
            mangled_args.push((format!("{}_{}", name, arg.0), arg.1.clone()))
        }
        let entry = FunctionTableEntry {
            name: name.clone(),
            method_parent: None,
            arity: args.len(),
            args: mangled_args,
            limits: vec![],
            effect: effect.clone().and_then(|x| Some(EffectEntry{name: x.get_symbol_name().expect("Expected a symbol"), deps: EffectDeps::Unsolved})),
            return_type: rettype,
            body: Expr::Naught,
            is_checked: true, // unsafe?
            is_extern: true,
            variadic,
            variables: HashMap::new(),

            //generic_monomorphs: HashSet::new(),
        };

        //println!("Adding function '{}()' (id# {})", name, id);

        self.items.insert(quantifier!(Root, Func(name.clone()), End), Entry::Func(entry));
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

    fn build_defblock(&mut self,
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

    fn build_assocdef(
        &mut self,
        parent: Quantifier,
        name: String,
        rettype: SymbolType,
        args: Vec<(String, SymbolType)>,
        limits: Vec<Expr>,
        effect: Option<Expr>,
        body: Expr,
    ) -> anyhow::Result<()> {
        //println!("Adding method '{}()' to {}", name, parent);
        

                //generic_monomorphs: HashSet::new(),

            // the type has no methods defined yet
            let the_entry = FunctionTableEntry {
                name: name.clone(),
                method_parent: Some(parent.clone()),
                arity: args.len(),
                args: args,
                limits,
                effect: effect.clone().and(Some(EffectEntry{name: effect.unwrap().get_symbol_name().expect("Expected a symbol"), deps: EffectDeps::Unsolved})),
                return_type: rettype,
                body,
                is_checked: false,
                is_extern: false,
                variadic: false,
                variables: HashMap::new(),

                //generic_monomorphs: HashSet::new(),
            };
            self.items
                .insert(
                    // using raw access to ensure the method is applied
                    parent.append(Quantifier::Func(Box::leak(Box::new(name)), Box::new(Quantifier::End))),
                    the_entry.into()
                );
        
        Ok(())
    }

    fn build_structdef(
        &mut self,
        name: String,
        members: Vec<(String, SymbolType)>,
        current_module: &Quantifier,
    ) -> anyhow::Result<()> {
        let entry = UserTypeTableEntry {
            isdefined: true,
            generics: members
                .iter()
                .filter(|e| e.1.is_generic())
                .map(|e| e.0.clone())
                .collect(),
            kind: UserTypeKind::Struct {
                fields: members.clone(),
            },
            is_checked: false,
            raw: SymbolType::Custom(
                name.clone(),
                members
                    .iter()
                    .filter(|e| e.1.is_generic())
                    .map(|e: &(String, SymbolType)| e.1.clone())
                    .collect(),
            ),
            //generic_monomorphs: HashSet::new(),
            methods: HashSet::new(),
        };
        let quant_name = current_module.append(quantifier!(Type(name), End));
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
        members: Vec<SymbolType>,
        current_module: &Quantifier
    ) -> anyhow::Result<(), anyhow::Error> {
        dbg!(members.clone());
        let entry = UserTypeTableEntry {
            isdefined: true,
            generics: members
                .iter()
                .filter(|e| matches!(e, SymbolType::Generic(..)))
                .map(|e| e.get_generic_name().clone())
                .collect(),
            kind: UserTypeKind::Enum {
                variants: members.clone(),
            },
            is_checked: false,
            raw: SymbolType::Custom(
                name.clone(),
                members.iter().filter(|e| e.is_generic()).cloned().collect(),
            ),
            //generic_monomorphs: HashSet::new(),
            methods: HashSet::new(),
        };
        let quant = current_module.append(quantifier!(Type(name), End));
        if self.items.get(&quant).is_none() {
            //println!("Adding enum '{}' (id# {})", name, id);
            self.items.insert(quant, Entry::Type(entry));
        } else {
            return Err(TypecheckingError::RedefinedType { name })?;
        }
        Ok(())
    }
}
