use serde::Deserialize;
use serde::Serialize;
use std::{
    borrow::Borrow, collections::{HashMap, HashSet}, fmt::{self, Debug}, ops::{Index, IndexMut}, path::PathBuf,
};

use crate::root::resource::{
    ast::{Ast, Expr, FileModule, Program, SymbolType},
    errors::{EnvironmentError, TypecheckingError},
};

#[derive(Debug, Clone)]
pub struct Table<Entry> {
    pub entries: HashMap<String, Entry>,
}

impl<Entry: std::clone::Clone + Debug> Table<Entry> {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: String, v: Entry) {
        //println!("setting {name} : {id} to {v:?}");
        if let std::collections::hash_map::Entry::Vacant(e) = self.entries.entry(name.clone()) {   
            e.insert(v);   
        } else {
            panic!("Cannot overwrite {name} with {v:?}, {name} already exists with value {:?}", self.entries[&name])
        }
    }

    pub fn get_id(&self, k: &String) -> Option<Entry> {
        self.entries.get(k).cloned()

    }

}

impl<Entry: std::clone::Clone + Debug> fmt::Display for Table<Entry>
where
    Entry: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;
        for (key, value) in &self.entries {
            write!(f, "{}: {}, ", key, value)?;
        }
        write!(f, "}}")
    }
}


impl <T, Entry> Index<T> for Table<Entry> where T: std::hash::Hash + Eq, std::string::String: Borrow<T> {
    type Output = Entry;

    fn index(&self, key: T)-> &Self::Output {
        return self.entries.get(&key).expect("Invalid Key!");
    }
}

impl <T, Entry>IndexMut<T> for Table<Entry> where T: std::hash::Hash + Eq, std::string::String: Borrow<T>  {

    fn index_mut(&mut self, key: T) -> &mut Self::Output {
        return self.entries.get_mut(&key).expect("Invalid Key!");
    }
}


#[derive(Debug, Clone)]
pub struct ModuleTableEntry {
    pub imports_from: HashSet<u64>,
}

#[derive(Debug, Clone)]
pub struct FunctionTableEntry {
    pub name: String,
    pub method_parent: Option<String>,
    pub arity: usize,
    pub args: Vec<(String, SymbolType)>,
    pub limits: Vec<Expr>,
    pub return_type: SymbolType,
    pub body: Vec<Expr>,
    pub is_checked: bool,
    pub is_extern: bool,
}

#[derive(Debug, Clone)]
pub struct MethodTableEntry {
    pub the_functions: Table<FunctionTableEntry>,
}

#[derive(Debug, Clone, Hash)]
pub struct VariableTableEntry {
    pub mytype: SymbolType,
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
    pub value: GenericValue
}

#[derive(Debug, Clone)]
pub struct UserTypeTableEntry {
    pub isdefined: bool,
    pub generics: Vec<String>,
    pub kind: UserTypeKind,
    pub is_checked: bool,
    pub raw: SymbolType,
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
}

#[derive(Debug, Clone, Hash)]
pub struct FileTableEntry {
    name: String,
    path: PathBuf,
    src: String,
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub file_table: Table<FileTableEntry>,
    pub module_table: Table<ModuleTableEntry>,
    pub function_table: Table<FunctionTableEntry>,
    pub usertype_table: Table<UserTypeTableEntry>,
    pub method_table: Table<MethodTableEntry>,
    pub current_variables: HashMap<String, HashMap<String, VariableTableEntry>>,
    pub current_generics: Table<GenericTableEntry>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            file_table: Table::new(),
            module_table: Table::new(),
            function_table: Table::new(),
            method_table: Table::new(),
            current_variables: HashMap::new(),
            current_generics: Table::new(),
            usertype_table: Table::new(),
        }
    }

    pub fn add_file(&mut self, name: &str, path: PathBuf, src: &str) -> anyhow::Result<()> {
        let the_name = path
            .file_name()
            .ok_or_else(|| EnvironmentError::BadFile { name: name.to_string() })?
            .to_str()
            .unwrap()
            .to_string();
        self.file_table.set(
            the_name,
            FileTableEntry {
                name: name.to_string(),
                path,
                src: src.to_string(),
            },
        );
        Ok(())
    }

    // pub fn convert_symboltype(&self, t: &SymbolType) -> PartialType {
    //     match t {
    //         SymbolType::Int => PartialType::Int,
    //         SymbolType::Uint => PartialType::Uint,
    //         SymbolType::Word => todo!(),
    //         SymbolType::Byte => PartialType::Byte,
    //         SymbolType::Flt => PartialType::Flt,
    //         SymbolType::Str => PartialType::Str,
    //         SymbolType::Char => PartialType::Char,
    //         SymbolType::Bool => PartialType::Bool,
    //         SymbolType::Mut(symbol_type) => {
    //             PartialType::Mut(Box::leak(Box::new(self.convert_symboltype(symbol_type))))
    //         }
    //         SymbolType::Fn(vec, symbol_type, _) => PartialType::Fn(
    //             Box::leak(Box::new(
    //                 vec.iter()
    //                     .map(|e| self.convert_symboltype(e))
    //                     .collect::<Vec<PartialType>>(),
    //             )),
    //             Box::leak(Box::new(self.convert_symboltype(symbol_type))),
    //         ),
    //         SymbolType::MethodFn { parent, f } => self.convert_symboltype(f),
    //         SymbolType::Naught => PartialType::Naught,
    //         SymbolType::Pointer(symbol_type) => PartialType::Pointer(Box::leak(Box::new(self.convert_symboltype(symbol_type)))),
    //         SymbolType::Custom(name, generics) => self.usertype_table.get_name(&name).ok_or(TypecheckingError::UndefinedType{name: name.to_string()}).unwrap().kind.convert(),
    //         SymbolType::Generic(name) => {
    //             PartialType::Generic(Box::leak(Box::new(name.clone())))
    //         }
    //             ,
    //         _ => todo!("{t:?}")
    //     }
    // }
    pub fn build(&mut self, p: Program) -> anyhow::Result<()>{
        let reversed_modules: Vec<&FileModule> = p.modules.iter().rev().collect();
        for module in reversed_modules {
            //println!("Building {:?}", module.name);
            self.build_module(module)?;
        }
        Ok(())
        //println!("{:#?}",self);
    }

    fn build_module(&mut self, m: &FileModule) -> anyhow::Result<()> {
        let imports_from: HashSet<u64> = HashSet::new();
        // for i in m.imports_from.clone().unwrap() {
        //     let import_id = self.module_table.get_name(&i);
        //     dbg!(&import_id);
        //     if import_id.is_some() {
        //         imports_from.insert(import_id.unwrap().id);
        //     }
        // }
        self.module_table
            .set(m.name.clone(), ModuleTableEntry { imports_from });
        for astnode in &m.body {
            self.build_ast(astnode)?
        }
        Ok(())
    }
    fn build_ast(&mut self, astnode: &Ast) -> anyhow::Result<()> {
        match astnode.clone() {
            Ast::FnDef {
                name,
                rettype,
                args,
                limits,
                body,
            } => self.build_funcdef(name, rettype, args, limits.unwrap_or(vec![]), body),
            Ast::MethodDef {
                parent,
                name,
                rettype,
                args,
                limits,
                body,
            } => self.build_methoddef(parent, name, rettype, args, limits.unwrap_or(vec![]), body),
            Ast::Struct {
                name,
                members,
            } => self.build_structdef(name, members),
            Ast::Enum {
                name,
                members,
            } => self.build_enumdef(name, members),
            //Ast::TypeDef { name, funcs } => self.build_typedef(name, funcs.into()),
            Ast::WithClause { include: _ } => Ok(()),
            Ast::ExternClause { name, args, ret } => self.build_externdef(name, ret, args),
            _ => todo!("{astnode:?}"),
        }
    }

    fn build_funcdef(
        &mut self,
        name: String,
        rettype: SymbolType,
        args: Vec<(String, SymbolType)>,
        limits: Vec<Expr>,
        body: Vec<Expr>,
    ) -> anyhow::Result<()> {
        let entry = FunctionTableEntry {
            name: name.clone(),
            method_parent: None,
            arity: args.len(),
            args,
            limits,
            return_type: rettype,
            body,
            is_checked: false,
            is_extern: false,
        };

        //println!("Adding function '{}()' (id# {})", name, id);

        self.function_table.set(name.clone(), entry);
        Ok(())
    }

    fn build_externdef(
        &mut self,
        name: String,
        rettype: SymbolType,
        args: Vec<SymbolType>,
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
            return_type: rettype,
            body: vec![],
            is_checked: true, // unsafe?
            is_extern: true,
        };

        //println!("Adding function '{}()' (id# {})", name, id);

        self.function_table.set(name.clone(), entry);
        Ok(())
    }

    fn build_methoddef(
        &mut self,
        parent: String,
        name: String,
        rettype: SymbolType,
        args: Vec<(String, SymbolType)>,
        limits: Vec<Expr>,
        body: Vec<Expr>,
    ) -> anyhow::Result<()> {
        //println!("Adding method '{}()' to {}", name, parent);
        if self.method_table.get_id(&parent).is_some() {
            // the type already has some methods
            let the_entry = FunctionTableEntry {
                name: name.clone(),
                method_parent: Some(parent.clone()),
                arity: args.len(),
                args,
                limits,
                return_type: rettype,
                body,
                is_checked: false,
                is_extern: false,
            };
            let mut the_method_entry = self.method_table.get_id(&parent).ok_or(TypecheckingError::NoMethods { name: parent.clone() })?;
            the_method_entry.the_functions.entries.insert(name, the_entry);
            self.method_table.entries.insert(parent, the_method_entry);
        } else {
            // the type has no methods defined yet
            let the_entry = FunctionTableEntry {
                name: name.clone(),
                method_parent: Some(parent.clone()),
                arity: args.len(),
                args,
                limits,
                return_type: rettype,
                body,
                is_checked: false,
                is_extern: false,
            };
            let mut the_table = Table::new();
            the_table.set(name, the_entry);
            self.method_table.entries.insert( // using raw access to ensure the method is applied
                parent,
                MethodTableEntry {
                    the_functions: the_table,
                },
            );
        }
Ok(())    }

    fn build_structdef(
        &mut self,
        name: String,
        members: Vec<(String, SymbolType)>,
    ) -> anyhow::Result<()> {
        let entry = UserTypeTableEntry {
            isdefined: true,
            generics: members
                .iter()
                .filter(|e| e.1.is_generic())
                .map(|e| e.0.clone())
                .collect(),
            kind: UserTypeKind::Struct { fields: members.clone() },
            is_checked: false,
            raw: SymbolType::Custom(name.clone(),  members
                .iter()
                .filter(|e| e.1.is_generic())
                .map(|e: &(String, SymbolType)| e.1.clone())
                .collect(),)
        };

        if self.usertype_table.get_id(&name).is_none() {
            //println!("Adding struct '{}' (id# {})", name, id);
            self.usertype_table.set(name, entry);
        } else {
            return Err(TypecheckingError::RedefinedType { name: name.clone() })?;
        }
        Ok(())
    }

    fn build_enumdef(
        &mut self,
        name: String,
        members: Vec<SymbolType>,
    ) -> anyhow::Result<(), anyhow::Error> {
        dbg!(members.clone());
        let entry = UserTypeTableEntry {
            isdefined: true,
            generics: members
                .iter()
                .filter(|e| matches!(e, SymbolType::Generic(..)))
                .map(|e| e.get_generic_name().clone())
                .collect(),
            kind: UserTypeKind::Enum { variants: members.clone() },
            is_checked: false,
            raw: SymbolType::Custom(name.clone(), members
            .iter()
            .filter(|e| e.is_generic())
            .map(|e: &SymbolType| e.clone())
            .collect(),
        )
        };

        if self.usertype_table.get_id(&name).is_none() {
            //println!("Adding enum '{}' (id# {})", name, id);
            self.usertype_table.set(name, entry);
        } else {
            return Err(TypecheckingError::RedefinedType { name })?;
        }
        Ok(())
    }
}
