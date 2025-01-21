use rayon::prelude::*;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    path::PathBuf,
};

use crate::root::resource::{
    ast::{Ast, Expr, FileModule, Program, SymbolType},
    errors::TypecheckingError,
};

#[derive(Debug, Clone)]
pub struct Table<Entry> {
    entries: HashMap<(usize, String), Entry>,
}

impl<Entry: std::clone::Clone + Debug> Table<Entry> {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: &String, id: usize, v: Entry) {
        //println!("setting {name} : {id} to {v:?}");
        if !self.entries.contains_key(&(id, name.to_string())) {
            self.entries.insert((id, name.to_string()), v);
        }
    }

    pub fn get_id(&self, k: &usize) -> Option<Entry> {
        let res: Vec<(&(usize, String), &Entry)> =
            self.entries.iter().filter(|e| e.0 .0 == *k).collect();
        match res.get(0) {
            Some(e) => Some(e.1.clone()),
            None => None,
        }
    }

    pub fn get_name(&self, k: &str) -> Option<Entry> {
        for e in &self.entries {
            if e.0 .1 == *k {
                return Some(e.1.clone());
            }
        }
        None
    }

    pub fn get_id_from_name(&self, k: &String) -> Option<usize> {
        for e in &self.entries {
            if e.0 .1 == *k {
                return Some(e.0 .0);
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
pub struct ModuleTableEntry {
    pub imports_from: HashSet<usize>,
    pub id: usize,
}

#[derive(Debug, Clone)]
pub struct FunctionTableEntry {
    pub name: String,
    pub arity: usize,
    pub args: Vec<(String, SymbolType)>,
    pub limits: Vec<Expr>,
    pub return_type: SymbolType,
    pub body: Vec<Expr>,
    pub is_checked: bool,
}

#[derive(Debug, Clone)]
pub struct MethodTableEntry {
    pub the_functions: Vec<FunctionTableEntry>,
}

#[derive(Debug, Clone)]
pub struct VariableTableEntry {
    pub name: String,
    pub mytype: SymbolType,
}

#[derive(Debug, Clone)]
pub struct GenericTableEntry {}

#[derive(Debug, Clone)]
pub struct UserTypeTableEntry {
    pub name: String,
    pub isdefined: bool,
    pub generics: Vec<String>,
    pub kind: UserTypeKind,
    pub is_checked: bool,
}

#[derive(Debug, Clone)]
pub enum UserTypeKind {
    Struct { fields: Vec<(String, SymbolType)> },
    Enum { variants: Vec<SymbolType> },
    Unknown,
}

impl UserTypeKind {
    pub fn get_fields(&self) -> anyhow::Result<Vec<(String, SymbolType)>> {
        match self {
            UserTypeKind::Struct { fields } => Ok(fields.clone()),
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FileTableEntry {
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
    pub current_variables: Table<VariableTableEntry>,
    pub current_generics: Table<GenericTableEntry>,
    pub currentfreeid: usize,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            file_table: Table::new(),
            module_table: Table::new(),
            function_table: Table::new(),
            method_table: Table::new(),
            current_variables: Table::new(),
            current_generics: Table::new(),
            usertype_table: Table::new(),
            currentfreeid: 0,
        }
    }

    pub fn get_new_id(&mut self) -> usize {
        self.currentfreeid += 1;
        self.currentfreeid
    }

    pub fn add_file(&mut self, name: &String, path: PathBuf, src: &String) {
        if !self.file_table.get_name(name).is_some() {
            let id = self.get_new_id();
            //println!("Adding {} (id# {}) to filetable", name, id);
            self.file_table.set(
                name,
                id,
                FileTableEntry {
                    path,
                    src: src.clone(),
                },
            );
        }
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
    pub fn build(&mut self, p: Program) {
        let reversed_modules: Vec<&FileModule> = p.modules.iter().rev().collect();
        for module in reversed_modules {
            //println!("Building {:?}", module.name);
            self.build_module(&module);
        }
        //println!("{:#?}",self);
    }

    fn build_module(&mut self, m: &FileModule) -> anyhow::Result<()> {
        let id: usize = self.get_new_id();
        let imports_from: HashSet<usize> = HashSet::new();
        // for i in m.imports_from.clone().unwrap() {
        //     let import_id = self.module_table.get_name(&i);
        //     dbg!(&import_id);
        //     if import_id.is_some() {
        //         imports_from.insert(import_id.unwrap().id);
        //     }
        // }
        self.module_table
            .set(&m.name, id, ModuleTableEntry { id, imports_from });
        for astnode in &m.body {
            self.build_ast(&astnode)?
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
                methods,
            } => self.build_structdef(name, members, methods),
            Ast::Enum {
                name,
                members,
                methods,
            } => self.build_enumdef(name, members, methods),
            //Ast::TypeDef { name, funcs } => self.build_typedef(name, funcs.into()),
            Ast::WithClause { include: _ } => Ok(()),
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
        let id: usize = self.get_new_id();
        //println!("Adding function '{}()' (id# {})", name, id);

        self.function_table.set(
            &name.clone(),
            id,
            FunctionTableEntry {
                name,
                arity: args.len(),
                args,
                limits,
                return_type: rettype,
                body: body,
                is_checked: false,
            },
        );
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
        println!("Adding method '{}()' to {}", name,  parent);
        if let Some(e) = self.method_table.get_name(&parent) {
            let parent_id = self.method_table.get_id_from_name(&parent).ok_or(
                TypecheckingError::UndefinedType {
                    name: parent.clone(),
                },
            )?;
            let mut new_functions = e.clone();
            new_functions.the_functions.push(FunctionTableEntry {
                name,
                arity: args.len(),
                args,
                limits,
                return_type: rettype,
                body: body,
                is_checked: false,
            });

            self.method_table.set(&parent, parent_id, new_functions);
        } else {
            let id: usize = self.get_new_id();
            self.method_table.set(
                &parent,
                id,
                MethodTableEntry {
                    the_functions: vec![FunctionTableEntry {
                        name,
                        arity: args.len(),
                        args,
                        limits,
                        return_type: rettype,
                        body: body,
                        is_checked: false,
                    }],
                },
            );
        }
        Ok(())
    }

    fn build_structdef(
        &mut self,
        name: String,
        members: Vec<(String, SymbolType)>,
        methods: Vec<Ast>,
    ) -> anyhow::Result<()> {
        if self.usertype_table.get_name(&name).is_none() {
            let id = self.get_new_id();
            //println!("Adding struct '{}' (id# {})", name, id);
            self.usertype_table.set(
                &name.clone(),
                id,
                UserTypeTableEntry {
                    name,
                    isdefined: true,
                    generics: members
                        .par_iter()
                        .filter(|e| matches!(e.1, SymbolType::Generic(..)))
                        .map(|e| e.0.clone())
                        .collect(),
                    kind: UserTypeKind::Struct { fields: members },
                    is_checked: false,
                },
            );
        } else {
            return Err(TypecheckingError::RedefinedType { name: name.clone() })?;
        }
        Ok(())
    }

    fn build_enumdef(
        &mut self,
        name: String,
        members: Vec<SymbolType>,
        methods: Vec<Ast>,
    ) -> anyhow::Result<(), anyhow::Error> {
        if self.usertype_table.get_name(&name).is_none() {
            let id = self.get_new_id();
            //println!("Adding enum '{}' (id# {})", name, id);
            self.usertype_table.set(
                &name.clone(),
                id,
                UserTypeTableEntry {
                    name,
                    isdefined: true,
                    generics: members
                        .par_iter()
                        .filter(|e| matches!(e, SymbolType::Generic(..)))
                        .map(|e| e.get_generic_name().clone())
                        .collect(),
                    kind: UserTypeKind::Enum { variants: members },
                    is_checked: false,
                },
            );
        } else {
            return Err(TypecheckingError::RedefinedType { name: name.clone() })?;
        }
        Ok(())
    }
}
