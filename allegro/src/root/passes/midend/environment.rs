use std::collections::{HashMap, HashSet};

use thin_vec::ThinVec;

use crate::root::resource::ast::{Ast, Expr, FnArgLimit, Program, SymbolType};

use super::typechecking::PartialType;

#[derive(Debug, Clone)]
pub struct Table<Entry> {
    entries: HashMap<String, Entry>,
}

impl<Entry: std::clone::Clone> Table<Entry> {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    pub fn set(&mut self, k: String, v: Entry) {
        self.entries.insert(k, v);
    }

    pub fn get(&mut self, k: &String) -> Option<Entry> {
        self.entries.get(k).cloned()
    }
}

#[derive(Debug, Clone)]
pub struct ModuleTableEntry {
    name: String,
    id: u32,
    imports_from: HashSet<u32>,
}

#[derive(Debug, Clone)]
pub struct FunctionTableEntry {
    name: String,
    arity: usize,
    arg_types: Vec<SymbolType>,
    return_type: SymbolType,
}

#[derive(Debug, Clone)]
pub struct VariableTableEntry {
    name: String,
    mytype: SymbolType,
}

#[derive(Debug, Clone)]
pub struct UserTypeTableEntry {
    name: String,
    isdefined: bool,
    generics: Vec<String>,
    methods: Vec<FunctionTableEntry>,
    kind: UserTypeKind,
}

#[derive(Debug, Clone)]
pub enum UserTypeKind {
    Struct { fields: Vec<(String, SymbolType)> },
    Enum { variants: Vec<String> },
    Unknown,
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub module_table: Table<ModuleTableEntry>,
    pub function_table: Table<FunctionTableEntry>,
    pub usertype_table: Table<UserTypeTableEntry>,
    pub current_variables: Table<VariableTableEntry>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            module_table: Table::new(),
            function_table: Table::new(),
            current_variables: Table::new(),
            usertype_table: Table::new(),
        }
    }

    pub fn build(&mut self, p: Program) {
        for module in p.modules {
            for astnode in module.body {
                self.build_ast(astnode);
            }
        }
        dbg!(self);
    }

    fn build_ast(&mut self, astnode: Ast) {
        match astnode {
            crate::root::resource::ast::Ast::FnDef {
                name,
                rettype,
                args,
                limits: _,
                body,
            } => {
                self.build_funcdef(name, rettype, args.into(), body);
            }
            Ast::Struct { name, members } => {
                self.build_structdef(name, members)
            }
            Ast::TypeDef { name, funcs } => {
                self.build_typedef(name, funcs.into())
            }
            Ast::WithClause { include } => {}
            _ => todo!("{astnode:?}"),
        }

    }

    fn build_funcdef(
        &mut self,
        name: String,
        rettype: SymbolType,
        args: Vec<(String, SymbolType)>,
        body: Vec<Expr>,
    ) {
        self.function_table.set(
            name.clone(),
            FunctionTableEntry {
                name: name,
                arity: args.len(),
                arg_types: args.iter().map(|e| e.1.clone()).collect(),
                return_type: rettype,
            },
        );
    }

    fn build_structdef(&mut self, name: String, members: Vec<(String, SymbolType)>) {
        if self.usertype_table.get(&name).is_none() {
            self.usertype_table.set(
                name.clone(),
                UserTypeTableEntry {
                    name,
                    isdefined: true,
                    generics: members
                        .iter()
                        .filter(|e| matches!(e.1, SymbolType::Generic(..)))
                        .map(|e| e.0.clone())
                        .collect(),
                    methods: vec![],
                    kind: UserTypeKind::Struct { fields: members },
                },
            );
        } else {
            let methods = self.usertype_table.get(&name).unwrap().methods;
            self.usertype_table.set(
                name.clone(),
                UserTypeTableEntry {
                    name,
                    isdefined: true,
                    generics: members
                        .iter()
                        .filter(|e| matches!(e.1, SymbolType::Generic(..)))
                        .map(|e| e.0.clone())
                        .collect(),
                    methods,
                    kind: UserTypeKind::Struct { fields: members },
                },
            );
        }
    }

    fn build_typedef(&mut self, name: SymbolType, methods: Vec<Ast>) {
        assert!(matches!(name, SymbolType::Custom(..)));
        let name = name.get_custom_name();
        let mut methodvec: Vec<FunctionTableEntry> = vec![];
        for method in methods {
            match method {
                Ast::MethodDef { parent, name, rettype, args, limits, body } => {
                    methodvec.push(FunctionTableEntry {
                        name,
                        arity: args.len(),
                        arg_types: args.iter().map(|e| e.1.clone()).collect(),
                        return_type: rettype,
                    });
                },
                _ => panic!(),
            }
        }
        if self.usertype_table.get(&name).is_none() {
            self.usertype_table.set(
                name.clone(),
                UserTypeTableEntry {
                    name,
                    isdefined: false,
                    generics: vec![],
                    methods: methodvec,
                    kind: UserTypeKind::Unknown
                },
            );
        } else {
            let previous = self.usertype_table.get(&name).unwrap();
            self.usertype_table.set(
                name.clone(),
                UserTypeTableEntry {
                    name,
                    isdefined: true,
                    generics: previous.generics,
                    methods: methodvec,
                    kind: previous.kind
                },
            );

        }
    }
}
