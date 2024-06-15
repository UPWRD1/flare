use crate::{root::legacy_resource::errors::Errors, error};

///Enum representing internal types used for typechecking.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum AKind {
    TyStr,
    TyInt,
    TyFlt,
    TyBool,
    TyMute,
    TyOp(Box<AKind>),
    TyUnknown,
    TyEof,
}

impl AKind {
    pub fn to_ctype(&self) -> String {
        match self {
            Self::TyInt => "int".to_string(),
            AKind::TyStr => "char*".to_string(),
            AKind::TyFlt => "double".to_string(),
            AKind::TyBool => "bool".to_string(),
            AKind::TyMute => "void".to_string(),
            AKind::TyUnknown => "null".to_string(),
            AKind::TyOp(t) => (*t.to_ctype()).to_string(),
            //AKind::TyUnknown => return "null".to_string(),
            _ => panic!("Invalid type {:?}", self),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            AKind::TyStr => "str".to_string(),
            AKind::TyInt => "int".to_string(),
            AKind::TyFlt => "flt".to_string(),
            AKind::TyBool => "bool".to_string(),
            AKind::TyMute => "..".to_string(),
            AKind::TyOp(t) => t.to_string(),
            AKind::TyUnknown => "??".to_string(),
            AKind::TyEof => "EOF".to_string(),
        }
    }

    pub fn to_string_pretty(&self) -> String {
        match self {
            AKind::TyStr => "string".to_string(),
            AKind::TyInt => "integer".to_string(),
            AKind::TyFlt => "float".to_string(),
            AKind::TyBool => "bool".to_string(),
            AKind::TyMute => "mute".to_string(),
            AKind::TyOp(t) => t.to_string(),
            AKind::TyUnknown => "??".to_string(),
            AKind::TyEof => "EOF".to_string(),
        }
    }

    pub fn is_numeric_type(&self) -> bool {
        match &self {
            Self::TyFlt | Self::TyInt => true,
            _ => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        match &self {
            Self::TyUnknown => true,
            _ => false,
        }
    }

    pub fn is_op(&self) -> bool {
        match &self {
            Self::TyOp(_) => true,
            _ => false,
        }
    }

    pub fn extract_op_type(&self) -> Self {
        match self {
            Self::TyOp(t) => *t.clone(),
            _ => panic!("Type {:?} is not an operation!", self),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Entry {
    pub name: String,
    pub arity: i32,
    pub value: AKind,
    pub is_mutable: bool,
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub scope: ScopeTable,
    pub symboltable: Vec<Entry>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ScopeTable {
    pub entries: Vec<Entry>,
    pub parent: Option<Box<ScopeTable>>,
}

impl ScopeTable {
    pub fn new_parent(&mut self) {
        //println!("Creating environment with parent {:?}", self.entries);
        let p = self.clone();
        let s = ScopeTable {
            entries: vec![],
            parent: Some(Box::new(p)),
        };
        self.entries = s.entries;
        self.parent = s.parent;
    }

    pub fn drop_enclosing(&mut self) {
        //println!("Dropping environment containing {:?}", self.entries);
        if self.parent.is_none() {
            panic!("Not Enclosing!");
        } else {
            let p = <Option<Box<ScopeTable>> as Clone>::clone(&self.parent)
                .unwrap()
                .entries
                .to_vec();
            self.entries = p;
            self.parent = None
        }
    }

    pub fn define(&mut self, e: Entry) {
        //println!("Defining {}", e.name);
        for i in &self.entries {
            if *i == e {
                error!(
                    Errors::LogicReassignmentScoped,
                    (i.name.clone(), self.clone())
                );
                //error_nocode!("Operation {} already exists!", e.name);
            }
        }
        self.entries.push(e.clone());
    }

    pub fn get_akind(&mut self, name: String) -> AKind {
        match self.entries.iter().position(|e| e.name == name) {
            Some(l) => self.entries[l].value.clone(),
            None => {
                if self.parent.is_some() {
                    self.parent.clone().unwrap().get_akind(name)
                } else {
                    panic!("Unknown value binding {}", name)
                }
            }
        }
    }

    pub fn get_mutability(&mut self, name: String) -> bool {
        match self.entries.iter().position(|e| e.name == name) {
            Some(l) => self.entries[l].is_mutable,
            None => {
                if self.parent.is_some() {
                    self.parent.clone().unwrap().get_mutability(name)
                } else {
                    panic!("Unknown value binding {}", name)
                }
            }
        }
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            scope: ScopeTable {
                entries: vec![],
                parent: None,
            },
            symboltable: vec![],
        }
    }

    pub fn define(&mut self, name: String, value: AKind, arity: i32, is_mutable: bool) {
        let e: Entry = Entry { name, arity, value, is_mutable};
        self.scope.define(e.clone());
        self.symboltable.push(e);
    }

    pub fn get_akind_scoped(&mut self, name: String) -> AKind {
        self.scope.get_akind(name)
    }

    pub fn is_this_mutable(&mut self, name: String) -> bool {
        self.get_mutability(name)
    }

    pub fn get_akind_symbol(&self, name: &String) -> AKind {
        match self.symboltable.iter().position(|e| &e.name == name) {
            Some(l) => self.symboltable[l].value.clone(),
            None => {
                //if self.parent.is_some() {
                //    self.parent.clone().unwrap().get_akind(name)
                //} else {
                panic!("Binding {} does not exist.", name);
                //}
            }
        }
    }

    pub fn get_arity(&mut self, name: String) -> i32 {
        match self.symboltable.iter().position(|e| e.name == name) {
            Some(l) => self.symboltable[l].arity.clone(),
            None => {
                //if self.parent.is_some() {
                //    self.parent.clone().unwrap().get_akind(name)
                //} else {
                panic!("Binding {} does not exist in this scope.", name);
                //}
            }
        }
    }

    pub fn get_mutability(&mut self, name: String) -> bool {
        match self.symboltable.iter().position(|e| e.name == name) {
            Some(l) => self.symboltable[l].is_mutable,
            None => {
                
                    panic!("Unknown value binding {}", name)
                
            }
        }
    }
}
