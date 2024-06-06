use crate::error;

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

    pub fn is_numeric_type(&self) -> bool {
        match &self {
            Self::TyFlt | Self::TyInt => true,
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
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub scope: ScopeTable,
    pub symboltable: Vec<Entry>,
}

#[derive(Debug, Clone)]
pub struct ScopeTable {
    pub entries: Vec<Entry>,
    pub parent: Option<Box<ScopeTable>>,
}

impl ScopeTable {
    pub fn new_parent(&self) -> Self {
        let p = self.clone();
        ScopeTable {
                entries: vec![],
                parent: Some(Box::new(p)),
            }
        
    }

    pub fn drop_enclosing(&self) -> Self {
        //if self.parent.is_none() {
        return ScopeTable {
            entries: vec![],
            parent: None,
        };
        //} else {
        //let p = *<Option<Box<Environment>> as Clone>::clone(&self.parent).unwrap();
        //p
        //}
    }

    pub fn get_arity(&mut self, name: String) -> i32 {
        match self.entries.iter().position(|e| e.name == name) {
            Some(l) => self.entries[l].arity,
            None => {
                if self.parent.is_some() {
                    self.clone().get_arity(name)
                } else {
                    panic!("Unknown value binding {}", name)
                }
            }
        }
    }

    pub fn define(&mut self, name: String, value: AKind, arity: i32) {
        let e: Entry = Entry { name, arity, value };
        for i in &self.entries {
            if *i == e {
                error!("Operation {} already exists!", e.name);
            }
        }
        self.entries.push(e.clone());
    }

    pub fn get_akind(&mut self, name: String) -> AKind {
        match self.entries.iter().position(|e| e.name == name) {
            Some(l) => self.entries[l].value.clone(),
            None => {
                //if self.parent.is_some() {
                //    self.parent.clone().unwrap().get_akind(name)
                //} else {
                panic!("Unknown value binding {}", name)
                //}
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
    
    pub fn define(&mut self, name: String, value: AKind, arity: i32) {
        let e: Entry = Entry { name: name.clone(), arity: arity.clone(), value: value.clone() };
        self.scope.define(name, value, arity);
        //self.symboltable.push(e);
    }

    pub fn get_akind_scoped(&mut self, name: String) -> AKind {
        self.scope.get_akind(name)
    }

    pub fn get_akind_symbol(&mut self, name: String) -> AKind {
        match self.symboltable.iter().position(|e| e.name == name) {
            Some(l) => self.symboltable[l].value.clone(),
            None => {
                //if self.parent.is_some() {
                //    self.parent.clone().unwrap().get_akind(name)
                //} else {
                error!("Binding {} does not exist in this scope.", name);
                //}
            }
        }    }

    pub fn get_arity(&mut self, name: String) -> i32 {
        self.scope.get_arity(name)
    }
}
