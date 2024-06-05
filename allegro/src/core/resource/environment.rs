use crate::error;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum AKind {
    TyStr,
    TyInt,
    TyFlt,
    TyBool,
    TyMute,
    TyOp,
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

            //AKind::TyUnknown => return "null".to_string(),
            _ => panic!("Invalid type {:?}", self),
           
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
    pub entries: Vec<Entry>,
    pub enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment { entries: vec![], enclosing: None }
    }

     pub fn new_with_previous(e: Environment) -> Self {
        Environment { entries: vec![], enclosing: Some(Box::new(e)) }
    }

    pub fn define(&mut self, name: String, value: AKind, arity: i32) {
        let e = Entry { name, arity, value };
        for i in &self.entries {
            if *i == e {
                error!("Operation {} already exists!", e.name);
            }
        }
        
        self.entries.push(e);
    }

    pub fn get_akind(&mut self, name: String) -> AKind {
        match self.entries.iter().position(|e| e.name == name) {
            Some(l) => self.entries[l].value.clone(),
            None => {
                if self.enclosing.is_some() {
                    self.enclosing.clone().unwrap().get_akind(name)
                } else {
                    panic!("Unknown value binding {}", name)
                }
            }
        }
    }

    pub fn get_arity(&mut self, name:String) -> i32 {
        match self.entries.iter().position(|e| e.name == name) {
            Some(l) => self.entries[l].arity.clone(),
            None => {
                if self.enclosing.is_some() {
                    self.enclosing.clone().unwrap().get_arity(name)
                } else {
                    panic!("Unknown value binding {}", name)
                }
            }
        }
    }
}