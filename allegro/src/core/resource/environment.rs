
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum AKind {
    TyStr,
    TyInt,
    TyFlt,
    TyBool,
    TyOp,
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub name: String,
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

    pub fn define(&mut self, name: String, value: AKind) {
        self.entries.push(Entry { name, value });
    }

    pub fn get(&mut self, name: String) -> AKind {
        match self.entries.iter().position(|e| e.name == name) {
            Some(l) => return self.entries[l].value.clone(),
            None => {
                if self.enclosing.is_some() {
                    return self.enclosing.clone().unwrap().get(name);
                } else {
                    panic!("Unknown value binding {}", name)
                }
            }
        }
    }
}