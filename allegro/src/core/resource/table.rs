use crate::core::resource::ast::*;

#[derive(Debug, Clone)]
pub struct GlobalTable {
    pub values: Table<ValDecl>,
    pub operations: Table<OpDecl>,
}

impl  GlobalTable {
    pub fn new() -> Self {
        GlobalTable { values: Table::new(), operations:  Table::new() }
    }
}

#[derive(Debug, Clone)]
pub struct Table<T> {
    pub entries: Vec<Entry<T>>,
}

#[derive(Debug, Clone)]
pub struct Entry<T> {
    pub hash: String,
    pub value: Option<T>,
}

impl <T> Table <T> {
    pub fn new() -> Self {
        Table { entries: vec![] }
    }
}