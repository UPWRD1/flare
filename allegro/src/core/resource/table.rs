use crate::core::resource::ast::*;

#[derive(Debug)]
pub struct GlobalTable {
    pub values: Table<ValDecl>,
    pub operations: Table<OpDecl>,
}

impl  GlobalTable {
    pub fn new() -> Self {
        GlobalTable { values: Table::new(), operations:  Table::new() }
    }
}

#[derive(Debug)]
pub struct Table<T> {
    hashes: Vec<i32>,
    values: Vec<T>,
}

impl <T> Table <T> {
    pub fn new() -> Self {
        Table { hashes: vec![], values: vec![] }
    }
}