use std::fmt;

use super::environment::ScopeTable;

#[derive(Clone)]
#[repr(usize)]
pub enum Errors {
    FatalInternal = 100,
    FatalOs = 101,

    SyntaxError = 200,
    SyntaxMissingChar(String) = 201,
    SyntaxMissingKeyword(String) = 202,
    SyntaxBadArguments(
        String,
        usize,
        usize
    ) = 203,
    
    LogicError = 300,
    LogicReassignmentScoped(String, ScopeTable) = 301,
}

impl fmt::Display for Errors {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let x = match self {
            Errors::FatalInternal => "Fatal internal compiler error".to_string(),
            Errors::FatalOs => "Fatal OS error".to_string(),
            Errors::SyntaxError => "Syntax error".to_string(),
            Errors::SyntaxMissingChar(c) => format!("Expected '{c}'"),
            Errors::SyntaxMissingKeyword(c) => format!("Missing keyword '{c}'"),
            Errors::SyntaxBadArguments(name, expected, found) => {
                let y =format!("Call to operation '{name}' requires {expected} arguments, found {found}");
                y
            },
            Errors::LogicError => "Logic Error".to_string(),
            Errors::LogicReassignmentScoped(name, e) => {
                let entry = e.clone().get_akind(name.to_string());
                format!("Item '{name}' cannot be redefined.\n    '{name}' is already defined as {:?}", entry)},
                };
        write!(f, "{}", x)
    }
}
