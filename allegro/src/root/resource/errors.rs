use std::fmt;

use super::environment::{AKind, ScopeTable};

#[derive(Clone)]
#[repr(usize)]
pub enum Errors {
    FatalInternal = 100,
    FatalOs = 101,

    SyntaxError = 200,
    SyntaxMissingChar(String) = 201,
    SyntaxMissingKeyword(String) = 202,
    SyntaxMissingType(String) = 203,
    SyntaxBadArguments(String, usize, usize) = 204,

    LogicError = 300,
    LogicReassignmentScoped(String, ScopeTable) = 301,

    TypeError = 400,
    TypeInvalidType(String, AKind, AKind) = 401,
    TypeInvalidReturn(String, AKind, AKind) = 402,
}

impl fmt::Display for Errors {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let x = match self {
            Errors::FatalInternal => "Fatal internal compiler error".to_string(),
            Errors::FatalOs => "Fatal OS error".to_string(),
            Errors::SyntaxError => "Syntax error".to_string(),
            Errors::SyntaxMissingChar(c) => format!("Expected '{c}'"),
            Errors::SyntaxMissingKeyword(c) => format!("Missing keyword '{c}'"),
            Errors::SyntaxMissingType(c) => format!("Missing type.\n\tHint: add '{c}'"),
            Errors::SyntaxBadArguments(name, expected, found) => {
                format!(
                    "Call to operation '{name}' requires {expected} arguments, found {found}"
                )
        
            }
            Errors::LogicError => "Logic Error".to_string(),
            Errors::LogicReassignmentScoped(name, e) => {
                let entry = e.clone().get_akind(name.to_string());
                format!(
                    "Item '{name}' cannot be redefined.\n    '{name}' is already defined as {:?}",
                    entry
                )
            },

            Errors::TypeError => "Type Error".to_string(),
            Errors::TypeInvalidType(n, e, f) => {
                format!("Expected '{n}' to be of type '{}' but found type '{}'", e.to_string(), f.to_string())
            }
            Errors::TypeInvalidReturn(n, e, f) => {
                format!("Expected '{n}' to be of type '{}' but found type '{}'", e.to_string(), f.to_string())
            }
        };
        write!(f, "{}", x)
    }
}
