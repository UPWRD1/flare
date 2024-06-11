use std::fmt::{self};

use super::environment::{AKind, ScopeTable};

#[derive(Clone)]
#[repr(usize)]
#[allow(dead_code)]
pub enum Errors {
    FatalInternal = 100,
    FatalOs = 101,
    MissingFile(String) = 102,

    SyntaxError = 200,
    SyntaxMissingChar(String) = 201,
    SyntaxMissingKeyword(String) = 202,
    SyntaxMissingType(String) = 203,
    SyntaxBadArguments(String, usize, usize) = 204,
    SyntaxUnexpectedEnd(String) = 205,

    LogicError = 300,
    LogicReassignmentScoped(String, ScopeTable) = 301,
    LogicReassignment(String) = 302,

    TypeError = 400,
    TypeInvalidType(String, AKind, AKind) = 401,
    TypeInvalidReturn(String, AKind, AKind) = 402,
    TypeIllegalEmptyType(String) = 403,
    TypeNotNumeric(String, AKind) = 404,
    TypeCannotAdd(String, AKind) = 405,

}

impl Errors {
    fn discriminant(&self) -> u8 {
        unsafe { *(self as *const Self as *const u8) }
    }
    
    pub fn get_hint(&self) -> Option<String> {
        match self {
            Errors::FatalInternal | 
            Errors::FatalOs | 
            Errors::MissingFile(_) | 
            Errors::SyntaxError | 
            Errors::SyntaxMissingChar(_) | 
            Errors::SyntaxMissingKeyword(_) | 
            Errors::SyntaxBadArguments(_, _, _) | 
            Errors::SyntaxUnexpectedEnd(_) | 
            Errors::LogicError | 
            Errors::LogicReassignmentScoped(_, _) | 
            Errors::LogicReassignment(_) | 
            Errors::TypeError | 
            Errors::TypeInvalidType(_, _, _) | 
            Errors::TypeInvalidReturn(_, _, _) | 
            Errors::TypeIllegalEmptyType(_) | 
            Errors::TypeNotNumeric(_, _) | 
            Errors::TypeCannotAdd(_, _) => None, 
            Errors::SyntaxMissingType(c) => Some(format!("add: {}", c.clone())),

        }
    }
}



impl fmt::Display for Errors {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let x = match self {
            Errors::FatalInternal => "Fatal internal compiler error".to_string(),
            Errors::FatalOs => "Fatal OS error".to_string(),
            Errors::MissingFile(s) => format!("Could not find file '{}'", s),


            Errors::SyntaxError => "Syntax error".to_string(),
            Errors::SyntaxMissingChar(c) => format!("Expected '{c}'"),
            Errors::SyntaxMissingKeyword(c) => format!("Missing keyword '{c}'"),
            Errors::SyntaxMissingType(_c) => format!("Missing type"),
            Errors::SyntaxBadArguments(name, expected, found) => {
                format!(
                    "Call to operation '{name}' requires {expected} arguments, found {found}"
                )
        
            }
            Errors::SyntaxUnexpectedEnd(s) => {
                format!("Unexpected end to {s}")
            }

            Errors::LogicError => "Logic Error".to_string(),
            Errors::LogicReassignmentScoped(name, e) => {
                let entry = e.clone().get_akind(name.to_string());
                format!(
                    "Item '{name}' cannot be redefined.\n    '{name}' is already defined as {:?}",
                    entry
                )
            },

            Errors::LogicReassignment(name)=> {
                format!(
                    "Item '{name}' cannot be redefined.",
                )
            },

            Errors::TypeError => "Type Error".to_string(),
            Errors::TypeInvalidType(n, e, f) => {
                format!("Expected {} '{n}' to be of type '{}'", f.to_string_pretty(), e.to_string_pretty())
            }
            Errors::TypeInvalidReturn(n, e, f) => {
                format!("Expected {} operation '{n}' to return type '{}'", f.to_string_pretty(), e.to_string_pretty())
            }
            Errors::TypeIllegalEmptyType(s) => {
                format!("Symbol {s} is empty!")
            }
            Errors::TypeNotNumeric(n, f) => {
                format!("Expected {} {n} to be type int or flt", f.to_string_pretty())
            },

            Errors::TypeCannotAdd(n, f) => {
                format!("Cannot add {} with operand {n}", f.to_string_pretty())
            }
        };
        let code: usize = self.discriminant().into();
        write!(f, "{} (error code {})", x, code)
    }
}
