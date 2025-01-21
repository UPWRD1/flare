use std::error::Error;
use thiserror::Error;

use crate::root::passes::midend::typechecking::PartialType;

pub trait CompilerError: Sized + Error {}


#[derive(Error, Debug)]
pub enum TypecheckingError {
    #[error("Missing Main Function")]
    MissingMainFunction,
    #[error("Invalid function return type (expected {expected:?}, got {found:?})")]
    InvalidFunctionReturnType {
        expected: PartialType,
        found: PartialType,
    },
    #[error("The variable '{name}' is not defined")]
    UndefinedVariable { name: String },

    #[error("The function '{name}' is not defined")]
    UndefinedFunction { name: String },

    #[error("The type '{obj}' has no method '{name}'")]
    UndefinedMethod { obj: String, name: String },

    #[error("The type '{name}' is not defined")]
    UndefinedType { name: String },

    #[error("Variable '{name}' has already been defined and is immutable")]
    NonMutableReassignment { name: String },

    #[error("Type '{name}' has already been defined")]
    RedefinedType {name: String},
    
    #[error("Type '{name}' has no defined methods")]
    NoMethods {name: String},
}

impl CompilerError for TypecheckingError {}

#[derive(Error, Debug)]
pub enum ParsingError {
    #[error("Error while parsing: '{msg}'")]
    NonspecificParsingError { msg: String }
}

impl CompilerError for ParsingError {}
